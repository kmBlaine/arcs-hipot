//! Device protocol handling and command execution

use std::{ io };
use tokio::io::{ AsyncWriteExt, AsyncReadExt };
use crate::{
    cmd::{ CmdSet, CmdDisplayFactory, SciMultiSeqDisplay, SciSingleSeqDisplay, AssociatedResearchDisplay },
    test_data::{ TestData, SciTestData, ParseTestDataErr }
};
// use units::{ Ampere, Volt, Ohm, Second, Milli, Micro, Base, Kilo };

struct Executor<T, D>
{
    line_ending: &'static str,
    io_handle: T,
    read_buf: Vec<u8>,
    // This marker exists so we can use a generic in order to know which serialization delegate to
    // at compile time use. However, since constructing an `fmt::Display` implementor for the
    // byte-coded commands is a purely functional operation, there is no actual data or state to store
    _cmd_serializer: std::marker::PhantomData<D>,
}

impl <T, D> Executor<T, D>
    where T: AsyncReadExt + AsyncWriteExt + Unpin + Send,
          D: CmdDisplayFactory
{
    fn with(line_ending: &'static str, io_handle: T) -> Self
    {
        Self {
            line_ending: line_ending,
            io_handle: io_handle,
            read_buf: Vec::with_capacity(128),
            _cmd_serializer: std::marker::PhantomData,
        }
    }

    /// Drops the first `n` bytes from the read buffer
    ///
    /// Drops all bytes if `n >= self.read_buf.len()`
    fn drop_first(&mut self, n: usize)
    {
        if n >= self.read_buf.len() {
            self.read_buf.clear();
        }
        else {
            // relocate any bytes after the Nth byte to index 0
            self.read_buf.rotate_left(n);
            // chop off the bytes we just consumed
            self.read_buf.truncate(self.read_buf.len() - n);
            // shrink the buffer's allocation to keep memory usage down
            self.read_buf.shrink_to(128);
        }
    }

    /// Returns the index of the first linefeed in the read buffer if any attempting to start looking
    /// at the suggested index.
    ///
    /// If the suggested index is out of bounds, then `None` is returned.
    fn find_line_ending(&self, start_hint: usize) -> Option<usize>
    {
        for index in start_hint..self.read_buf.len() {
            if self.read_buf[index] == 0x0A {
                return Some(index);
            }
        }

        None
    }

    /// Reads a line (series of bytes terminated by `LF` / 0x0A) into the read buffer and returns
    /// how many bytes are in the line
    ///
    /// # Cancel Safety
    /// This function is cancel safe e.g. when used inside of a `tokio::select!`. This is because
    /// this will never destroy contents of the read buffer -- only append.
    async fn read_line(&mut self) -> Result<usize, std::io::Error>
    {
        let mut total_bytes_read = 0;
        // try to find the ending in already-buffered data first
        let mut end_index = self.find_line_ending(0);

        while end_index.is_none() {
            // TODO this could avoid a double copy with some unsafe trickery
            // fully initialize the vector and then use slicing to write directly into it
            // however this requires manually setting the length field which is tricky to get right
            let mut temp_buf = [0u8; 64];

            match self.io_handle.read(&mut temp_buf[..]).await {
                Ok(bytes_read) => {
                    let prior_end = total_bytes_read;
                    total_bytes_read += bytes_read;
                    self.read_buf.extend_from_slice(&temp_buf[..bytes_read]);
                    end_index = self.find_line_ending(prior_end);
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }

        return Ok(end_index.unwrap() + 1);
    }

    /// Executes the given command, sending to the device and checking the response
    ///
    /// When the device responds with a positive ACK, `Ok` is returned. When an error occurrs on
    /// reading/writing to the stream, `Err` is returned. When the command is successfully written and
    /// the device responds with a negative ACK (NAK), an error is returned with an `ErrorKind`
    /// depending on the nature if the command. If the command asks the device set up a test it does not
    /// support -- e.g. a ground bond when it is a hipot-only tester -- then an I/O error with the kind
    /// `Unsupported` is returned. In all other NAK cases, `InvalidData` is returned.
    async fn exec_cmd(&mut self, cmd: CmdSet) -> Result<usize, io::Error>
    {
        let serialized = format!("{}{}", D::display_cmd(cmd), self.line_ending);
        self.io_handle.write_all(serialized.as_bytes()).await?;
        let response_len = self.read_line().await?;

        if response_len < 2 || self.read_buf[0] == 0x15 {
            self.drop_first(response_len);
            Err(io::Error::from(io::ErrorKind::InvalidInput))
        }
        else {
            Ok(response_len)
        }
    }

    async fn exec_all(&mut self, cmds: &[CmdSet]) -> Result<(), io::Error>
    {
        for cmd in cmds.iter() {
            let response_len = self.exec_cmd(cmd.clone()).await?;
            self.drop_first(response_len);
        }

        Ok(())
    }

    fn get_string(&mut self, size: usize) -> Result<String, std::string::FromUtf8Error>
    {
        let mut response = Vec::with_capacity(size);
        // These devices will reply with extended ASCII, specifically an omega 'Ω' for resistance values
        // thus we need to do some simple substitutions to turn into proper UTF8
        for byte in &self.read_buf[0..size] {
            if *byte == 0xEAu8 {
                let start_loc = response.len();
                let omega = 'Ω';
                response.resize(start_loc + omega.len_utf8(), 0);
                omega.encode_utf8(&mut response[start_loc..]);
            }
            else {
                response.push(*byte);
            }
        }
        self.drop_first(size);

        String::from_utf8(response)
    }

    async fn get_test_data<P>(
        &mut self,
        step_num: u32,
    )
        -> Result<P, ParseTestDataErr>

        where P: std::str::FromStr<Err = ParseTestDataErr>,
    {
        let response_len = self.exec_cmd(CmdSet::GetTestData(step_num)).await?;
        let response = self.get_string(response_len)?;
        let data = response.parse::<P>()?;
        
        Ok(data)
    }
}

pub struct SciSingleExecutor<T>
{
    delegate: Executor<T, SciSingleSeqDisplay>,
}

impl <T> SciSingleExecutor<T>
    where T: AsyncReadExt + AsyncWriteExt + Unpin + Send
{
    pub fn with(io_handle: T) -> Self
    {
        Self {
            delegate: Executor::with("\n", io_handle),
        }
    }

    pub async fn exec_cmd(&mut self, cmd: CmdSet) -> Result<usize, std::io::Error>
    {
        self.delegate.exec_cmd(cmd).await
    }

    pub async fn exec_all(&mut self, cmds: &[CmdSet]) -> Result<(), std::io::Error>
    {
        self.delegate.exec_all(cmds).await
    }

    pub async fn get_test_data(&mut self, step_num: u32) -> Result<TestData, ParseTestDataErr>
    {
        let data = self.delegate.get_test_data::<SciTestData>(step_num).await?;
        Ok(data.into())
    }
}

pub struct SciMultiExecutor<T>
{
    delegate: Executor<T, SciMultiSeqDisplay>,
}

impl <T> SciMultiExecutor<T>
    where T: AsyncReadExt + AsyncWriteExt + Unpin + Send
{
    pub fn with(io_handle: T) -> Self
    {
        Self {
            delegate: Executor::with("\n", io_handle),
        }
    }

    pub async fn exec_cmd(&mut self, cmd: CmdSet) -> Result<usize, std::io::Error>
    {
        self.delegate.exec_cmd(cmd).await
    }

    pub async fn exec_all(&mut self, cmds: &[CmdSet]) -> Result<(), std::io::Error>
    {
        self.delegate.exec_all(cmds).await
    }

    pub async fn get_test_data(&mut self, step_num: u32) -> Result<TestData, ParseTestDataErr>
    {
        let data = self.delegate.get_test_data::<SciTestData>(step_num).await?;
        Ok(data.into())
    }
}

pub struct ArExecutor<T>
{
    delegate: Executor<T, AssociatedResearchDisplay>,
}

impl <T> ArExecutor<T>
    where T: AsyncReadExt + AsyncWriteExt + Unpin + Send
{
    pub fn with(io_handle: T) -> Self
    {
        Self {
            delegate: Executor::with("\r\n", io_handle),
        }
    }

    pub async fn exec_cmd(&mut self, cmd: CmdSet) -> Result<usize, std::io::Error>
    {
        self.delegate.exec_cmd(cmd).await
    }

    pub async fn exec_all(&mut self, cmds: &[CmdSet]) -> Result<(), std::io::Error>
    {
        self.delegate.exec_all(cmds).await
    }

    pub async fn get_test_data(&mut self, _step_num: u32) -> Result<TestData, ParseTestDataErr>
    {
        unimplemented!("Parsing of Associated Research device memory is not complete")
    }
}
