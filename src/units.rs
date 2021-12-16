
use std::{
    fmt::{ self, Write },
    ops::{ Add, Sub, Mul, Div },
    cmp::{ PartialEq, PartialOrd, Eq, Ord, Ordering },
};


/// Unsigned binary coded decimal value with nano (10e-9) precision
#[derive(Debug, Clone, Copy)]
struct BcdNano
{
    nanos: u64,
}

impl Add for BcdNano
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self
    {
        Self { nanos: self.nanos + rhs.nanos }
    }
}

impl Sub for BcdNano
{
    type Output = Self;

    fn sub(self, rhs: Self) -> Self
    {
        Self { nanos: self.nanos - rhs.nanos }
    }
}

impl Mul for BcdNano
{
    type Output = Self;

    fn mul(self, rhs: Self) -> Self
    {
        Self { nanos: self.nanos * rhs.nanos }
    }
}

impl Div for BcdNano
{
    type Output = Self;

    fn div(self, rhs: Self) -> Self
    {
        Self { nanos: self.nanos / rhs.nanos }
    }
}

impl PartialEq for BcdNano
{
    fn eq(&self, rhs: &Self) -> bool
    {
        self.nanos == rhs.nanos
    }
}

impl PartialOrd for BcdNano
{
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering>
    {
        self.nanos.partial_cmp(&rhs.nanos)
    }
}

impl Eq for BcdNano {}
impl Ord for BcdNano
{
    fn cmp(&self, rhs: &Self) -> Ordering
    {
        self.partial_cmp(rhs).unwrap()
    }
}

impl BcdNano
{
    /// Construct a new value from a number with a specified magnitude
    ///
    /// This will panic if the number overflows a 64-bit unsigned integer when converted into billionth
    /// precision version of itself
    fn from<S: Scalar>(num: u64) -> Self
    {
        let multiplier = 10u64.pow((9 + S::magnitude()) as u32);
        Self { nanos: num * multiplier }
    }

    /// Construct a new value from its whole and fractional parts with reference to a particular
    /// magnitude
    ///
    /// For example, to construct 4.35 with thousandths precision, you would specify `from_parts::<Milli>(4, 350_000)`
    fn from_parts<S: Scalar>(whole: u64, fraction: u64) -> Self
    {
        let divisor = 10u64.pow((9 + S::magnitude()) as u32);
        let whole = whole + fraction / divisor;
        Self { nanos: whole * divisor + fraction % divisor }
    }

    fn from_f64<S: Scalar>(num: f64) -> Self
    {
        let multiplier = 10f64.powf((9 + S::magnitude()) as f64);
        Self { nanos: (num * multiplier) as u64 }
    }

    fn as_f64<S: Scalar>(&self) -> f64
    {
        let divisor = 10u64.pow((9 + S::magnitude()) as u32);
        let whole = self.nanos / divisor;
        let fraction = self.nanos % divisor;

        whole as f64 + (fraction as f64 / divisor as f64)
    }

    /// Returns the fractional part of this number (i.e. digits after the decimal point) when represented with the
    /// given scalar
    fn fraction<S: Scalar>(&self) -> u64
    {
        let divisor = 10u64.pow((9 + S::magnitude()) as u32);
        self.nanos % divisor
    }

    /// Returns the normal part of this number (i.e. digits before the decimal place) when represented with the given
    /// scalar.
    fn normal<S: Scalar>(&self) -> u64
    {
        let divisor = 10u64.pow((9 + S::magnitude()) as u32);
        self.nanos / divisor
    }

    /// Returns this number as a tuple of its normal part and fractional part when represented with the given scalar
    fn as_parts<S: Scalar>(&self) -> (u64, u64)
    {
        let divisor = 10u64.pow((9 + S::magnitude()) as u32);
        (self.nanos / divisor, self.nanos % divisor)
    }
}

/// Defines a scalar prefix type for displaying units without changing the underlying value e.g.
/// "giga-" or "micro-"
///
/// # Implementation
/// Although this trait is public, users should not attempt to implement this trait. The reason is
/// because this trait is intended for working with opaque private types in the library backend.
/// When doing manipulations, particularly formatting, there are promises about indices into arrays
/// must be upheld which are subject to change between releases.
pub trait Scalar
{
    /// Return the power of 10 of this scalar
    ///
    /// For example, a prefix of "milli-" should return -3.
    fn magnitude() -> i32;

    /// Return this scalar's written shorthand notation
    ///
    /// For example, a prefix of "giga-" should return "G"
    fn notation() -> &'static str;
}

macro_rules! impl_prefix
{
    { $name:ident, $magnitude:literal, $notation:literal } => {
        pub struct $name {}

        impl $name
        {
            pub const MAGNITUDE: i32 = $magnitude;
            pub const NOTATION: &'static str = $notation;
        }

        impl Scalar for $name
        {
            fn magnitude() -> i32
            {
                Self::MAGNITUDE
            }

            fn notation() -> &'static str
            {
                Self::NOTATION
            }
        }
    }
}

impl_prefix!{ Nano, -9, "n" }
impl_prefix!{ Micro, -3, "u" }
impl_prefix!{ Milli, -3, "m" }
impl_prefix!{ Base, 0, "" }
impl_prefix!{ Kilo, 3, "k" }
impl_prefix!{ Mega, 6, "M" }
impl_prefix!{ Giga, 9, "G" }

pub struct UnitDisplay
{
    symbol: &'static str,
    notation: &'static str,
    magnitude: i32,
    value: BcdNano,
    hide_unit: bool,
}

impl fmt::Display for UnitDisplay
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        let mut divisor = 10u64.pow((9 + self.magnitude) as u32);
        let whole = self.value.nanos / divisor;
        let mut fraction = self.value.nanos % divisor;
        write!(f, "{}", whole)?;

        if let Some(precision) = f.precision() {
            if precision != 0 {
                f.write_char('.')?;
            }

            for _index in 0..precision {
                if fraction == 0 || divisor == 0 {
                    f.write_char('0')?;
                }
                else {
                    divisor /= 10;
                    let digit = fraction / divisor;
                    fraction = fraction % divisor;
                    f.write_char(unsafe { std::char::from_u32_unchecked((digit + 0x30) as u32) })?;
                }
            }
        }
        else {
            if fraction != 0 {
                f.write_char('.')?;
            }
            while fraction != 0 {
                divisor /= 10;
                let digit = fraction / divisor;
                fraction = fraction % divisor;
                f.write_char(unsafe { std::char::from_u32_unchecked((digit + 0x30) as u32) })?;
            }
        }

        if self.hide_unit {
            Ok(())
        }
        else {
            f.write_str(self.notation)?;
            f.write_str(self.symbol)
        }
    }
}


/// An electrical current value with nanoamp precision
#[derive(Debug, Clone, Copy)]
pub struct Ampere
{
    value: BcdNano,
}

/// An resistance value with nanoohm precision
#[derive(Debug, Clone, Copy)]
pub struct Ohm
{
    value: BcdNano,
}

/// An voltage value with nanovolt precision
#[derive(Debug, Clone, Copy)]
pub struct Volt
{
    value: BcdNano,
}

/// An time duration with nanosecond precision
#[derive(Debug, Clone, Copy)]
pub struct Second
{
    value: BcdNano,
}

macro_rules! impl_unit
{
    { $u:ty, $symbol:literal } => {
        impl From<BcdNano> for $u
        {
            fn from(this: BcdNano) -> Self
            {
                Self { value: this }
            }
        }

        impl $u
        {
            /// The written shorthand symbol
            pub const SYMBOL: &'static str = $symbol;

            /// Displays the unit with a given scalar prefix
            pub fn display<S: Scalar>(&self) -> UnitDisplay
            {
                UnitDisplay {
                    symbol: Self::SYMBOL,
                    notation: S::notation(),
                    magnitude: S::magnitude(),
                    value: self.value,
                    hide_unit: false,
                }
            }

            /// Displays the unit with a given scalar prefix but hides the symbol and prefix in the
            /// output
            pub fn display_anon<S: Scalar>(&self) -> UnitDisplay
            {
                UnitDisplay {
                    symbol: Self::SYMBOL,
                    notation: S::notation(),
                    magnitude: S::magnitude(),
                    value: self.value,
                    hide_unit: false,
                }
            }

            /// Displays the unit with no scalar prefix and with its symbol
            pub fn display_base(&self) -> UnitDisplay
            {
                self.display::<Base>()
            }

            /// Displays the unit with no scalar prefix and hides the symbol in the output
            pub fn display_anon_base(&self) -> UnitDisplay
            {
                self.display_anon::<Base>()
            }

            pub fn from<S: Scalar>(num: u64) -> Self
            {
                BcdNano::from::<S>(num).into()
            }

            /// Constructs a new value from a whole number of base units
            pub fn from_base(num: u64) -> Self
            {
                Self::from::<Base>(num)
            }

            /// Construct a new value from its whole and fractional parts with reference to a particular
            /// magnitude
            ///
            /// For example, to construct 4.35 with thousandths precision, you would specify `from_parts::<Milli>(4, 350_000)`
            pub fn from_parts<S: Scalar>(whole: u64, fraction: u64) -> Self
            {
                BcdNano::from_parts::<S>(whole, fraction).into()
            }

            pub fn from_f64<S: Scalar>(num: f64) -> Self
            {
                BcdNano::from_f64::<S>(num).into()
            }

            /// Constructs a new value from a floating point number of base units
            pub fn from_f64_base(num: f64) -> Self
            {
                BcdNano::from_f64::<Base>(num).into()
            }

            pub fn as_f64<S: Scalar>(&self) -> f64
            {
                self.value.as_f64::<S>()
            }

            /// Returns the fractional part of this number (i.e. digits after the decimal point) when represented with the
            /// given scalar
            pub fn fraction<S: Scalar>(&self) -> u64
            {
                self.value.fraction::<S>()
            }

            /// Returns the normal part of this number (i.e. digits before the decimal place) when represented with the given
            /// scalar.
            pub fn normal<S: Scalar>(&self) -> u64
            {
                self.value.normal::<S>()
            }

            /// Returns this number as a tuple of its normal part and fractional part when represented with the given scalar
            pub fn as_parts<S: Scalar>(&self) -> (u64, u64)
            {
                self.value.as_parts::<S>()
            }
        }

        impl Add for $u
        {
            type Output = Self;

            fn add(self, rhs: Self) -> Self
            {
                Self { value: self.value + rhs.value }
            }
        }

        impl Sub for $u
        {
            type Output = Self;

            fn sub(self, rhs: Self) -> Self
            {
                Self { value: self.value - rhs.value }
            }
        }

        impl Mul for $u
        {
            type Output = Self;

            fn mul(self, rhs: Self) -> Self
            {
                Self { value: self.value * rhs.value }
            }
        }

        impl Div for $u
        {
            type Output = Self;

            fn div(self, rhs: Self) -> Self
            {
                Self { value: self.value / rhs.value }
            }
        }

        impl PartialEq for $u
        {
            fn eq(&self, rhs: &Self) -> bool
            {
                self.value == rhs.value
            }
        }

        impl PartialOrd for $u
        {
            fn partial_cmp(&self, rhs: &Self) -> Option<Ordering>
            {
                self.value.partial_cmp(&rhs.value)
            }
        }

        impl Eq for $u {}
        impl Ord for $u
        {
            fn cmp(&self, rhs: &Self) -> Ordering
            {
                self.partial_cmp(rhs).unwrap()
            }
        }
    }
}

impl_unit!{ Ampere, "A" }
impl_unit!{ Ohm, "Ω" }
impl_unit!{ Volt, "V" }
impl_unit!{ Second, "S" }

// impl fmt::Display for Ampere
// {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result
//     {
//         self.display::<Base>().fmt(f)
//     }
// }

macro_rules! fval
{
    ( $num:expr, $u:ident ) => {
        $u::from_f64_base($num)
    };
    ( $num:expr, $prefix:ident $u:ident ) => {
        $u::from_f64::<$prefix>($num)
    };
}

macro_rules! ival
{
    ( $num:expr, $u:ident ) => {
        $u::from_base($num)
    };
    ( $num:expr, $prefix:ident $u:ident ) => {
        $u::from::<$prefix>($num)
    };
}

macro_rules! view
{
    ( $u:expr ) => {
        $u.display_base()
    };
    ( $u:expr, $prefix:ident ) => {
        $u.display::<$prefix>()
    };
}

macro_rules! view_anon
{
    ( $u:expr ) => {
        $u.display_anon_base()
    };
    ( $u:expr, $prefix:ident ) => {
        $u.display_anon::<$prefix>()
    };
}

// macro_rules! display
// {
//     ( $prefix:ident $value:expr ) => {
//         $value.display_no_unit
//     };
// }

#[cfg(test)]
mod tests
{
    use super::{ BcdNano, Prefixed, Ampere, Milli };
    #[test]
    fn precision_truncates()
    {
        let prefixed = Prefixed {
            magnitude: 0,
            notation: "",
            value: BcdNano { nanos: 12345 * 1_000_000 },
        };

        assert_eq!(&format!("{:.2}", prefixed), "12.34");
    }

    #[test]
    fn precision_zero_extends()
    {
        let prefixed = Prefixed {
            magnitude: -1,
            notation: "",
            value: BcdNano { nanos: 12345 * 1_000_000 },
        };

        assert_eq!(&format!("{:.5}", prefixed), "123.45000");
    }

    #[test]
    fn no_precision_prints_all()
    {
        let prefixed = Prefixed {
            magnitude: -2,
            notation: "",
            value: BcdNano { nanos: 12_345_678_000 },
        };

        assert_eq!(&format!("{}", prefixed), "1234.5678");
    }

    #[test]
    fn format_unit()
    {
        let amp = val!(12345.678, Milli Ampere);
        // let amp = Ampere { value: BcdNano { nanos: 12_345_678_000 } };
        assert_eq!(&format!("{}", amp.display::<Milli>()), "12345.678mA");
    }
}
