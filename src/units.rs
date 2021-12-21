//! Type-safe engineering units with string serialization
//!
//! # Introduction
//! While there are several libraries which offer type-safe dimensional analysis and systems of
//! measurement (`uom`, `dimensioned`, just to name a couple), these libraries are often extremely
//! complicated and boast many features which are just _not necessary_ even for their own sake.
//! Thus, you want a banana but what you get is the banana, the monkey holding it, and the tree
//! he's sitting in if you use one of these system-of-measurement libraries.
//!
//! This would be okay for an end user developing an application who would probably leverage all
//! these features. But for libraries and downstream users of said libraries, this can turn into
//! irreconcilable API and dependency management hell because many of them are experimental and
//! change wildly between releases. It should be the goal of every library to be as self-contained
//! as possible in order to avoid such dependency hell and imposition on the user.
//!
//! And finally, this library is nigh exclusively concerned with parsing and serialization of a very
//! short list of well-defined, known-precision values and _not_ with doing complex math and
//! numerical science with them.
//!
//! So to avoid dependency explosion, frequent API breaking changes, dependency hell, and imposition
//! on the user, this library handles type-safe units with a custom implementation.
//!
//! # Units and Precision
//! This library defines the following units:
//!   - Ampere
//!   - Ohm
//!   - Second
//!   - Volt
//!
//! Internally, these are handled as nanoamps, nanoohms, nanoseconds, and nanovolts using an
//! unsigned 64-bit integer for storage. This gives a range for any given value of
//! [0; 18,446,744,073.709551616] -- or approximately 18.4 gigaunits -- and a constant precision of
//! nine (9) digits after the decimal point.
//!
//! ## Why not floating point?
//! A 64-bit integer which handles things in nano-level precision should be more than enough to
//! represent the maximum and minimum value these devices ever report, which is 1 gigaohm and 1
//! microamp respectively.
//!
//! Also, if you aren't aware, IEEE 754 floating point a major "screw you" to type safety and
//! predictable program behavior. Some of its key pain points:
//!   - Represents fractional values by summation of negative powers of 2, which means most base-10
//!     decimals cannot be perfectly represented
//!     - This therefore leads to roundoff in calculations causing unintuitive outcomes.
//!       For example `(11 / 10 * 9 / 10) == (1.1 * 0.9)` evaluates to `false` despite the the two
//!       sides being mathematically equavalent.
//!   - Negative zero
//!   - Positive and negative infinity
//!   - Not a number i.e. `NaN`
//!   - `NaN` != `NaN`
//!   - `+Inf` != `+Inf`
//!   - `-Inf` != `-Inf`
//!   - `-Inf` + `+Inf` = ???
//!
//! By using integer operations and binary coded decimal, a lot of this messiness is completely
//! avoided. For instance, there isn't even such a thing as `NaN` or `Infinity`.
//!
//! ## Math and Comparison
//! All units implement the `Add` and `Sub` operations. Presently, they delegate to the implementations
//! to the `u64` primitive type, so all regular rules of integer arithmetic apply.
//!
//! Multiplication and division are very intentionally **not** implemented because they create a
//! a change in dimension i.e. an ampere times an ampere is an ampere-squared, not just an ampere.
//! Also, there just isn't much use for impementation since this library is intended for automated
//! testing where the values are usually fixed in a standard and known ahead of time.
//!
//! Each unit type, as a result of being stored as an integer, has a comparison implementation and a 
//! total ordering. The `<`, `>`, `>=`, and `<=` operators will work as expected without surprise
//! behavior.
//!
//! # Handing Values to This Library
//! Each unit in this library offers a variety of constructor methods to safely build a value which
//! can be handed to the library. Most of these methods take a scalar type parameter so they know
//! what kind of unit you are feeding them and so you can work in quantities that make sensor for a
//! given context:
//!
//! ```
//! let leak_current = Ampere::from::<Milli>(30);  // Construct a 30mA value
//! let check_voltage = Volt::from_f64::<Kilo>(1.25);  // Construct a 1.25kV value
//! let ramp_time = Second::from_parts::<Base>(2, 400_000_000);  // Construct a 2.4S value
//! ```
//!
//! **Please note that the `from_parts()` methods require you to scale the fractional portion to
//! account for all digits down to 10e-9.**
//!
//! For example:
//! ```
//! let check_voltage = Volt::from_parts::<Kilo>(1, 250);  // this is actually 1.000000000025kV
//! println!("{}", check_voltage.display_kilo());
//! ```
//!
//! This prints `1000.000000025V`, not `1250V`.
//!
//! You would actually need to write:
//!
//! ```
//! let check_voltage = Volt::from_parts::<Kilo>(1, 250_000_000_000);
//! ```
//!
//! ## Adding pieces to make something more intuitive
//! Alternatively, you could create a value by summing its parts:
//!
//! ```
//! let check_voltage = Volt::from::<Kilo>(1) + Volt::from::<Base>(250);  // This is 1.25kV
//! ```
//!
//! ## Using the `ival` and `fval` macros
//! Using turbofish syntax to create units can be ugly to read and cumbersome to write. This library
//! supplies a couple of declarative macros to make instantiating units in a more natural, readable way.
//! These methods are just syntax suger to substitute arguments into the `from()`, `from_f64()` and
//! `from_parts()` methods.
//!
//! `ival!` (short for "integer value") creates quantity using some integer in the selected magnitude:
//!
//! ```
//! let gnd_check_current = ival!(25, Amp);  // Create a value with no prefix, in this case 25 amps
//! let leakage_max = ival!(25, Milli Amp);  // Create a value of 25 milliamps
//! ```
//!
//! The `fval!` (short for "float value") macro works identically but does so using `f64` values:
//!
//! ```
//! let gnd_check_current = fval!(25.0, Amp);    // Create a value with no prefix, in this case 25 amps
//! let check_voltage = ival!(1.75, Kilo Volt);  // Create a value of 
//! ```
//! 
//! **Note that using `fval!` will sometimes result in values slightly different from what you
//! expected because of floating point roundoff.**
//!
//! You can also use expressions to create the value in these macros:
//! ```
//! fn ac_dc_hipot_voltage(is_ac: bool) {
//!     fval!(
//!         {
//!             1250.0 * if is_ac {
//!                 1.0f64
//!             } else {
//!                 2.0f64.sqrt()
//!             }
//!         },
//!         Volt
//!     )
//! }
//! ```
//!
//! ### Combining addition of parts and macros
//! Because these macros are just syntax sugar for the constructors of the units, you can add them
//! together just like with the quantities:
//!
//! ```
//! let check_voltage = ival!(1, Kilo Volt) + ival!(250, Volt);  // This is 1.25kV
//! ```
//!
//! # Receiving values from this library
//! The unit types in this library can be converted into more generally useful values using a series
//! of introspection methods or simply by serializing them to a string. Each one can be:
//!   - Decomposed into its parts using the `normal()`, `fraction()`, and `as_parts()` methods
//!   - They can be turned into an `f64` using the `as_f64()` method
//!   - Serialized to a string using the `display()` and `display_anon()` methods
//!
//! The decomposition methods each take a scalar prefix type argument so it knows where to divide
//! and truncate the number. This also makes it so you can decompose it in quantities which are
//! context appropriate.
//!
//! See the documentation for the `UnitDisplay` struct for details on string serialization.

use std::{
    fmt::{ self, Write },
    ops::{ Add, Sub, },
    cmp::{ PartialEq, PartialOrd, Eq, Ord, Ordering },
};

pub mod scalar
{
    //! Metric scalars/prefixes for units using zero-cost abstractions
    //!
    //! This module presents a zero-cost way of metric-prefixed parsing and displaying of units. It
    //! does this by defining a set of zero-sized types which implement the `Scalar` trait for
    //! communicating their magnitude and symbology. This allows these types to be used as generic
    //! type parameters for defining what magnitude a unit should be parsed and serialized as meaning
    //! the compiler can inline everything at compile time. No memory usage; no lookup tables for
    //! dynamic types and functions.

    /// Defines a scalar prefix type for parsing and serializing units without changing the underlying
    /// value e.g. "giga-" or "micro-"
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
    impl_prefix!{ Micro, -6, "u" }
    impl_prefix!{ Milli, -3, "m" }
    impl_prefix!{ Base, 0, "" }
    impl_prefix!{ Kilo, 3, "k" }
    impl_prefix!{ Mega, 6, "M" }
    impl_prefix!{ Giga, 9, "G" }
}

use scalar::{ Scalar, Base };

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

/// Serialization implementor for units
///
/// This structure is strictly for use as a string serializer (such as for use in `format!` or
/// `write!` calls) and as such is completely opaque. It can and can only be acquired by calling
/// some display constructor on a unit in memory. It will serialize a contained unit in simple decimal
/// format using a supplied metric prefix. For example:
///
/// ```
/// let amp = Ampere::from_parts::<Base>(1, 230_000_000);
/// println!("{}", amp.display::<Milli>())
/// ```
///
/// would output `1230mA`.
///
/// # Supported `fmt::Display` attributes
/// This struct will represent the contained unit using as many significant digits as necessary.
/// The digits after the decimal point may be limited via truncation by supplying a precision
/// argument to a formatter:
///
/// ```
/// let amp = Ampere::from_parts::<Base>(1, 234_567_890);
/// println!("{:.2}", amp.display::<Milli>())
/// ```
/// 
/// Output: `1234.56mA`
///
/// Note that it will NOT zero pad if the unit can be precisely represented.
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
                    hide_unit: true,
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

/// Construct a unit type from a float with a certain prefix/magnitude
#[macro_export]
macro_rules! fval
{
    ( $num:expr, $u:ident ) => {
        $u::from_f64_base($num)
    };
    ( $num:expr, $prefix:ident $u:ident ) => {
        $u::from_f64::<$crate::units::scalar::$prefix>($num)
    };
}

/// Construct a unit type from an integer with a certain prefix/magnitude
#[macro_export]
macro_rules! ival
{
    ( $num:expr, $u:ident ) => {
        $u::from_base($num)
    };
    ( $num:expr, $prefix:ident $u:ident ) => {
        $u::from::<$crate::units::scalar::$prefix>($num)
    };
}

/// Make a serializer for a unit using a certain prefix/magnitude
///
/// This is syntax sugar for `unit.display::<PREFIX>()` which can be messy to read and write. If no
/// prefix argument is supplied, this displays base units.
///
/// The scalar is attached using the full crate path, so it does not need to be in scope for this
/// macro to be used.
#[macro_export]
macro_rules! view
{
    ( $u:expr ) => {
        $u.display_base()
    };
    ( $u:expr, $prefix:ident ) => {
        $u.display::<$crate::units::scalar::$prefix>()
    };
}

/// Make a serializer for a unit using a certain prefix/magnitude that displays only the number in
/// the output
///
/// This is syntax sugar for `unit.display_anon::<PREFIX>()` which can be messy to read and write.
/// If no prefix is given, then this displays base units.
///
/// The scalar is attached using the full crate path, so it does not need to be in scope for this
/// macro to be used.
#[macro_export]
macro_rules! view_anon
{
    ( $u:expr ) => {
        $u.display_anon_base()
    };
    ( $u:expr, $prefix:ident ) => {
        $u.display_anon::<$crate::units::scalar::$prefix>()
    };
}

#[cfg(test)]
mod tests
{
    use super::{
        Ampere,
        scalar::{ Milli, Micro, Base },
    };
    #[test]
    fn precision_truncates()
    {
        let val = Ampere::from::<Micro>(12_340);
        assert_eq!(&format!("{:.2}", val.display_anon::<Milli>()), "12.34");
    }

    #[test]
    fn precision_zero_extends()
    {
        let val = Ampere::from::<Micro>(123_450);
        assert_eq!(&format!("{:.5}", val.display_anon::<Milli>()), "123.45000");
    }

    #[test]
    fn precision_forces_decimal()
    {
        let val = Ampere::from_base(12);
        assert_eq!(&format!("{:.1}", val.display_anon::<Base>()), "12.0");
    }

    #[test]
    fn no_precision_prints_all()
    {
        let val = Ampere::from_parts::<Base>(12, 345_678_000);
        assert_eq!(&format!("{}", val.display_anon_base()), "12.345678");
    }

    #[test]
    fn format_unit()
    {
        let amp = ival!(12_345_678, Micro Ampere);
        assert_eq!(&format!("{}", amp.display::<Milli>()), "12345.678mA");
    }
}
