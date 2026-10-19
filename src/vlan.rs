// Copyright 2026 Oxide Computer Company

//! VLAN identifiers as carried in an IEEE 802.1Q tag.
//!
//! A VLAN ID is a 12-bit field. [`VlanId`] accepts everything the field can
//! hold except 4095, which 802.1Q reserves. 0 can show up on real packets
//! (priority tagging), so the base type does not outright reject it.
//! Instead, we provide [`NonZeroVlanId`], which wraps [`VlanId`] for interfaces
//! that can't take the null VLAN ID.
//!
//! See [IEEE 802.1Q] §9.6, and [RFC 4363], which uses the same 1..=4094 range
//! for its `VlanIndex`.
//!
//! [IEEE 802.1Q]: https://standards.ieee.org/ieee/802.1Q/10323/
//! [RFC 4363]: https://datatracker.ietf.org/doc/html/rfc4363

use std::num::ParseIntError;

/// Error returned when attempting to construct a [VlanId] or
/// [NonZeroVlanId].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct VlanIdError {
    value: u16,
    min: u16,
}

impl VlanIdError {
    /// Why the ID value was rejected.
    ///
    /// The reason is a total derived function of the value itself. It is
    /// `const`, meaning that the `new_assert` constructors can report it from a
    /// const context, where a panic message may be a `&str` but cannot be
    /// formatted.
    pub const fn reason(&self) -> &'static str {
        match self.value {
            0 => "the null VLAN ID, which names no VLAN",
            VlanId::RESERVED => "reserved for implementation use",
            _ => "outside the 12-bit VLAN ID field",
        }
    }
}

impl std::fmt::Display for VlanIdError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "invalid VLAN ID {}: {}; must be in the range {}..={}",
            self.value,
            self.reason(),
            self.min,
            VlanId::MAX.get()
        )
    }
}

impl std::error::Error for VlanIdError {}

/// Error returned when parsing a [`VlanId`] or [`NonZeroVlanId`] from a
/// string.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum VlanIdParseError {
    /// The string could not be parsed as a `u16`.
    InvalidInt(ParseIntError),
    /// The parsed integer was out of range.
    Value(VlanIdError),
}

impl std::fmt::Display for VlanIdParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VlanIdParseError::InvalidInt(e) => e.fmt(f),
            VlanIdParseError::Value(e) => e.fmt(f),
        }
    }
}

impl std::error::Error for VlanIdParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            VlanIdParseError::InvalidInt(error) => Some(error),
            VlanIdParseError::Value(error) => Some(error),
        }
    }
}

/// An IEEE 802.1Q VLAN ID.
///
/// Every [VlanId] contains a value in `0..=4094`, which is the range that the
/// 12-bit field can hold excepting 4095, which is reserved for implementation
/// use and never valid. The null VLAN ID (0) is representable here since it
/// appears on legit packet data.
///
/// For interfaces that must name an actual VLAN, use [`NonZeroVlanId`].
///
/// See [IEEE 802.1Q] §9.6 for the encoding and the reserved values.
///
/// [IEEE 802.1Q]: https://standards.ieee.org/ieee/802.1Q/10323/
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(try_from = "u16", into = "u16")
)]
pub struct VlanId(u16);

impl VlanId {
    /// Maximum 12-bit VLAN ID value (0xFFF / 4095). This value is reserved by
    /// IEEE 802.1Q and cannot be used to construct a [VlanId]. Hence the
    /// `u16`: [`VlanId`] itself can't hold 4095.
    pub const RESERVED: u16 = 4095;
    /// Maximum valid VLAN ID value (0xFFE / 4094)
    pub const MAX: Self = Self(4094);
    /// IEEE 802.1Q Null VLAN ID. This VLAN ID represents untagged traffic, but
    /// the presence of a VLAN tag allows the frames to have 802.1p QoS bits.
    pub const NULL: Self = Self(0);

    /// Create a new [VlanId] after validating that `value` is within the
    /// permitted range (`0..=4094`).
    ///
    /// A `const fn` where `value` is checked while compiling whenever the call
    /// sits in a const context, such as the initializer of a `const` item or
    /// a `const` block. Used anywhere else, this check happens at run time.
    ///
    /// See [`VlanId::new_assert`] for the panicking form.
    ///
    /// # Errors
    ///
    /// Returns [`VlanIdError`] for the reserved VLAN ID (4095) and for
    /// anything above 4095, which the 12-bit VLAN ID field cannot hold.
    ///
    /// # Examples
    ///
    /// ```
    /// use oxnet::VlanId;
    ///
    /// const UPLINK: VlanId = match VlanId::new(100) {
    ///     Ok(id) => id,
    ///     Err(_) => unreachable!(),
    /// };
    /// assert_eq!(UPLINK.get(), 100);
    /// assert_eq!(VlanId::new(VlanId::NULL.get()), Ok(VlanId::NULL));
    /// assert!(VlanId::new(4095).is_err());
    /// ```
    pub const fn new(value: u16) -> Result<Self, VlanIdError> {
        if value < Self::RESERVED {
            return Ok(Self(value));
        }
        Err(VlanIdError { value, min: 0 })
    }

    /// Create a new [VlanId], panicking if `value` is not within the valid
    /// range (`0..=4094`).
    ///
    /// Intended for constants: a const context is evaluated while compiling,
    /// so an invalid `value` fails the build rather than the run. Elsewhere,
    /// it panics like any other assertion.
    ///
    /// # Panics
    ///
    /// Panics under the same conditions [`VlanId::new`] returns an error,
    /// reporting [`VlanIdError::reason`].
    ///
    /// # Examples
    ///
    /// ```
    /// use oxnet::VlanId;
    ///
    /// const UPLINK: VlanId = VlanId::new_assert(100);
    /// assert_eq!(UPLINK.get(), 100);
    /// ```
    ///
    /// The reserved VLAN ID does not compile:
    ///
    /// ```compile_fail
    /// const RESERVED: oxnet::VlanId = oxnet::VlanId::new_assert(4095);
    /// ```
    pub const fn new_assert(value: u16) -> Self {
        match Self::new(value) {
            Ok(id) => id,
            Err(error) => panic!("{}", error.reason()),
        }
    }

    /// Returns the value of the [VlanId] as a raw `u16`.
    pub const fn get(&self) -> u16 {
        self.0
    }
}

impl TryFrom<u16> for VlanId {
    type Error = VlanIdError;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl From<VlanId> for u16 {
    fn from(id: VlanId) -> Self {
        id.0
    }
}

impl std::fmt::Display for VlanId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::str::FromStr for VlanId {
    type Err = VlanIdParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let value: u16 = s.parse().map_err(VlanIdParseError::InvalidInt)?;
        VlanId::new(value).map_err(VlanIdParseError::Value)
    }
}

/// A [VlanId] known not to be the null VLAN ID.
///
/// This wraps [`VlanId`], excluding 0 by construction.
///
/// Every [NonZeroVlanId] contains a value in `1..=4094` and names a VLAN
/// that may actually be configured. [RFC 4363] uses the same range for
/// its `VlanIndex`.
///
/// [`From`] widens back to [`VlanId`], whereas [`TryFrom`] narrows,
/// failing on the null VLAN ID.
///
/// [RFC 4363]: https://datatracker.ietf.org/doc/html/rfc4363
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(try_from = "u16", into = "u16")
)]
pub struct NonZeroVlanId(VlanId);

impl NonZeroVlanId {
    /// Smallest non-zero VLAN ID (0x001 / 1).
    pub const MIN: Self = Self(VlanId(1));
    /// Largest valid VLAN ID (0xFFE / 4094).
    pub const MAX: Self = Self(VlanId::MAX);

    /// Create a new [NonZeroVlanId] after validating that `value` is within
    /// the specified range: `1..=4094`.
    ///
    /// A `const fn`, checked while compiling in const contexts like
    /// [`VlanId::new`].
    ///
    /// # Errors
    ///
    /// Returns [`VlanIdError`] for the null VLAN ID (0), for the reserved
    /// VLAN ID (4095), and for anything above 4095, which the 12-bit VLAN ID
    /// field cannot hold.
    ///
    /// # Examples
    ///
    /// ```
    /// use oxnet::NonZeroVlanId;
    ///
    /// assert!(NonZeroVlanId::new(100).is_ok());
    /// assert!(NonZeroVlanId::new(0).is_err());
    /// assert!(NonZeroVlanId::new(4095).is_err());
    /// ```
    pub const fn new(value: u16) -> Result<Self, VlanIdError> {
        if value == 0 {
            return Err(VlanIdError { value, min: 1 });
        }
        match VlanId::new(value) {
            Ok(id) => Ok(Self(id)),
            Err(_) => Err(VlanIdError { value, min: 1 }),
        }
    }

    /// Create a new [NonZeroVlanId], panicking if `value` is not within the
    /// range `1..=4094`.
    ///
    /// Intended for constant-usage, like [`VlanId::new_assert`].
    ///
    /// # Panics
    ///
    /// Panics under the same conditions [`NonZeroVlanId::new`] returns an
    /// error, reporting a [`VlanIdError::reason`].
    ///
    /// # Examples
    ///
    /// ```
    /// use oxnet::NonZeroVlanId;
    ///
    /// const UPLINK: NonZeroVlanId = NonZeroVlanId::new_assert(100);
    /// assert_eq!(UPLINK.get(), 100);
    /// ```
    ///
    /// The null VLAN ID does not compile:
    ///
    /// ```compile_fail
    /// const NULL: oxnet::NonZeroVlanId = oxnet::NonZeroVlanId::new_assert(0);
    /// ```
    ///
    /// Neither does the reserved VLAN ID:
    ///
    /// ```compile_fail
    /// const RESERVED: oxnet::NonZeroVlanId = oxnet::NonZeroVlanId::new_assert(4095);
    /// ```
    pub const fn new_assert(value: u16) -> Self {
        match Self::new(value) {
            Ok(id) => id,
            Err(error) => panic!("{}", error.reason()),
        }
    }

    /// Returns the underlying [VlanId].
    pub const fn vlan_id(&self) -> VlanId {
        self.0
    }

    /// Returns the value of the [NonZeroVlanId] as a raw `u16`.
    pub const fn get(&self) -> u16 {
        self.0.get()
    }
}

impl From<NonZeroVlanId> for VlanId {
    fn from(id: NonZeroVlanId) -> Self {
        id.0
    }
}

impl TryFrom<VlanId> for NonZeroVlanId {
    type Error = VlanIdError;

    fn try_from(id: VlanId) -> Result<Self, Self::Error> {
        Self::new(id.get())
    }
}

impl TryFrom<u16> for NonZeroVlanId {
    type Error = VlanIdError;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl From<NonZeroVlanId> for u16 {
    fn from(id: NonZeroVlanId) -> Self {
        id.get()
    }
}

impl std::fmt::Display for NonZeroVlanId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get())
    }
}

impl std::str::FromStr for NonZeroVlanId {
    type Err = VlanIdParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let value: u16 = s.parse().map_err(VlanIdParseError::InvalidInt)?;
        NonZeroVlanId::new(value).map_err(VlanIdParseError::Value)
    }
}

#[cfg(feature = "slog")]
impl slog::Value for VlanId {
    fn serialize(
        &self,
        _record: &slog::Record,
        key: slog::Key,
        serializer: &mut dyn slog::Serializer,
    ) -> slog::Result {
        serializer.emit_u16(key, self.0)
    }
}

#[cfg(feature = "slog")]
impl slog::Value for NonZeroVlanId {
    fn serialize(
        &self,
        _record: &slog::Record,
        key: slog::Key,
        serializer: &mut dyn slog::Serializer,
    ) -> slog::Result {
        serializer.emit_u16(key, self.get())
    }
}

#[cfg(feature = "schemars")]
impl schemars::JsonSchema for VlanId {
    fn schema_name() -> String {
        "VlanId".to_string()
    }

    fn json_schema(_: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        schemars::schema::SchemaObject {
            metadata: Some(Box::new(schemars::schema::Metadata {
                title: Some("A VLAN ID".to_string()),
                description: Some(
                    "An IEEE 802.1Q VLAN ID, in the range 0-4094 (4095 is reserved)".to_string(),
                ),
                ..Default::default()
            })),
            instance_type: Some(schemars::schema::InstanceType::Integer.into()),
            number: Some(Box::new(schemars::schema::NumberValidation {
                minimum: Some(0.0),
                maximum: Some(f64::from(VlanId::MAX.get())),
                ..Default::default()
            })),
            extensions: crate::schema_util::extension("VlanId", "0.1.8"),
            ..Default::default()
        }
        .into()
    }
}

#[cfg(feature = "schemars")]
impl schemars::JsonSchema for NonZeroVlanId {
    fn schema_name() -> String {
        "NonZeroVlanId".to_string()
    }

    fn json_schema(_: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        schemars::schema::SchemaObject {
            metadata: Some(Box::new(schemars::schema::Metadata {
                title: Some("A non-zero VLAN ID".to_string()),
                description: Some(
                    "An IEEE 802.1Q VLAN ID, in the range 1-4094 (0 is the null VLAN ID and \
                     4095 is reserved)"
                        .to_string(),
                ),
                ..Default::default()
            })),
            instance_type: Some(schemars::schema::InstanceType::Integer.into()),
            number: Some(Box::new(schemars::schema::NumberValidation {
                minimum: Some(f64::from(NonZeroVlanId::MIN.get())),
                maximum: Some(f64::from(NonZeroVlanId::MAX.get())),
                ..Default::default()
            })),
            extensions: crate::schema_util::extension("NonZeroVlanId", "0.1.8"),
            ..Default::default()
        }
        .into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_constructor_admits_its_range() {
        for value in 0..=u16::MAX {
            let valid = value <= 4094;
            let non_zero = (1..=4094).contains(&value);

            assert_eq!(VlanId::new(value).is_ok(), valid, "VlanId::new({value})");
            assert_eq!(
                NonZeroVlanId::new(value).is_ok(),
                non_zero,
                "NonZeroVlanId::new({value})"
            );
            assert_eq!(
                value.to_string().parse::<VlanId>().is_ok(),
                valid,
                "parse VlanId {value}"
            );
            assert_eq!(
                value.to_string().parse::<NonZeroVlanId>().is_ok(),
                non_zero,
                "parse NonZeroVlanId {value}"
            );

            if let Ok(id) = VlanId::new(value) {
                assert_eq!(u16::from(id), value);
                assert_eq!(VlanId::new_assert(value), id);

                match NonZeroVlanId::try_from(id) {
                    Ok(nz) => {
                        assert_ne!(value, 0);
                        assert_eq!(VlanId::from(nz), id);
                        assert_eq!(NonZeroVlanId::new_assert(value), nz);
                        assert_eq!(nz.vlan_id(), id);
                    }
                    Err(error) => {
                        assert_eq!(value, 0);
                        assert_eq!(error.reason(), "the null VLAN ID, which names no VLAN");
                    }
                }
            }
        }
    }

    #[test]
    fn preserves_public_vlan_constants() {
        assert_eq!(VlanId::RESERVED, 4095);
        assert_eq!(VlanId::MAX.get(), 4094);
        assert_eq!(VlanId::NULL.get(), 0);
        assert_eq!(NonZeroVlanId::MIN.get(), 1);
        assert_eq!(NonZeroVlanId::MAX.get(), 4094);
        assert!(VlanId::new(VlanId::RESERVED).is_err());
    }

    #[test]
    fn error_message_names_the_reason_and_the_range() {
        assert_eq!(
            VlanId::new(4095).unwrap_err().to_string(),
            "invalid VLAN ID 4095: reserved for implementation use; \
             must be in the range 0..=4094"
        );
        assert_eq!(
            VlanId::new(u16::MAX).unwrap_err().to_string(),
            "invalid VLAN ID 65535: outside the 12-bit VLAN ID field; \
             must be in the range 0..=4094"
        );
        assert_eq!(
            NonZeroVlanId::new(0).unwrap_err().to_string(),
            "invalid VLAN ID 0: the null VLAN ID, which names no VLAN; \
             must be in the range 1..=4094"
        );
        assert_eq!(
            NonZeroVlanId::new(4095).unwrap_err().to_string(),
            "invalid VLAN ID 4095: reserved for implementation use; \
             must be in the range 1..=4094"
        );
    }

    #[test]
    fn parse_error_keeps_its_cause() {
        use std::error::Error;

        let lexical = "abc".parse::<VlanId>().unwrap_err();
        assert!(lexical.source().unwrap().is::<ParseIntError>());

        let range = "0".parse::<NonZeroVlanId>().unwrap_err();
        assert!(range.source().unwrap().is::<VlanIdError>());
    }

    #[test]
    fn from_str_rejects_non_numeric() {
        assert!(matches!(
            "abc".parse::<VlanId>(),
            Err(VlanIdParseError::InvalidInt(_))
        ));
        assert!(matches!(
            "-1".parse::<NonZeroVlanId>(),
            Err(VlanIdParseError::InvalidInt(_))
        ));
    }

    #[cfg(feature = "serde")]
    mod serde_tests {
        use super::*;

        #[test]
        fn json_roundtrip() {
            for v in [0, 1, 100, 4094] {
                let id = VlanId::new(v).unwrap();
                let json = serde_json::to_string(&id).unwrap();
                assert_eq!(json, v.to_string());
                let deserialized: VlanId = serde_json::from_str(&json).unwrap();
                assert_eq!(id, deserialized);
            }

            for v in [1, 100, 4094] {
                let id = NonZeroVlanId::new(v).unwrap();
                let json = serde_json::to_string(&id).unwrap();
                assert_eq!(json, v.to_string());
                let deserialized: NonZeroVlanId = serde_json::from_str(&json).unwrap();
                assert_eq!(id, deserialized);
            }
        }

        #[test]
        fn deserialize_enforces_each_range() {
            assert!(serde_json::from_str::<VlanId>("0").is_ok());
            assert!(serde_json::from_str::<VlanId>("4095").is_err());
            assert!(serde_json::from_str::<NonZeroVlanId>("0").is_err());
            assert!(serde_json::from_str::<NonZeroVlanId>("4095").is_err());
        }
    }
}
