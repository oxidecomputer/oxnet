// Copyright 2025 Oxide Computer Company

use std::num::ParseIntError;

/// Error returned when attempting to construct a [VlanId].
#[derive(Clone, Debug, PartialEq)]
pub struct VlanIdError(u16);

impl std::fmt::Display for VlanIdError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "invalid VLAN ID {}; must be less than {}",
            self.0,
            VlanId::RESERVED
        )
    }
}

impl std::error::Error for VlanIdError {}

/// Error returned when parsing a [`VlanId`] from a string.
#[derive(Clone, Debug)]
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

impl std::error::Error for VlanIdParseError {}

/// An IEEE 802.1Q VLAN ID.
///
/// Every [VlanId] contains a value in `0..=4094`; 4095 is reserved.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(try_from = "u16", into = "u16")
)]
pub struct VlanId(u16);

impl VlanId {
    /// Maximum 12-bit VLAN ID value (0xFFF / 4095). This value is reserved by
    /// IEEE 802.1Q and cannot be used to construct a [VlanId].
    pub const RESERVED: u16 = 4095;
    /// Maximum valid VLAN ID value (0xFFE / 4094)
    pub const MAX: Self = Self(4094);
    /// IEEE 802.1Q Null VID. This VID represents untagged traffic, but the
    /// presence of a VLAN tag allows the frames to have 802.1p QoS bits.
    pub const NULL: Self = Self(0);
    /// IEEE 802.1Q Default PVID value
    pub const DEFAULT_PVID: Self = Self(1);
    /// IEEE 802.1Q Default SR PVID value
    pub const DEFAULT_SR_PVID: Self = Self(2);

    /// Create a new [VlanId] after validating that `value` is within the
    /// permitted range (`0..=4094`).
    pub const fn new(value: u16) -> Result<Self, VlanIdError> {
        if value < Self::RESERVED {
            return Ok(Self(value));
        }
        Err(VlanIdError(value))
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
                maximum: Some(f64::from(VlanId::RESERVED - 1)),
                ..Default::default()
            })),
            extensions: crate::schema_util::extension("VlanId", "0.1.7"),
            ..Default::default()
        }
        .into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_accepts_valid_values() {
        assert_eq!(VlanId::new(0).unwrap().0, 0);
        assert_eq!(VlanId::new(100).unwrap().0, 100);
        assert_eq!(VlanId::new(4094).unwrap().0, 4094);
    }

    #[test]
    fn new_rejects_invalid_values() {
        assert!(VlanId::new(VlanId::RESERVED).is_err());
        assert!(VlanId::new(u16::MAX).is_err());
    }

    #[test]
    fn try_from_mirrors_new() {
        assert!(VlanId::try_from(0u16).is_ok());
        assert!(VlanId::try_from(4094u16).is_ok());
        assert!(VlanId::try_from(4095u16).is_err());
        assert!(VlanId::try_from(u16::MAX).is_err());
    }

    #[test]
    fn from_vlan_id_for_u16_roundtrips() {
        let id = VlanId::new(42).unwrap();
        let raw: u16 = id.into();
        assert_eq!(raw, 42);
        assert_eq!(VlanId::try_from(raw).unwrap(), id);
    }

    #[test]
    fn display_and_from_str_roundtrip() {
        for v in [0, 1, 100, 4094] {
            let id = VlanId::new(v).unwrap();
            let s = id.to_string();
            let parsed: VlanId = s.parse().unwrap();
            assert_eq!(id, parsed);
        }
    }

    #[test]
    fn from_str_rejects_non_numeric() {
        assert!("abc".parse::<VlanId>().is_err());
        assert!("".parse::<VlanId>().is_err());
        assert!("-1".parse::<VlanId>().is_err());
    }

    #[test]
    fn from_str_rejects_out_of_range() {
        assert!("4095".parse::<VlanId>().is_err());
        assert!("65535".parse::<VlanId>().is_err());
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
        }

        #[test]
        fn deserialize_rejects_out_of_range() {
            let result = serde_json::from_str::<VlanId>("4095");
            assert!(result.is_err());
        }
    }
}
