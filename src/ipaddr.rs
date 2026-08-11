// Copyright 2026 Oxide Computer Company

use std::net::{AddrParseError, IpAddr, Ipv4Addr, Ipv6Addr};

/// An error during the parsing of a [UnicastLinkLocalIpAddr],
/// [UnicastLinkLocalIpv4Addr] or [UnicastLinkLocalIpv6Addr].
#[derive(Debug, Clone)]
pub enum UnicastLinkLocalIpAddrParseError {
    /// Failure to parse the input as an IP address.
    InvalidAddr(AddrParseError),
    /// The parsed address is not unicast link-local.
    NotUnicastLinkLocal(UnicastLinkLocalIpAddrError),
}

impl std::fmt::Display for UnicastLinkLocalIpAddrParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidAddr(error) => error.fmt(f),
            Self::NotUnicastLinkLocal(error) => error.fmt(f),
        }
    }
}

impl std::error::Error for UnicastLinkLocalIpAddrParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::InvalidAddr(error) => Some(error),
            Self::NotUnicastLinkLocal(error) => Some(error),
        }
    }
}

/// An error during the creation of a [UnicastLinkLocalIpAddr],
/// [UnicastLinkLocalIpv4Addr] or [UnicastLinkLocalIpv6Addr].
#[derive(Copy, Debug, Clone, PartialEq)]
pub struct UnicastLinkLocalIpAddrError(IpAddr);

impl UnicastLinkLocalIpAddrError {
    /// Returns the address that failed validation.
    pub fn addr(&self) -> IpAddr {
        self.0
    }
}

impl std::fmt::Display for UnicastLinkLocalIpAddrError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "input is not unicast link-local: {}", self.0)
    }
}

impl std::error::Error for UnicastLinkLocalIpAddrError {}

/// An IP address, either IPv4 or IPv6, that falls within the Link-Local range.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum UnicastLinkLocalIpAddr {
    /// An IPv4 address within the Link-local range (169.254.0.0/16).
    V4(UnicastLinkLocalIpv4Addr),
    /// An IPv6 address within the Link-local range (fe80::/10).
    V6(UnicastLinkLocalIpv6Addr),
}

impl UnicastLinkLocalIpAddr {
    /// Create a new [UnicastLinkLocalIpAddr] from an [IpAddr].
    pub fn new(addr: IpAddr) -> Result<Self, UnicastLinkLocalIpAddrError> {
        match addr {
            IpAddr::V4(ip4) => UnicastLinkLocalIpv4Addr::try_from(ip4).map(Self::V4),
            IpAddr::V6(ip6) => UnicastLinkLocalIpv6Addr::try_from(ip6).map(Self::V6),
        }
    }

    /// Returns [`true`] if this address is an [`IPv4` address], and [`false`]
    /// otherwise.
    pub fn is_ipv4(&self) -> bool {
        matches!(self, Self::V4(_))
    }

    /// Returns [`true`] if this address is an [`IPv6` address], and [`false`]
    /// otherwise.
    pub fn is_ipv6(&self) -> bool {
        matches!(self, Self::V6(_))
    }
}

impl From<UnicastLinkLocalIpv4Addr> for UnicastLinkLocalIpAddr {
    fn from(value: UnicastLinkLocalIpv4Addr) -> Self {
        Self::V4(value)
    }
}

impl From<UnicastLinkLocalIpv6Addr> for UnicastLinkLocalIpAddr {
    fn from(value: UnicastLinkLocalIpv6Addr) -> Self {
        Self::V6(value)
    }
}

impl TryFrom<IpAddr> for UnicastLinkLocalIpAddr {
    type Error = UnicastLinkLocalIpAddrError;

    fn try_from(value: IpAddr) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl TryFrom<Ipv4Addr> for UnicastLinkLocalIpAddr {
    type Error = UnicastLinkLocalIpAddrError;

    fn try_from(value: Ipv4Addr) -> Result<Self, Self::Error> {
        Self::new(value.into())
    }
}

impl TryFrom<Ipv6Addr> for UnicastLinkLocalIpAddr {
    type Error = UnicastLinkLocalIpAddrError;

    fn try_from(value: Ipv6Addr) -> Result<Self, Self::Error> {
        Self::new(value.into())
    }
}

impl From<UnicastLinkLocalIpAddr> for IpAddr {
    fn from(value: UnicastLinkLocalIpAddr) -> Self {
        match value {
            UnicastLinkLocalIpAddr::V4(ip4) => IpAddr::V4(ip4.to_addr()),
            UnicastLinkLocalIpAddr::V6(ip6) => IpAddr::V6(ip6.to_addr()),
        }
    }
}

impl std::fmt::Display for UnicastLinkLocalIpAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnicastLinkLocalIpAddr::V4(inner) => write!(f, "{inner}"),
            UnicastLinkLocalIpAddr::V6(inner) => write!(f, "{inner}"),
        }
    }
}

impl std::str::FromStr for UnicastLinkLocalIpAddr {
    type Err = UnicastLinkLocalIpAddrParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let addr: IpAddr = s
            .parse()
            .map_err(UnicastLinkLocalIpAddrParseError::InvalidAddr)?;
        Self::try_from(addr).map_err(UnicastLinkLocalIpAddrParseError::NotUnicastLinkLocal)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for UnicastLinkLocalIpAddr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        <String as serde::Deserialize>::deserialize(deserializer)?
            .parse()
            .map_err(serde::de::Error::custom)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for UnicastLinkLocalIpAddr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_str(self)
    }
}

#[cfg(feature = "schemars")]
impl schemars::JsonSchema for UnicastLinkLocalIpAddr {
    fn schema_name() -> String {
        "UnicastLinkLocalIpAddr".to_string()
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        use crate::schema_util::label_schema;

        schemars::schema::SchemaObject {
            subschemas: Some(Box::new(schemars::schema::SubschemaValidation {
                one_of: Some(vec![
                    label_schema("v4", gen.subschema_for::<UnicastLinkLocalIpv4Addr>()),
                    label_schema("v6", gen.subschema_for::<UnicastLinkLocalIpv6Addr>()),
                ]),
                ..Default::default()
            })),
            extensions: crate::schema_util::extension("UnicastLinkLocalIpAddr", "0.1.7"),
            ..Default::default()
        }
        .into()
    }
}

/// An IPv4 address guaranteed to exist within the link-local range.
///
/// Validation follows [`Ipv4Addr::is_link_local`] and accepts every address in
/// `169.254.0.0/16`, including `169.254.0.0/24` and `169.254.255.0/24`.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct UnicastLinkLocalIpv4Addr(Ipv4Addr);

impl UnicastLinkLocalIpv4Addr {
    /// Create a [UnicastLinkLocalIpv4Addr] from four 8-bit octets.
    pub fn new(a: u8, b: u8, c: u8, d: u8) -> Result<Self, UnicastLinkLocalIpAddrError> {
        let new = Ipv4Addr::new(a, b, c, d);
        Self::from_addr(new)
    }

    /// Returns the four element byte array that make up this address.
    pub fn octets(&self) -> [u8; 4] {
        self.0.octets()
    }

    /// Create a [UnicastLinkLocalIpv4Addr] from a four element byte array.
    pub fn from_octets(octets: [u8; 4]) -> Result<Self, UnicastLinkLocalIpAddrError> {
        let new = Ipv4Addr::from(octets);
        Self::from_addr(new)
    }

    /// Converts this address into a [u32] in native byte order.
    pub fn to_bits(self) -> u32 {
        self.0.to_bits()
    }

    /// Create a [UnicastLinkLocalIpv4Addr] from a native byte order [u32].
    pub fn from_bits(bits: u32) -> Result<Self, UnicastLinkLocalIpAddrError> {
        let new = Ipv4Addr::from_bits(bits);
        Self::from_addr(new)
    }

    /// Converts this address into the underlying [Ipv4Addr].
    pub fn to_addr(self) -> Ipv4Addr {
        self.0
    }

    /// Create a [UnicastLinkLocalIpv4Addr] from an [Ipv4Addr].
    pub fn from_addr(addr: Ipv4Addr) -> Result<Self, UnicastLinkLocalIpAddrError> {
        if addr.is_link_local() {
            return Ok(Self(addr));
        }
        Err(UnicastLinkLocalIpAddrError(addr.into()))
    }
}

impl From<UnicastLinkLocalIpv4Addr> for u32 {
    fn from(value: UnicastLinkLocalIpv4Addr) -> Self {
        value.to_bits()
    }
}

impl TryFrom<u32> for UnicastLinkLocalIpv4Addr {
    type Error = UnicastLinkLocalIpAddrError;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        Self::from_bits(value)
    }
}

impl TryFrom<[u8; 4]> for UnicastLinkLocalIpv4Addr {
    type Error = UnicastLinkLocalIpAddrError;

    fn try_from(value: [u8; 4]) -> Result<Self, Self::Error> {
        Self::from_octets(value)
    }
}

impl TryFrom<Ipv4Addr> for UnicastLinkLocalIpv4Addr {
    type Error = UnicastLinkLocalIpAddrError;

    fn try_from(value: Ipv4Addr) -> Result<Self, Self::Error> {
        Self::from_addr(value)
    }
}

impl From<UnicastLinkLocalIpv4Addr> for Ipv4Addr {
    fn from(value: UnicastLinkLocalIpv4Addr) -> Self {
        value.to_addr()
    }
}

impl From<UnicastLinkLocalIpv4Addr> for IpAddr {
    fn from(value: UnicastLinkLocalIpv4Addr) -> Self {
        IpAddr::V4(value.to_addr())
    }
}

impl std::fmt::Display for UnicastLinkLocalIpv4Addr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::str::FromStr for UnicastLinkLocalIpv4Addr {
    type Err = UnicastLinkLocalIpAddrParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let addr: Ipv4Addr = s
            .parse()
            .map_err(UnicastLinkLocalIpAddrParseError::InvalidAddr)?;
        Self::try_from(addr).map_err(UnicastLinkLocalIpAddrParseError::NotUnicastLinkLocal)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for UnicastLinkLocalIpv4Addr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        <String as serde::Deserialize>::deserialize(deserializer)?
            .parse()
            .map_err(serde::de::Error::custom)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for UnicastLinkLocalIpv4Addr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_str(self)
    }
}

#[cfg(feature = "schemars")]
const UNICAST_LINK_LOCAL_IPV4_ADDR_REGEX: &str = concat!(
    r"^169\.254\.",
    r"([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\.",
    r"([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])$",
);

#[cfg(feature = "schemars")]
impl schemars::JsonSchema for UnicastLinkLocalIpv4Addr {
    fn schema_name() -> String {
        "UnicastLinkLocalIpv4Addr".to_string()
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        let schema = gen.subschema_for::<Ipv4Addr>();
        let mut schema_object = schema.into_object();
        schema_object.metadata = Some(Box::new(schemars::schema::Metadata {
            title: Some("A unicast link-local IPv4 address".to_string()),
            description: Some("An IPv4 address in 169.254.0.0/16".to_string()),
            examples: vec!["169.254.1.1".into()],
            ..Default::default()
        }));
        schema_object.string = Some(Box::new(schemars::schema::StringValidation {
            pattern: Some(UNICAST_LINK_LOCAL_IPV4_ADDR_REGEX.to_string()),
            ..Default::default()
        }));
        schema_object.extensions =
            crate::schema_util::extension("UnicastLinkLocalIpv4Addr", "0.1.7");
        schema_object.into()
    }
}

/// An IPv6 address guaranteed to exist within the Link-local range.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct UnicastLinkLocalIpv6Addr(Ipv6Addr);

impl UnicastLinkLocalIpv6Addr {
    /// Create a [UnicastLinkLocalIpv6Addr] from eight 16-bit segments.
    #[expect(clippy::too_many_arguments, reason = "mirrors std::net::Ipv6Addr::new")]
    pub fn new(
        a: u16,
        b: u16,
        c: u16,
        d: u16,
        e: u16,
        f: u16,
        g: u16,
        h: u16,
    ) -> Result<Self, UnicastLinkLocalIpAddrError> {
        let new = Ipv6Addr::new(a, b, c, d, e, f, g, h);
        Self::from_addr(new)
    }

    /// Returns the 16-element byte array that makes up this address.
    pub fn octets(&self) -> [u8; 16] {
        self.0.octets()
    }

    /// Create a [UnicastLinkLocalIpv6Addr] from a 16-element byte array.
    pub fn from_octets(octets: [u8; 16]) -> Result<Self, UnicastLinkLocalIpAddrError> {
        let new = Ipv6Addr::from(octets);
        Self::from_addr(new)
    }

    /// Converts this address into a `u128` in native byte order.
    pub fn to_bits(self) -> u128 {
        self.0.to_bits()
    }

    /// Create a [UnicastLinkLocalIpv6Addr] from a native byte order [u128].
    pub fn from_bits(bits: u128) -> Result<Self, UnicastLinkLocalIpAddrError> {
        let new = Ipv6Addr::from_bits(bits);
        Self::from_addr(new)
    }

    /// Returns the eight 16-bit segments that make up this address.
    pub fn segments(&self) -> [u16; 8] {
        self.0.segments()
    }

    /// Create a [UnicastLinkLocalIpv6Addr] from an eight element 16-bit array.
    pub fn from_segments(segments: [u16; 8]) -> Result<Self, UnicastLinkLocalIpAddrError> {
        let new = Ipv6Addr::from(segments);
        Self::from_addr(new)
    }

    /// Create a [UnicastLinkLocalIpv6Addr] from an [Ipv6Addr].
    pub fn from_addr(addr: Ipv6Addr) -> Result<Self, UnicastLinkLocalIpAddrError> {
        if addr.is_unicast_link_local() {
            return Ok(Self(addr));
        }
        Err(UnicastLinkLocalIpAddrError(addr.into()))
    }

    /// Converts this address into the underlying [Ipv6Addr].
    pub fn to_addr(self) -> Ipv6Addr {
        self.0
    }
}

impl From<UnicastLinkLocalIpv6Addr> for u128 {
    fn from(value: UnicastLinkLocalIpv6Addr) -> Self {
        value.to_bits()
    }
}

impl TryFrom<u128> for UnicastLinkLocalIpv6Addr {
    type Error = UnicastLinkLocalIpAddrError;

    fn try_from(value: u128) -> Result<Self, Self::Error> {
        Self::from_bits(value)
    }
}

impl TryFrom<[u8; 16]> for UnicastLinkLocalIpv6Addr {
    type Error = UnicastLinkLocalIpAddrError;

    fn try_from(value: [u8; 16]) -> Result<Self, Self::Error> {
        Self::from_octets(value)
    }
}

impl TryFrom<[u16; 8]> for UnicastLinkLocalIpv6Addr {
    type Error = UnicastLinkLocalIpAddrError;

    fn try_from(value: [u16; 8]) -> Result<Self, Self::Error> {
        Self::from_segments(value)
    }
}

impl TryFrom<Ipv6Addr> for UnicastLinkLocalIpv6Addr {
    type Error = UnicastLinkLocalIpAddrError;

    fn try_from(value: Ipv6Addr) -> Result<Self, Self::Error> {
        Self::from_addr(value)
    }
}

impl From<UnicastLinkLocalIpv6Addr> for Ipv6Addr {
    fn from(value: UnicastLinkLocalIpv6Addr) -> Self {
        value.to_addr()
    }
}

impl From<UnicastLinkLocalIpv6Addr> for IpAddr {
    fn from(value: UnicastLinkLocalIpv6Addr) -> Self {
        IpAddr::V6(value.to_addr())
    }
}

impl std::fmt::Display for UnicastLinkLocalIpv6Addr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::str::FromStr for UnicastLinkLocalIpv6Addr {
    type Err = UnicastLinkLocalIpAddrParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let addr: Ipv6Addr = s
            .parse()
            .map_err(UnicastLinkLocalIpAddrParseError::InvalidAddr)?;
        Self::try_from(addr).map_err(UnicastLinkLocalIpAddrParseError::NotUnicastLinkLocal)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for UnicastLinkLocalIpv6Addr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        <String as serde::Deserialize>::deserialize(deserializer)?
            .parse()
            .map_err(serde::de::Error::custom)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for UnicastLinkLocalIpv6Addr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_str(self)
    }
}

#[cfg(feature = "schemars")]
// The inherited `ipv6` format describes the full address syntax; this pattern
// only narrows the first segment to fe80..=febf.
const UNICAST_LINK_LOCAL_IPV6_ADDR_REGEX: &str = r"^[fF][eE][89aAbB][0-9a-fA-F]:";

#[cfg(feature = "schemars")]
impl schemars::JsonSchema for UnicastLinkLocalIpv6Addr {
    fn schema_name() -> String {
        "UnicastLinkLocalIpv6Addr".to_string()
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        let schema = gen.subschema_for::<Ipv6Addr>();
        let mut schema_object = schema.into_object();
        schema_object.metadata = Some(Box::new(schemars::schema::Metadata {
            title: Some("A unicast link-local IPv6 address".to_string()),
            description: Some("An IPv6 address in fe80::/10".to_string()),
            examples: vec!["fe80::1".into()],
            ..Default::default()
        }));
        schema_object.string = Some(Box::new(schemars::schema::StringValidation {
            pattern: Some(UNICAST_LINK_LOCAL_IPV6_ADDR_REGEX.to_string()),
            ..Default::default()
        }));
        schema_object.extensions =
            crate::schema_util::extension("UnicastLinkLocalIpv6Addr", "0.1.7");
        schema_object.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_str_parses_ipv4_link_local_address() {
        let addr: UnicastLinkLocalIpv4Addr = "169.254.1.2".parse().unwrap();
        assert_eq!(addr.to_addr(), Ipv4Addr::new(169, 254, 1, 2));
    }

    #[test]
    fn from_str_parses_ipv6_link_local_address() {
        let addr: UnicastLinkLocalIpv6Addr = "fe80::1".parse().unwrap();
        assert_eq!(addr.to_addr(), "fe80::1".parse::<Ipv6Addr>().unwrap());
    }

    #[test]
    fn from_str_parses_either_address_family() {
        let ipv4: UnicastLinkLocalIpAddr = "169.254.1.2".parse().unwrap();
        let ipv6: UnicastLinkLocalIpAddr = "febf::1".parse().unwrap();

        assert!(ipv4.is_ipv4());
        assert!(ipv6.is_ipv6());
    }

    #[test]
    fn from_str_rejects_malformed_address() {
        let error = "not-an-address"
            .parse::<UnicastLinkLocalIpAddr>()
            .unwrap_err();
        assert!(matches!(
            error,
            UnicastLinkLocalIpAddrParseError::InvalidAddr(_)
        ));
    }

    #[test]
    fn from_str_rejects_non_link_local_addresses() {
        for addr in ["192.0.2.1", "2001:db8::1"] {
            let error = addr.parse::<UnicastLinkLocalIpAddr>().unwrap_err();
            assert!(matches!(
                error,
                UnicastLinkLocalIpAddrParseError::NotUnicastLinkLocal(_)
            ));
        }
    }

    #[test]
    fn ipv4_constructors_accept_entire_link_local_range() {
        for octets in [[169, 254, 0, 0], [169, 254, 255, 255]] {
            let expected = Ipv4Addr::from(octets);
            let validated = UnicastLinkLocalIpv4Addr::from_addr(expected).unwrap();
            let generic = UnicastLinkLocalIpAddr::from(validated);

            assert_eq!(validated.to_addr(), expected);
            assert_eq!(validated.octets(), octets);
            assert_eq!(validated.to_bits(), expected.to_bits());
            assert_eq!(
                UnicastLinkLocalIpv4Addr::new(octets[0], octets[1], octets[2], octets[3]).unwrap(),
                validated
            );
            assert_eq!(
                UnicastLinkLocalIpv4Addr::from_octets(octets).unwrap(),
                validated
            );
            assert_eq!(
                UnicastLinkLocalIpv4Addr::try_from(octets).unwrap(),
                validated
            );
            assert_eq!(
                UnicastLinkLocalIpv4Addr::from_bits(expected.to_bits()).unwrap(),
                validated
            );
            assert_eq!(
                UnicastLinkLocalIpv4Addr::try_from(expected.to_bits()).unwrap(),
                validated
            );
            assert_eq!(
                UnicastLinkLocalIpAddr::new(expected.into()).unwrap(),
                generic
            );
            assert_eq!(UnicastLinkLocalIpv4Addr::try_from(expected), Ok(validated));
            assert_eq!(UnicastLinkLocalIpAddr::try_from(expected), Ok(generic));
            assert_eq!(
                UnicastLinkLocalIpAddr::try_from(IpAddr::V4(expected)),
                Ok(generic)
            );
            assert_eq!(Ipv4Addr::from(validated), expected);
            assert_eq!(IpAddr::from(validated), IpAddr::V4(expected));
            assert_eq!(IpAddr::from(generic), IpAddr::V4(expected));
        }
    }

    #[test]
    fn ipv4_constructors_reject_addresses_outside_link_local_range() {
        for octets in [[169, 253, 255, 255], [169, 255, 0, 0]] {
            let addr = Ipv4Addr::from(octets);
            let expected = IpAddr::V4(addr);
            let errors = [
                UnicastLinkLocalIpv4Addr::from_addr(addr).unwrap_err(),
                UnicastLinkLocalIpv4Addr::new(octets[0], octets[1], octets[2], octets[3])
                    .unwrap_err(),
                UnicastLinkLocalIpv4Addr::from_octets(octets).unwrap_err(),
                UnicastLinkLocalIpv4Addr::try_from(octets).unwrap_err(),
                UnicastLinkLocalIpv4Addr::from_bits(addr.to_bits()).unwrap_err(),
                UnicastLinkLocalIpv4Addr::try_from(addr.to_bits()).unwrap_err(),
                UnicastLinkLocalIpv4Addr::try_from(addr).unwrap_err(),
                UnicastLinkLocalIpAddr::new(expected).unwrap_err(),
                UnicastLinkLocalIpAddr::try_from(addr).unwrap_err(),
                UnicastLinkLocalIpAddr::try_from(expected).unwrap_err(),
            ];

            for error in errors {
                assert_eq!(error.addr(), expected);
            }
        }
    }

    #[test]
    fn ipv6_constructors_accept_link_local_range_boundaries() {
        for segments in [
            [0xfe80, 0, 0, 0, 0, 0, 0, 0],
            [
                0xfebf, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff,
            ],
        ] {
            let expected = Ipv6Addr::from(segments);
            let octets = expected.octets();
            let validated = UnicastLinkLocalIpv6Addr::from_addr(expected).unwrap();
            let generic = UnicastLinkLocalIpAddr::from(validated);

            assert_eq!(validated.to_addr(), expected);
            assert_eq!(validated.octets(), octets);
            assert_eq!(validated.segments(), segments);
            assert_eq!(validated.to_bits(), expected.to_bits());
            assert_eq!(
                UnicastLinkLocalIpv6Addr::new(
                    segments[0],
                    segments[1],
                    segments[2],
                    segments[3],
                    segments[4],
                    segments[5],
                    segments[6],
                    segments[7]
                )
                .unwrap(),
                validated
            );
            assert_eq!(
                UnicastLinkLocalIpv6Addr::from_octets(octets).unwrap(),
                validated
            );
            assert_eq!(
                UnicastLinkLocalIpv6Addr::try_from(octets).unwrap(),
                validated
            );
            assert_eq!(
                UnicastLinkLocalIpv6Addr::from_segments(segments).unwrap(),
                validated
            );
            assert_eq!(
                UnicastLinkLocalIpv6Addr::try_from(segments).unwrap(),
                validated
            );
            assert_eq!(
                UnicastLinkLocalIpv6Addr::from_bits(expected.to_bits()).unwrap(),
                validated
            );
            assert_eq!(
                UnicastLinkLocalIpv6Addr::try_from(expected.to_bits()).unwrap(),
                validated
            );
            assert_eq!(
                UnicastLinkLocalIpAddr::new(expected.into()).unwrap(),
                generic
            );
            assert_eq!(UnicastLinkLocalIpv6Addr::try_from(expected), Ok(validated));
            assert_eq!(UnicastLinkLocalIpAddr::try_from(expected), Ok(generic));
            assert_eq!(
                UnicastLinkLocalIpAddr::try_from(IpAddr::V6(expected)),
                Ok(generic)
            );
            assert_eq!(Ipv6Addr::from(validated), expected);
            assert_eq!(IpAddr::from(validated), IpAddr::V6(expected));
            assert_eq!(IpAddr::from(generic), IpAddr::V6(expected));
        }
    }

    #[test]
    fn ipv6_constructors_reject_addresses_outside_link_local_range() {
        for segments in [
            [
                0xfe7f, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff,
            ],
            [0xfec0, 0, 0, 0, 0, 0, 0, 0],
            [0xff02, 0, 0, 0, 0, 0, 0, 1],
        ] {
            let addr = Ipv6Addr::from(segments);
            let octets = addr.octets();
            let expected = IpAddr::V6(addr);
            let errors = [
                UnicastLinkLocalIpv6Addr::from_addr(addr).unwrap_err(),
                UnicastLinkLocalIpv6Addr::new(
                    segments[0],
                    segments[1],
                    segments[2],
                    segments[3],
                    segments[4],
                    segments[5],
                    segments[6],
                    segments[7],
                )
                .unwrap_err(),
                UnicastLinkLocalIpv6Addr::from_octets(octets).unwrap_err(),
                UnicastLinkLocalIpv6Addr::try_from(octets).unwrap_err(),
                UnicastLinkLocalIpv6Addr::from_segments(segments).unwrap_err(),
                UnicastLinkLocalIpv6Addr::try_from(segments).unwrap_err(),
                UnicastLinkLocalIpv6Addr::from_bits(addr.to_bits()).unwrap_err(),
                UnicastLinkLocalIpv6Addr::try_from(addr.to_bits()).unwrap_err(),
                UnicastLinkLocalIpv6Addr::try_from(addr).unwrap_err(),
                UnicastLinkLocalIpAddr::new(expected).unwrap_err(),
                UnicastLinkLocalIpAddr::try_from(addr).unwrap_err(),
                UnicastLinkLocalIpAddr::try_from(expected).unwrap_err(),
            ];

            for error in errors {
                assert_eq!(error.addr(), expected);
            }
        }
    }

    #[cfg(all(feature = "serde", feature = "schemars"))]
    #[test]
    fn serde_serializes_all_address_types_as_canonical_strings() {
        let generic_v4: UnicastLinkLocalIpAddr = "169.254.1.2".parse().unwrap();
        let generic_v6: UnicastLinkLocalIpAddr = "FE80:0:0:0:0:0:0:1".parse().unwrap();
        let ipv4: UnicastLinkLocalIpv4Addr = "169.254.1.2".parse().unwrap();
        let ipv6: UnicastLinkLocalIpv6Addr = "FE80:0:0:0:0:0:0:1".parse().unwrap();

        assert_eq!(
            serde_json::to_string(&generic_v4).unwrap(),
            r#""169.254.1.2""#
        );
        assert_eq!(serde_json::to_string(&generic_v6).unwrap(), r#""fe80::1""#);
        assert_eq!(serde_json::to_string(&ipv4).unwrap(), r#""169.254.1.2""#);
        assert_eq!(serde_json::to_string(&ipv6).unwrap(), r#""fe80::1""#);
    }

    #[cfg(all(feature = "serde", feature = "schemars"))]
    #[test]
    fn serde_round_trips_all_address_types() {
        let generic_v4: UnicastLinkLocalIpAddr = "169.254.1.2".parse().unwrap();
        let generic_v6: UnicastLinkLocalIpAddr = "fe80::1".parse().unwrap();
        let ipv4: UnicastLinkLocalIpv4Addr = "169.254.1.2".parse().unwrap();
        let ipv6: UnicastLinkLocalIpv6Addr = "fe80::1".parse().unwrap();

        let generic_v4_json = serde_json::to_string(&generic_v4).unwrap();
        let generic_v6_json = serde_json::to_string(&generic_v6).unwrap();
        let ipv4_json = serde_json::to_string(&ipv4).unwrap();
        let ipv6_json = serde_json::to_string(&ipv6).unwrap();

        assert_eq!(
            serde_json::from_str::<UnicastLinkLocalIpAddr>(&generic_v4_json).unwrap(),
            generic_v4
        );
        assert_eq!(
            serde_json::from_str::<UnicastLinkLocalIpAddr>(&generic_v6_json).unwrap(),
            generic_v6
        );
        assert_eq!(
            serde_json::from_str::<UnicastLinkLocalIpv4Addr>(&ipv4_json).unwrap(),
            ipv4
        );
        assert_eq!(
            serde_json::from_str::<UnicastLinkLocalIpv6Addr>(&ipv6_json).unwrap(),
            ipv6
        );
    }

    #[cfg(all(feature = "serde", feature = "schemars"))]
    #[test]
    fn serde_accepts_link_local_range_boundaries() {
        for addr in [r#""169.254.0.0""#, r#""169.254.255.255""#] {
            assert!(serde_json::from_str::<UnicastLinkLocalIpAddr>(addr).is_ok());
            assert!(serde_json::from_str::<UnicastLinkLocalIpv4Addr>(addr).is_ok());
        }

        for addr in [
            r#""fe80::""#,
            r#""febf:ffff:ffff:ffff:ffff:ffff:ffff:ffff""#,
        ] {
            assert!(serde_json::from_str::<UnicastLinkLocalIpAddr>(addr).is_ok());
            assert!(serde_json::from_str::<UnicastLinkLocalIpv6Addr>(addr).is_ok());
        }
    }

    #[cfg(all(feature = "serde", feature = "schemars"))]
    #[test]
    fn serde_rejects_non_link_local_addresses() {
        for addr in [r#""169.253.255.255""#, r#""169.255.0.0""#, r#""192.0.2.1""#] {
            assert!(serde_json::from_str::<UnicastLinkLocalIpAddr>(addr).is_err());
            assert!(serde_json::from_str::<UnicastLinkLocalIpv4Addr>(addr).is_err());
        }

        for addr in [
            r#""fe7f:ffff:ffff:ffff:ffff:ffff:ffff:ffff""#,
            r#""fec0::""#,
            r#""2001:db8::1""#,
        ] {
            assert!(serde_json::from_str::<UnicastLinkLocalIpAddr>(addr).is_err());
            assert!(serde_json::from_str::<UnicastLinkLocalIpv6Addr>(addr).is_err());
        }
    }

    #[cfg(feature = "schemars")]
    #[test]
    fn schema_patterns_match_link_local_boundaries() {
        let ipv4 = regress::Regex::new(UNICAST_LINK_LOCAL_IPV4_ADDR_REGEX).unwrap();
        let ipv6 = regress::Regex::new(UNICAST_LINK_LOCAL_IPV6_ADDR_REGEX).unwrap();

        for addr in ["169.254.0.0", "169.254.255.255"] {
            assert!(ipv4.find(addr).is_some(), "expected {addr} to match");
        }
        for addr in ["169.253.255.255", "169.255.0.0"] {
            assert!(ipv4.find(addr).is_none(), "expected {addr} not to match");
        }
        for addr in ["fe80::", "FE9a::1", "feaf::1", "febf:ffff::1"] {
            assert!(ipv6.find(addr).is_some(), "expected {addr} to match");
        }
        for addr in ["fe7f::", "fec0::", "ff02::1"] {
            assert!(ipv6.find(addr).is_none(), "expected {addr} not to match");
        }
    }
}
