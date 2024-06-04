// Copyright 2024 Oxide Computer Company

use std::{
    net::{AddrParseError, IpAddr, Ipv4Addr, Ipv6Addr},
    num::ParseIntError,
};

/// A prefix error during the creation of an [IpNet], [Ipv4Net], or [Ipv6Net]
#[derive(Debug, Clone)]
pub struct IpNetPrefixError(u8);

impl std::fmt::Display for IpNetPrefixError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "invalid network prefix {}", self.0)
    }
}
impl std::error::Error for IpNetPrefixError {}

/// An error during the parsing of an [IpNet], [Ipv4Net], or [Ipv6Net]
#[derive(Debug, Clone)]
pub enum IpNetParseError {
    /// Failure to parse the address
    InvalidAddr(AddrParseError),
    /// Bad prefix value
    PrefixValue(IpNetPrefixError),
    /// No slash to indicate the prefix
    NoPrefix,
    /// Prefix parse error
    InvalidPrefix(ParseIntError),
}

impl std::fmt::Display for IpNetParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IpNetParseError::InvalidAddr(e) => e.fmt(f),
            IpNetParseError::PrefixValue(e) => {
                write!(f, "invalid prefix value: {}", e)
            }
            IpNetParseError::NoPrefix => write!(f, "missing '/' character"),
            IpNetParseError::InvalidPrefix(e) => e.fmt(f),
        }
    }
}
impl std::error::Error for IpNetParseError {}

/// A subnet, either IPv4 or IPv6
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[cfg_attr(feature = "serde", serde(untagged))]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum IpNet {
    /// An IPv4 subnet
    V4(Ipv4Net),
    /// An IPv6 subnet
    V6(Ipv6Net),
}

impl IpNet {
    /// Create an IpNet with the given address and prefix width.
    pub fn new(addr: IpAddr, prefix: u8) -> Result<Self, IpNetPrefixError> {
        match addr {
            IpAddr::V4(addr) => Ok(Self::V4(Ipv4Net::new(addr, prefix)?)),
            IpAddr::V6(addr) => Ok(Self::V6(Ipv6Net::new(addr, prefix)?)),
        }
    }

    /// Create an IpNet with the given address and prefix width with no checks
    /// for the validity of the prefix length.
    pub const fn new_unchecked(addr: IpAddr, prefix: u8) -> Self {
        match addr {
            IpAddr::V4(addr) => Self::V4(Ipv4Net::new_unchecked(addr, prefix)),
            IpAddr::V6(addr) => Self::V6(Ipv6Net::new_unchecked(addr, prefix)),
        }
    }

    /// Create an IpNet that contains *exclusively* the given address.
    pub fn host_net(addr: IpAddr) -> Self {
        match addr {
            IpAddr::V4(addr) => Self::V4(Ipv4Net::host_net(addr)),
            IpAddr::V6(addr) => Self::V6(Ipv6Net::host_net(addr)),
        }
    }

    /// Return the base address.
    pub const fn addr(&self) -> IpAddr {
        match self {
            IpNet::V4(inner) => IpAddr::V4(inner.addr()),
            IpNet::V6(inner) => IpAddr::V6(inner.addr()),
        }
    }

    /// Return the prefix address (the base address with the mask applied).
    pub fn prefix(&self) -> IpAddr {
        match self {
            IpNet::V4(inner) => inner.prefix().into(),
            IpNet::V6(inner) => inner.prefix().into(),
        }
    }

    /// Return the prefix length.
    pub const fn width(&self) -> u8 {
        match self {
            IpNet::V4(inner) => inner.width(),
            IpNet::V6(inner) => inner.width(),
        }
    }

    /// Return `true` iff the subnet contains only the base address i.e. the
    /// size is exactly one address.
    pub const fn is_host_net(&self) -> bool {
        match self {
            IpNet::V4(inner) => inner.is_host_net(),
            IpNet::V6(inner) => inner.is_host_net(),
        }
    }

    /// Return `true`` if the provided address is contained in self.
    ///
    /// This returns `false` if the address and the network are of different IP
    /// families.
    pub fn contains(&self, addr: IpAddr) -> bool {
        match (self, addr) {
            (IpNet::V4(net), IpAddr::V4(ip)) => net.contains(ip),
            (IpNet::V6(net), IpAddr::V6(ip)) => net.contains(ip),
            (_, _) => false,
        }
    }
}

impl From<Ipv4Net> for IpNet {
    fn from(n: Ipv4Net) -> IpNet {
        IpNet::V4(n)
    }
}

impl From<Ipv6Net> for IpNet {
    fn from(n: Ipv6Net) -> IpNet {
        IpNet::V6(n)
    }
}

impl std::fmt::Display for IpNet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IpNet::V4(inner) => write!(f, "{}", inner),
            IpNet::V6(inner) => write!(f, "{}", inner),
        }
    }
}

impl std::str::FromStr for IpNet {
    type Err = IpNetParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let Some((addr_str, prefix_str)) = s.split_once('/') else {
            return Err(IpNetParseError::NoPrefix);
        };

        let prefix = prefix_str.parse().map_err(IpNetParseError::InvalidPrefix)?;
        let addr = addr_str.parse().map_err(IpNetParseError::InvalidAddr)?;
        IpNet::new(addr, prefix).map_err(IpNetParseError::PrefixValue)
    }
}

#[cfg(feature = "schemars")]
impl schemars::JsonSchema for IpNet {
    fn schema_name() -> String {
        "IpNet".to_string()
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        use crate::schema_util::label_schema;
        schemars::schema::SchemaObject {
            subschemas: Some(Box::new(schemars::schema::SubschemaValidation {
                one_of: Some(vec![
                    label_schema("v4", gen.subschema_for::<Ipv4Net>()),
                    label_schema("v6", gen.subschema_for::<Ipv6Net>()),
                ]),
                ..Default::default()
            })),
            extensions: crate::schema_util::extension("IpNet", "0.1.0"),
            ..Default::default()
        }
        .into()
    }
}

/// The maximum width of an IPv4 subnet
pub const IPV4_NET_WIDTH_MAX: u8 = 32;

/// An IPv4 subnet
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ipv4Net {
    addr: Ipv4Addr,
    width: u8,
}

impl Ipv4Net {
    /// Create an Ipv4Net with the given address and prefix width.
    pub fn new(addr: Ipv4Addr, width: u8) -> Result<Self, IpNetPrefixError> {
        if width > IPV4_NET_WIDTH_MAX {
            Err(IpNetPrefixError(width))
        } else {
            Ok(Self { addr, width })
        }
    }

    /// Create an Ipv4Net with the given address and prefix width with no
    /// checks for the validity of the prefix length.
    pub const fn new_unchecked(addr: Ipv4Addr, width: u8) -> Self {
        Self { addr, width }
    }

    /// Create an Ipv4Net that contains *exclusively* the given address.
    pub const fn host_net(addr: Ipv4Addr) -> Self {
        Self {
            addr,
            width: IPV4_NET_WIDTH_MAX,
        }
    }

    /// Return the base address used to create this subnet.
    pub const fn addr(&self) -> Ipv4Addr {
        self.addr
    }

    /// Return the prefix width.
    pub const fn width(&self) -> u8 {
        self.width
    }

    pub(crate) fn mask(&self) -> u32 {
        u32::MAX
            .checked_shl((IPV4_NET_WIDTH_MAX - self.width) as u32)
            .unwrap_or(0)
    }

    /// Return true iff the subnet contains only the base address i.e. the
    /// size is exactly one address.
    pub const fn is_host_net(&self) -> bool {
        self.width == IPV4_NET_WIDTH_MAX
    }

    /// Return the number of addresses contained within this subnet or None for
    /// a /0 subnet whose value would be one larger than can be represented in
    /// a `u32`.
    pub const fn size(&self) -> Option<u32> {
        1u32.checked_shl((IPV4_NET_WIDTH_MAX - self.width) as u32)
    }

    /// Return the prefix address (the base address with the mask applied).
    pub fn prefix(&self) -> Ipv4Addr {
        self.first_addr()
    }

    /// Return the network address for subnets as applicable; /31 and /32
    /// subnets return `None`.
    pub fn network(&self) -> Option<Ipv4Addr> {
        (self.width < 31).then(|| self.first_addr())
    }

    /// Return the broadcast address for subnets as applicable; /31 and /32
    /// subnets return `None`.
    pub fn broadcast(&self) -> Option<Ipv4Addr> {
        (self.width < 31).then(|| self.last_addr())
    }

    /// Return the first address within this subnet.
    pub fn first_addr(&self) -> Ipv4Addr {
        let addr: u32 = self.addr.into();
        Ipv4Addr::from(addr & self.mask())
    }

    /// Return the last address within this subnet.
    pub fn last_addr(&self) -> Ipv4Addr {
        let addr: u32 = self.addr.into();
        Ipv4Addr::from(addr | !self.mask())
    }

    /// Return the first host address within this subnet. For /31 and /32
    /// subnets that is the first address; for wider subnets this returns
    /// the address immediately after the network address.
    pub fn first_host(&self) -> Ipv4Addr {
        let mask = self.mask();
        let addr: u32 = self.addr.into();
        let first = addr & mask;
        if self.width == 31 || self.width == 32 {
            Ipv4Addr::from(first)
        } else {
            Ipv4Addr::from(first + 1)
        }
    }

    /// Return the last host address within this subnet. For /31 and /32
    /// subnets that is the last address (there is no broadcast address); for
    /// wider subnets this returns the address immediately before the broadcast
    /// address.
    pub fn last_host(&self) -> Ipv4Addr {
        let mask = self.mask();
        let addr: u32 = self.addr.into();
        let last = addr | !mask;
        if self.width == 31 || self.width == 32 {
            Ipv4Addr::from(last)
        } else {
            Ipv4Addr::from(last - 1)
        }
    }

    /// Return `true` iff the given IP address is within the subnet.
    pub fn contains(&self, other: Ipv4Addr) -> bool {
        let mask = self.mask();
        let addr: u32 = self.addr.into();
        let other: u32 = other.into();

        (addr & mask) == (other & mask)
    }

    /// Return the nth address within this subnet or none if `n` is larger than
    /// the size of the subnet.
    pub fn nth(&self, n: usize) -> Option<Ipv4Addr> {
        let addr: u32 = self.addr.into();
        let nth = addr.checked_add(n.try_into().ok()?)?;
        (nth <= self.last_addr().into()).then_some(nth.into())
    }

    /// Produce an iterator over all addresses within this subnet.
    pub fn addr_iter(&self) -> impl Iterator<Item = Ipv4Addr> {
        Ipv4NetIter {
            next: Some(self.first_addr().into()),
            last: self.last_addr().into(),
        }
    }

    /// Produce an iterator over all hosts within this subnet. For /31 and /32
    /// subnets, this is all addresses; for all larger subnets this excludes
    /// the first (network) and last (broadcast) addresses.
    pub fn host_iter(&self) -> impl Iterator<Item = Ipv4Addr> {
        Ipv4NetIter {
            next: Some(self.first_host().into()),
            last: self.last_host().into(),
        }
    }
}

impl std::fmt::Display for Ipv4Net {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", &self.addr, self.width)
    }
}

impl std::str::FromStr for Ipv4Net {
    type Err = IpNetParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let Some((addr_str, prefix_str)) = s.split_once('/') else {
            return Err(IpNetParseError::NoPrefix);
        };

        let prefix = prefix_str.parse().map_err(IpNetParseError::InvalidPrefix)?;
        let addr = addr_str.parse().map_err(IpNetParseError::InvalidAddr)?;
        Ipv4Net::new(addr, prefix).map_err(IpNetParseError::PrefixValue)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Ipv4Net {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        String::deserialize(deserializer)?
            .parse()
            .map_err(<D::Error as serde::de::Error>::custom)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Ipv4Net {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&format!("{}", self))
    }
}

#[cfg(feature = "schemars")]
const IPV4_NET_REGEX: &str = concat!(
    r#"^(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\.){3}"#,
    r#"([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])"#,
    r#"/([0-9]|1[0-9]|2[0-9]|3[0-2])$"#,
);

#[cfg(feature = "schemars")]
impl schemars::JsonSchema for Ipv4Net {
    fn schema_name() -> String {
        "Ipv4Net".to_string()
    }

    fn json_schema(_: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        schemars::schema::SchemaObject {
            metadata: Some(Box::new(schemars::schema::Metadata {
                title: Some("An IPv4 subnet".to_string()),
                description: Some("An IPv4 subnet, including prefix and prefix length".to_string()),
                examples: vec!["192.168.1.0/24".into()],
                ..Default::default()
            })),
            instance_type: Some(schemars::schema::InstanceType::String.into()),
            string: Some(Box::new(schemars::schema::StringValidation {
                pattern: Some(IPV4_NET_REGEX.to_string()),
                ..Default::default()
            })),
            extensions: crate::schema_util::extension("Ipv4Net", "0.1.0"),
            ..Default::default()
        }
        .into()
    }
}

/// The highest value for an IPv6 subnet prefix
pub const IPV6_NET_WIDTH_MAX: u8 = 128;

/// An IPv6 subnet
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ipv6Net {
    addr: Ipv6Addr,
    width: u8,
}

impl Ipv6Net {
    /// Create an Ipv6Net with the given base address and prefix width.
    pub fn new(addr: Ipv6Addr, width: u8) -> Result<Self, IpNetPrefixError> {
        if width > IPV6_NET_WIDTH_MAX {
            Err(IpNetPrefixError(width))
        } else {
            Ok(Self { addr, width })
        }
    }

    /// Create an Ipv6Net with the given address and prefix width with no
    /// checks for the validity of the prefix length.
    pub const fn new_unchecked(addr: Ipv6Addr, width: u8) -> Self {
        Self { addr, width }
    }

    /// Create an Ipv6Net that contains *exclusively* the given address.
    pub const fn host_net(addr: Ipv6Addr) -> Self {
        Self {
            addr,
            width: IPV6_NET_WIDTH_MAX,
        }
    }

    /// Return the base address used to create this subnet.
    pub const fn addr(&self) -> Ipv6Addr {
        self.addr
    }

    /// Return the prefix width.
    pub const fn width(&self) -> u8 {
        self.width
    }

    pub(crate) fn mask(&self) -> u128 {
        u128::MAX
            .checked_shl((IPV6_NET_WIDTH_MAX - self.width) as u32)
            .unwrap_or(0)
    }

    /// Return the netmask address derived from prefix length.
    pub fn mask_addr(&self) -> Ipv6Addr {
        Ipv6Addr::from(self.mask())
    }

    /// Return true iff the subnet contains only the base address i.e. the
    /// size is exactly one address.
    pub const fn is_host_net(&self) -> bool {
        self.width == IPV6_NET_WIDTH_MAX
    }

    /// Return the number of addresses contained within this subnet or None for
    /// a /0 subnet whose value would be one larger than can be represented in
    /// a `u128`.
    pub const fn size(&self) -> Option<u128> {
        1u128.checked_shl((IPV6_NET_WIDTH_MAX - self.width) as u32)
    }

    /// Return the prefix address (the base address with the mask applied).
    pub fn prefix(&self) -> Ipv6Addr {
        self.first_addr()
    }

    /// Return `true` if this subnetwork is in the IPv6 Unique Local Address
    /// range defined in RFC 4193, e.g., `fd00:/8`
    pub fn is_unique_local(&self) -> bool {
        // TODO: Delegate to `Ipv6Addr::is_unique_local()` when stabilized.
        self.first_addr().octets()[0] == 0xfd
    }

    /// Return the first address within this subnet.
    pub fn first_addr(&self) -> Ipv6Addr {
        let addr: u128 = self.addr.into();
        Ipv6Addr::from(addr & self.mask())
    }

    /// The broadcast address for this subnet which is also the last address.
    pub fn last_addr(&self) -> Ipv6Addr {
        let addr: u128 = self.addr.into();
        Ipv6Addr::from(addr | !self.mask())
    }

    /// Return an interator over the addresses of this subnet.
    pub fn iter(&self) -> impl Iterator<Item = Ipv6Addr> {
        Ipv6NetIter {
            next: Some(self.first_addr().into()),
            last: self.last_addr().into(),
        }
    }

    /// Return `true` if the address is within the subnet.
    pub fn contains(&self, other: Ipv6Addr) -> bool {
        let mask = self.mask();
        let addr: u128 = self.addr.into();
        let other: u128 = other.into();

        (addr & mask) == (other & mask)
    }

    /// Return the nth address within this subnet or none if `n` is larger than
    /// the size of the subnet.
    pub fn nth(&self, n: u128) -> Option<Ipv6Addr> {
        let addr: u128 = self.addr.into();
        let nth = addr.checked_add(n)?;
        (nth <= self.last_addr().into()).then_some(nth.into())
    }

    /// Returns `true` iff this subnet is wholly contained within `other`.
    pub fn is_subnet_of(&self, other: &Self) -> bool {
        other.first_addr() <= self.first_addr() && other.last_addr() >= self.last_addr()
    }

    /// Returns `true` iff `other` is wholly contained within this subnet.
    pub fn is_supernet_of(&self, other: &Self) -> bool {
        other.is_subnet_of(self)
    }
}

impl std::fmt::Display for Ipv6Net {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", &self.addr, self.width)
    }
}

impl std::str::FromStr for Ipv6Net {
    type Err = IpNetParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let Some((addr_str, prefix_str)) = s.split_once('/') else {
            return Err(IpNetParseError::NoPrefix);
        };

        let prefix = prefix_str.parse().map_err(IpNetParseError::InvalidPrefix)?;
        let addr = addr_str.parse().map_err(IpNetParseError::InvalidAddr)?;
        Ipv6Net::new(addr, prefix).map_err(IpNetParseError::PrefixValue)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Ipv6Net {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        String::deserialize(deserializer)?
            .parse()
            .map_err(<D::Error as serde::de::Error>::custom)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Ipv6Net {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&format!("{}", self))
    }
}

#[cfg(feature = "schemars")]
const IPV6_NET_REGEX: &str = concat!(
    r#"^("#,
    r#"([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|"#,
    r#"([0-9a-fA-F]{1,4}:){1,7}:|"#,
    r#"([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|"#,
    r#"([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|"#,
    r#"([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|"#,
    r#"([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|"#,
    r#"([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|"#,
    r#"[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|"#,
    r#":((:[0-9a-fA-F]{1,4}){1,7}|:)|"#,
    r#"fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|"#,
    r#"::(ffff(:0{1,4}){0,1}:){0,1}"#,
    r#"((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}"#,
    r#"(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|"#,
    r#"([0-9a-fA-F]{1,4}:){1,4}:"#,
    r#"((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}"#,
    r#"(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])"#,
    r#")\/([0-9]|[1-9][0-9]|1[0-1][0-9]|12[0-8])$"#,
);

#[cfg(feature = "schemars")]
impl schemars::JsonSchema for Ipv6Net {
    fn schema_name() -> String {
        "Ipv6Net".to_string()
    }

    fn json_schema(_: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        schemars::schema::SchemaObject {
            metadata: Some(Box::new(schemars::schema::Metadata {
                title: Some("An IPv6 subnet".to_string()),
                description: Some("An IPv6 subnet, including prefix and subnet mask".to_string()),
                examples: vec!["fd12:3456::/64".into()],
                ..Default::default()
            })),
            instance_type: Some(schemars::schema::InstanceType::String.into()),
            string: Some(Box::new(schemars::schema::StringValidation {
                pattern: Some(IPV6_NET_REGEX.to_string()),
                ..Default::default()
            })),
            extensions: crate::schema_util::extension("Ipv6Net", "0.1.0"),
            ..Default::default()
        }
        .into()
    }
}

pub struct Ipv4NetIter {
    next: Option<u32>,
    last: u32,
}

impl Iterator for Ipv4NetIter {
    type Item = Ipv4Addr;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next?;
        if next == self.last {
            self.next = None;
        } else {
            self.next = Some(next + 1)
        }
        Some(next.into())
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        let next = self.next?;
        let nth = next.checked_add(n as u32)?;
        self.next = (nth <= self.last).then_some(nth);
        self.next()
    }
}

pub struct Ipv6NetIter {
    next: Option<u128>,
    last: u128,
}

impl Iterator for Ipv6NetIter {
    type Item = Ipv6Addr;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next?;
        if next == self.last {
            self.next = None;
        } else {
            self.next = Some(next + 1)
        }
        Some(next.into())
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        let next = self.next?;
        let nth = next.checked_add(n as u128)?;
        self.next = (nth <= self.last).then_some(nth);
        self.next()
    }
}

#[cfg(feature = "ipnetwork")]
mod ipnetwork_feature {
    use super::*;
    use ipnetwork::{IpNetwork, Ipv4Network, Ipv6Network};

    impl From<IpNetwork> for IpNet {
        fn from(value: IpNetwork) -> Self {
            match value {
                IpNetwork::V4(net) => Self::V4(net.into()),
                IpNetwork::V6(net) => Self::V6(net.into()),
            }
        }
    }

    impl From<IpNet> for IpNetwork {
        fn from(value: IpNet) -> Self {
            match value {
                IpNet::V4(net) => Self::V4(net.into()),
                IpNet::V6(net) => Self::V6(net.into()),
            }
        }
    }

    impl From<Ipv4Network> for Ipv4Net {
        fn from(value: Ipv4Network) -> Self {
            Self {
                addr: value.ip(),
                width: value.prefix(),
            }
        }
    }

    impl From<Ipv4Net> for Ipv4Network {
        fn from(value: Ipv4Net) -> Self {
            Self::new(value.addr, value.width).unwrap()
        }
    }

    impl From<Ipv6Network> for Ipv6Net {
        fn from(value: Ipv6Network) -> Self {
            Self {
                addr: value.ip(),
                width: value.prefix(),
            }
        }
    }

    impl From<Ipv6Net> for Ipv6Network {
        fn from(value: Ipv6Net) -> Self {
            Self::new(value.addr, value.width).unwrap()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ipv6_regex() {
        let re = regress::Regex::new(IPV6_NET_REGEX).unwrap();
        for case in [
            "1:2:3:4:5:6:7:8",
            "1:a:2:b:3:c:4:d",
            "1::",
            "::1",
            "::",
            "1::3:4:5:6:7:8",
            "1:2::4:5:6:7:8",
            "1:2:3::5:6:7:8",
            "1:2:3:4::6:7:8",
            "1:2:3:4:5::7:8",
            "1:2:3:4:5:6::8",
            "1:2:3:4:5:6:7::",
            "2001::",
            "fd00::",
            "::100:1",
            "fd12:3456::",
        ] {
            for prefix in 0..=128 {
                let net = format!("{case}/{prefix}");
                assert!(
                    re.find(&net).is_some(),
                    "Expected to match IPv6 case: {}",
                    prefix,
                );
            }
        }
    }

    #[test]
    fn test_ipv4_net_operations() {
        let x: IpNet = "0.0.0.0/0".parse().unwrap();
        assert_eq!(x, IpNet::V4("0.0.0.0/0".parse().unwrap()));
    }

    #[test]
    fn test_ipnet_serde() {
        let net_str = "fd00:2::/32";
        let net: IpNet = net_str.parse().unwrap();
        let ser = serde_json::to_string(&net).unwrap();

        assert_eq!(format!(r#""{}""#, net_str), ser);
        let net_des = serde_json::from_str::<IpNet>(&ser).unwrap();
        assert_eq!(net, net_des);

        let net_str = "fd00:47::1/64";
        let net: IpNet = net_str.parse().unwrap();
        let ser = serde_json::to_string(&net).unwrap();

        assert_eq!(format!(r#""{}""#, net_str), ser);
        let net_des = serde_json::from_str::<IpNet>(&ser).unwrap();
        assert_eq!(net, net_des);

        let net_str = "192.168.1.1/16";
        let net: IpNet = net_str.parse().unwrap();
        let ser = serde_json::to_string(&net).unwrap();

        assert_eq!(format!(r#""{}""#, net_str), ser);
        let net_des = serde_json::from_str::<IpNet>(&ser).unwrap();
        assert_eq!(net, net_des);

        let net_str = "0.0.0.0/0";
        let net: IpNet = net_str.parse().unwrap();
        let ser = serde_json::to_string(&net).unwrap();

        assert_eq!(format!(r#""{}""#, net_str), ser);
        let net_des = serde_json::from_str::<IpNet>(&ser).unwrap();
        assert_eq!(net, net_des);
    }

    #[test]
    fn test_ipnet_size() {
        let net = Ipv4Net::host_net("1.2.3.4".parse().unwrap());
        assert_eq!(net.size(), Some(1));
        assert_eq!(net.width(), 32);
        assert_eq!(net.mask(), 0xffff_ffff);

        let net = Ipv4Net::new("1.2.3.4".parse().unwrap(), 24).unwrap();
        assert_eq!(net.size(), Some(256));
        assert_eq!(net.width(), 24);
        assert_eq!(net.mask(), 0xffff_ff00);

        let net = Ipv4Net::new("0.0.0.0".parse().unwrap(), 0).unwrap();
        assert_eq!(net.size(), None);
        assert_eq!(net.width(), 0);
        assert_eq!(net.mask(), 0);

        let net = Ipv6Net::host_net("fd00:47::1".parse().unwrap());
        assert_eq!(net.size(), Some(1));
        assert_eq!(net.width(), 128);
        assert_eq!(net.mask(), 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff);

        let net = Ipv6Net::new("fd00:47::1".parse().unwrap(), 56).unwrap();
        assert_eq!(net.size(), Some(0x0000_0000_0000_0100_0000_0000_0000_0000));
        assert_eq!(net.width(), 56);
        assert_eq!(net.mask(), 0xffff_ffff_ffff_ff00_0000_0000_0000_0000);
    }

    #[test]
    fn test_iter() {
        let ipnet = Ipv4Net::new(Ipv4Addr::new(0, 0, 0, 0), 0).unwrap();

        let actual = ipnet.addr_iter().take(5).collect::<Vec<_>>();
        let expected = (0..5).map(Ipv4Addr::from).collect::<Vec<_>>();
        assert_eq!(actual, expected);

        let actual = ipnet.addr_iter().skip(5).take(10).collect::<Vec<_>>();
        let expected = (5..15).map(Ipv4Addr::from).collect::<Vec<_>>();
        assert_eq!(actual, expected);

        let ipnet = Ipv6Net::new(Ipv6Addr::new(0, 0, 0, 0, 0, 0, 0, 0), 0).unwrap();

        let actual = ipnet.iter().take(5).collect::<Vec<_>>();
        let expected = (0..5).map(Ipv6Addr::from).collect::<Vec<_>>();
        assert_eq!(actual, expected);

        let actual = ipnet.iter().skip(5).take(10).collect::<Vec<_>>();
        let expected = (5..15).map(Ipv6Addr::from).collect::<Vec<_>>();
        assert_eq!(actual, expected);
    }
}
