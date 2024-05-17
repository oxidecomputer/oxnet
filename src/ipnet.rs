// Copyright 2024 Oxide Computer Company

use std::{
    net::{AddrParseError, IpAddr, Ipv4Addr, Ipv6Addr},
    num::ParseIntError,
};

#[derive(Debug, Clone)]
pub struct IpNetPrefixError(u8);

#[derive(Debug, Clone)]
pub enum IpNetParseError {
    InvalidAddr(AddrParseError),
    PrefixValue(IpNetPrefixError),
    MissingSlash,
    InvalidPrefix(ParseIntError),
}

impl std::fmt::Display for IpNetParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

/// A subnet, either IPv4 or IPv6
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[cfg_attr(feature = "serde", serde(untagged))]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum IpNet {
    V4(Ipv4Net),
    V6(Ipv6Net),
}

impl IpNet {
    pub fn new(addr: IpAddr, prefix: u8) -> Result<Self, IpNetPrefixError> {
        match addr {
            IpAddr::V4(addr) => Ok(Self::V4(Ipv4Net::new(addr, prefix)?)),
            IpAddr::V6(addr) => Ok(Self::V6(Ipv6Net::new(addr, prefix)?)),
        }
    }
    pub fn host_net(addr: IpAddr) -> Self {
        match addr {
            IpAddr::V4(addr) => Self::V4(Ipv4Net::host_net(addr)),
            IpAddr::V6(addr) => Self::V6(Ipv6Net::host_net(addr)),
        }
    }

    /// Returns the base address.
    pub fn addr(&self) -> IpAddr {
        match self {
            IpNet::V4(inner) => inner.addr().into(),
            IpNet::V6(inner) => inner.addr().into(),
        }
    }

    /// Returns the prefix length.
    pub fn prefix(&self) -> u8 {
        match self {
            IpNet::V4(inner) => inner.prefix(),
            IpNet::V6(inner) => inner.prefix(),
        }
    }

    pub fn is_host_net(&self) -> bool {
        match self {
            IpNet::V4(inner) => inner.is_host_net(),
            IpNet::V6(inner) => inner.is_host_net(),
        }
    }

    // Returns the first address in this subnet.
    // pub fn first_address(&self) -> IpAddr {
    //     match self {
    //         IpNet::V4(inner) => IpAddr::from(inner.iter().next().unwrap()),
    //         IpNet::V6(inner) => IpAddr::from(inner.iter().next().unwrap()),
    //     }
    // }

    // Return the last address in this subnet.
    //
    // For a subnet of size 1, e.g., a /32, this is the same as the first
    // address.
    // NOTE: This is a workaround for the fact that the `ipnetwork` crate's
    // iterator provides only the `Iterator::next()` method. That means that
    // finding the last address is linear in the size of the subnet, which is
    // completely untenable and totally avoidable with some addition. In the
    // long term, we should either put up a patch to the `ipnetwork` crate or
    // move the `ipnet` crate, which does provide an efficient iterator
    // implementation.
    // pub fn last_address(&self) -> IpAddr {
    //     match self {
    //         IpNet::V4(inner) => {
    //             let base: u32 = inner.network().into();
    //             let size = inner.size() - 1;
    //             std::net::IpAddr::V4(std::net::Ipv4Addr::from(base + size))
    //         }
    //         IpNet::V6(inner) => {
    //             let base: u128 = inner.network().into();
    //             let size = inner.size() - 1;
    //             std::net::IpAddr::V6(std::net::Ipv6Addr::from(base + size))
    //         }
    //     }
    // }

    // Return true if the provided address is contained in self.
    //
    // This returns false if the address and the network are of different IP
    // families.
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
        let Some(ii) = s.find('/') else {
            return Err(IpNetParseError::MissingSlash);
        };

        let addr_str = &s[..ii];
        let prefix_str = &s[ii + 1..];

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

pub const IPV4_NET_PREFIX_MAX: u8 = 32;

/// An IPv4 subnet
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ipv4Net {
    addr: Ipv4Addr,
    prefix: u8,
}

impl Ipv4Net {
    pub fn new(addr: Ipv4Addr, prefix: u8) -> Result<Self, IpNetPrefixError> {
        if prefix > IPV4_NET_PREFIX_MAX {
            Err(IpNetPrefixError(prefix))
        } else {
            Ok(Self { addr, prefix })
        }
    }
    pub fn host_net(addr: Ipv4Addr) -> Self {
        Self {
            addr,
            prefix: IPV4_NET_PREFIX_MAX,
        }
    }
    pub fn is_host_net(&self) -> bool {
        self.prefix == IPV4_NET_PREFIX_MAX
    }
    pub fn addr(&self) -> Ipv4Addr {
        self.addr
    }

    pub fn prefix(&self) -> u8 {
        self.prefix
    }

    pub fn size(&self) -> u32 {
        1u32 << self.prefix
    }

    // TODO should this be necessary or should we enforce during new?
    // TODO aka first_address
    /// XXX Canonical addr
    pub fn network(&self) -> Ipv4Addr {
        let addr: u32 = self.addr.into();
        Ipv4Addr::from(addr & self.mask_impl())
    }

    fn mask_impl(&self) -> u32 {
        ((1u32 << self.prefix) - 1) << (IPV4_NET_PREFIX_MAX - self.prefix)
    }

    /// Return `true` if the IP address is within the network.
    pub fn contains(&self, other: Ipv4Addr) -> bool {
        let prefix: u32 = self.addr.into();
        let other_addr: u32 = other.into();

        (other_addr & self.mask_impl()) == prefix
    }

    pub fn broadcast(&self) -> Ipv4Addr {
        let addr: u32 = self.addr.into();
        let last = addr | ((1u32 << (IPV4_NET_PREFIX_MAX - self.prefix)) - 1);
        Ipv4Addr::from(last)
    }

    pub fn iter(&self) -> Ipv4NetIter {
        let addr: u32 = self.addr.into();
        let last = addr | ((1u32 << (IPV4_NET_PREFIX_MAX - self.prefix)) - 1);
        Ipv4NetIter {
            next: Some(addr),
            last,
        }
    }

    pub fn nth(&self, n: u32) -> Option<Ipv4Addr> {
        let size = 1 << (IPV4_NET_PREFIX_MAX - self.prefix);
        (n < size).then(|| {
            let addr: u32 = self.addr.into();
            let nth = (addr & self.mask_impl()) + n;
            nth.into()
        })
    }
}

impl std::fmt::Display for Ipv4Net {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", &self.addr, self.prefix)
    }
}

impl std::str::FromStr for Ipv4Net {
    type Err = IpNetParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let Some(ii) = s.find('/') else {
            return Err(IpNetParseError::MissingSlash);
        };

        let addr_str = &s[..ii];
        let prefix_str = &s[ii + 1..];

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

pub const IPV6_NET_PREFIX_MAX: u8 = 128;

/// An IPv6 subnet
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ipv6Net {
    addr: Ipv6Addr,
    prefix: u8,
}

impl Ipv6Net {
    pub fn new(addr: Ipv6Addr, prefix: u8) -> Result<Self, IpNetPrefixError> {
        if prefix > IPV6_NET_PREFIX_MAX {
            Err(IpNetPrefixError(prefix))
        } else {
            Ok(Self { addr, prefix })
        }
    }

    pub fn host_net(addr: Ipv6Addr) -> Self {
        Self {
            addr,
            prefix: IPV6_NET_PREFIX_MAX,
        }
    }

    pub fn is_host_net(&self) -> bool {
        self.prefix == IPV6_NET_PREFIX_MAX
    }

    pub fn addr(&self) -> Ipv6Addr {
        self.addr
    }

    pub fn prefix(&self) -> u8 {
        self.prefix
    }

    pub fn size(&self) -> u128 {
        1u128 << self.prefix
    }

    /// Return `true` if this subnetwork is in the IPv6 Unique Local Address
    /// range defined in RFC 4193, e.g., `fd00:/8`
    pub fn is_unique_local(&self) -> bool {
        // TODO: Delegate to `Ipv6Addr::is_unique_local()` when stabilized.
        //self.addr.network().octets()[0] == 0xfd
        todo!()
    }

    fn mask_impl(&self) -> u128 {
        ((1u128 << self.prefix) - 1) << (IPV6_NET_PREFIX_MAX - self.prefix)
    }

    pub fn mask(&self) -> Ipv6Addr {
        self.mask_impl().into()
    }

    /// Return `true` if the address is within the subnet.
    pub fn contains(&self, other: Ipv6Addr) -> bool {
        let addr: u128 = self.addr.into();
        let other: u128 = other.into();
        let mask = self.mask_impl();

        (addr & mask) == (other & mask)
    }

    // TODO should this be necessary or should we enforce during new?
    // TODO aka first_address
    /// XXX Canonical addr
    pub fn network(&self) -> Ipv6Addr {
        let addr: u128 = self.addr.into();
        Ipv6Addr::from(addr & self.mask_impl())
    }

    pub fn iter(&self) -> Ipv6NetIter {
        let addr: u128 = self.addr.into();
        let last = addr + ((1u128 << (IPV6_NET_PREFIX_MAX - self.prefix)) - 1);
        Ipv6NetIter {
            next: Some(addr),
            last,
        }
    }

    pub fn nth(&self, n: u128) -> Option<Ipv6Addr> {
        let size = 1 << (IPV6_NET_PREFIX_MAX - self.prefix);
        (n < size).then(|| {
            let addr: u128 = self.addr.into();
            let nth = (addr & self.mask_impl()) + n;
            nth.into()
        })
    }

    pub fn broadcast(&self) -> Ipv6Addr {
        let addr: u128 = self.addr.into();
        let last = addr | ((1u128 << (IPV6_NET_PREFIX_MAX - self.prefix)) - 1);
        Ipv6Addr::from(last)
    }

    pub fn is_subnet_of(&self, other: &Self) -> bool {
        other.addr <= self.addr && other.broadcast() >= self.broadcast()
    }
    pub fn is_supernet_of(&self, other: &Self) -> bool {
        other.is_subnet_of(self)
    }
}

impl std::fmt::Display for Ipv6Net {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", &self.addr, self.prefix)
    }
}

impl std::str::FromStr for Ipv6Net {
    type Err = IpNetParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let Some(ii) = s.find('/') else {
            return Err(IpNetParseError::MissingSlash);
        };

        let addr_str = &s[..ii];
        let prefix_str = &s[ii + 1..];

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
        let nth = next + n as u32;
        (nth <= self.last).then_some(nth.into())
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
        let nth = next + n as u128;
        (nth <= self.last).then_some(nth.into())
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
                prefix: value.prefix(),
            }
        }
    }

    impl From<Ipv4Net> for Ipv4Network {
        fn from(value: Ipv4Net) -> Self {
            Self::new(value.addr, value.prefix).unwrap()
        }
    }

    impl From<Ipv6Network> for Ipv6Net {
        fn from(value: Ipv6Network) -> Self {
            Self {
                addr: value.ip(),
                prefix: value.prefix(),
            }
        }
    }

    impl From<Ipv6Net> for Ipv6Network {
        fn from(value: Ipv6Net) -> Self {
            Self::new(value.addr, value.prefix).unwrap()
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
        //TODO: none of this actually exercises
        // schemars::schema::StringValidation bits and the schemars
        // documentation is not forthcoming on how this might be accomplished.
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
}
