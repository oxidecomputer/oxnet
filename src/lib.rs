// Copyright 2026 Oxide Computer Company

#![forbid(unsafe_code)]
#![deny(missing_docs)]
#![doc = include_str!("../README.md")]

mod ipaddr;
mod ipnet;
mod multicast;
#[cfg(feature = "schemars")]
mod schema_util;
mod sockaddr;
mod vlan;

pub use ipnet::{
    IpNet, IpNetParseError, IpNetPrefixError, Ipv4Net, Ipv6Net, IPV4_NET_WIDTH_MAX,
    IPV6_NET_WIDTH_MAX,
};

pub use ipaddr::{
    UnicastLinkLocalIpAddr, UnicastLinkLocalIpAddrError, UnicastLinkLocalIpAddrParseError,
    UnicastLinkLocalIpv4Addr, UnicastLinkLocalIpv6Addr,
};

#[cfg(feature = "ula")]
pub use ipnet::{UlaBuildError, UlaBuilder};
pub use multicast::MulticastMac;
pub use sockaddr::{SocketAddrJson, SocketAddrV4Json, SocketAddrV6Json};
pub use vlan::{VlanId, VlanIdError, VlanIdParseError};
