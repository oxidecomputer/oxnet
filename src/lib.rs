// Copyright 2024 Oxide Computer Company

#![forbid(unsafe_code)]
#![deny(missing_docs)]
#![doc = include_str!("../README.md")]

mod ipnet;
mod multicast;
#[cfg(feature = "schemars")]
mod schema_util;

pub use ipnet::{
    IpNet, IpNetParseError, IpNetPrefixError, Ipv4Net, Ipv6Net, IPV4_NET_WIDTH_MAX,
    IPV6_NET_WIDTH_MAX,
};
pub use multicast::MulticastMac;
