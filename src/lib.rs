// Copyright 2024 Oxide Computer Company

#![forbid(unsafe_code)]
#![doc = include_str!("../README.md")]

mod ipnet;
#[cfg(feature = "schemars")]
mod schema_util;

pub use ipnet::{IpNet, Ipv4Net, Ipv6Net};
