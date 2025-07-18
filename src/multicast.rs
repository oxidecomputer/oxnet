// Copyright 2025 Oxide Computer Company

//! Multicast MAC address derivation from IP addresses.

use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};

use crate::IpNet;

/// Trait for deriving multicast MAC addresses from IP addresses.
pub trait MulticastMac {
    /// Derive the multicast MAC address from this IP address as a byte array.
    ///
    /// For IPv4 addresses, follows [RFC 1112 Section 6.4][rfc1112]: places the low-order
    /// 23 bits of the IP address into the low-order 23 bits of the Ethernet
    /// address 01-00-5E-00-00-00.
    ///
    /// For IPv6 addresses, follows [RFC 2464 Section 7][rfc2464]: places the low-order
    /// 32 bits of the IP address into the low-order 32 bits of the Ethernet
    /// address 33-33-00-00-00-00.
    ///
    /// [rfc1112]: https://datatracker.ietf.org/doc/html/rfc1112#section-6.4
    /// [rfc2464]: https://datatracker.ietf.org/doc/html/rfc2464#section-7
    fn derive_multicast_mac(&self) -> [u8; 6];

    /// Derive the multicast MAC address from this IP address as a `macaddr::MacAddr6`.
    ///
    /// This is a convenience method that converts the byte array result to the
    /// `macaddr` crate's `MacAddr6` type.
    #[cfg(feature = "macaddr")]
    fn derive_multicast_mac_addr(&self) -> macaddr::MacAddr6 {
        macaddr::MacAddr6::from(self.derive_multicast_mac())
    }
}

impl MulticastMac for IpAddr {
    fn derive_multicast_mac(&self) -> [u8; 6] {
        match self {
            IpAddr::V4(ipv4) => ipv4.derive_multicast_mac(),
            IpAddr::V6(ipv6) => ipv6.derive_multicast_mac(),
        }
    }
}

impl MulticastMac for Ipv4Addr {
    fn derive_multicast_mac(&self) -> [u8; 6] {
        let octets = self.octets();
        // Take the last 23 bits of the IPv4 address (mask the high bit of the second octet)
        [
            0x01,
            0x00,
            0x5e,
            octets[1] & 0x7f, // Clear the high bit to get only 23 bits total
            octets[2],
            octets[3],
        ]
    }
}

impl MulticastMac for Ipv6Addr {
    fn derive_multicast_mac(&self) -> [u8; 6] {
        let octets = self.octets();
        // Take the last 4 bytes (32 bits) of the IPv6 address
        [0x33, 0x33, octets[12], octets[13], octets[14], octets[15]]
    }
}

impl MulticastMac for IpNet {
    fn derive_multicast_mac(&self) -> [u8; 6] {
        match self {
            IpNet::V4(ipv4_net) => ipv4_net.addr().derive_multicast_mac(),
            IpNet::V6(ipv6_net) => ipv6_net.addr().derive_multicast_mac(),
        }
    }
}

#[cfg(feature = "ipnetwork")]
impl MulticastMac for ipnetwork::IpNetwork {
    fn derive_multicast_mac(&self) -> [u8; 6] {
        self.ip().derive_multicast_mac()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_derive_multicast_mac_ipv4() {
        let ipv4_addr = Ipv4Addr::new(224, 1, 1, 1);
        let mac = ipv4_addr.derive_multicast_mac();
        let expected = [0x01, 0x00, 0x5e, 0x01, 0x01, 0x01];
        assert_eq!(mac, expected);

        // Test edge case with high bit set in second octet
        let ipv4_addr = Ipv4Addr::new(224, 129, 1, 1); // 0x81 in second octet
        let mac = ipv4_addr.derive_multicast_mac();
        let expected = [0x01, 0x00, 0x5e, 0x01, 0x01, 0x01]; // High bit masked off
        assert_eq!(mac, expected);
    }

    #[test]
    fn test_derive_multicast_mac_ipv6() {
        let ipv6_addr = Ipv6Addr::new(0xff02, 0, 0, 0, 0, 0, 0, 0x0001);
        let mac = ipv6_addr.derive_multicast_mac();
        let expected = [0x33, 0x33, 0x00, 0x00, 0x00, 0x01];
        assert_eq!(mac, expected);
    }

    #[test]
    fn test_derive_multicast_mac_generic() {
        // Test IPv4
        let ipv4_addr = IpAddr::V4(Ipv4Addr::new(224, 1, 1, 1));
        let mac = ipv4_addr.derive_multicast_mac();
        let expected = [0x01, 0x00, 0x5e, 0x01, 0x01, 0x01];
        assert_eq!(mac, expected);

        // Test IPv6
        let ipv6_addr = IpAddr::V6(Ipv6Addr::new(0xff02, 0, 0, 0, 0, 0, 0, 0x0001));
        let mac = ipv6_addr.derive_multicast_mac();
        let expected = [0x33, 0x33, 0x00, 0x00, 0x00, 0x01];
        assert_eq!(mac, expected);
    }

    #[cfg(feature = "ipnetwork")]
    #[test]
    fn test_derive_multicast_mac_ipnetwork() {
        use ipnetwork::IpNetwork;
        use std::str::FromStr;

        // Test IPv4 network
        let ipv4_net = IpNetwork::from_str("224.1.1.1/32").unwrap();
        let mac = ipv4_net.derive_multicast_mac();
        let expected = [0x01, 0x00, 0x5e, 0x01, 0x01, 0x01];
        assert_eq!(mac, expected);

        // Test IPv6 network
        let ipv6_net = IpNetwork::from_str("ff02::1/128").unwrap();
        let mac = ipv6_net.derive_multicast_mac();
        let expected = [0x33, 0x33, 0x00, 0x00, 0x00, 0x01];
        assert_eq!(mac, expected);
    }

    #[test]
    fn test_derive_multicast_mac_ipnet() {
        use std::str::FromStr;

        // Test IPv4 network
        let ipv4_net = IpNet::from_str("224.1.1.1/32").unwrap();
        let mac = ipv4_net.derive_multicast_mac();
        let expected = [0x01, 0x00, 0x5e, 0x01, 0x01, 0x01];
        assert_eq!(mac, expected);

        // Test IPv6 network
        let ipv6_net = IpNet::from_str("ff02::1/128").unwrap();
        let mac = ipv6_net.derive_multicast_mac();
        let expected = [0x33, 0x33, 0x00, 0x00, 0x00, 0x01];
        assert_eq!(mac, expected);
    }

    #[cfg(feature = "macaddr")]
    #[test]
    fn test_derive_multicast_mac_addr() {
        let ipv4_addr = Ipv4Addr::new(224, 1, 1, 1);
        let mac_addr = ipv4_addr.derive_multicast_mac_addr();
        let expected = macaddr::MacAddr6::new(0x01, 0x00, 0x5e, 0x01, 0x01, 0x01);
        assert_eq!(mac_addr, expected);

        let ipv6_addr = Ipv6Addr::new(0xff02, 0, 0, 0, 0, 0, 0, 0x0001);
        let mac_addr = ipv6_addr.derive_multicast_mac_addr();
        let expected = macaddr::MacAddr6::new(0x33, 0x33, 0x00, 0x00, 0x00, 0x01);
        assert_eq!(mac_addr, expected);

        // Test with IpAddr enum
        let ip = IpAddr::V4(Ipv4Addr::new(224, 1, 1, 1));
        let mac_addr = ip.derive_multicast_mac_addr();
        let expected = macaddr::MacAddr6::new(0x01, 0x00, 0x5e, 0x01, 0x01, 0x01);
        assert_eq!(mac_addr, expected);
    }
}
