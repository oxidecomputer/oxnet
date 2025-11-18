# Changelog

## Next

## [0.1.4] - 2025-11-18

* Bumps Rust min-version to 1.85
* Adds `is_administratively_scoped_multicast()` for IPv4 (239.0.0.0/8) and IPv6 (scopes 4, 5, 8)
* Adds `is_admin_local_multicast()` for IPv6 scope 4
* Adds `is_local_multicast()` for IPv4 Local scope (239.255.0.0/16)
* Adds `is_site_local_multicast()` for IPv6 scope 5
* Adds `is_org_local_multicast()` for IPv4 (239.192.0.0/14) and IPv6 scope 8

## [0.1.3] - 2025-07-18

* Adds `MulticastMac` trait for deriving multicast MAC addresses from IP addresses
* Implements RFC 1112 (IPv4) and RFC 2464 (IPv6) multicast MAC derivation
* Optional `macaddr` feature for `MacAddr6` integration

## [0.1.2] - 2025-05-25

* Bumps Rust min-version to 1.84 for direct `is_unique_local` call on IPv6
      addresses.
* Adds `is_admin_scoped_multicast` check for multicast IPv6 addresses that are
      site-local or org scoped

## [0.1.1] - 2025-02-25

* Adds `is_subnet_of`/`is_supernet_of`/`overlaps`, for verifying disjoint
      ranges. Previously this was only on IPv6.
* Adds `is_network_address`, for checking that none of the host bits are set
      (and ensuring we have a canonical form).
* Adds `is_multicast` and `is_loopback` (forwards to std/core).

## [0.1.0] - 2025-02-10

Initial release.

[0.1.3]: https://github.com/oxidecomputer/oxnet/releases/oxnet-0.1.3
[0.1.2]: https://github.com/oxidecomputer/oxnet/releases/oxnet-0.1.2
[0.1.1]: https://github.com/oxidecomputer/oxnet/releases/oxnet-0.1.1
[0.1.0]: https://github.com/oxidecomputer/oxnet/releases/oxnet-0.1.0
