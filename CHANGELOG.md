# Changelog

## Next

* Bumps Rust min-version to 1.85

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
