# Changelog

## [0.1.1] - 2025-02-25

* Adds `is_subnet_of`/`is_supernet_of`/`overlaps`, for verifying disjoint
      ranges. Previously this was only on IPv6.
* Adds `is_network_address`, for checking that none of the host bits are set
      (and ensuring we have a canonical form).
* Adds `is_multicast` and `is_loopback` (forwards to std/core).

## [0.1.0] - 2025-02-10

Initial release.

[0.1.1]: https://github.com/oxidecomputer/oxnet/releases/oxnet-0.1.1
[0.1.0]: https://github.com/oxidecomputer/oxnet/releases/oxnet-0.1.0
