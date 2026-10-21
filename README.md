# oxnet

[![oxnet on crates.io](https://img.shields.io/crates/v/oxnet)](https://crates.io/crates/oxnet)
[![Documentation (latest release)](https://img.shields.io/badge/docs-latest%20version-brightgreen.svg)](https://docs.rs/oxnet)
[![License](https://img.shields.io/badge/license-Apache-green.svg)](LICENSE-APACHE)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE-MIT)

An accumulation of primitive networking-related types.

## Adding new types

When adding a public type with a `schemars::JsonSchema` implementation, add it
to `schema_util::tests::test_all_schemas`, then regenerate the checked-in
`all_schemas.json` file. Do not edit the generated file manually:

```console
$ EXPECTORATE=overwrite cargo test --locked --all-features \
    schema_util::tests::test_all_schemas
```

Finally, run the complete test suite:

```console
$ cargo test --locked --all-features
```
