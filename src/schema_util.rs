// Copyright 2024 Oxide Computer Company

use schemars::schema::{Metadata, Schema, SchemaObject, SubschemaValidation};
use serde_json::json;

/// Insert another level of schema indirection in order to provide an
/// additional title for a subschema. This allows generators to infer a better
/// variant name for an "untagged" enum.
pub(crate) fn label_schema(label: &str, schema: Schema) -> Schema {
    SchemaObject {
        metadata: Some(
            Metadata {
                title: Some(label.to_string()),
                ..Default::default()
            }
            .into(),
        ),
        subschemas: Some(
            SubschemaValidation {
                all_of: Some(vec![schema]),
                ..Default::default()
            }
            .into(),
        ),
        ..Default::default()
    }
    .into()
}

/// In the custom schemas we emit, we include this simple extension:
/// ```json
/// {
///     "crate": "oxnet",
///     "version": "<version>",
///     "path": "oxnet::<typename>"
/// }
/// ```
/// Rust generators may use the first element of the path to determine the
/// crate (and if that crate is available in generated code). The version
/// indicates the earliest version of the crate in which a
/// serialization-compatible revision of the type appears. For example, if a
/// client is generated with "oxnet@0.2.0", it may use types from version 0.2.0
/// or 0.2.1, but not 0.1.0 or 0.4.0. While 0.1.0 is earlier, 0.2.0 indicates a
/// breaking boundary so compatibility is not guaranteed.
///
/// The version that appears in a given type's extension should only be updated
/// when a semver breaking version of the crate is released, and in that case
/// all types should be updated to the new version.
pub(crate) fn extension(
    type_name: &str,
    version: &str,
) -> schemars::Map<String, serde_json::Value> {
    [(
        "x-rust-type".to_string(),
        json!({
            "crate": "oxnet",
            "version": version,
            "path": format!("oxnet::{}", type_name),
        }),
    )]
    .into_iter()
    .collect()
}

#[cfg(test)]
mod tests {
    use schemars::gen::SchemaGenerator;

    use crate::*;

    /// Add any new types that this crate exports to this test to validate the
    /// JSON Schema encoding.
    #[test]
    fn test_all_schemas() {
        let mut gen = SchemaGenerator::default();

        let _ = gen.subschema_for::<IpNet>();
        let _ = gen.subschema_for::<Ipv4Net>();
        let _ = gen.subschema_for::<Ipv6Net>();

        let root = gen.into_root_schema_for::<()>();

        expectorate::assert_contents(
            "all_schemas.json",
            &serde_json::to_string_pretty(&root).expect("json serialization failed"),
        );
    }
}
