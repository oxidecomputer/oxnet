// Copyright 2026 Oxide Computer Company

use std::hash::{Hash, Hasher};
use zeroize::{ZeroizeOnDrop, Zeroizing};

/// An MD5 authentication key represented as a printable ASCII string.
///
/// The key contains between 1 and 80 bytes, inclusive, and every byte is in
/// the printable ASCII range (`0x20..=0x7e`). This follows the recommendation
/// for TCP MD5 keys in RFC 2385 section 4.5.
///
/// The [`Debug`](std::fmt::Debug) implementation redacts the key, and its
/// allocation is zeroized when the value is dropped. Converting it into a
/// [`String`] transfers responsibility for zeroizing that allocation to the
/// caller. Its serialized representation contains the key as a plain string.
#[derive(Clone, Eq, PartialEq, ZeroizeOnDrop)]
pub struct Md5AuthString(Zeroizing<String>);

impl Md5AuthString {
    /// Maximum key length in bytes.
    pub const MAX_LEN: usize = 80;

    /// Creates an MD5 authentication string after validating its contents.
    pub fn new(source: String) -> Result<Self, Md5AuthStringError> {
        let source = Zeroizing::new(source);

        if source.is_empty() {
            return Err(Md5AuthStringError::Empty);
        }

        if source.len() > Self::MAX_LEN {
            return Err(Md5AuthStringError::TooLong { len: source.len() });
        }

        if !source.chars().all(|c| c.is_ascii_graphic() || c == ' ') {
            return Err(Md5AuthStringError::NotPrintableAscii);
        }

        Ok(Self(source))
    }

    /// Returns the key as a byte slice.
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }

    /// Returns the key as a string slice.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Returns the underlying string, transferring responsibility for
    /// zeroizing it to the caller.
    pub fn into_inner(mut self) -> String {
        std::mem::take(&mut *self.0)
    }
}

impl Hash for Md5AuthString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl TryFrom<String> for Md5AuthString {
    type Error = Md5AuthStringError;

    fn try_from(source: String) -> Result<Self, Self::Error> {
        Self::new(source)
    }
}

impl From<Md5AuthString> for String {
    fn from(source: Md5AuthString) -> Self {
        source.into_inner()
    }
}

impl std::fmt::Debug for Md5AuthString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Md5AuthString(<redacted>)")
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Md5AuthString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let source = <String as serde::Deserialize>::deserialize(deserializer)?;
        Self::new(source).map_err(serde::de::Error::custom)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Md5AuthString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

#[cfg(feature = "schemars")]
impl schemars::JsonSchema for Md5AuthString {
    fn schema_name() -> String {
        "Md5AuthString".to_string()
    }

    fn json_schema(_: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        schemars::schema::SchemaObject {
            metadata: Some(Box::new(schemars::schema::Metadata {
                title: Some("An MD5 authentication string".to_string()),
                description: Some(
                    "A nonempty printable ASCII string of at most 80 bytes".to_string(),
                ),
                ..Default::default()
            })),
            instance_type: Some(schemars::schema::InstanceType::String.into()),
            string: Some(Box::new(schemars::schema::StringValidation {
                max_length: Some(Self::MAX_LEN as u32),
                min_length: Some(1),
                pattern: Some(r"^[ -~]+$".to_string()),
            })),
            extensions: crate::schema_util::extension("Md5AuthString", "0.1.8"),
            ..Default::default()
        }
        .into()
    }
}

impl std::error::Error for Md5AuthStringError {}

/// An error returned when an MD5 authentication string violates its required
/// invariants.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Md5AuthStringError {
    /// The string is empty.
    Empty,
    /// The string exceeds [`Md5AuthString::MAX_LEN`] bytes.
    TooLong {
        /// The actual string length in bytes.
        len: usize,
    },
    /// The string contains a byte outside the printable ASCII range.
    NotPrintableAscii,
}

impl std::fmt::Display for Md5AuthStringError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty => write!(f, "MD5 auth string must not be empty"),
            Self::TooLong { len } => write!(
                f,
                "MD5 auth string length must be <= {}, found {len}",
                Md5AuthString::MAX_LEN
            ),
            Self::NotPrintableAscii => write!(
                f,
                "MD5 auth string must be fully comprised of printable ASCII characters"
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accepts_printable_ascii_within_length_limit() {
        for byte in b' '..=b'~' {
            let source = char::from(byte).to_string();
            assert_eq!(Md5AuthString::new(source.clone()).unwrap().as_str(), source);
        }

        let source = "x".repeat(Md5AuthString::MAX_LEN);
        let key = Md5AuthString::new(source.clone()).unwrap();
        assert_eq!(key.as_str(), source);
        assert_eq!(key.as_bytes(), source.as_bytes());
        assert_eq!(String::from(key), source);
    }

    #[test]
    fn rejects_strings_outside_invariants() {
        assert_eq!(
            Md5AuthString::new(String::new()),
            Err(Md5AuthStringError::Empty)
        );

        let len = Md5AuthString::MAX_LEN + 1;
        assert_eq!(
            Md5AuthString::new("x".repeat(len)),
            Err(Md5AuthStringError::TooLong { len })
        );

        for source in ["line\nfeed", "tab\tkey", "nul\0key", "non-ASCII-é"] {
            assert_eq!(
                Md5AuthString::new(source.to_string()),
                Err(Md5AuthStringError::NotPrintableAscii)
            );
        }
    }

    #[test]
    fn debug_redacts_inner_string() {
        let key = Md5AuthString::new("super secret".to_string()).unwrap();
        assert_eq!(format!("{key:?}"), "Md5AuthString(<redacted>)");
    }

    #[cfg(all(feature = "serde", feature = "schemars"))]
    #[test]
    fn serde_round_trip_preserves_invariants() {
        let key = Md5AuthString::new("secret key".to_string()).unwrap();
        let json = serde_json::to_string(&key).unwrap();
        assert_eq!(json, r#""secret key""#);
        assert_eq!(serde_json::from_str::<Md5AuthString>(&json).unwrap(), key);

        assert!(serde_json::from_str::<Md5AuthString>(r#"""#).is_err());
        assert!(serde_json::from_str::<Md5AuthString>(r#""line\nfeed""#).is_err());
    }

    #[cfg(feature = "schemars")]
    #[test]
    fn json_schema_matches_invariants() {
        let schema = schemars::schema_for!(Md5AuthString);
        let validation = schema.schema.string.expect("string validation");

        assert_eq!(validation.min_length, Some(1));
        assert_eq!(validation.max_length, Some(Md5AuthString::MAX_LEN as u32));
        assert_eq!(validation.pattern.as_deref(), Some(r"^[ -~]+$"));
        assert_eq!(
            schema.schema.extensions.get("x-rust-type"),
            Some(&serde_json::json!({
                "crate": "oxnet",
                "version": "0.1.8",
                "path": "oxnet::Md5AuthString",
            }))
        );
    }
}
