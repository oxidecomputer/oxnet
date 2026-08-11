// Copyright 2026 Oxide Computer Company

use std::{
    net::{SocketAddr, SocketAddrV4, SocketAddrV6},
    ops::{Deref, DerefMut},
};

macro_rules! socket_addr_json_wrapper {
    ($wrapper:ident, $inner:ty) => {
        /// A wrapper around `
        #[doc = stringify!($inner)]
        /// ` that implements schemars::JsonSchema to indicate the appropriate
        /// Rust type to use for generated clients.
        #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        #[cfg_attr(feature = "serde", serde(transparent))]
        pub struct $wrapper(pub $inner);

        impl Deref for $wrapper {
            type Target = $inner;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl DerefMut for $wrapper {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }

        impl From<$inner> for $wrapper {
            fn from(addr: $inner) -> Self {
                Self(addr)
            }
        }

        #[cfg(feature = "schemars")]
        impl schemars::JsonSchema for $wrapper {
            fn schema_name() -> String {
                stringify!($inner).to_string()
            }

            fn json_schema(
                gen: &mut schemars::gen::SchemaGenerator,
            ) -> schemars::schema::Schema {
                let schema = gen.subschema_for::<$inner>();
                let mut schema_object = schema.into_object();
                schema_object.extensions.insert(
                    "x-rust-type".to_string(),
                    serde_json::json!({
                        "crate": "std",
                        "version": ">=1.0.0",
                        "path": concat!("std::net::", stringify!($inner)),
                    }),
                );
                schema_object.into()
            }

            fn is_referenceable() -> bool {
                // We want this to be inlined like the inner type.
                false
            }
        }
    };
}

socket_addr_json_wrapper!(SocketAddrJson, SocketAddr);
socket_addr_json_wrapper!(SocketAddrV4Json, SocketAddrV4);
socket_addr_json_wrapper!(SocketAddrV6Json, SocketAddrV6);

impl From<SocketAddrV4> for SocketAddrJson {
    fn from(addr: SocketAddrV4) -> Self {
        Self(SocketAddr::from(addr))
    }
}

impl From<SocketAddrV6> for SocketAddrJson {
    fn from(addr: SocketAddrV6) -> Self {
        Self(SocketAddr::from(addr))
    }
}

#[cfg(test)]
mod tests {
    #[cfg(all(feature = "schemars", feature = "serde"))]
    #[test]
    fn test_sockaddr_serialization() {
        use super::*;

        let base: SocketAddrV4 = "0.0.0.0:0".parse().unwrap();
        let wrap = SocketAddrV4Json::from(base);

        assert_eq!(
            serde_json::to_string(&wrap).unwrap(),
            serde_json::to_string(&base).unwrap(),
        );

        let base: SocketAddrV6 = "[::]:0".parse().unwrap();
        let wrap = SocketAddrV6Json::from(base);

        assert_eq!(
            serde_json::to_string(&wrap).unwrap(),
            serde_json::to_string(&base).unwrap(),
        );

        let base: SocketAddr = "0.0.0.0:0".parse().unwrap();
        let wrap = SocketAddrJson::from(base);

        assert_eq!(
            serde_json::to_string(&wrap).unwrap(),
            serde_json::to_string(&base).unwrap(),
        );
    }
}
