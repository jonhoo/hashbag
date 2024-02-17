use crate::HashBag;
use core::fmt;
use core::hash::{BuildHasher, Hash};
use core::marker::PhantomData;
use serde::de::{SeqAccess, Visitor};
use serde::ser::{SerializeSeq, Serializer};
use serde::Deserializer;
use serde::{Deserialize, Serialize};

pub(crate) struct HashBagVisitor<T, S> {
    marker: PhantomData<fn() -> HashBag<T, S>>,
}

impl<T, S> HashBagVisitor<T, S>
where
    T: Eq + Hash,
    S: BuildHasher + Clone,
{
    fn new() -> Self {
        HashBagVisitor {
            marker: PhantomData,
        }
    }
}

impl<'de, T, S> Visitor<'de> for HashBagVisitor<T, S>
where
    T: Deserialize<'de> + Eq + Hash,
    S: BuildHasher + Clone + Default,
{
    type Value = HashBag<T, S>;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("a HashBag")
    }

    fn visit_seq<M>(self, mut access: M) -> Result<Self::Value, M::Error>
    where
        M: SeqAccess<'de>,
    {
        let mut bag: HashBag<T, S> =
            HashBag::with_capacity_and_hasher(access.size_hint().unwrap_or(0), Default::default());

        while let Some(entry) = access.next_element::<(T, usize)>()? {
            bag.insert_many(entry.0, entry.1);
        }

        Ok(bag)
    }
}

impl<'de, T, S> Deserialize<'de> for HashBag<T, S>
where
    T: Deserialize<'de> + Eq + Hash,
    S: BuildHasher + Clone + Default,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_seq(HashBagVisitor::<T, S>::new())
    }
}

impl<T, H> Serialize for HashBag<T, H>
where
    T: Serialize + Eq + Hash,
    H: BuildHasher + Clone,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut bag = serializer.serialize_seq(Some(self.set_len()))?;

        for (entry, count) in self.set_iter() {
            bag.serialize_element(&(entry, count))?;
        }

        bag.end()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use serde_json;

    #[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
    struct VeryHelpfulStruct {
        pub(crate) name: String,
    }

    #[test]
    fn format_simple_data() {
        let vikings: HashBag<String> = ["Einar", "Olaf", "Olaf", "Harald", "Harald", "Harald"]
            .iter()
            .map(|s| s.to_string())
            .collect();
        let einar = "Einar".to_string();
        let olaf = "Olaf".to_string();
        let harald = "Harald".to_string();
        assert_eq!(vikings.get(&einar), Some((&einar, 1)));
        assert_eq!(vikings.get(&olaf), Some((&olaf, 2)));
        assert_eq!(vikings.get(&harald), Some((&harald, 3)));
        println!("Constructed: {:?}", vikings);
        let jsonified_vikings: String =
            serde_json::to_string(&vikings).expect("Unable to convert data to json!");
        println!("JSON: {}", jsonified_vikings);
        let reconstituted_vikings: HashBag<String> =
            serde_json::from_str(&jsonified_vikings).expect("Unable to convert json to hashbag!");
        println!("From json: {:?}", reconstituted_vikings);
        assert_eq!(vikings, reconstituted_vikings);
    }

    #[test]
    fn format_struct_data() {
        let vikings: HashBag<VeryHelpfulStruct> =
            ["Einar", "Olaf", "Olaf", "Harald", "Harald", "Harald"]
                .iter()
                .map(|n| VeryHelpfulStruct {
                    name: n.to_string(),
                })
                .collect();
        let einar = VeryHelpfulStruct {
            name: "Einar".to_string(),
        };
        let olaf = VeryHelpfulStruct {
            name: "Olaf".to_string(),
        };
        let harald = VeryHelpfulStruct {
            name: "Harald".to_string(),
        };
        assert_eq!(vikings.get(&einar), Some((&einar, 1)));
        assert_eq!(vikings.get(&olaf), Some((&olaf, 2)));
        assert_eq!(vikings.get(&harald), Some((&harald, 3)));
        println!("Constructed: {:?}", vikings);
        let jsonified_vikings: String =
            serde_json::to_string(&vikings).expect("Unable to convert data to json!");
        println!("JSON: {}", jsonified_vikings);
        let reconstituted_vikings: HashBag<VeryHelpfulStruct> =
            serde_json::from_str(&jsonified_vikings).expect("Unable to convert json to hashbag!");
        println!("From json: {:?}", reconstituted_vikings);
        assert_eq!(vikings, reconstituted_vikings);
    }

    #[test]
    fn repeat_simple_entries() {
        let jsonified_vikings: String =
            "[[\"Einar\",1],[\"Olaf\",1],[\"Olaf\",1],[\"Harald\",2],[\"Harald\",1]]".to_string();
        let reconstituted_vikings: HashBag<String> =
            serde_json::from_str(&jsonified_vikings).expect("Unable to convert json to hashbag!");
        let einar = "Einar".to_string();
        let olaf = "Olaf".to_string();
        let harald = "Harald".to_string();
        assert_eq!(reconstituted_vikings.get(&einar), Some((&einar, 1)));
        assert_eq!(reconstituted_vikings.get(&olaf), Some((&olaf, 2)));
        assert_eq!(reconstituted_vikings.get(&harald), Some((&harald, 3)));
    }

    #[test]
    fn repeat_struct_entries() {
        let jsonified_vikings: String =
            "[[{\"name\":\"Einar\"},1],[{\"name\":\"Olaf\"},1],[{\"name\":\"Olaf\"},1],[{\"name\":\"Harald\"},2],[{\"name\":\"Harald\"},1]]".to_string();
        let reconstituted_vikings: HashBag<VeryHelpfulStruct> =
            serde_json::from_str(&jsonified_vikings).expect("Unable to convert json to hashbag!");
        let einar = VeryHelpfulStruct {
            name: "Einar".to_string(),
        };
        let olaf = VeryHelpfulStruct {
            name: "Olaf".to_string(),
        };
        let harald = VeryHelpfulStruct {
            name: "Harald".to_string(),
        };
        assert_eq!(reconstituted_vikings.get(&einar), Some((&einar, 1)));
        assert_eq!(reconstituted_vikings.get(&olaf), Some((&olaf, 2)));
        assert_eq!(reconstituted_vikings.get(&harald), Some((&harald, 3)));
    }

    #[test]
    fn serde_bincode() {
        let vikings: HashBag<String> = ["Einar", "Olaf", "Olaf", "Harald", "Harald", "Harald"]
            .iter()
            .map(|s| s.to_string())
            .collect();
        let einar = "Einar".to_string();
        let olaf = "Olaf".to_string();
        let harald = "Harald".to_string();
        assert_eq!(vikings.get(&einar), Some((&einar, 1)));
        assert_eq!(vikings.get(&olaf), Some((&olaf, 2)));
        assert_eq!(vikings.get(&harald), Some((&harald, 3)));
        println!("Constructed: {:?}", vikings);
        let bincoded_vikings =
            bincode::serialize(&vikings).expect("Unable to serialize to bincode!");
        let reconstituted_vikings: HashBag<String> =
            bincode::deserialize(&bincoded_vikings).expect("Unable to deserialize bincode!");
        println!("From bincode: {:?}", reconstituted_vikings);
        assert_eq!(vikings, reconstituted_vikings);
    }
}
