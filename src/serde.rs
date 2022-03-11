use crate::HashBag;
use core::fmt;
use core::hash::{BuildHasher, Hash};
use core::marker::PhantomData;
use serde::de::{Error, MapAccess, SeqAccess, Visitor};
use serde::ser::{SerializeSeq, SerializeStruct, Serializer};
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

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a HashBag")
    }

    fn visit_seq<M>(self, mut access: M) -> Result<Self::Value, M::Error>
    where
        M: SeqAccess<'de>,
    {
        let mut bag: HashBag<T, S> =
            HashBag::with_capacity_and_hasher(access.size_hint().unwrap_or(0), Default::default());

        while let Some(entry) = access.next_element::<EntryWrapper<T>>()? {
            bag.insert_many(entry.entry, entry.count);
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
        let mut bag = serializer.serialize_seq(Some(self.len()))?;

        for (entry, count) in self.set_iter() {
            bag.serialize_element(&EntryWrapper { entry, count })?;
        }

        bag.end()
    }
}

pub(crate) struct EntryWrapper<T> {
    pub(crate) entry: T,
    pub(crate) count: usize,
}

#[derive(Deserialize)]
#[serde(field_identifier, rename_all = "lowercase")]
enum Field {
    Entry,
    Count,
}

//struct WrapperVisitor;
struct WrapperVisitor<T> {
    marker: PhantomData<T>,
}

impl<'de, T> Visitor<'de> for WrapperVisitor<T>
where
    T: Deserialize<'de>,
{
    type Value = EntryWrapper<T>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("struct HashBag Entry Wrapper")
    }

    fn visit_seq<V>(self, mut seq: V) -> Result<EntryWrapper<T>, V::Error>
    where
        V: SeqAccess<'de>,
    {
        let entry = seq
            .next_element()?
            .ok_or_else(|| Error::invalid_length(0, &self))?;
        let count = seq
            .next_element()?
            .ok_or_else(|| Error::invalid_length(1, &self))?;
        Ok(EntryWrapper { entry, count })
    }

    fn visit_map<V>(self, mut map: V) -> Result<EntryWrapper<T>, V::Error>
    where
        V: MapAccess<'de>,
    {
        let mut entry = None;
        let mut count = None;
        while let Some(key) = map.next_key()? {
            match key {
                Field::Entry => {
                    if entry.is_some() {
                        return Err(Error::duplicate_field("secs"));
                    }
                    entry = Some(map.next_value()?);
                }
                Field::Count => {
                    if count.is_some() {
                        return Err(Error::duplicate_field("nanos"));
                    }
                    count = Some(map.next_value()?);
                }
            }
        }
        let entry = entry.ok_or_else(|| Error::missing_field("entry"))?;
        let count = count.ok_or_else(|| Error::missing_field("count"))?;
        Ok(EntryWrapper { entry, count })
    }
}

impl<'de, T> Deserialize<'de> for EntryWrapper<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        const FIELDS: &'static [&'static str] = &["entry", "count"];
        deserializer.deserialize_struct(
            "EntryWrapper",
            FIELDS,
            WrapperVisitor {
                marker: PhantomData,
            },
        )
    }
}

impl<T> Serialize for EntryWrapper<T>
where
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Color", 3)?;
        state.serialize_field("entry", &self.entry)?;
        state.serialize_field("count", &self.count)?;
        state.end()
    }
}
