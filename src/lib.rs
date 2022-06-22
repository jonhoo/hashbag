//! An unordered multiset/bag implementation backed by `HashMap`.
//!
//! A bag, unlike a set, allows duplicate values, and keeps track of how many
//! duplicates each value holds. This type of collection is often referred to
//! as an unordered multiset (see also C++'s [`std::unordered_multiset`]).
//!
//! This multiset/bag is implemented using a `HashMap<T, usize>` and so requires
//! that the stored type implements `Hash + Eq`.
//!
//! For usage examples, see the primary type [`HashBag`].
//!
//! If you want to use a hash table with [amortized resizes](https://github.com/jonhoo/griddle/),
//! set the `amortize` feature.
//!
//! (De)serialization via serde is also available with the `serde` feature.
//! Deserialization note: if the incoming data contains two instances of `T` that are the same, the resulting `HashBag` will merge
//! the counts of those instances.
//!
//!   [`std::unordered_multiset`]: http://www.cplusplus.com/reference/unordered_set/unordered_multiset/
#![deny(missing_docs, missing_debug_implementations, unreachable_pub)]
#![cfg_attr(doc, deny(rustdoc::broken_intra_doc_links))]
#![warn(rust_2018_idioms)]

#[cfg(feature = "amortize")]
use griddle::HashMap;
use std::borrow::Borrow;
use std::collections::hash_map::RandomState;
#[cfg(not(feature = "amortize"))]
use std::collections::HashMap;
use std::hash::{BuildHasher, Hash};

#[cfg(feature = "serde")]
mod serde;

/// A hash bag implemented as a `HashMap` where the value is `usize`.
///
/// A bag, unlike a set, allows duplicate values, and keeps track of how many
/// duplicates each value holds. This type of collection is often referred to
/// as an unordered multiset.
///
/// As with the [`HashMap`] type, a `HashBag` requires that the elements
/// implement the [`Eq`] and [`Hash`] traits. This can frequently be achieved by
/// using `#[derive(PartialEq, Eq, Hash)]`. If you implement these yourself,
/// it is important that the following property holds:
///
/// ```text
/// k1 == k2 -> hash(k1) == hash(k2)
/// ```
///
/// In other words, if two keys are equal, their hashes must be equal.
///
/// It is a logic error for an item to be modified in such a way that the
/// item's hash, as determined by the [`Hash`] trait, or its equality, as
/// determined by the [`Eq`] trait, changes while it is in the bag.
///
/// # Examples
///
/// ```
/// use hashbag::HashBag;
/// // Type inference lets us omit an explicit type signature (which
/// // would be `HashBag<String>` in this example).
/// let mut books = HashBag::new();
///
/// // Add some books.
/// // Since we are a library, we have many copies.
/// books.insert("A Dance With Dragons".to_string());
/// books.insert("To Kill a Mockingbird".to_string());
/// books.insert("To Kill a Mockingbird".to_string());
/// books.insert("The Odyssey".to_string());
/// books.insert("The Odyssey".to_string());
/// books.insert("The Odyssey".to_string());
/// books.insert("The Great Gatsby".to_string());
/// books.insert("The Great Gatsby".to_string());
/// books.insert("The Great Gatsby".to_string());
/// books.insert("The Great Gatsby".to_string());
///
/// // When we count the number of books, duplicates are included.
/// assert_eq!(books.len(), 10);
///
/// // Check for a specific one.
/// if books.contains("The Winds of Winter") == 0 {
///     println!("We have {} books, but The Winds of Winter ain't one.",
///              books.len());
/// }
///
/// // Remove a book.
/// let had_copies = books.remove("The Odyssey");
/// // Remove returns how many copies of that book we had.
/// assert_eq!(had_copies, 3);
///
/// // Iterate over everything.
/// // Duplicates will be listed multiple times.
/// for book in &books {
///     println!("{}", book);
/// }
///
/// // Iterate over each distinct book.
/// for (book, copies) in books.set_iter() {
///     println!("{} ({} copies)", book, copies);
/// }
///
/// // Extract the books and their counts.
/// for (book, copies) in books {
///     println!("{} ({} copies)", book, copies);
/// }
/// ```
///
/// The easiest way to use `HashBag` with a custom type is to derive
/// [`Eq`] and [`Hash`]. We must also derive [`PartialEq`], this will in the
/// future be implied by [`Eq`].
///
/// ```
/// use hashbag::HashBag;
/// #[derive(Hash, Eq, PartialEq, Debug, Clone)]
/// struct Viking {
///     name: String,
///     power: usize,
/// }
///
/// let mut vikings = HashBag::new();
///
/// vikings.insert(Viking { name: "Einar".to_string(), power: 9 });
/// vikings.insert(Viking { name: "Einar".to_string(), power: 9 });
/// vikings.insert(Viking { name: "Olaf".to_string(), power: 4 });
/// vikings.insert(Viking { name: "Olaf".to_string(), power: 5 });
/// vikings.insert(Viking { name: "Harald".to_string(), power: 8 });
///
/// // Use derived implementation to print the vikings.
/// // Notice that all duplicates are printed.
/// for v in &vikings {
///     println!("{:?}", v);
/// }
///
/// // Since the derived implementation compares all the fields,
/// // vikings that share a name but not a power are not duplicates.
/// for (v, n) in vikings.set_iter() {
///     println!("{:?} ({} of them!)", v, n);
/// }
///
/// // HashBags themselves can also be compared for equality,
/// // and will do so by considering both the values and their counts.
/// let mut vikings2 = vikings.clone();
/// assert_eq!(vikings, vikings2);
/// let fallen = vikings.iter().next().unwrap();
/// vikings2.remove(fallen);
/// assert_ne!(vikings, vikings2);
/// vikings2.insert(Viking { name: "Snorre".to_string(), power: 1 });
/// assert_ne!(vikings, vikings2);
/// ```
///
/// A `HashBag` with fixed list of elements can be initialized from an array:
///
/// ```
/// use hashbag::HashBag;
///
/// let mut viking_names: HashBag<&'static str> =
///     [ "Einar", "Olaf", "Harald" ].iter().cloned().collect();
/// // use the values stored in the bag
/// ```
///
/// You can also extend the bag easily:
///
/// ```
/// use hashbag::HashBag;
///
/// let mut vikings: HashBag<String> = HashBag::new();
/// vikings.extend(std::iter::once("Snorre".to_string()));
/// assert_eq!(vikings.contains("Snorre"), 1);
///
/// // You can extend with many instances at once:
/// vikings.extend(std::iter::once(("Snorre".to_string(), 4)));
/// assert_eq!(vikings.contains("Snorre"), 5);
///
/// // Extension also works with reference iterators if the type is Clone:
/// let einar = String::from("Einar");
/// vikings.extend(std::iter::once(&einar));
/// assert_eq!(vikings.contains(&einar), 1);
///
/// // And extend with many instances at once:
/// vikings.extend(std::iter::once((&einar, 4)));
/// assert_eq!(vikings.contains(&einar), 5);
/// ```
pub struct HashBag<T, S = RandomState> {
    items: HashMap<T, usize, S>,
    count: usize,
}

impl<T: Clone + Hash, S: Clone + BuildHasher> Clone for HashBag<T, S> {
    fn clone(&self) -> Self {
        Self {
            items: self.items.clone(),
            count: self.count,
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.items.clone_from(&source.items);
        self.count = source.count;
    }
}

impl<T: Hash + Eq> HashBag<T, RandomState> {
    /// Creates an empty `HashBag`.
    ///
    /// The hash bag is initially created with a capacity of 0, so it will not allocate until it
    /// is first inserted into.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    /// let bag: HashBag<i32> = HashBag::new();
    /// ```
    #[inline]
    pub fn new() -> HashBag<T, RandomState> {
        Self::with_hasher(RandomState::new())
    }

    /// Creates an empty `HashBag` with the specified capacity.
    ///
    /// The hash bag will be able to hold at least `capacity` distinct values without
    /// reallocating. If `capacity` is 0, the hash bag will not allocate.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    /// let bag: HashBag<i32> = HashBag::with_capacity(10);
    /// assert!(bag.capacity() >= 10);
    /// ```
    #[inline]
    pub fn with_capacity(capacity: usize) -> HashBag<T, RandomState> {
        Self::with_capacity_and_hasher(capacity, RandomState::new())
    }
}

impl<T, S> HashBag<T, S> {
    /// Returns the number of distinct values the bag can hold without reallocating.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    /// let bag: HashBag<i32> = HashBag::with_capacity(100);
    /// assert!(bag.capacity() >= 100);
    /// ```
    #[inline]
    pub fn capacity(&self) -> usize {
        self.items.capacity()
    }

    /// An iterator visiting all elements in arbitrary order.
    ///
    /// The iterator element type is `&'a T`.
    /// Duplicates are yielded as many times as they appear in the bag.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    /// let mut bag = HashBag::new();
    /// bag.insert("a");
    /// bag.insert("b");
    /// bag.insert("b");
    ///
    /// // Will print in an arbitrary order.
    /// // b will be printed twice.
    /// for x in bag.iter() {
    ///     println!("{}", x);
    /// }
    /// ```
    #[inline]
    pub fn iter(&self) -> Iter<'_, T> {
        Iter::new(self.items.iter(), self.count)
    }

    /// An iterator visiting all distinct elements in arbitrary order.
    ///
    /// The iterator element type is `(&'a T, usize)`.
    /// Duplicated values are yielded once along with a count of the number of occurrences.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    /// let mut bag = HashBag::new();
    /// bag.insert("a");
    /// bag.insert("b");
    /// bag.insert("b");
    ///
    /// // Will print in an arbitrary order.
    /// for (x, n) in bag.set_iter() {
    ///     println!("{} {}", x, n);
    /// }
    /// ```
    #[inline]
    pub fn set_iter(&self) -> SetIter<'_, T> {
        SetIter(self.items.iter())
    }

    /// Returns the number of elements in the bag.
    ///
    /// Duplicates are counted.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let mut bag = HashBag::new();
    /// assert_eq!(bag.len(), 0);
    /// bag.insert(1);
    /// assert_eq!(bag.len(), 1);
    /// bag.insert(1);
    /// assert_eq!(bag.len(), 2);
    /// ```
    #[inline]
    pub fn len(&self) -> usize {
        self.count
    }

    /// Returns the number of elements in the bag.
    ///
    /// Duplicates are not counted.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let mut bag = HashBag::new();
    /// assert_eq!(bag.set_len(), 0);
    /// bag.insert(1);
    /// assert_eq!(bag.set_len(), 1);
    /// bag.insert(1);
    /// assert_eq!(bag.set_len(), 1);
    /// ```
    #[inline]
    pub fn set_len(&self) -> usize {
        self.items.len()
    }

    /// Returns `true` if the bag contains no elements.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let mut bag = HashBag::new();
    /// assert!(bag.is_empty());
    /// bag.insert(1);
    /// assert!(!bag.is_empty());
    /// ```
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Clears the bag, returning all elements in an iterator.
    ///
    /// Duplicates appear only in the count yielded for each element.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let mut bag: HashBag<_> = [1, 2, 3, 3].iter().cloned().collect();
    /// assert!(!bag.is_empty());
    ///
    /// // prints
    /// //   1 1
    /// //   2 1
    /// //   3 2
    /// // in an arbitrary order
    /// for (i, n) in bag.drain() {
    ///     println!("{} {}", i, n);
    /// }
    ///
    /// assert!(bag.is_empty());
    /// ```
    #[inline]
    pub fn drain(&mut self) -> Drain<'_, T> {
        self.count = 0;
        Drain(self.items.drain())
    }

    /// Clears the bag, removing all values.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let mut bag = HashBag::new();
    /// bag.insert(1);
    /// bag.clear();
    /// assert!(bag.is_empty());
    /// ```
    #[inline]
    pub fn clear(&mut self) {
        self.count = 0;
        self.items.clear();
    }
}

impl<T, S> HashBag<T, S>
where
    T: Eq + Hash,
    S: BuildHasher,
{
    /// Creates a new empty hash bag which will use the given hasher to hash
    /// keys.
    ///
    /// The hash bag is also created with the default initial capacity.
    ///
    /// Warning: `hasher` is normally randomly generated, and
    /// is designed to allow `HashBag`s to be resistant to attacks that
    /// cause many collisions and very poor performance. Setting it
    /// manually using this function can expose a DoS attack vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    /// use std::collections::hash_map::RandomState;
    ///
    /// let s = RandomState::new();
    /// let mut bag = HashBag::with_hasher(s);
    /// bag.insert(2);
    /// ```
    #[inline]
    pub fn with_hasher(hash_builder: S) -> HashBag<T, S> {
        HashBag {
            items: HashMap::with_hasher(hash_builder),
            count: 0,
        }
    }

    /// Creates an empty `HashBag` with the specified capacity, using
    /// `hasher` to hash the keys.
    ///
    /// The hash bag will be able to hold at least `capacity` distinct values
    /// without reallocating. If `capacity` is 0, the hash bag will not allocate.
    ///
    /// Warning: `hasher` is normally randomly generated, and
    /// is designed to allow `HashBag`s to be resistant to attacks that
    /// cause many collisions and very poor performance. Setting it
    /// manually using this function can expose a DoS attack vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    /// use std::collections::hash_map::RandomState;
    ///
    /// let s = RandomState::new();
    /// let mut bag = HashBag::with_capacity_and_hasher(10, s);
    /// bag.insert(1);
    /// ```
    #[inline]
    pub fn with_capacity_and_hasher(capacity: usize, hash_builder: S) -> HashBag<T, S> {
        HashBag {
            items: HashMap::with_capacity_and_hasher(capacity, hash_builder),
            count: 0,
        }
    }

    /// Returns a reference to the bag's [`BuildHasher`].
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    /// use std::collections::hash_map::RandomState;
    ///
    /// let hasher = RandomState::new();
    /// let bag: HashBag<i32> = HashBag::with_hasher(hasher);
    /// let hasher: &RandomState = bag.hasher();
    /// ```
    #[inline]
    pub fn hasher(&self) -> &S {
        self.items.hasher()
    }

    /// Reserves capacity for at least `additional` more distinct values
    /// to be inserted in the `HashBag`. The collection may reserve more
    /// space to avoid frequent reallocations.
    ///
    /// # Panics
    ///
    /// Panics if the new allocation size overflows `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    /// let mut bag: HashBag<i32> = HashBag::new();
    /// bag.reserve(10);
    /// assert!(bag.capacity() >= 10);
    /// ```
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.items.reserve(additional)
    }

    /// Shrinks the capacity of the ba as much as possible. It will drop
    /// down as much as possible while maintaining the internal rules
    /// and possibly leaving some space in accordance with the resize policy.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let mut bag = HashBag::with_capacity(100);
    /// bag.insert(1);
    /// bag.insert(2);
    /// assert!(bag.capacity() >= 100);
    /// bag.shrink_to_fit();
    /// assert!(bag.capacity() >= 2);
    /// ```
    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.items.shrink_to_fit()
    }

    /// Returns the number of instances of `value` in the bag.
    ///
    /// The value may be any borrowed form of the bag's value type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let bag: HashBag<_> = [1, 2, 3, 3].iter().cloned().collect();
    /// assert_eq!(bag.contains(&1), 1);
    /// assert_eq!(bag.contains(&3), 2);
    /// assert_eq!(bag.contains(&4), 0);
    /// ```
    #[inline]
    pub fn contains<Q: ?Sized>(&self, value: &Q) -> usize
    where
        T: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.items.get(value).cloned().unwrap_or(0)
    }

    /// Returns a reference to the value in the bag, if any, that is equal to the given value,
    /// along with its number of occurrences.
    ///
    /// The value may be any borrowed form of the bag's value type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let bag: HashBag<_> = [1, 2, 3, 3].iter().cloned().collect();
    /// assert_eq!(bag.get(&2), Some((&2, 1)));
    /// assert_eq!(bag.get(&3), Some((&3, 2)));
    /// assert_eq!(bag.get(&4), None);
    /// ```
    #[inline]
    pub fn get<Q: ?Sized>(&self, value: &Q) -> Option<(&T, usize)>
    where
        T: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.items
            .get_key_value(value)
            .map(|(t, count)| (t, *count))
    }

    /// Adds a value to the bag.
    ///
    /// The number of occurrences of the value previously in the bag is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let mut bag = HashBag::new();
    ///
    /// assert_eq!(bag.insert(2), 0);
    /// assert_eq!(bag.insert(2), 1);
    /// assert_eq!(bag.insert(2), 2);
    /// assert_eq!(bag.set_len(), 1);
    /// assert_eq!(bag.len(), 3);
    /// ```
    #[inline]
    pub fn insert(&mut self, value: T) -> usize {
        self.insert_many(value, 1)
    }

    /// Adds multiple occurrences of a value to the bag.
    ///
    /// The number of occurrences of the value previously in the bag is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let mut bag = HashBag::new();
    ///
    /// assert_eq!(bag.insert_many(2, 1), 0);
    /// assert_eq!(bag.insert_many(2, 2), 1);
    /// assert_eq!(bag.insert_many(2, 4), 3);
    /// assert_eq!(bag.set_len(), 1);
    /// assert_eq!(bag.len(), 7);
    /// ```
    #[inline]
    pub fn insert_many(&mut self, value: T, count: usize) -> usize {
        self.count += count;
        let n = self.items.entry(value).or_insert(0);
        let was_there = *n;
        *n += count;
        was_there
    }

    /// Adds a value to the bag, replacing all existing occurrences, if any, that equal the given
    /// one.
    ///
    /// The number of occurrences of the value previously in the bag is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let mut bag = HashBag::new();
    /// bag.insert(Vec::<i32>::new());
    /// bag.insert(Vec::<i32>::new());
    /// assert_eq!(bag.contains(&[][..]), 2);
    /// assert_eq!(bag.get(&[][..]).unwrap().0.capacity(), 0);
    ///
    /// bag.replace(Vec::with_capacity(10));
    /// assert_eq!(bag.contains(&[][..]), 1);
    /// assert_eq!(bag.get(&[][..]).unwrap().0.capacity(), 10);
    /// ```
    #[inline]
    pub fn replace(&mut self, value: T) -> usize {
        let n = self.items.remove(&value).unwrap_or(0);
        self.count -= n;
        self.items.insert(value, 1);
        self.count += 1;
        n
    }

    /// Removes a value from the bag.
    ///
    /// The number of occurrences of the value previously in the bag is returned.
    ///
    /// The value may be any borrowed form of the bag's value type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let mut bag = HashBag::new();
    ///
    /// bag.insert_many('x', 2);
    /// assert_eq!(bag.contains(&'x'), 2);
    /// assert_eq!(bag.remove(&'x'), 2);
    /// assert_eq!(bag.contains(&'x'), 1);
    /// assert_eq!(bag.remove(&'x'), 1);
    /// assert_eq!(bag.contains(&'x'), 0);
    /// assert_eq!(bag.remove(&'x'), 0);
    /// ```
    #[inline]
    pub fn remove<Q: ?Sized>(&mut self, value: &Q) -> usize
    where
        T: Borrow<Q>,
        Q: Hash + Eq,
    {
        match self.items.get_mut(value) {
            None => 0,
            #[cfg(debug_assertions)]
            Some(n) if *n == 0 => unreachable!(),
            Some(n) if *n == 1 => {
                self.count -= 1;
                self.items.remove(value);
                1
            }
            Some(n) => {
                self.count -= 1;
                *n -= 1;
                *n + 1
            }
        }
    }

    /// Returns an iterator over all the elements that are in `self` with a
    /// higher occurrence count than in `other`. The count in the returned
    /// iterator represents how many more of a given element are in `self` than
    /// `other`.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    /// use std::collections::HashSet;
    /// use std::iter::FromIterator;
    ///
    /// let a: HashBag<_> = [1, 2, 3, 3].iter().cloned().collect();
    /// let b: HashBag<_> = [2, 3].iter().cloned().collect();
    /// let expected: HashSet<_> = HashSet::from_iter([(&1, 1), (&3, 1)]);
    /// let actual: HashSet<_> = a.difference(&b).collect();
    /// assert_eq!(expected, actual);
    /// ```
    pub fn difference<'a>(
        &'a self,
        other: &'a HashBag<T, S>,
    ) -> impl Iterator<Item = (&'a T, usize)> {
        self.items.iter().filter_map(move |(x, &self_count)| {
            let other_count = other.contains(x);
            if self_count > other_count {
                Some((x, self_count - other_count))
            } else {
                None
            }
        })
    }

    /// Removes a value that is equal to the given one, and returns it if it was the last.
    ///
    /// If the matching value is not the last, a reference to the remainder is given, along with
    /// the number of occurrences prior to the removal.
    ///
    /// The value may be any borrowed form of the bag's value type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let mut bag: HashBag<_> = [1, 2, 3, 3].iter().cloned().collect();
    /// assert_eq!(bag.try_take(&2), Ok(2));
    /// assert_eq!(bag.try_take(&3), Err(Some((&3, 2))));
    /// assert_eq!(bag.try_take(&3), Ok(3));
    /// assert_eq!(bag.try_take(&4), Err(None));
    /// ```
    #[inline]
    pub fn try_take<Q: ?Sized>(&mut self, value: &Q) -> Result<T, Option<(&T, usize)>>
    where
        T: Borrow<Q>,
        Q: Hash + Eq,
    {
        // TODO: it should be possible to make this more efficient
        match self.items.remove_entry(value) {
            Some((t, 1)) => {
                self.count -= 1;
                Ok(t)
            }
            Some((t, n)) => {
                self.count -= 1;
                self.items.insert(t, n - 1);
                Err(Some(
                    self.items
                        .get_key_value(value)
                        .map(|(t, n)| (t, *n + 1))
                        .unwrap(),
                ))
            }
            None => Err(None),
        }
    }

    /// Removes and returns all occurrences of the value, if any, that is equal to the given one.
    ///
    /// The value may be any borrowed form of the bag's value type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let mut bag: HashBag<_> = [1, 2, 3, 3].iter().cloned().collect();
    /// assert_eq!(bag.take_all(&2), Some((2, 1)));
    /// assert_eq!(bag.take_all(&3), Some((3, 2)));
    /// assert_eq!(bag.take_all(&2), None);
    /// assert_eq!(bag.take_all(&3), None);
    /// ```
    #[inline]
    pub fn take_all<Q: ?Sized>(&mut self, value: &Q) -> Option<(T, usize)>
    where
        T: Borrow<Q>,
        Q: Hash + Eq,
    {
        let (t, n) = self.items.remove_entry(value)?;
        self.count -= n;
        Some((t, n))
    }

    /// Retains only the values specified by the predicate.
    ///
    /// In other words, for each value `v` retain only `f(&v)` occurrences.
    ///
    /// # Examples
    ///
    /// ```
    /// use hashbag::HashBag;
    ///
    /// let xs = [0,0,0,0,0,1,1,1,1,2,2,2,3,3,4];
    /// let mut bag: HashBag<i32> = xs.iter().cloned().collect();
    /// bag.retain(|&k, _| k as usize);
    /// assert_eq!(bag.set_len(), 4); // >= 1 of all but value 0
    /// assert_eq!(bag.len(), 6);
    /// assert_eq!(bag.contains(&0), 0);
    /// assert_eq!(bag.contains(&1), 1);
    /// assert_eq!(bag.contains(&2), 2);
    /// assert_eq!(bag.contains(&3), 2);
    /// assert_eq!(bag.contains(&4), 1);
    /// ```
    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&T, usize) -> usize,
    {
        let count = &mut self.count;
        self.items.retain(|t, n| {
            let keep = std::cmp::min(*n, f(t, *n));
            *count -= *n - keep;
            if keep == 0 {
                false
            } else {
                *n = keep;
                true
            }
        });
    }
}

// ======== standard traits

use std::fmt;

impl<T> fmt::Debug for HashBag<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_set().entries(self.iter()).finish()
    }
}

impl<T, S> Default for HashBag<T, S>
where
    T: Eq + Hash,
    S: BuildHasher + Default,
{
    fn default() -> Self {
        Self::with_hasher(S::default())
    }
}

impl<T, S> PartialEq<HashBag<T, S>> for HashBag<T, S>
where
    T: Eq + Hash,
    S: BuildHasher,
{
    fn eq(&self, other: &Self) -> bool {
        self.count == other.count && self.items == other.items
    }
}

impl<T, S> Eq for HashBag<T, S>
where
    T: Eq + Hash,
    S: BuildHasher,
{
}

impl<'a, T, S> Extend<&'a T> for HashBag<T, S>
where
    T: 'a + Eq + Hash + Clone,
    S: BuildHasher,
{
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
        for e in iter {
            self.insert(e.clone());
        }
    }
}

impl<'a, T, S> Extend<(&'a T, usize)> for HashBag<T, S>
where
    T: 'a + Eq + Hash + Clone,
    S: BuildHasher,
{
    fn extend<I: IntoIterator<Item = (&'a T, usize)>>(&mut self, iter: I) {
        for (e, n) in iter {
            self.count += n;
            *self.items.entry(e.clone()).or_insert(0) += n;
        }
    }
}

impl<T, S> Extend<T> for HashBag<T, S>
where
    T: Eq + Hash,
    S: BuildHasher,
{
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for e in iter {
            self.insert(e);
        }
    }
}

impl<T, S> Extend<(T, usize)> for HashBag<T, S>
where
    T: Eq + Hash,
    S: BuildHasher,
{
    fn extend<I: IntoIterator<Item = (T, usize)>>(&mut self, iter: I) {
        for (e, n) in iter {
            self.count += n;
            *self.items.entry(e).or_insert(0) += n;
        }
    }
}

impl<T, S> std::iter::FromIterator<T> for HashBag<T, S>
where
    T: Eq + Hash,
    S: BuildHasher + Default,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut bag = Self::default();
        bag.extend(iter);
        bag
    }
}

impl<'a, T, S> IntoIterator for &'a HashBag<T, S> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;
    fn into_iter(self) -> Iter<'a, T> {
        self.iter()
    }
}

impl<T, S> IntoIterator for HashBag<T, S> {
    type Item = (T, usize);
    type IntoIter = IntoIter<T>;
    fn into_iter(self) -> IntoIter<T> {
        IntoIter(self.items.into_iter())
    }
}

// ======== iterators

#[cfg(feature = "amortize")]
type IterInner<'a, T> = griddle::hash_map::Iter<'a, T, usize>;
#[cfg(not(feature = "amortize"))]
type IterInner<'a, T> = std::collections::hash_map::Iter<'a, T, usize>;

/// An iterator over the items of a `HashBag`.
///
/// Each value is repeated as many times as it occurs in the bag.
///
/// This `struct` is created by [`HashBag::iter`].
/// See its documentation for more.
pub struct Iter<'a, T> {
    iter: IterInner<'a, T>,
    repeat: Option<(&'a T, usize)>,
    left: usize,
}

impl<'a, T> std::iter::FusedIterator for Iter<'a, T> where IterInner<'a, T>: std::iter::FusedIterator
{}

impl<'a, T> ExactSizeIterator for Iter<'a, T> where IterInner<'a, T>: ExactSizeIterator {}

impl<'a, T> Clone for Iter<'a, T> {
    fn clone(&self) -> Self {
        Iter {
            iter: self.iter.clone(),
            repeat: self.repeat,
            left: self.left,
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Iter<'_, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_set().entries(self.clone()).finish()
    }
}

impl<'a, T> Iter<'a, T> {
    fn new(it: IterInner<'a, T>, n: usize) -> Self {
        Self {
            iter: it,
            repeat: None,
            left: n,
        }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some((t, ref mut n)) = self.repeat {
            if *n == 0 {
                self.repeat = None;
            } else {
                *n -= 1;
                self.left -= 1;
                return Some(t);
            }
        }

        let (next, n) = self.iter.next()?;
        if *n > 1 {
            self.repeat = Some((next, *n - 1));
        }
        self.left -= 1;
        Some(next)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.left, Some(self.left))
    }
}

/// An iterator over the distinct items of a `HashBag` and their occurrence counts.
///
/// This `struct` is created by [`HashBag::set_iter`].
/// See its documentation for more.
pub struct SetIter<'a, T>(IterInner<'a, T>);

impl<'a, T> std::iter::FusedIterator for SetIter<'a, T> where
    IterInner<'a, T>: std::iter::FusedIterator
{
}

impl<'a, T> ExactSizeIterator for SetIter<'a, T> where IterInner<'a, T>: ExactSizeIterator {}

impl<'a, T> Clone for SetIter<'a, T> {
    fn clone(&self) -> Self {
        SetIter(self.0.clone())
    }
}

impl<T: fmt::Debug> fmt::Debug for SetIter<'_, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_set().entries(self.clone()).finish()
    }
}

impl<'a, T> Iterator for SetIter<'a, T> {
    type Item = (&'a T, usize);
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(t, n)| (t, *n))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

#[cfg(feature = "amortize")]
type IntoIterInner<T> = griddle::hash_map::IntoIter<T, usize>;
#[cfg(not(feature = "amortize"))]
type IntoIterInner<T> = std::collections::hash_map::IntoIter<T, usize>;

/// An owning iterator over the distinct items of a `HashBag` and their occurrence counts.
///
/// This `struct` is created by using the implementation of [`IntoIterator`] for [`HashBag`].
pub struct IntoIter<T>(IntoIterInner<T>);

impl<T> std::iter::FusedIterator for IntoIter<T> where IntoIterInner<T>: std::iter::FusedIterator {}

impl<T> ExactSizeIterator for IntoIter<T> where IntoIterInner<T>: ExactSizeIterator {}

impl<T: fmt::Debug> fmt::Debug for IntoIter<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(fmt)
    }
}

impl<T> Iterator for IntoIter<T> {
    type Item = (T, usize);
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

#[cfg(feature = "amortize")]
type DrainInner<'a, T> = griddle::hash_map::Drain<'a, T, usize>;
#[cfg(not(feature = "amortize"))]
type DrainInner<'a, T> = std::collections::hash_map::Drain<'a, T, usize>;

/// An draining iterator over the distinct items of a `HashBag` and their occurrence counts.
///
/// This `struct` is created by [`HashBag::drain`].
/// See its documentation for more.
pub struct Drain<'a, T>(DrainInner<'a, T>);

impl<'a, T> std::iter::FusedIterator for Drain<'a, T> where
    DrainInner<'a, T>: std::iter::FusedIterator
{
}

impl<'a, T> ExactSizeIterator for Drain<'a, T> where DrainInner<'a, T>: ExactSizeIterator {}

impl<'a, T: fmt::Debug> fmt::Debug for Drain<'a, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(fmt)
    }
}

impl<'a, T> Iterator for Drain<'a, T> {
    type Item = (T, usize);
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format_all_the_things() {
        let mut vikings: HashBag<&'static str> =
            ["Einar", "Olaf", "Harald"].iter().cloned().collect();
        println!("{:?}", vikings);
        println!("{:?}", vikings.iter());
        println!("{:?}", vikings.set_iter());
        println!("{:?}", vikings.clone().into_iter());
        println!("{:?}", vikings.drain());
    }

    #[test]
    fn sane_iterators() {
        let mut vikings: HashBag<&'static str> =
            ["Einar", "Einar", "Harald"].iter().cloned().collect();
        assert_eq!(vikings.iter().count(), 3);
        assert_eq!(vikings.iter().size_hint(), (3, Some(3)));
        assert_eq!(vikings.iter().clone().count(), 3);
        assert_eq!(vikings.set_iter().count(), 2);
        assert_eq!(vikings.set_iter().clone().count(), 2);
        assert_eq!(vikings.set_iter().size_hint(), (2, Some(2)));
        let ii = vikings.clone().into_iter();
        assert_eq!(ii.size_hint(), (2, Some(2)));
        assert_eq!(ii.count(), 2);
        let di = vikings.drain();
        assert_eq!(di.size_hint(), (2, Some(2)));
        assert_eq!(di.count(), 2);
    }

    #[test]
    fn test_difference_size_hint() {
        let bag: HashBag<_> = [3, 2, 1].iter().cloned().collect();
        let empty_bag = HashBag::new();
        let mut difference = bag.difference(&empty_bag);

        // Since the difference has the same number of entries as the bag, we
        // can predict how the size_hint() will behave, because the iteration
        // order does not matter
        assert_eq!(difference.size_hint(), (0, Some(3)));
        difference.next().unwrap();
        assert_eq!(difference.size_hint(), (0, Some(2)));
        difference.next().unwrap();
        assert_eq!(difference.size_hint(), (0, Some(1)));
        difference.next().unwrap();
        assert_eq!(difference.size_hint(), (0, Some(0)));
        assert_eq!(difference.next(), None);
        assert_eq!(difference.size_hint(), (0, Some(0)));
    }

    #[test]
    fn test_difference_from_empty() {
        do_test_difference(&[], &[], &[]);
        do_test_difference(&[], &[1], &[]);
        do_test_difference(&[], &[1, 1], &[]);
        do_test_difference(&[], &[1, 1, 2], &[]);
    }

    #[test]
    fn test_difference_from_one() {
        do_test_difference(&[1], &[], &[1]);
        do_test_difference(&[1], &[1], &[]);
        do_test_difference(&[1], &[1, 1], &[]);
        do_test_difference(&[1], &[2], &[1]);
        do_test_difference(&[1], &[1, 2], &[]);
        do_test_difference(&[1], &[2, 2], &[1]);
    }

    #[test]
    fn test_difference_from_duplicate_ones() {
        do_test_difference(&[1, 1], &[], &[1, 1]);
        do_test_difference(&[1, 1], &[1], &[1]);
        do_test_difference(&[1, 1], &[1, 1], &[]);
        do_test_difference(&[1, 1], &[2], &[1, 1]);
        do_test_difference(&[1, 1], &[1, 2], &[1]);
        do_test_difference(&[1, 1], &[2, 2], &[1, 1]);
    }

    #[test]
    fn test_difference_from_one_one_two() {
        do_test_difference(&[1, 1, 2], &[], &[1, 1, 2]);
        do_test_difference(&[1, 1, 2], &[1], &[1, 2]);
        do_test_difference(&[1, 1, 2], &[1, 1], &[2]);
        do_test_difference(&[1, 1, 2], &[2], &[1, 1]);
        do_test_difference(&[1, 1, 2], &[1, 2], &[1]);
        do_test_difference(&[1, 1, 2], &[2, 2], &[1, 1]);
    }

    #[test]
    fn test_difference_from_larger_bags() {
        do_test_difference(&[1, 2, 2, 3], &[3], &[1, 2, 2]);
        do_test_difference(&[1, 2, 2, 3], &[4], &[1, 2, 2, 3]);
        do_test_difference(&[2, 2, 2, 2], &[2, 2], &[2, 2]);
        do_test_difference(&[2, 2, 2, 2], &[], &[2, 2, 2, 2]);
    }

    fn do_test_difference(
        self_entries: &[isize],
        other_entries: &[isize],
        expected_entries: &[isize],
    ) {
        let this = self_entries.iter().collect::<HashBag<_>>();
        let other = other_entries.iter().collect::<HashBag<_>>();
        let expected = expected_entries.iter().collect::<HashBag<_>>();
        let mut actual = HashBag::new();
        for (t, n) in this.difference(&other) {
            actual.insert_many(*t, n);
        }

        assert_eq!(actual, expected);
    }
}
