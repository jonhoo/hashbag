[![Crates.io](https://img.shields.io/crates/v/hashbag.svg)](https://crates.io/crates/hashbag)
[![Documentation](https://docs.rs/hashbag/badge.svg)](https://docs.rs/hashbag/)
[![Build Status](https://dev.azure.com/jonhoo/jonhoo/_apis/build/status/hashbag?branchName=master)](https://dev.azure.com/jonhoo/jonhoo/_build/latest?definitionId=17&branchName=master)
[![Codecov](https://codecov.io/github/jonhoo/hashbag/coverage.svg?branch=master)](https://codecov.io/gh/jonhoo/hashbag)
[![Dependency status](https://deps.rs/repo/github/jonhoo/hashbag/status.svg)](https://deps.rs/repo/github/jonhoo/hashbag)

An unordered multiset/bag implementation backed by `HashMap`.

A bag, unlike a set, allows duplicate values, and keeps track of how many
duplicates each value holds. This type of collection is often referred to
as an unordered multiset (see also C++'s [`std::unordered_multiset`]).

  [`std::unordered_multiset`]: http://www.cplusplus.com/reference/unordered_set/unordered_multiset/

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
