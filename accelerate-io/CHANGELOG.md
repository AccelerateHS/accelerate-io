# Change Log

Notable changes to the project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and the
project adheres to the [Haskell Package Versioning
Policy (PVP)](https://pvp.haskell.org)


## [next]
### Changed
  * Split into separate packages

## [1.2.0.0] - 2018-04-03
### Changed
  * Split the different conversion functions into separate modules, rather than having a single `Data.Array.Accelerate.IO` module which export everything.
  * Conversion to/from `ByteString` is now non-copying

### Added
  * Conversions between `Data.Vector.Unboxed`
  * Instances for `Data.Vector.Generic`
  * Support for AoS representations

### Fixed
  * Image created by `writeImageToBMP` flipped vertically ([#289])


## [1.0.0.1] - 2017-10-14
### Fixed
  * `fromIArray` would fail with exception "Error in array index" when the IArray indices were not zero-based. This has been fixed.


## [1.0.0.0] - 2017-03-31
  * stable release


[next]:       https://github.com/AccelerateHS/accelerate-io/compare/1.2.0.0...HEAD
[1.2.0.0]:    https://github.com/AccelerateHS/accelerate-io/compare/1.0.0.1...1.2.0.0
[1.0.0.1]:    https://github.com/AccelerateHS/accelerate-io/compare/1.0.0.0...1.0.0.1
[1.0.0.0]:    https://github.com/AccelerateHS/accelerate-io/compare/0.15.1.0...1.0.0.0


[#289]:       https://github.com/AccelerateHS/accelerate/issues/289

