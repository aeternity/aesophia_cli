# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
### Changed
### Removed

## [7.2.1]
### Added
- Decode calldata functionality:
  `aesophia_cli --decode_calldata cb_KxG3+3bAG1StlAV3 --calldata_fun main_ test/contracts/identity.aes`
- Generate ACI from stub/partial contract with `--no-code` flag
  `aesophia_cli --create_json_aci <contract_stub> --no_code`
- Add encode/decode functionality for Sophia values:
  `aesophia_cli --encode_value "(42, true)" --value_type "int * bool"`
  `aesophia_cli --decode_value cb_VNLOFXc= --value_type "int * bool"`
### Changed
- Updated to [Sophia 7.2.1](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#721)
- add `src_file` to default compiler opts for better error reporting
- fixed bug with include files when using `--validate`

## [7.1.0]
### Changed
- Updated to [Sophia 7.1.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#710)

## [7.0.1]
### Changed
- Updated to [Sophia 7.0.1](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#701)

## [7.0.0]
### Added
- Option `--oneline_errors` for printing errors on one (long) line.
### Changed
- Fixed a bug in calldata encoding - the input was not properly UTF8 encoded.
- Updated to [Sophia 7.0.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#700)
### Removed
- Support for AEVM backend
- `-b` option to set backend (now only FATE is supported)
- `decode_data` feature, as it worked only on AEVM

## [6.1.0]
### Changed
- Updated to [Sophia 6.1.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#610---2021-10-20)

## [6.0.2]
### Added
### Changed
- Updated to [Sophia 6.0.2](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#602)
### Removed

## [6.0.1]
### Added
### Changed
- Updated to [Sophia 6.0.1](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#601)
### Removed

## [6.0.0]
### Added
### Changed
- Updated to [Sophia 6.0.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#600)
### Removed

## [5.0.0]
### Added
### Changed
- Updated to [Sophia 5.0.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#500)
### Removed

## [4.3.1] - 2020-04-21
### Added
### Changed
- Fixed included compiler binary file, which was broken due to incorrect local system dependencies.
  Because the aesophia version hasn't changed, the compiler in this release
  continues to report as `v4.3.0`.
### Removed

## [4.3.0] - 2020-04-02
### Added
### Changed
- Updated to [Sophia 4.3.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#430)
### Removed

## [4.2.0] - 2020-01-15
### Added
- Added option `--compiled_by` to extract the compiler version from either a file or a
  contract byte array.
### Changed
- Updated to [Sophia 4.2.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#420---2020-01-15)
### Removed

## [4.1.0] - 2019-11-26
### Added
- Added option `--validate` to check if bytecode from the chain was compiled from given
  source code.
### Changed
- Fix bug with standard library includes.
### Removed

[Unreleased]: https://github.com/aeternity/aesophia_cli/compare/v7.2.1...HEAD
[7.2.1]: https://github.com/aeternity/aesophia_cli/compare/v7.1.0...v7.2.1
[7.1.0]: https://github.com/aeternity/aesophia_cli/compare/v7.0.1...v7.1.0
[7.0.1]: https://github.com/aeternity/aesophia_cli/compare/v7.0.0...v7.0.1
[7.0.0]: https://github.com/aeternity/aesophia_cli/compare/v6.1.0...v7.0.0
[6.1.0]: https://github.com/aeternity/aesophia_cli/compare/v6.0.2...v6.1.0
[6.0.2]: https://github.com/aeternity/aesophia_cli/compare/v6.0.1...v6.0.2
[6.0.1]: https://github.com/aeternity/aesophia_cli/compare/v6.0.0...v6.0.1
[6.0.0]: https://github.com/aeternity/aesophia_cli/compare/v5.0.0...v6.0.0
[5.0.0]: https://github.com/aeternity/aesophia_cli/compare/v4.3.1...v5.0.0
[4.3.1]: https://github.com/aeternity/aesophia_cli/compare/v4.3.0...v4.3.1
[4.3.0]: https://github.com/aeternity/aesophia_cli/compare/v4.2.0...v4.3.0
[4.2.0]: https://github.com/aeternity/aesophia_cli/compare/v4.1.0...v4.2.0
[4.1.0]: https://github.com/aeternity/aesophia_cli/releases/tag/v4.1.0
