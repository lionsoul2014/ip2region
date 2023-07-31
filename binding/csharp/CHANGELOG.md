# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

## [2.0.1] - 2023-07-30

### Added
- Support netstandard2.0

## [2.0.0] - 2023-07-26

### Removed
- Remove nuget include xdb file
- Searcher cache policy default parameters
- Searcher xdb file path default parameters

### Added
- Dependent file query policies CachePolicy.VectorIndex, CachePolicy.File support thread-safe concurrent queries
- Dramatically optimizes overall performance