# conduit-aeson

## Overview

A simple library that allows streaming parsing of large JSON objects and arrays
using Aeson, Attoparsec and Conduit.

It is important to note that only the top level elements of arrays and objects
can be streamed, in other words streaming of nested objects is not supported.

This library is suitable for parsing of very large json files as well as
infinite streams of JSON objects.

## Status

| Github Actions | Coveralls | Hackage | Nightly | LTS |
|:--------------:|:---------:|:-------:|:-------:|:---:|
| [![Build Status][GA-badge]][GA-link] | [![Coverage Status][Coveralls-badge]][Coveralls-link] | [![Hackage][Hackage-badge]][Hackage-link] | [![Nightly][Nightly-badge]][Nightly-link] | [![LTS][LTS-badge]][LTS-link]

[GA-badge]: https://github.com/lehins/conduit-aeson/workflows/CI/badge.svg
[GA-link]: https://github.com/lehins/conduit-aeson/actions
[Coveralls-badge]: https://coveralls.io/repos/github/lehins/conduit-aeson/badge.svg?branch=master
[Coveralls-link]: https://coveralls.io/github/lehins/conduit-aeson?branch=master
[Hackage-badge]: https://img.shields.io/hackage/v/conduit-aeson.svg
[Hackage-link]: https://hackage.haskell.org/package/conduit-aeson
[Nightly-badge]: https://www.stackage.org/package/conduit-aeson/badge/nightly
[Nightly-link]: https://www.stackage.org/nightly/package/conduit-aeson
[LTS-badge]: https://www.stackage.org/package/conduit-aeson/badge/lts
[LTS-link]: https://www.stackage.org/lts/package/conduit-aeson
