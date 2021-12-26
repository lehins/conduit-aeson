# conduit-aeson

## Overview

A simple library that allows streaming parsing of large JSON objects and arrays
using Aeson, Attoparsec and Conduit.

It is important to note that only the top level elements of arrays and objects
can be streamed, in other words streaming of nested objects is not supported.

This library is suitable for parsing of very large json files as well as
infinite streams of JSON objects.

## Status

| Language | Github Actions | Coveralls |
|:--------:|:--------------:|:---------:|
| ![GitHub top language](https://img.shields.io/github/languages/top/lehins/conduit-aeson.svg) | [![Build Status](https://github.com/lehins/conduit-aeson/workflows/CI/badge.svg)](https://github.com/lehins/conduit-aeson/actions) | [![Coverage Status](https://coveralls.io/repos/github/lehins/conduit-aeson/badge.svg?branch=master)](https://coveralls.io/github/lehins/conduit-aeson?branch=master) |
