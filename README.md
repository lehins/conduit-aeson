# conduit-aeson

A simple library that allows streaming parsing of large JSON objects and arrays
using Aeson, Attoparsec and Conduit.

It is important to note that only the rop level elements of arrays and objects
can be streamed, in other words streaming of nested objects is not supported.

This library is suitable for parsing of very large json files as well as
infinite streams of JSON objects.
