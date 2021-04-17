# scheme-lsp-server

## Introduction

** EXPERIMENTAL **

An implementation of the LSP (Language Server Protocol) for Scheme.

## Goals

This software aims to support several Scheme implementations. This shall
be achieved by writing as much logic as possible in R7RS Scheme, and
separating implementation-specific code to the corresponding directories. Currently only CHICKEN 5 and to a lesser extend Guile are supported.
More details on the supported features soon.

*Note*: this code is still in an early development stage. Design suggestions are welcome.

## API


```
[function] (start-lsp-server tcp-port [debug-level 0])
```

Start an LSP server listening on `tcp-port`. An optional number `debug-level` ranging between 0 (silent) and 3 (verbose) may be provided to
control verbosity of the LSP server.
