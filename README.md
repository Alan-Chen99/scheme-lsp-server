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
[parameter] (lsp-server-log-level)
```
An integer to control the server's verbosity, ranging from 0 (show only `error` messages) to 3 (verbose).

```
[function] (start-lsp-server tcp-port)
[function] (start-lsp-server/background tcp-port)
```

Start an LSP server listening on `tcp-port`. `start-lsp-server/background` starts the server on a new thread and returns it.
