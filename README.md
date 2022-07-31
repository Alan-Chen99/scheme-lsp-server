# scheme-lsp-server

## Introduction

** EXPERIMENTAL **

A LSP (Language Server Protocol) server for Scheme.

## Goals

This software aims to support several Scheme implementations. To achieve this,
the code is designed to contain as much logic as possible in R7RS Scheme,
separating implementation-specific code in different directories.

Currently only CHICKEN 5 and Guile are supported.

*Note*: this code is still in an early development stage and the API may change.
Change suggestions are welcome.

## API

```
[parameter] (lsp-server-log-level)
```
A symbol to control the server's verbosity. It can be either 'error, 'warning,
'info or 'debug.

```
[procedure] (lsp-server-start/stdio)
```

Start an LSP server listening on stdio.


```
[procedure] (lsp-server-start/tcp tcp-port-number)
```

Start an LSP server listening on `tcp-port-number`.

## Supported Features


<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">&#xa0;</th>
<th scope="col" class="org-left">Guile</th>
<th scope="col" class="org-left">CHICKEN</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Find signature</td>
<td class="org-left">X</td>
<td class="org-left">X</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">Find documentation</td>
<td class="org-left">X</td>
<td class="org-left">X</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">Autocomplete identifier</td>
<td class="org-left">X</td>
<td class="org-left">X</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">Jump to definition</td>
<td class="org-left">X</td>
<td class="org-left">X</td>
</tr>
</tbody>
</table>

## Notes to specific implementations

### CHICKEN

The implementation for "jump to definition" is for now quite limited. I wrote
a ctags-inspired code for CHICKEN that scans the project files for definitions.
Additionally, the user can set the environment variable `CHICKEN_SOURCE_PATH`
to a directory containing the installer's source, so that the server can
provide information to internally defined functions.

In the future we can refine this solution to be more "project-aware". Ideas
are welcome.

### GUILE

Most of the current implemention relies on Geiser. We include the corresponding
Scheme files in our repository (git submodules was discarded to simplify
packaging and automatic installation from LSP clients.

## Ideas on extending support to other Schemes

Here are some ideas on how add support to other Scheme implementation without
increasing much code complexity:

### decide which build system to use.

Currently we use two build systems:
`chicken-install` with its egg definitions, and `autotools` for Guile. Ideally
we should come up with a solution that can be used across all supported
implementations. Possible candidates are Snow or Akku. Alternatively we could
consider extending the existing `autotools` based scripts.

### move more code to the common session.

Ideally we would leave to the implementation-specific files only code needed
to provide programming-language features (find signature, documentation etc.).
This is unfortunately still not the case, since the server relies on
 functionalities not provided by the RnRS standards, like JSON (used by our
 JSON-RPC library) and TCP sockets.
It would be extremely helpful if those bits are solved by separate libraries
or SRFIs. Guile's version for JSON-RPC already uses SRFI 180, so I would suggest
to adopt it by other implementations.
