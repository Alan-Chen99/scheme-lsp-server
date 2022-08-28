# scheme-lsp-server

## Table of contents
1. [Introduction](#introduction)
2. [Installing](#installing)
3. [API](#api)
4. [Supported features](#supported-features)
5. [Notes to specific implementations](#specific-implementations)
6. [Known issues](#known-issues)
7. [Existing clients](#existing-clients)
8. [Contributing](#contributing)

## <a name="user-content-introduction"></a> Introduction

** EXPERIMENTAL **

A LSP (Language Server Protocol) server for Scheme.

### Goals

This software aims to support several Scheme implementations. To achieve this,
the code is designed to contain as much logic as possible in R7RS Scheme,
separating implementation-specific code in different directories.

Currently only CHICKEN 5 and Guile are supported.

*Note*: this code is still in an early development stage and the API may change.
Change suggestions are welcome.

## <a name="user-content-installing"></a> Installing

First a remark. Some LSP clients (currently both of them) will
install an LSP server automatically. If anything goes wrong (or you want
to integrate the LSP server with a new client), please follow the
instructions below.

### CHICKEN

First install some dependencies, including chicken-doc's documentation:

```
$ chicken-install -s apropos chicken-doc srfi-18
$ cd `csi -p '(chicken-home)'`
$ curl http://3e8.org/pub/chicken-doc/chicken-doc-repo.tgz | sudo tar zx
```

Then install the LSP server with

```
chicken-install -s lsp-server
```

### Gambit

You can install the library by simply running

```
$ gsi -install codeberg.org/rgherdt/scheme-lsp-server
```

You can now import the library using its fully qualified name

```
> (import (codeberg.org/rgherdt/scheme-lsp-server))
```

Instead of installing it, you can also call it by invoking `gsi` whitelisting my repo:

```
$ gsi -:whitelist=codeberg.org/rgherdt/scheme-lsp-server
> (import (codeberg.org/rgherdt/scheme-lsp-server))
```

In order to use the command line tool, copy the file
`~/.gambit_userlib/codeberg.org/rgherdt/scheme-lsp-server/@/gambit/gambit-lsp-server` 
to a directory in your `PATH`.

### GUILE
Guile's version of the LSP server is packaged using automake. Make
sure Guile 3 **AND** its development libraries are installed. On Debian
you can install it using:
```
# apt install guile-3.0 guile-3.0-dev
```

Now switch to the `./guile` folder and run:

```
./configure && make && sudo make install
```


## <a name="user-content-api"></a>API

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

## <a name="user-content-supported-features"></a>Supported features


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

## <a name="user-content-specific-implementations"></a>Notes to specific implementations

### CHICKEN

The implementation for "jump to definition" is for now quite limited. I wrote
a ctags-inspired code for CHICKEN that scans the project files for definitions.
Additionally, the user can set the environment variable `CHICKEN_SOURCE_PATH`
to a directory containing the installer's source, so that the server can
provide information to internally defined functions.

In the future we can refine this solution to be more "project-aware". Ideas
are welcome.

### GUILE

Most of the current implementation relies on Geiser. We include the corresponding
Scheme files in our repository (git submodules was discarded to simplify
packaging and automatic installation from LSP clients).

## <a name="user-content-known-issues"></a>Known issues

### [Guile] No LSP information when library definition is missing

Currently the LSP server only compiles (and imports) files that contain a
library definition. In Scheme it's common though to separate the library
definition and its implementation in different files (as this same library
does). This means that if you open an implementation file (i.e. a scheme file
without library definition) first, the LSP server won't provide much
information. By opening the corresponding library definition (doesn't matter if
before of after opening the first file) it should work properly.

I experimented with a couple of workarounds for this, but was not 100% satisfied
by the result. One idea was to simply compile/import every single file found
in a project that contains a library definition. But this would bloat the
run-time, and in my experiments lead to weird behavior on some large projects.
Ideally we would like to keep track which source files belong to which library
definition, but I'm not sure what's the best way to achieve this.

### [CHICKEN] Slow startup on large projects

For CHICKEN we scan the project and parse files in order to fetch LSP-related
information. On large projects, you may experience a delay until
information starts to be received. LSP provides ways of giving feedback
to the user when an operation takes a long time, we can add support to it
in the future.

## <a name="user-content-existing-clients"></a>Existing clients

### VSCodium: https://codeberg.org/rgherdt/vscode-scheme-lsp

### Emacs lsp-mode: https://codeberg.org/rgherdt/emacs-lsp-scheme

### Emacs Eglot: https://github.com/joaotavora/eglot
Add the following to your `.emacs` configuration:

```
(require 'eglot)
(add-to-list 'eglot-server-programs
             `(scheme-mode . ("guile-lsp-server")))
(add-hook 'scheme-mode-hook 'eglot-ensure)
```

Replace `"guile-lsp-server"` by your chosen scheme implementation.

## <a name="user-content-contributing"></a>Contributing

### Creating an LSP client

`scheme-lsp-server` supports two modes. It can either operate by listening
on `stdio` (using `lsp-server-start/stdio`) or `TCP` (using
`lsp-server-start/tcp`). A command-line tool is available that you can call
from your client. Here an example call (analogous to `chicken-lsp-server`):

```
guile-lsp-server --tcp 4242
```

Leaving out the `--tcp` flag starts the server in `stdio` mode. More information
can be obtained with the `guile-lsp-server --help` command as usual.

If you create an LSP client using this server, please let me know so we can
keep this list up-to-date.

### Ideas on extending support to other Schemes

Here are some ideas on how to add support to other Scheme implementation without
increasing much code complexity:

#### decide which build system to use.

Currently we use two build systems:
`chicken-install` with its egg definitions, and `autotools` for Guile. Ideally
we should come up with a solution that can be used across all supported
implementations. Possible candidates are Snow or Akku. Alternatively we could
consider extending the existing `autotools` based scripts.

Note that this may be irrelevant in some cases. Gambit, for instance, now
supports loading libraries directly from `git` repositories.

#### create needed portable libraries

This library relies on non-standardized features, like TCP support and JSON
(indirectly through `scheme-json-rpc` (https://codeberg.org/rgherdt/scheme-json-rpc/).
It would be extremely helpful if those bits are solved by separate libraries
or SRFIs. Guile's version for JSON-RPC already uses SRFI 180, that can solve
the JSON problem.

#### contribute to Geiser

Since `scheme-lsp-server` uses Geiser, we can get better LSP support by help improving it.
