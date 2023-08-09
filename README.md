# scheme-lsp-server

## Table of contents
1. [Introduction](#introduction)
2. [Installing](#installing)
3. [API](#api)
4. [Command-line tool](#cli)
5. [Supported features](#supported-features)
6. [Notes to specific implementations](#specific-implementations)
7. [Known issues](#known-issues)
8. [Existing clients](#existing-clients)
9. [Contributing](#contributing)

## <a name="user-content-introduction"></a> Introduction

** EXPERIMENTAL **

An LSP (Language Server Protocol) server for Scheme.

This software aims to support several Scheme implementations. To achieve this,
the code is designed to contain as much logic as possible in R7RS Scheme,
separating implementation-specific code in different directories.

*Note*: this code is still in an early development stage and the API may change.
Change suggestions are welcome.


### Supported implementations

Currently CHICKEN 5, Gambit 4.9.4+ and Guile 3+ are supported. See [Supported features](#supported-features), [Notes to specific implementations](#specific-implementations) and [Known issues](#known-issues) for more information.


## <a name="user-content-installing"></a> Installing

First a remark. Some LSP clients (like [lsp-scheme](https://codeberg.org/rgherdt/emacs-lsp-scheme)
and [vscode-scheme-lsp](https://codeberg.org/rgherdt/vscode-scheme-lsp)) will
install an LSP server automatically. If you prefer to install it manually, please follow the
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

Note that you need Gambit 4.9.4 or later in order to use this lib.
You can install the library and its dependencies by running

```
$ gsi -install \
      codeberg.org/rgherdt/srfi \
      github.com/ashinn/irregex \
      github.com/rgherdt/chibi-scheme \
      codeberg.org/rgherdt/scheme-json-rpc/json-rpc \
      codeberg.org/rgherdt/scheme-lsp-server/lsp-server
```


You can now import the library using its fully qualified name

```
> (import (codeberg.org/rgherdt/scheme-lsp-server lsp-server))
```

Instead of installing it, you can also call it by invoking `gsi` with `-:whitelist`

```
$ gsi -:whitelist=codeberg.org/rgherdt/scheme-lsp-server
> (import (codeberg.org/rgherdt/scheme-lsp-server lsp-server))
```

The command-line tool is available as a script `gambit/gambit-lsp-server.scm`,
which you can put in your `PATH`. Alternatively, you can compile it for better
performance (see next).

#### Improving performance through compilation

You may notice that `gambit-lsp-server` takes some seconds to start. This can
be annoying, since some LSP clients fire up the client for each .scm file
or project. You can improve performance by compiling the library. For this
you need to compile Gambit using it's current `master` branch, since the 4.9.4
release contains bugs regarding some R7RS forms. You can compile the library
and the executable by running the script `gambit/compile.sh`
```
$ cd ~/.gambit_userlib/codeberg.org/rgherdt/scheme-lsp-server/@/gambit
$ ./compile.sh
```

### Guile

#### Installation with Guix

The recommended way is to install this library using Guix. Unfortunately it
is still not available at the official Guix channels, but you can use the
provided channel `guix.scm`:

```
guix package -f guix.scm
```

The executable should now be available at `${HOME}/.guix-profile/bin/guile-lsp-server`.

#### Manual installation

Guile's version of the LSP server is packaged using automake. Make
sure Guile 3 **AND** its development libraries are installed. On Debian
you can install it using:
```
# apt install guile-3.0 guile-3.0-dev
```

`guile-lsp-server` has following dependencies:

- srfi-145
- srfi-180
- guile-irregex

You may use following script to install them automatically.

```
./guile/scripts/install-deps.sh --prefix=<PREFIX>
```

Finally, switch to the `./guile` folder and run:

```
./configure && make && sudo make install
```

*Note: Make sure the PREFIX (installation directory) is in your %load-path
 and %load-compiled-path. For example, under Linux, add the following to your
 `./bashrc` file*

```
export GUILE_LOAD_COMPILED_PATH=...:/usr/local/lib/guile/3.0/site-ccache
export GUILE_LOAD_PATH=...:/usr/local/share/guile/site/3.0

```
#### Let your LSP client install it

Currently both `emacs-lsp` and `vscode-scheme-lsp` will try to install the
LSP server in a local directory if none is found.

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

## <a name="user-content-cli"></a>Command-line tool

This programs also comes with command line tools to start the server. They are called
`chicken-lsp-server`, `guile-lsp-server` etc. All of them provide the same interface.
Run `guile-lsp-server --help` for more information.

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
<th scope="col" class="org-left">CHICKEN</th>
<th scope="col" class="org-left">Gambit</th>
<th scope="col" class="org-left">Guile</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Find signature</td>
<td class="org-left">X</td>
<td class="org-left">X</td>
<td class="org-left">X</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">Find documentation</td>
<td class="org-left">X</td>
<td class="org-left"></td>
<td class="org-left">X</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">Autocomplete identifier</td>
<td class="org-left">X</td>
<td class="org-left">X</td>
<td class="org-left">X</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">Jump to definition</td>
<td class="org-left">X</td>
<td class="org-left">X</td>
<td class="org-left">X</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">Diagnostics</td>
<td class="org-left">X</td>
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

### Diagnostics show too many errors/warnings

Currently diagnostics are computed by calling the implementation compiler.
When code is split in a library definition and an implementation file, we
have a problem that the LSP server is not able to provide correct information
when opening only the implementation file. I still didn't find an elegant way
to solve this. An workaround is that while opening a file, the LSP server
tries to find different files with the same base name but different extension
(.sld, .ss, .scm). If one of them contains a library definition, that file is
compiled instead, providing the correct information.

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
