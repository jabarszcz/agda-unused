# agda-unused

`agda-unused` checks for unused code in an Agda project, including:

- variables
- definitions
- postulates
- data & record types
- `import` statements
- `open` statements
- pattern synonyms

`agda-unused` takes a filepath representing an Agda code file and checks for
unused code in that file and its dependencies. By default, `agda-unused` does
not check public items that could be imported elsewhere. But with the `--global`
flag, `agda-unused` treats the given file as a description of the public
interface of the project, and additionally checks for unused files and unused
public items in dependencies. (See below for more on `--global`.)

Supported Agda versions: `>= 2.8.0 && < 2.8.1`

## Example

File `~/Test.agda`:

```
module Test where

open import Agda.Builtin.Bool
  using (Bool; false; true)
open import Agda.Builtin.Unit

_∧_
  : Bool
  → Bool
  → Bool
false ∧ x
  = false
_ ∧ y
  = y
```

Command:

```
$ agda-unused Test.agda
```

Output:

```
Test.agda:4.23-27: unused imported item ‘true’
Test.agda:5.1-30: unused import ‘Agda.Builtin.Unit’
Test.agda:11.9-10: unused variable ‘x’
```

## Usage

```
agda-unused - check for unused code in an Agda project

Usage: agda-unused [FILE] [(-g|--global) | --local]
                   [(--only CATEGORY) | (--all-but CATEGORY) | --all]
                   [-j|--json] [--config FILE | --no-config]

  Check for unused code in FILE (or use 'file' from config)

Available options:
  -h,--help                Show this help text
  -g,--global              Treat FILE as the project's complete public interface
  --local                  Only report private unused code (default)
  --only CATEGORY          Only report these categories (repeatable)
  --all-but CATEGORY       Report all categories but these (repeatable)
  --all                    Report all categories (override config filter)
  -i,--include-path DIR    Look for imports in DIR
  -l,--library LIB         Use library LIB
  --library-file FILE      Use FILE instead of the standard libraries file
  --no-libraries           Don't use any library files
  --no-default-libraries   Don't use default libraries
  -j,--json                Format output as JSON
  --config FILE            Use this config file instead of auto-discovery
  --no-config              Don't load any config file

Categories: data, definitions, imports, import-items, modules, module-items,
opens, open-items, pattern-synonyms, postulates, records, record-constructors,
variables, mutual
```

## Configuration

`agda-unused` looks for a `.agda-unused.yaml` configuration file starting from
the checked file's directory and searching upward. Use `--config FILE` to
specify one explicitly, or `--no-config` to skip config loading.

Example `.agda-unused.yaml`:

```yaml
file: Everything.agda  # default file when no FILE argument
global: true           # equivalent to --global
all-but:               # mutually exclusive with 'only'
  - variables
```

Agda library options can also be set in the config: `include` (list of
include paths), `libraries` (list of library names), `library-file` (override
`~/.agda/libraries`), `use-libraries` and `use-default-libraries` (booleans,
both default to `true`). These are rarely needed since Agda resolves libraries
from `.agda-lib` files automatically.

CLI flags take precedence over config file settings. For list fields
(`include`, `libraries`), CLI values replace (not append) config values.

## Global

If the `--global` flag is given, all declarations in the given file must be
imports. The set of imported items is treated as the public interface of the
project; these items will not be marked unused. The public items in dependencies
of the given module may be marked unused, unlike the default behavior. We also
check for unused files.

To perform a global check on an Agda project, first create a file that imports
exactly the intended public interface of your project. For example:

File `Everything.agda`:

```
module Everything where

import A
  using (f)
import B
  hiding (g)
import C
```

Command:

```
$ agda-unused Everything.agda --global
```

## JSON

If the `--json` flag is given, the output is a JSON object with two fields:

- `type`: One of `"none"`, `"unused"`, `"error"`.
- `message`: A string, the same as the usual output of `agda-unused`.

The `"none"` type indicates that there is no unused code.

## Approach

We make a single pass through the given Agda module and its dependencies:

- When a new item (variable, definition, etc.) appears, we mark it unused.
- When an existing item appears, we mark it used.

This means, for example, if we have three definitions (say `f`, `g`, `h`), each
depending on the previous one, then `f` and `g` are considered used, while `h`
is considered unused. If we remove `h` and run `agda-unused` again, it will now
report that `g` is unused. This behavior is different from Haskell's built-in
tool, which would report that all three identifiers are unused on the first run.

## Limitations

We work with Agda's concrete syntax. This is a necessary choice, since Agda's
abstract syntax doesn't distinguish between qualified and (opened) unqualified
names, which would make it impossible to determine whether certain `open`
statements are unused. However, using concrete syntax comes with several
drawbacks:

- We do not parse mixfix operators; if the parts of a mixfix operator are used
  in order in an expression, then we mark the mixfix operator as used.
- We do not distinguish between overloaded constructors; if a constructor is
  used, then we mark all constructors in scope with the same name as used.

Additionally, we currently do not support the following Agda features:

- [unquoting declarations](https://agda.readthedocs.io/en/v2.8.0/language/reflection.html#id3)
- [lone constructors](https://agda.readthedocs.io/en/v2.8.0/language/mutual-recursion.html#interleaved-mutual-blocks)

`agda-unused` will produce an error if your code uses these language features.

Instance declarations are always treated as used, since determining which
instances are selected requires type-checking.  Imports that only provide
instances can be suppressed with `-- agda-unused: instances` (see below).

When `open import M as N` or `open module N = M` is used, qualified access
(`N.foo`) and unqualified access (`foo`) are tracked together.  This means that
if only qualified access is used, the redundant `open` is not reported.

## Suppression Comments

You can suppress specific reports with inline comments:

- `-- agda-unused: ignore` — suppress all reports on the same line
- `-- agda-unused: instances` — suppress whole-import reports on the same line
  (useful for imports that provide instances)

Examples:

```agda
-- Suppress a report for a definition used only via a REWRITE pragma,
-- which agda-unused doesn't track:
+-assoc : ...       -- agda-unused: ignore
{-# REWRITE +-assoc #-}

-- Keep the import (it provides instances used implicitly by the type
-- checker):
import Data.Nat.Properties  -- agda-unused: instances
```

The `instances` marker only suppresses the whole-import report. If the import
has a `using` list with individually unused items, those are still reported.
This is useful since `agda-unused` does not perform type checking and cannot
determine whether instances from an import are actually used.
