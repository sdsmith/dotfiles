---
name: ast-grep
description: Learn how to use AST-Grep to analyze code abstract syntax trees and find patterns in C++ code.
---

# What is ast-grep?

ast-grep is an AST-based tool to search, lint, and rewrite code at massive scale.

* **Search**: As a command line tool, `ast-grep` can precisely search code based on AST, running through ten thousand files in sub seconds.
* **Lint**: You can use ast-grep as a linter with a flexible rule system.
* **Rewrite**: ast-grep provides API to traverse and manipulate syntax trees.

> Think of ast-grep as a hybrid of grep, eslint, and codemod.

## Supported Languages

ast-grep supports C and Cpp (among others). Cpp is a superset of C, so you can reuse Cpp rules with C code. The `languageGlobs` option in `sgconfig.yml` can force ast-grep to parse `.c` files as Cpp.

# Quick Start

## Pattern

`ast-grep` uses **patterns** to find similar code. Think of patterns like those in `grep`, but instead of text, they match AST nodes. We can write patterns like ordinary code, and it will match all code that has the same syntactical structure.

### Meta Variables

Use a **metavariable** to match any single AST node. Metavariables begin with `$` and are typically uppercase (e.g., `$VAR`).

The pattern must be enclosed in single quotes `'` to prevent the shell from interpreting the `$` sign.

```shell
ast-grep --pattern '$FUNC($$$ARGS)' --lang cpp src/
```

### Multi Meta Variable

Use `$$$` to match zero or more AST nodes, including function arguments, parameters, or statements. These variables can also be named, for example: `console.log($$$ARGS)`.

### Meta Variable Capturing

You can reuse same-name meta variables to find previously occurred AST nodes. For example, the pattern `$A == $A` will match `x == x` but not `x == y`.

### Non Capturing Match

Meta variables with name starting with underscore `_` will not be captured. Each occurrence of `$_FUNC` can match different content because they are not captured.

### Capture Unnamed Nodes

A meta variable pattern `$META` will capture named nodes by default. To capture unnamed nodes, use double dollar sign `$$VAR`.

## Language Specification for C++

For C++, use `--lang cpp` on the command line or `language: Cpp` in YAML rules. For C, use `--lang c` or `language: c`.

Note: in C/C++, single quotes are used for character literals, so string patterns in YAML rules should use double quotes. The rule pattern `'hello world'` will have a parsing error in C/Java/Rust because single quote is used for character literal.

# Rule Essentials

Pattern is handy for simple search, but rules are more expressive for complicated cases. Rules are like CSS selectors that compose together to filter AST nodes.

## A Minimal Example

```yaml
id: no-raw-pointer-cast
language: Cpp
rule:
  pattern: (int)$EXPR
```

* `id` is a unique short string for the rule.
* `language` is the programming language (e.g., `Cpp`, `c`).
* `rule` accepts a rule object and defines what code will be matched.

## Running Rules

### `ast-grep run -p 'PATTERN'`

Ad-hoc search from the command line:

```bash
ast-grep run -p 'dynamic_cast<$T>($EXPR)' --lang cpp src/
```

### `ast-grep scan --rule`

Run one rule from a file:

```bash
ast-grep scan --rule my-rule.yml src/
```

Caveat: `--rule` bypasses any `sgconfig.yml` — including its `languageGlobs`, so headers are not searched. Use `--filter` when you need config globs.

### `ast-grep scan --filter`

Run a subset of rules **from the project config** by id regex, keeping the config's `languageGlobs`:

```bash
ast-grep scan --config sgconfig.yml --filter my-rule src/
```

### `ast-grep scan --inline-rules`

Run a rule directly from the command line:

```bash
ast-grep scan --inline-rules '
id: my-rule
language: Cpp
rule:
  pattern: dynamic_cast<$T>($EXPR)
' src/
```

## Rule Object

Every rule field is optional but at least one must be present. A node matches if and only if it satisfies all fields.

```yaml
rule:
  # atomic rule
  pattern: 'search.pattern'
  kind: 'tree_sitter_node_kind'
  regex: 'rust|regex'
  # relational rule
  inside: { pattern: 'sub.rule' }
  has: { kind: 'sub_rule' }
  follows: { regex: 'can|use|any' }
  precedes: { kind: 'multi_keys', pattern: 'in.sub' }
  # composite rule
  all: [ {pattern: 'match.all'}, {kind: 'match_all'} ]
  any: [ {pattern: 'match.any'}, {kind: 'match_any'} ]
  not: { pattern: 'not.this' }
  matches: 'utility-rule'
```

Each key may appear only **once** per rule object — two `has:` (or two `inside:`) keys fail to parse with `duplicate field has`. Combine same-kind conditions with `all:`:

```yaml
rule:
  kind: try_statement
  all:
    - has: { field: body, kind: compound_statement }
    - has: { kind: catch_clause }
```

## Three Rule Categories

* **Atomic Rule**: the most basic rule that checks if AST nodes match.
* **Relational Rule**: rules that check if a node is surrounded by another node.
* **Composite Rule**: rules that combine sub-rules using logical operators.

# Atomic Rule

## `pattern`

Pattern will match one single syntax node. By default, a string `pattern` is parsed and matched as a whole.

### Pattern Object

When a string pattern is ambiguous or incomplete, use a pattern object with `context`, `selector`, and `strictness`:

```yaml
pattern:
  context: 'int x = $VAL;'
  selector: init_declarator
```

* `context` (required): surrounding code that resolves ambiguity.
* `selector` (optional): the sub-syntax node kind to match within the context.
* `strictness` (optional): `cst`, `smart` (default), `ast`, `relaxed`, or `signature`.

### `strictness` options (most strict to least):

* `cst`: All nodes in pattern and target must match. No node skipped.
* `smart`: All pattern nodes matched, but skips unnamed nodes in target. Default.
* `ast`: Only named AST nodes matched. All unnamed nodes skipped.
* `relaxed`: Named nodes matched. Comments and unnamed nodes ignored.
* `signature`: Only named nodes' kinds matched. Comments, unnamed nodes, and text ignored.

## `kind`

Use `kind` to match by tree-sitter node type (e.g., `if_statement`, `call_expression`, `function_definition`):

```yaml
rule:
  kind: function_definition
```

### ESQuery style `kind`

```yaml
rule:
  kind: call_expression > identifier
```

This matches an `identifier` node that is a child of `call_expression`.

## `regex`

Matches the AST node's text against a Rust regular expression:

```yaml
rule:
  regex: "(?i)assert"
```

Combine `regex` with other rules to ensure it's applied to the correct AST node.

ast-grep uses the Rust `regex` crate: **no lookahead/lookbehind** — `tassert(?!NoThrow)` fails with "Rule contains invalid regex matcher". Express exclusions with `not` instead. Match any callee starting with `tassert` except the `NoThrow` family:

```yaml
rule:
  kind: call_expression
  all:
    - has:
        field: function
        regex: '\btassert'
    - has:
        field: function
        not:
          regex: 'NoThrow'
```

## `nthChild`

Select nodes based on position among siblings (1-based, named nodes only):

```yaml
rule:
  kind: parameter_declaration
  nthChild: 2
```

# Relational Rules

* `inside`: the target node must be **inside** a node matching the sub rule.
* `has`: the target node must **have** a child node matching the sub rule.
* `follows`: the target node must **follow** a node matching the sub rule.
* `precedes`: the target node must **precede** a node matching the sub rule.

All relational rules accept a sub rule object and support `stopBy` and `field` options.

### `stopBy`

* `neighbor` (default): only match one level further.
* `end`: search until reaching the root/leaf/end of siblings.
* A rule object: search stops when the rule matches.

```yaml
rule:
  pattern: delete $PTR
  inside:
    kind: compound_statement
    stopBy: end
```

### `field`

Specify a node by its field name. **Caution**: field names are language-specific. This looks natural but never matches in Cpp — `function_definition` has no `name` field:

```yaml
rule:
  # WRONG for Cpp: silent no-match
  kind: function_definition
  has:
    field: name
    regex: '^test_'
```

In tree-sitter-cpp the function name is nested in the declarator:

```yaml
rule:
  kind: function_definition
  has:
    kind: function_declarator
    has:
      field: declarator   # the identifier
      regex: '^test_'
```

Verified tree-sitter-cpp fields (ast-grep 0.45.2):

| Node kind | Fields |
|-----------|--------|
| `call_expression` | `function`, `arguments` |
| `function_definition` | `type`, `declarator`, `body` (no `name`!) |
| `function_declarator` | `declarator` (the identifier), `parameters` |
| `try_statement` | `body` |
| `catch_clause` | `body` (the `catch (...)` parameter is a `parameter_list`) |

`--debug-query` prints node kinds but NOT field names, and there is no `ast-grep debug` subcommand. To discover a field name empirically, probe with `has: { field: <guess>, kind: <expected child kind> }` against a scratch file via `scan --inline-rules` — zero matches means the field name is wrong.

# Composite Rule

## `all`

Matches nodes satisfying all rules:

```yaml
rule:
  all:
    - pattern: $PTR->$FIELD
    - kind: member_expression
```

## `any`

Matches nodes satisfying any one rule:

```yaml
rule:
  any:
    - pattern: malloc($SIZE)
    - pattern: calloc($COUNT, $SIZE)
```

## `not`

Matches nodes that do not satisfy the rule:

```yaml
rule:
  pattern: $PTR
  not:
    pattern: this
```

## `matches`

References a utility rule by id (see Utility Rules below).

## `all` and `any` Refer to Rules, Not Nodes

`all` means a node should satisfy all rules. `any` means satisfy any one. A rule tests one node at a time, independently.

# Utility Rules

Reuse common rules via `utils`:

```yaml
utils:
  is-raw-pointer:
    any:
      - kind: pointer_declarator
      - pattern: $TYPE*
rule:
  matches: is-raw-pointer
```

## Local Utility Rules

Defined in the `utils` field of the config file. Keys are identifiers used in `matches`.

## Global Utility Rules

Defined in a separate file in a `utils/` directory, with `id` and `language` fields:

```yaml
# utils/is-raw-pointer.yml
id: is-raw-pointer
language: Cpp
rule:
  any:
    - kind: pointer_declarator
    - pattern: $TYPE*
```

## Recursive Rule Trick

```yaml
utils:
  is-pointer-type:
    any:
      - kind: pointer_declarator
      - has:
          matches: is-pointer-type
rule:
  matches: is-pointer-type
```

Rule cannot have a cyclic dependency in composite components, but self-referencing in relational components like `inside` or `has` is allowed.

# Rewrite Code

## Using `--rewrite`

```bash
ast-grep run --pattern 'NULL' --rewrite 'nullptr' --lang cpp src/
```

Use `--update-all` or `-U` to automatically accept changes. Use `--interactive` for interactive mode.

## Using `fix` in YAML Rule

```yaml
id: replace-null-with-nullptr
language: Cpp
rule:
  pattern: NULL
fix: nullptr
```

### Meta Variables in Rewrite

Meta variables in `fix` are replaced by whatever they matched in the pattern:

```yaml
id: use-static-cast
language: Cpp
rule:
  pattern: (int)$EXPR
fix: static_cast<int>($EXPR)
```

`fix` is a template string, not parsed by tree-sitter. Non-matched meta-variables are replaced by empty string.

### Expand the Matching Range

`fix` can accept an object with `template`, `expandStart`, and `expandEnd` to expand the fixing range beyond the matched node:

```yaml
fix:
  template: ''
  expandEnd: { regex: ';' }
```

# Transform

`transform` applies string operations to meta variables before using them in `fix`:

```yaml
transform:
  NEW_VAR:
    replace:
      source: $VAR_NAME
      replace: regex
      by: replacement
```

ast-grep 0.38.3+ supports string-style syntax:

```yaml
transform:
  NEW_VAR: replace($VAR_NAME, replace=regex, by=replacement)
  ANOTHER: substring($VAR_NAME, startChar=1, endChar=-1)
```

## Supported transformations

* `replace`: Regex replace text in a meta-variable. Supports capture groups `(?<NAME>.*)` referenced in `by`.
* `substring`: Cut leading/trailing characters.
* `convert`: Change string case (e.g., camelCase to snake_case).
* `rewrite`: Apply rewriter rules to a meta-variable AST.

## Multiple Sequential Transformations

Each transformation outputs a meta-variable usable as input to later transformations:

```yaml
transform:
  UPPER:
    convert:
      source: $NAME
      toCase: upperCase
fix: $UPPER
```

# Rewriter in Fix

`rewriters` apply rules to specific sub-nodes of a match:

```yaml
rewriters:
- id: fix-arg
  rule:
    pattern: $ARG
  fix: '/* fixed */ $ARG'
rule:
  pattern: func($$$ARGS)
transform:
  FIXED:
    rewrite:
      rewriters: [fix-arg]
      source: $$$ARGS
fix: func($FIXED)
```

Rewriter rules require `id`, `rule`, and `fix`. Multiple rewriters can be applied; first matching rewriter wins. Use `joinBy` to specify an alternative joiner.

# Project Configuration

## `sgconfig.yml`

```yaml
ruleDirs:
  - rules
languageGlobs:
  Cpp:                # map: language -> glob list (NOT a list of {language, globs})
    - '*.cpp'         # defaults are REPLACED — re-list the standard extensions...
    - '*.cc'
    - '*.cxx'
    - '*.c++'
    - '*.hpp'
    - '*.hh'
    - '*.hxx'
    - '*.ipp'
    - '*.h'           # ...alongside the ones you are adding
    - '*.inc'
```

All YAML files under `rules/` are treated as rule files, so every rule in every `ruleDirs` entry runs on `ast-grep scan` — matches from multiple rules interleave in the output (isolate one with `--filter` or by `ruleId` in JSON output). `ast-grep scan` requires `sgconfig.yml` in the project root (or use `--config path/to/config.yml`).

### `languageGlobs` gotchas

Verified on ast-grep 0.45.2:

* By default, `language: Cpp` does **not** parse `.h` / `.hpp` / `.inc` files — header-only code is silently skipped unless you add the extensions.
* Assigning `languageGlobs` for a language **replaces** its default extension list. Listing only `*.h` makes `.cpp` files stop being scanned. Always re-list the defaults alongside your additions.
* The schema is a map of language name to glob list; a list of `{language, globs}` mappings fails to parse. The schema has changed across ast-grep versions — check `ast-grep --version` when a known-good config stops parsing.

## Project Discovery

`ast-grep scan` walks up the directory tree **from the CWD** to find `sgconfig.yml` (like git finds `.git`). The positional `PATHS` arguments are the scan roots and may point anywhere:

```bash
ast-grep scan /abs/path/to/repo/src
```

You can also skip discovery with `--config path/to/sgconfig.yml` from any directory (paths inside the config resolve relative to the config file).

# Lint Rule

A lint rule file extends the rule object with linter fields:

```yaml
id: no-malloc
language: Cpp
rule:
  pattern: malloc($SIZE)
message: Avoid malloc, use new or smart pointers
severity: warning
note: |
  Using malloc in C++ can lead to memory leaks if not paired with free.
  Prefer std::make_unique or std::make_shared for automatic lifetime management.
```

## `constraints`

Constrain what kind of meta variables should match:

```yaml
rule:
  pattern: $FUNC($$$ARGS)
constraints:
  FUNC:
    regex: '^test_'
```

Constraints are applied after `rule` matching and do not work inside `not`.

## `files`/`ignores`

```yaml
files:
  - "src/**"
  - "include/**"
ignores:
  - "third_party/**"
```

Paths are relative to the project root. Do not prefix with `./`.

# Test Your Rule

Test files use `valid` and `invalid` cases:

```yaml
id: no-malloc
valid:
  - 'auto p = std::make_unique<int>(42);'
  - 'int* p = new int;'
invalid:
  - 'void* p = malloc(10);'
```

Run with:

```bash
ast-grep test --skip-snapshot-tests
```

Without `--skip-snapshot-tests`, use `--update-all` or `-U` to generate snapshots, then run `ast-grep test` again.

## Test Setup

In `sgconfig.yml`:

```yaml
ruleDirs:
  - rules
testConfigs:
  - testDir: rule-tests
```

## Validate on a Synthetic File First

Before scanning a large tree, run the rule against a small scratch file containing labeled positive AND negative cases, and check the matched line numbers:

```bash
ast-grep scan --rule my-rule.yml --json=compact scratch/ | \
    python3 -c "import json,sys; print([m['range']['start']['line']+1 for m in json.load(sys.stdin)])"
```

The negative cases are what make the rule trustworthy — for a try/catch rule: a non-empty catch, the call appearing only inside the catch, a rethrow, a lone `;` in the catch body. Note that a scratch `.h` file will NOT be scanned by `--rule` (it bypasses config `languageGlobs`); use a `.cpp` scratch file or run with `--config`.

## Keep a Per-Query Test Corpus (mytools convention)

For rules maintained in `~/mytools/ast-grep/queries/`, do not throw the
synthetic validation file away — save it next to the rule and extend it
whenever the rule changes. Every query owns a folder named after it:

```
~/mytools/ast-grep/
  sgconfig.yml               ruleDirs: [queries], testConfigs: [rule-tests]
  check.sh                   one-command verification of all of the below
  queries/
    <rule-id>.yml            the rule
    <rule-id>/               whole-file .cpp corpus; the line where a match
                             starts is end-marked `// YES` (rule must flag)
                             or `// no` (must not), optional `: reason`
  rule-tests/
    <rule-id>/test.yml       native valid/invalid cases for `ast-grep test`
    __snapshots__/           generated by `ast-grep test -U`
```

When creating or updating a query, build/extend both the corpus and the
native test file, then verify with:

```bash
~/mytools/ast-grep/check.sh
```

It asserts, and fails loudly on:

* `ast-grep test` passes — catches rule regressions (failing cases), a
  stray `*.yml` under `queries/` (`missing field language`), stale
  snapshots, and a broken or missing `testConfigs` dir;
* every rule's actual match lines over its own corpus equal its `// YES`
  lines exactly — catches both rule regressions and stale corpus
  annotations, with a precise diff;
* a config-driven scan over `queries/` loads every rule and produces the
  combined match total.

**Never put `*.yml` under `queries/<rule-id>/`**: `ruleDirs` loads every
`*.yml` under `queries/` *recursively* as a rule, so a native test file
(which has no `language` field) breaks both `ast-grep scan` and
`ast-grep test` with `missing field language`. Corpus folders hold `.cpp`
only; native `test.yml` files go in `rule-tests/<rule-id>/`.

# CLI Overview

* `ast-grep run -p 'PATTERN'` — ad-hoc search/rewrite
* `ast-grep scan` — scan codebase with project rules
* `ast-grep scan -r rule.yml` — run one rule
* `ast-grep scan --filter <regex>` — run rules with matching ids from the config
* `ast-grep scan --inline-rules '...'` — run inline rule
* `ast-grep test` — test rules
* `ast-grep new` — create project scaffolding
* `ast-grep run --debug-query` — inspect tree-sitter AST of a pattern

## Useful Flags

* `--lang cpp` / `-l cpp` — specify language
* `--config <path>` — use an sgconfig.yml anywhere, no cd required
* `--filter <regex>` — run only rules whose ids match, keeping the config's `languageGlobs` (`--rule` bypasses both)
* `--interactive` / `-i` — interactive rewrites (both `run` and `scan`)
* `--update-all` / `-U` — auto-accept rewrites (both `run` and `scan`; on `scan` it applies every rule's `fix`)
* `--json=pretty|compact|stream` — JSON output; `range.start.line` is 0-based, and each match carries its `ruleId` — filter on it when several rules ran
* `--files-with-matches` — print only paths with hits
* `--color never` — disable ANSI color

## `--debug-query`

Outputs the tree-sitter AST of the query pattern. Useful for understanding how a pattern is parsed:

```bash
ast-grep run -p 'dynamic_cast<$T>($EXPR)' --lang cpp --debug-query
```

# C/C++ Catalog Examples

## Match Function Call in C

tree-sitter-c parses `test(a)` as `macro_type_specifier`, not `call_expression`. Use a contextual pattern:

```yaml
id: match-function-call
language: c
rule:
  pattern:
    context: $M($$$);
    selector: call_expression
```

```c
#define test(x) (2*x)
int a = test(2);
int main(){
    int b = test(2);
}
```

Note: tree-sitter-c parses code fragments differently:
* `test(a)` → `macro_type_specifier`
* `test(a);` → `expression_statement -> call_expression`
* `int b = test(a)` → `declaration -> init_declarator -> call_expression`

## Rewrite Method to Function Call

Convert struct method calls to function calls with the struct pointer as first argument:

```yaml
id: method_receiver
language: c
rule:
  pattern: $R.$METHOD($$$ARGS)
transform:
  MAYBE_COMMA:
    replace:
      source: $$$ARGS
      replace: '^.+'
      by: ', '
fix:
  $METHOD(&$R$MAYBE_COMMA$$$ARGS)
```

```c
void test_func() {
    some_struct->field.method();
    some_struct->field.other_method(1, 2, 3);
}
```

Becomes:

```c
void test_func() {
    method(&some_struct->field);
    other_method(&some_struct->field, 1, 2, 3);
}
```

## Rewrite Check to Yoda Condition

```yaml
id: may-the-force-be-with-you
language: c
rule:
  pattern: $A == $B
  inside:
    kind: parenthesized_expression
    inside: {kind: if_statement}
constraints:
  B: { kind: number_literal }
fix: $B == $A
```

```c
if (myNumber == 42) { /* ... */}  // matches
if (notMatch == another) { /* ... */}  // does not match
```

## Fix Format String Vulnerability (Cpp)

```yaml
id: fix-format-security-error
language: Cpp
rule:
  pattern: $PRINTF($S, $VAR)
constraints:
  PRINTF:
    { regex: "^sprintf|fprintf$" }
  VAR:
    not:
      any:
      - { kind: string_literal }
      - { kind: concatenated_string }
fix: $PRINTF($S, "%s", $VAR)
```

```cpp
// Error
fprintf(stderr, out);
sprintf(&buffer[2], obj->Text);
// OK
fprintf(stderr, "%s", out);
sprintf(&buffer[2], "%s", obj->Text);
```

## Find Struct Inheritance (Cpp)

A pattern like `struct $S: $INHERITS` won't work because it doesn't have correct AST structure. Spell out the full syntax:

```shell
ast-grep --lang cpp --pattern 'struct $SOMETHING: $INHERITS { $$$BODY; }'
```

```cpp
struct Bar: Baz {
  int a, b;
}
```

## Match an Empty Block (Cpp)

To match a `compound_statement` containing no statements — an empty function or catch body — use the pattern `{ }` with `relaxed` strictness, which tolerates comment-only bodies:

```yaml
rule:
  kind: compound_statement
  pattern:
    context: '{ }'
    strictness: relaxed
```

Matches `{}`, `{ }`, and `{ /* intentionally empty */ }`; does not match `{ do_work(); }` or `{ ; }`. To treat comment-only bodies as non-empty, match the node text instead: `regex: '^\{\s*\}$'`.
