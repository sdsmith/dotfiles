---
name: stewart-code-review
description: Review a code change and report findings — never edits. Scales from a routine single-pass review (scope discipline, Rust idioms, project conventions, standard correctness concerns) to a high-rigor two-stage subagent review (correctness then style, reported verbatim) for invariant-heavy or safety-sensitive changes. Use when the user asks for a code review, says "review this change/diff/PR", wants a pre-commit or pre-PR pass, or is reviewing a security-sensitive service, fallback/recovery path, wire-protocol/buffer code, or any diff whose thesis is "we always retain enough state to do X safely".
---

# Stewart Code Review

A reviewer-style pass over a code change. Output is a structured report — **never edit, never auto-fix**. The user decides what to act on.

This skill has two depths. Pick one based on the change:

- **Lite (single pass):** the default for routine diffs. One reviewer, one report, using the checklist in *Lite review*.
- **Full (two subagent stages):** for changes where the hard part is *proving an invariant holds*, not spotting a typo. Correctness and style are split into separate subagents whose findings are relayed **verbatim**. Use the *Full review* section.

> Core principle (matters most in Full, true in both): the bugs you miss in an invariant-heavy change are the ones you *reasoned away*, not the ones you failed to see. Every "this is fine because X" is a liability. Treat your own reassurances as the highest-risk statements in the review.

## Choosing depth

Use **Full** when any of these hold; otherwise use **Lite**:

- Security-sensitive service, especially "fail open" / "fail safe" / fallback / recovery paths.
- Wire-protocol, parser, or buffer-management code where the claim is "we retain / consume exactly the right bytes".
- State machines / phase transitions where "this state can't be re-entered" or "this transition always fires" is load-bearing.
- The change's one-line thesis is a *negative or an always*: "we never lose the in-flight message", "we can't double-enter freeflow", "this only triggers on recoverable errors".
- A large diff (roughly >500 lines) touching a critical path, where a shallow pass will miss the interesting bugs.

If unsure, ask the user, or start Lite and escalate to Full if the change turns out to hinge on an invariant.

## Identifying the diff (both depths)

In order of preference:

- User named a ref / range / PR → use that.
- Working tree has unstaged changes → `git diff` (and `git diff --staged` if both).
- Otherwise the branch vs trunk → `git diff $(git merge-base HEAD origin/master)...HEAD` (or `main`).
- If multiple plausible interpretations, ask which one before reading.

Read the whole diff once to get the shape before judging any piece. For Full, also save the diff to a temp file so subagents can read it without re-deriving it.

---

# Lite review (single pass)

Run the checklist below in order — categories are ordered by what causes the most pain — then write the report in *Output format*. One section per category; include only categories that produced findings.

You may run the build/tests to settle a specific doubt (see *Verify, don't only reason*), but do not spam `cargo check`/`fmt` over the whole diff as a matter of course. If you spot something that needs verification and can't cheaply check it, call it out as "looks like it might not compile / might not hold" rather than asserting it.

## Checklist

### 1. Scope creep — the #1 thing to catch

The user has been burned repeatedly by changes that extend past what was asked. Flag every instance:

- [ ] **Visibility changes.** `pub` / `pub(crate)` added to items the task didn't require. Especially methods quietly promoted to public.
- [ ] **Unconditional `pub use` in `mod.rs` for test-only types.** If a type is re-exported in a `pub use` block but is only consumed by tests in other modules, flag it. The correct fix is a `#[cfg(test)] pub use` entry (see `src/config/mod.rs` for the existing pattern with `ValidatedConfig` and `FaultActionMapping`). Do *not* suggest importing directly from a private submodule (e.g. `crate::config::validated::TypeName`).
- [ ] **Signature changes** to existing functions when the task was an addition. New parameters, changed return types, new generic bounds.
- [ ] **Removed or rewritten `TODO`/`FIXME`/`XXX` comments.** Treat these as load-bearing unless the task explicitly addressed them.
- [ ] **Edits to files outside the stated task surface.** If the task was "add a test for X", touching `mod.rs` or `lib.rs` is suspect.
- [ ] **"Fixes" to pre-existing issues** that weren't part of the request — unrelated clippy lints squashed, unrelated compile errors patched, dead-code warnings silenced. These belong in a follow-up, not this diff.
- [ ] **Drive-by renames / formatting churn** in untouched code blocks.
- [ ] **New dependencies** in `Cargo.toml` for a task that doesn't obviously need them.

For each finding, name the file and line, and say *what the in-scope change actually was* so the user can decide whether to keep or revert.

### 2. Rust idioms (for any Rust diff)

- [ ] **`panic!` / `unwrap` / `expect` / `assert!` in non-test code.** Especially in new public APIs or state-transition functions — these should return `Result`. (The `connection_phase_to_*` family is the in-repo model.)
- [ ] **Runtime checks where the type system could enforce.** `match` on stringly-typed config, runtime "is this variant in the allowed set" checks, defaults that aren't validated at compile time. Prefer sealed traits, `const fn` validation of associated consts, exhaustive enum matches, `#[non_exhaustive]`.
- [ ] **Missing exhaustiveness.** `match` with `_ => ...` on an enum the diff might extend later. Consider whether the wildcard should be removed so the compiler nags on additions.
- [ ] **Public traits without sealing** when they're not meant to be implemented downstream. Add a private supertrait or sealed marker.
- [ ] **`anyhow::Context` imported alongside a custom `*Context` trait** (e.g. `ConnectionErrorContext`). This has caused method-resolution ambiguity before — confirm the right one is dispatched.
- [ ] **Borrow-checker workarounds that smell.** `RefCell`/`Rc` reaches, `unsafe`, `clone()` on hot paths to silence borrowck. Often there's a better factoring.
- [ ] **`Option` extraction via `if let Some` chains** where `Option::map` / closure helpers would read cleaner.
- [ ] **Lifetimes/generics introduced for one caller.** Check if a simpler signature works.
- [ ] **Make it easy to keep disparate parts of the code in sync.** If adding a new enum or field means other parts of the code need updating, try to enforce that through design to get compiler errors as reminders (e.g. destructuring, full match cases, `const _` assertions).

### 3. Tests

- [ ] **Behavior, not implementation.** Tests should exercise public APIs and survive refactors. Flag tests that assert on private state or implementation shape.
- [ ] **Real code, not a model.** Beware a test that reconstructs the expected result with a test-local helper mimicking production logic — that verifies the model, not the shipped path. Prefer at least one test that runs the production path and asserts the observable output.
- [ ] **Each new public behavior has a test.** No silent feature additions.
- [ ] **Negative cases** for new validation / state-transition logic — not just the happy path.
- [ ] **Global-state pollution.** Prometheus metrics, statics, `lazy_static` — has caused flaky leak-across-tests before. Confirm new tests reset or use isolated registries.
- [ ] **No `#[ignore]` / commented-out tests** sneaking through.
- [ ] **Test names describe behavior** (`returns_err_when_preauth_phase_invalid`), not mechanism (`test_transition_function_2`).

### 4. Error handling & messages

- [ ] **Fault / error variants render in CamelCase** when the rest of the codebase does. Check `Display` impls and `assert!(msg.contains("..."))` matchers — snake_case mismatches have bitten before.
- [ ] **`?` propagation preserves context.** New error paths should add `.context("...")` (or the project's equivalent) when the bare error would be ambiguous at the caller.
- [ ] **Source chains aren't lost.** Replacing an error with a stringified message drops the underlying cause.
- [ ] **Single-handling discipline.** For ownership-based errors (`ConnectionError`), confirm the variant is consumed exactly once on the handle path.
- [ ] **Config surface matches code.** For every configurable flag/field the change advertises (yaml, docs, README), confirm the code actually consults it. A flag accepted by config parsing but never read on the handling path is a silent no-op bug.

### 5. Standard reviewer concerns

- [ ] **Correctness:** Does the code do what the diff message / surrounding context says it does? Off-by-one, wrong operator, swapped arguments, inverted condition.
- [ ] **Naming:** Does each new name say what the thing is/does without reading the body? Watch for `data`, `info`, `helper`, `manager` — usually means the role hasn't been named. Check names against the *family* of sibling names, not just per-item plausibility.
- [ ] **Dead code / unused imports / unused variables** introduced by the change.
- [ ] **Debug artifacts:** `println!`, `dbg!`, `eprintln!`, commented-out code, leftover scratch comments.
- [ ] **Comments earn their keep.** A comment should explain *why*, not *what*. Restating the code, "added for X", or "used by Y" are noise. Comments must not hardcode drift-prone facts (a value, count, default, magic number, log ID that also lives in code) — express those in code or a rustdoc intra-doc link (`[`Type`]`, `[`Self::method`]`) that survives renames.
- [ ] **Magic numbers / strings** introduced without a named constant.
- [ ] **Concurrency:** Anything new behind `Arc`/`Mutex`/`RwLock` — is the lock scope tight, is there an obvious deadlock or lock-order hazard, are we holding a lock across `.await`?
- [ ] **Logging level appropriate** (no `info!` for hot-path internal events, no `error!` for expected conditions).

### 6. Build hygiene

- [ ] **Long `cargo` output piped to a log file** when reproducing failures — repo convention is `2>&1 | tee /tmp/x.log | tail -30`, not raw streaming. Only relevant if the diff touches scripts/docs that show example commands.

---

# Full review (two subagent stages)

For invariant-heavy / safety-sensitive changes. Ordinary review is good at pattern-spotting missing/mismatched things and bad at proving invariants and tracking state — this depth forces the second discipline.

## Process

Run two stages. Each stage is a **separate subagent** with a fresh context and a focused brief. Collect each subagent's report and **relay both verbatim** to the user — do not merge, summarize, re-prioritize, or drop findings. The user asked for the reviewers' findings, not your digest of them.

1. **Establish the diff and the thesis.**
   - Find the range (see *Identifying the diff*).
   - Write down, in one sentence, the change's *thesis* — the invariant it claims to uphold. Pass this to both subagents so they review against intent, not just against "is this reasonable code".
   - Save the full diff to a temp file so subagents can read it without re-deriving it.

2. **Stage 1 — Correctness subagent.** Brief it with the *Correctness checklist* and the *Epistemic rules*. It must read the *actual source files and the call graph*, not just the diff. It returns a severity-grouped report plus an explicit verdict on whether the thesis holds.

3. **Stage 2 — Style subagent.** Brief it to first *derive the codebase's conventions* from unchanged neighbouring code, then check the diff against them, using the *Style checklist*. Correctness is out of its lane.

4. **Report both stages verbatim**, clearly separated. Then, and only then, you may add a short "biggest gaps" note of your own if the user wants prioritization — but never in place of the raw findings.

## Epistemic rules (enforce in the correctness stage)

These are non-negotiable and are what make the Full depth worth its cost:

1. **Never certify a negative without proof.** Any finding of the form "X can't happen because Y" may go in a *verified-correct* section **only** if backed by one of: a test that exercises it, a type-level guarantee (exhaustive match, sealed type, ownership), or a `const`/compile-time assert. Otherwise it is demoted to a finding: *"I found no explicit guard against X; confirm this is intended."*
   - Concrete trap from the field: "no double-entry, gated by the phase no longer being PreAuth" was asserted as correct — the reviewer actually wanted an explicit drop-and-don't-retry guard. Reasoning that a re-entry is unreachable is exactly the claim to distrust.

2. **"Gated by an invariant" ≠ "explicitly guarded".** When the safety of a path depends on some *other* code having set a phase/flag/length correctly, flag the absence of a local guard even if you believe the upstream invariant holds. Defence in depth is the reviewer's default recommendation for safety-critical re-entrancy.

3. **Trace state, not just control flow.** For any change whose thesis is about retained/consumed data, do a dedicated pass that, *at every read and every flush site*, writes down: what is in the buffer, who owns it, and whether it's assumed empty/flushed. This is the pass that catches:
   - reads that should offset from the current buffer length but don't (buffer reuse with stale contents),
   - two code paths that read the same thing differently (e.g. a checked vs unchecked read helper) with nothing enforcing which is used,
   - bytes that are mutated in place before being forwarded (checksum stripped, header consumed) so "the original message" isn't what's actually sent.

   Tracing control flow alone (when does fallback fire) will miss all of these; you must trace *where the bytes are*.

4. **Config surface must match code.** For every configurable flag/field the change advertises (yaml, docs, README), confirm the code actually consults it. A flag that is accepted by config parsing but never read on the handling path is a silent no-op bug.

5. **Distinguish upstream vs downstream / self vs peer.** In a proxy or any two-sided component, an error classifier that can't tell which side failed will mislabel and mis-alert. Check that "peer disconnect" logic can't swallow a *self/upstream* failure, and that it inspects the whole error chain if the codebase wraps errors (but verify the wrapping semantics — e.g. `anyhow::Context` preserves downcasting, so "it's wrapped therefore hidden" is not automatically true).

## Correctness checklist (brief for Stage 1 subagent)

- [ ] **Trigger conditions are exactly right.** Both directions: fires on everything it should, fires on *nothing* it shouldn't. Enumerate the error/state taxonomy and check each.
- [ ] **The thesis holds end-to-end, tested by the real code — not by a model.** Beware tests that reconstruct the expected result with a *test-local helper* mimicking production logic; that verifies the model, not the shipped path. Demand at least one test that runs the production path and asserts the observable output byte-for-byte.
- [ ] **State-machine completeness.** Every state handled; no unreachable transition assumed reachable and vice versa; re-entry explicitly guarded (rules 1 & 2).
- [ ] **Data/buffer state trace** (rule 3) at every read/flush.
- [ ] **Config/flag surface consulted** (rule 4).
- [ ] **Error classification by side/recoverability** (rule 5).
- [ ] **Ownership / async cancellation.** Moves, early drops, use-after-move, borrows held across `.await`, cancellation leaving half-updated state.
- [ ] **Safety/security of the failure mode.** For "fail open", ask specifically: does failing open bypass a control that was supposed to *block/terminate*? Is there any error where open is the unsafe direction?
- [ ] **Ordering of checks with side effects.** e.g. emitting an "entered mode X" alert *before* the operation that could fail to actually enter X — leaves observers lied to.
- [ ] **Panics on the failure path.** `unwrap`/`expect`/`panic!` reachable during recovery.

The subagent must read the source (follow the call graph), obey the epistemic rules, not edit, and return findings grouped by severity plus a plain verdict on the thesis. It must place claims in *verified-correct* only when rule 1 is satisfied.

## Style checklist (brief for Stage 2 subagent)

Style review here means **consistency and idiom**, and its most valuable output is usually a *theme*, not a pile of nits.

- [ ] **Derive conventions first.** Before judging, build an explicit inventory from unchanged neighbouring code: enum/type naming family, transition/helper function naming (e.g. `transition_to_*`, `reset`), logging-ID conventions and suffixes, test attribute order, exhaustiveness-assert patterns, module layout. Then check the diff against that inventory. This is what turns "one odd name" into "these five names don't fit the family" — the theme is the finding.
- [ ] **Naming consistency across the whole change**, not just per-item plausibility. A name can be locally fine and still break the module's vocabulary.
- [ ] **Idiomatic Rust, strictly.** Iterator combinators where clearer, `?`, `match` vs `if let`, avoid needless clones/allocs, `impl Trait`, newtypes over stringly-typed, enums over booleans, `#[must_use]`, derives. Only genuinely-more-idiomatic suggestions; invent no house style.
- [ ] **Comments must not hardcode drift-prone facts.** Any comment restating a value, count, default, magic number, or log ID that also lives in code → flag; the fact belongs in code or a rustdoc reference.
- [ ] **Prefer rustdoc intra-doc links over bare names.** `[`Type`]` / `[`Self::method`]` survive renames; bare backtick names don't. Flag bare references that should be links.
- [ ] **Ordinary hygiene:** dead code, unused imports (check for more even if one was already removed), commented-out code, TODOs, gratuitous reformatting churn unrelated to the change, missing/inconsistent rustdoc on public items, anything rustfmt would rewrite.

The subagent must not report logic bugs (that's Stage 1) and must cite, for each consistency finding, the existing pattern the new code should match.

## Beyond the two stages — flag, don't silently drop

The two-stage split (correctness, style) can drop a whole class of valuable review: **design / product / stakeholder questions** — "should this flag even apply here?", "does another team need to be coordinated for these new labels or alert IDs?", "should there be a metric/counter for this?". These aren't bugs and aren't style, so both agents will skip them. Within the correctness stage, allow surfacing config-surface mismatches and new-observability-needs-a-stakeholder notes under an *Info* heading, and tell the user explicitly if a dedicated design-review stage would add value. Don't pretend two stages cover everything.

---

# Verify, don't only reason

The single biggest lesson from past reviews: **static reasoning produced false "verified correct" claims.** Where the environment allows it, close the loop:

- If the code can be checked out, run the build / `cargo check` and run the relevant tests. Read what the tests *assert as executed behavior*, not what their names or comments claim.
- An external reviewer (or an AI reviewer like Copilot) confidently claiming a compile error, or you confidently claiming an invariant holds — both are structural claims that a short mechanical check can settle. Settle them.
- If you genuinely cannot build/run (read-only mirror, no toolchain), say so explicitly in the report and downgrade every correctness claim to "believed, unverified".

If the repo forbids something (e.g. this repo says never clear the Bazel cache), honour it. Verification means using the sanctioned build/test commands, not inventing new ones. In Lite mode keep verification targeted (settle a specific doubt); don't turn every review into a full build/test cycle.

---

# Output format

## Lite

Markdown report. Skip empty categories. For each finding: file:line reference, one-sentence description, and what to consider doing — but **do not write the fix**.

```
## Code review

**Scope (X findings)**
- [src/connection/context.rs:142] `pub fn` added on `preauth_phase_mut`; task description only asked for a test. Consider reverting to `pub(crate)`.
- [src/fault/action.rs:88] TODO removed without addressing it. Restore unless it's truly resolved.

**Rust idioms (X findings)**
- [src/proxy.rs:301] New `unwrap()` on `Result<Stream>` — prefer `?` returning `ConnectionError`.
- [src/fault/registry.rs:55] Runtime `iter().any(...)` validating that `DEFAULT_ACTION` is in `VALID_ACTIONS`; compile-time `const _: () = assert!(...)` would catch this at build time. (See existing pattern in `fault/sealed.rs`.)

**Tests (X findings)**
- [tests/fault_config.rs] `validates_default_action` only covers the happy case; add a negative-case test where `DEFAULT_ACTION` is not in `VALID_ACTIONS`.

**Error handling (X findings)**
- [src/connection/error.rs:23] New `Display` impl renders `connection_refused`; rest of codebase uses CamelCase (`ConnectionRefused`). Existing assertion in `tests/error_format.rs:18` will fail.

**Standard (X findings)**
- [src/proxy.rs:412] `let manager = ...` — what does it manage? Suggest `connection_pool_handle` or similar.
- [src/fault/dispatch.rs:77] `dbg!` left in.

**Summary:** N findings, M of which are scope creep. The in-scope change (`<one-line summary>`) looks correct on its own merits.
```

Keep the report tight. One sentence per finding. If a finding is uncertain (e.g. "this *might* be a borrow-checker hazard"), say so explicitly — don't pad with hedges.

## Full

Relay each subagent's report verbatim, in order, clearly separated:

```
# Stage 1 — Correctness
<the correctness subagent's report, unedited>

# Stage 2 — Style
<the style subagent's report, unedited>
```

Only if the user asks for prioritization, append:

```
## Biggest gaps (reviewer's synthesis)
- ...
```

Never let the synthesis replace the verbatim findings.

---

# What not to do

- **Do not edit any file.** Not even to apply an "obvious" fix. The whole point of this skill is to report.
- **Do not summarize or re-rank the subagents' findings when asked to relay them** (Full). Verbatim means verbatim.
- **Do not certify a negative without proof** (Full rule 1). If you catch yourself writing "this can't happen because…", stop and either prove it or demote it to a finding.
- **Do not confuse "I traced the control flow" with "I traced the state".** The state trace is mandatory for buffer/retention theses.
- **Do not skip verification when the toolchain is available.** A false "verified correct" is worse than an honest "unverified".
- **Do not invent findings to look thorough.** An empty section is fine; a clean diff is a valid outcome. Do not pad the style pass with nits while missing the naming/consistency theme.
- **Do not lecture.** Cite the line, say what's off in one sentence, move on.
- **Do not propose alternative architectures** unless the diff itself is architecture-level. Stay at the level of the change.
