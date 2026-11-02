---
name: review-follow-ups
description: Collect every pending follow-up parked in a PRD task folder, deduplicate and categorize them, walk them with the user one by one, and convert the approved ones into tasks on the project task tracker. Use after an execute-tasks run, when the user asks to review, triage, or act on the parked follow-ups, or wants follow-ups converted into tasks.
---

# Review Follow-Ups

The review end of the task pipeline. `/execute-tasks` parks follow-ups and never converts them — by design, the user reviews them. This skill is that review: collect every pending follow-up, categorize it, walk it with the user, and turn the approved ones into real tasks filed in the shared backlog work dir, which the executor picks up on its next run.

This is a prompt-driven skill, not a deterministic script: explore, present, ask, then write.

## Input

The user passes a PRD folder, a task folder, or a follow-ups folder — or nothing. Resolve any of the three to the PRD folder (the one holding `tasks/` and `follow-ups/`). With no argument, discover every PRD folder in scope: every `.agents.local/work/` tree under the git root (a monorepo can hold one per project) — follow `.agents.local/task-tracker.md` if it names another root. Collect across all of them, and group by folder.

## Conventions

- **Follow-up file**: `follow-ups/<slug>.md`, the `/execute-tasks` template — `## Parent` (which task noted it, with the date), `## Type` (`FOLLOW-UP — needs user review`), `## What to build`, `## Acceptance criteria`. Two on-disk shapes exist by history — `follow-ups/` beside `tasks/`, and `tasks/follow-ups/` — both are that folder's follow-ups home, and each keeps its own `done/` subdir.
- **Pending**: a follow-up file not yet handled — no `## RESOLUTION` section and not yet in `done/`; or a follow-up item noted in a task file's `## COMMENTS` — under its follow-ups heading (`### FOLLOW-UPS` as written, or a `FOLLOW-UPS:` line as `/execute-task`'s contract words it) — with no parked file. Both kinds are equally pending — a noted-only item was never lost, it was never parked. A file carrying a legacy `## RESOLUTION` / `## Decision` / `## Resolution` section (the older templates) is already handled — out of the pool.
- **Handled**: a follow-up is handled when it is dismissed (duplicate, will-not-do, moot, already done, routed elsewhere) **or** a task is filed for it. Handling records itself by appending a `## RESOLUTION` section at the end of the follow-up file — what the review of the follow-up resulted in and why (named `RESOLUTION`, never `COMMENTS`, so it never reads as a task file's running comments) — then checking its acceptance box and moving the file into its folder's `done/` subdir (`follow-ups/done/`, or `tasks/follow-ups/done/` where the folder keeps follow-ups under `tasks/`). Handled files never re-enter the pending pool; the user re-opens one by asking.
- **Backlog work dir**: `.agents.local/work/backlog/` — the one shared work dir every approved follow-up converts into, whatever PRD folder it came from: `backlog/tasks/<NN>-<slug>.md`, with `<NN>-` continuing the backlog's own sequence and never reusing a number. The PRD folders never gain task files from this skill.
- **Priority**: every follow-up that becomes a backlog task carries one — a `## Priority` section in the task file, one of `SS: Showstopper`, `P0: Must do`, `P1: Should do`, `P2: want to do`. The walk proposes the priority with the disposition; the user confirms or re-grades it.
- **Task files**: `tasks/<NN>-<slug>.md`, the `/prd-to-tasks` template (`## Parent`, `## Type` (AFK or HITL), `## Phase`, `## What to build`, `## Acceptance criteria`, `## Blocked by`); `tasks/done/` holds completed ones.
- **PRD**: `PRD.md` beside `tasks/`, if present.

## Process

### 1. Collect

For every PRD folder in scope:

- Read every `follow-ups/*.md`. Parse the title, `## Parent`, `## What to build`. No `## RESOLUTION` section → pending.
- Scan every task file — `tasks/done/*.md` first, then any remaining `tasks/*.md` — for the follow-ups list inside `## COMMENTS`: the bullets under a `### FOLLOW-UPS` heading (the shape agents actually write) or after a `FOLLOW-UPS:` line (the shape `/execute-task`'s contract words). Every bullet with no parked file is **noted-only**, still pending.
- Cross-check the PRD's own deferred-work notes ("further notes", "removed in its own commit", and the like) only where it explicitly defers work to a later decision — surface those as noted-only with the PRD as the origin. Do not invent items from ordinary prose.

Deduplicate by substance, not wording: the same item noted by two tasks, or parked in two PRD folders, is one entry with every origin listed. If the pool is empty, say so plainly and stop — show the counts checked (folders scanned, files parked, items noted), so "nothing pending" is verified, not assumed.

### 2. Categorize

Each entry gets one **status** (parked / noted-only) and one **kind**. Suggested kinds — use them where they fit, and say so when one doesn't:

- **Correctness gap** — behavior wrong in a reachable case
- **Decision / spec conflict** — two authoritative sources disagree; the work is deciding
- **Unowned deferred work** — the plan named it, no task owned it
- **Test gap** — a missing drift-guard assertion
- **Adjacent design change** — deliberately out of scope, bigger blast radius
- **Process / cleanup** — not tied to one behavior

### 3. Present the collection

One table, grouped by PRD folder: every pending entry — title, kind, status, origin(s), one-line gist. Under it, the dedup merges made and any cross-folder duplicates. Then confirm with the user before walking anything: is the scope right, is anything missing or wrongly merged, and do they want the items walked one at a time (the default) or grouped by kind.

### 4. Walk each item with the user

One item at a time (or per kind-group, if they chose that). For each, show:

- **The item's own words** — its `## What to build`, or the noting agent's bullet, lightly edited to read standalone
- **Origin(s)**, and any evidence the noting agent recorded
- **A proposed disposition**, with a default and the reasoning behind it:
  - **convert to task** — AFK for concrete work, HITL when the item *is* a decision the user must make; carries a proposed priority (SS/P0/P1/P2)
  - **reject** — not worth a task; say why
  - **keep parked** — right question, wrong time; say what would re-open it
  - **merge** — into another entry being converted
  - **already done** — the walk itself finds it was fixed; record the commit
  - **route to a design skill** — an item too big for one task in this PRD (an adjacent design change, say) goes to `/grill-with-docs` or `/context-to-prd` as its own thread

Then end the turn and wait. The user's per-item answer is the whole point of this skill — never convert, reject, keep, or merge anything on the agent's own judgment. At a gate with no answer, wait; that is the design, not a stall.

### 5. Convert and handle the approved ones

For each approved conversion, write a task file in the shared backlog work dir — never in the PRD folder:

- `.agents.local/work/backlog/tasks/<NN>-<slug>.md`; `<NN>-` continues past the highest number anywhere in `backlog/tasks/` **and** `backlog/tasks/done/` — never reuse a number.
- The `/prd-to-tasks` template exactly, plus `## Priority` (SS/P0/P1/P2 per the user's decision). `## Parent` references the follow-up file (and the noting task, for a noted-only entry). `## Type` per the user's decision. `## Phase` — `Follow-ups`. `## What to build` carries the follow-up's own text, tightened to end-to-end behavior. `## Acceptance criteria` — concrete and checkable, from the follow-up's own criteria plus anything the walk surfaced.
- Publish in dependency order — blockers first — so `## Blocked by` references real backlog filenames; a decision task blocks the work that depends on its decision.

Then record every walked entry's outcome by appending to its follow-up file:

```markdown
## RESOLUTION

- [<date>] <disposition> — <what it became: backlog task <NN> filename, priority <SS/P0/P1/P2> / dismissed: <reason — duplicate, will-not-do, moot; kept parked: what would re-open it; already done: <commit>; routed to <skill>>
```

checking its acceptance box, and moving the file into its folder's follow-ups `done/` subdir — handled, whether the outcome was a filed task or a dismissal. Append-only — never rewrite the prose above. A noted-only entry gets its parked file written first (the `/execute-tasks` template), then the RESOLUTION outcome, then the move — so the decision outlives the walk and the next collection finds the item parked and handled. A cross-folder duplicate is handled in every folder it appears in, pointing at the one task.

Commit the tracker files as the tracker's conventions allow: if the tracker tree is gitignored there is nothing to commit; otherwise stage only the files this skill touched. Never `git add -A`, never push.

### 6. Final report

- the collection: folders scanned, entries pending, dedup merges
- the decisions, one line each: entry → disposition
- the backlog tasks created (number, title, AFK/HITL, priority, blockers) and the follow-ups handled (RESOLUTION recorded, moved to `done/`)
- what was dismissed, kept parked, or routed, and why
- the next step: `/execute-tasks` over the backlog work dir — it picks up the filed tasks in dependency order

## Rules

- The user decides every item. No conversions, rejections, keeps, or merges on the agent's own judgment.
- Never execute a follow-up — that is `/execute-tasks`'s job, over the tasks this skill writes.
- Follow-up files are append-only (`## RESOLUTION`); the noting prose is never rewritten, and a handled file moves to `done/` as-is.
- Never modify the PRD, and never modify a task file's existing content.
- Handled is handled: a file in `done/` (or carrying a legacy `## RESOLUTION` / `## Decision` section) stays out of the pending pool until the user re-opens it.
- Deduplicate by substance; keep every origin in the merged entry.
