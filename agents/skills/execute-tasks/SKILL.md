---
name: execute-tasks
description: Orchestrate execution of every remaining implementation task in a task folder serially — one general subagent per task, each running the execute-task skill on its task — stopping for human approval only on major deviations (out-of-scope changes, or ones a later task depends on), and parking noted follow-ups as review tasks. Use when the user passes a task-folder path (from prd-to-tasks) and asks to execute, run, or work through the tasks.
---

# Execute Tasks

Orchestrate execution of every remaining task in a task folder: serial, one subagent per task, with human gates. The gates are the point of this skill — **major** deviations stop for approval, failures stop, HITL tasks stop; minor and procedural deviations are recorded and reported, never gated. Follow-ups are parked for review, never auto-executed.

Division of labor: this skill owns the queue, the gates, the verification, and the follow-up parking. The `/execute-task` skill (singular) owns the complete per-task execution workflow — each subagent runs it on its task. Hand it off; never inline a copy of the workflow here.

Test ownership: implementers run tests to verify their change; the reviewer never runs the project's builds or tests — its boundary is code reading plus small exploratory probes; this skill owns the backstop, enforced in the per-task verification step.

## Input

The user passes the path to a task folder or a PRD's path produced from `prd-to-tasks`. The path arrives as the skill's argument. The use may optionally specify the code review skill to use.

## Conventions

- **`/execute-task`**: the per-task execution skill the subagent runs. It owns the execution workflow and the `## COMMENTS` / final-report formats this skill parses.
- **Executable tasks**: files directly in the task folder named `<NN>-<slug>.md` (`NN` = digits), not yet in `done/`.
- **`done/`**: completed tasks (the task executor moves them there). Re-invoking on the same folder resumes — done tasks are skipped.
- **`follow-ups/`**: parked items that need user review. This skill creates them; it never executes them.
- **PRD**: `../PRD.md` relative to the task folder, if it exists — the source plan the tasks were broken down from.
- Task files follow the `prd-to-tasks` template (`## Parent`, `## Type` (AFK or HITL), `## Phase`, `## What to build`, `## Acceptance criteria`, `## Blocked by`). Parse `## Type` for HITL; parse `## Blocked by` for dependencies (task numbers or filenames).

## Setup

1. Resolve **code review skill**: If the user specified the code review skill to use, use that. Otherwise use the **default code review skill** specified in `AGENTS.local.md`. If that file doesn't exist, prompt the user to run `setup-my-skills` to create it. If the **code review skill** is still not resolved, ask the user.
1. Resolve the path. Be forgiving: if it contains a `tasks/` subdirectory (and a `PRD.md`), use `<path>/tasks`. It must exist and contain at least one `<NN>-<slug>.md` file — if only done tasks exist, report "nothing to do" and stop.
2. Discover the surroundings:
   - PRD: `../PRD.md` (may be absent — tasks then stand alone).
   - Git root: `git -C <task folder> rev-parse --show-toplevel`.
   - Project root: the nearest ancestor of the task folder (at or below the git root) containing `AGENTS.md`; if none, the git root.
3. Build the queue: every executable task, ascending numeric order. A task is **ready** when every task its `## Blocked by` names is in `done/`. Execute ready tasks in ascending order — always serial, never two subagents at once.
4. Sanity-check the graph: a blocker referencing an unknown task (check `done/` too) or a dependency cycle → stop and report it.
5. Establish a green baseline: run the project's test command (from its `AGENTS.md`; ask the user if undocumented). If red, stop — never build on a broken baseline.
6. Create a todo list with one item per queued task and keep it updated through the run.

## Task execution steps

### Per-task loop

For each ready task, in order:

1. **HITL gate**: if the task's Type is HITL, stop and tell the user what the task needs from them. Do not send HITL tasks to a subagent. Resume when they resolve it (they handle it themselves and move it to `done/`, or edit it to AFK).
2. Verify blockers are in `done/` (they should be, by ordering — if not, stop and report).
3. Record the current HEAD (`git -C <project root> rev-parse HEAD`).
4. Launch **one** `general` subagent (Task tool) with the handoff prompt below, filled in — the subagent runs the `/execute-task` skill on the task. Do not start the next task before this one resolves.
5. **Verify the outcome yourself** — never trust the report alone:
   - the task file moved to `done/` (gone from the task folder);
   - at least one new commit exists (`git log <HEAD>..` non-empty);
   - a `## COMMENTS` section was appended to the moved file;
   - the project's test command ran green **after the latest change** (per the report or COMMENTS) — a green run from before the last fix doesn't count. If the report or COMMENTS don't show it, run the suite yourself; red is the failure gate. If you had to run it, warn the user — the gap is visible and fixable upstream.
6. Read the appended `## COMMENTS` and cross-check the subagent's final report. Extract: deviations/improvisations, follow-ups, completeness. The COMMENTS headings and the report headings are `/execute-task`'s contract — parse them, don't re-specify them.
7. **Deviation gate — major deviations only**: a deviation is **major** when it (a) changes behavior, an artifact, or an interface beyond what the task/PRD authorizes, (b) contradicts a PRD decision or crosses an out-of-scope boundary, or (c) changes something a later task depends on — an interface, artifact shape, file layout, naming scheme, or convention another task builds on. On a major deviation: **stop**. Present to the user: the task, commit hash, each major deviation (in the agent's words), and the options (approve / rework / reject) — the `question` tool fits. Do not start the next task until they answer.
   - Approved → append to the done task's COMMENTS: `- [<date>] User approved: <deviation summary>`; continue.
   - Rework/reject → follow the user's direction (typically: move the task back out of `done/`, relaunch a subagent with their corrective instructions quoted verbatim in the prompt).
   - **Minor and procedural deviations never gate.** Implementation order (TDD sequencing included), naming and formatting choices inside the task's scope, a design choice the task left open, verification done operationally rather than as a committed test, and cosmetic leftovers the agent deliberately did not churn: record them in the checkpoint line and the final report, and keep going. Do not present them for approval, and do not stretch the major category — the test is scope and downstream impact, not unfamiliarity.
8. **Follow-ups**: for each noted follow-up item, create `<task folder>/follow-ups/<slug>.md` from the template below and commit it. Mention them in the checkpoint. Never execute them. If a follow-up materially affects remaining tasks (contradicts the plan or changes something later tasks build on), stop and ask before continuing.
9. **Failure gate**: if the task is incomplete (not moved to `done/`, no commit, tests red, or the subagent explicitly gave up): stop, present the failure (task, the agent's report, repo state), and ask how to proceed.
10. **Checkpoint**: one short line to the user — task number, commit short hash, deviations (none / recorded / gated+approved), follow-ups added (count). Update the todo list. Continue.

### Final check

Once the all the tasks are complete:

1. Launch a review subagent with the **code review skill** to verify all the changes.
2. Fix all the issues in a single implementation subagent.

## Subagent handoff prompt

Fill in every `<...>` and send as the Task prompt (subagent type: `general`). It must be self-contained — the subagent sees nothing of this conversation — and it is deliberately minimal: `/execute-task` owns the execution workflow, so the handoff only points at it.

```
Run the /execute-task skill on this task.

Task file: <abs path to the task file>
PRD: <abs path to the PRD, or "none — the task file is the whole spec">
Task folder (the task's blockers live under its done/ subfolder): <abs path to the task folder>
Project root: <project root>
Git root: <git root>
Code review skill: <**code review skill**>

Follow the skill exactly — it is the complete workflow: task intake, dependencies, exploration, implementation (/tdd), feedback loops, commit, bookkeeping, final report.

You are running autonomously: no human is available inside this run. Where the skill would involve a human (a HITL task, an undocumented test command, an unresolvable ambiguity), stop that thread and surface it in your final report instead of improvising.

Report back using the skill's final-report headings: SUMMARY / COMMITS / TESTS / DEVIATIONS / FOLLOW-UPS / NEXT.
```

## Follow-up task template

`<task folder>/follow-ups/<slug>.md`:

```
# <title>

## Parent

Noted during <source task number> (<source task filename>), <date>.

## Type

FOLLOW-UP — needs user review; nothing executes it until the user converts it.

## What to build

<the noted item, in the noting agent's words, lightly edited to read standalone>

## Acceptance criteria

- [ ] Reviewed by the user and converted into a real task (or rejected) — this file is a parking place, not a plan
```

Commit follow-up files as they're created (only those files).

## Final report

When the queue is empty, report:

- every task completed (number, slug, commit short hash), in execution order
- the major deviations that gated the queue and were approved along the way (task + one-liner each), plus the minor/procedural deviations recorded without gating
- the follow-ups awaiting user review (`follow-ups/` listing)
- HITL items the user resolved, and anything still open
- overall test status

## Rules

- Serial execution, always. One subagent per task. Never start task N+1 before task N is done and gated.
- The execution workflow lives in `/execute-task` — hand it off, never inline or duplicate it.
- The deviation gate is for **major** deviations: an agent's "I had to improvise" stops the queue when it leaves the PRD's scope or changes something a later task builds on, and is a checkpoint footnote when it does not. Gating on trivial burns the user's attention, which is the scarcest thing the run consumes.
- Never auto-execute follow-ups, and never convert them to executable tasks yourself — the user reviews them.
- Never modify the PRD. Completed task content is append-only (COMMENTS, approval notes).
- Never push; never commit unrelated files.
- Resume-safe: re-invocation skips `done/`.
- At a gate with no user answer, wait — that is the design, not a stall.
