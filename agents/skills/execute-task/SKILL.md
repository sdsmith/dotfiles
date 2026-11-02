---
name: execute-task
description: "Execute one task (possibly created from the `prd-to-tasks` skill) — the complete, self-contained workflow for an execution agent: intake the task file and its PRD, confirm blockers are done, explore, implement (TDD), run feedback loops, commit, bookkeep the task file, and report back with deviations and follow-ups."
---

# Execute Task

Work on the provided task. If you have not been provided one, then ask for one to work on.

You will work on the AFK tasks only, not the HITL ones. If your task is HITL, let the user know what is open and work with them to resolve it. If you are running as an autonomous subagent (no human available), stop instead and surface the HITL task in your final report.

## Workflow

### CODE REVIEW SKILL

Make sure a **code review skill** to use has been specified. If not, stop and report it.

### TASK INTAKE

Read the task file fully — it is the authoritative spec. Read the PRD its `## Parent` section references, at least every section the task cites. Read the project's `AGENTS.md` (and `AGENTS.local.md` if present): its ground rules (test commands, doc-sync requirements, style) are binding, and tests ship in the same change as behavior.

### TASK DEPENDENCIES

Confirm that the task's dependencies are done, meaning under `tasks/done/`, for each task named in the task's `## Blocked by` section. If any are not done, list the pending tasks and stop working.

### EXPLORATION

Explore the repo. Stay strictly inside the task's scope — no drive-by refactors, no unrelated fixes.

### IMPLEMENTATION

Use /tdd to complete the task.

### FEEDBACK LOOPS

Before committing, run the feedback loops:

Run the recommended test for the project. If not available, then:

- Run unit tests
- Run end to end (e2e) tests
- Run linters

The suite must be green before you commit. Note the command and result — it goes in the bookkeeping and the final report.

### CODE REVIEW

Create a subagent that runs the **code review skill** to review the change. The review subagent runs the skill end-to-end.

The **review subagent** prompt should include:

- The fixed point to review.
- The diff command: `git diff <fixed-point>...HEAD` (three dots so the comparison is against the merge-base).
- Task details: include task file and PRD paths if available.
- Facts already verified: test counts, renamed, priori-test adaptions, deviations.
- If there is a task file, include the "findings directory" as `.agents.local/work/<feature>/review/review-task-NN/` (zero-padded). Otherwise don't mention the findings directory.

### FIX

When code review has completed, make updates based on the feedback.

### COMMIT

Make a git commit. Stage ONLY the files you changed — never `git add -A`, `git add .`, or `git add -u`; the repo may contain unrelated dirty or untracked files that must never be swept in. Do not push. Do not create branches or PRs. The commit message must:

1. Include key decisions made
2. Include files changed
3. Blockers or notes for next iteration

### TASK BOOKKEEPING

If the task is complete, move the task file to `tasks/done/` and append a `## COMMENTS` section at the end of the moved file recording, in this order:

- What was done (brief, end to end)
- Assumptions made
- DEVIATIONS: every deviation from the task/PRD or thing you had to improvise — one bullet each, explicit and honest, or "none"
- FOLLOW-UPS: out-of-scope items worth future work, or "none"
- Test command + result

Don't remove old comments, just append. Commit the bookkeeping with the change (or as a follow-up commit).

If the task is not complete, leave the task file where it is — do not move it to `done/` — and append a `## COMMENTS` section with what was done and exactly where you stopped and why. Commit only if the tree stays green.

Never modify the PRD, other task files, or anything outside the task's scope.

### FINAL REPORT

Report back using exactly these headings:

- SUMMARY: one paragraph, the end-to-end change
- COMMITS: hash(es)
- TESTS: command + pass/fail
- DEVIATIONS: bullet list, or "none"
- FOLLOW-UPS: bullet list, or "none"
- NEXT: what the next task's implementer must know

## FINAL RULES

ONLY WORK ON A SINGLE TASK.
