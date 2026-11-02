---
name: setup-my-skills
description: Sets up an `## Agent skills` block in AGENTS.local.md and `.agents.local/` to scaffold the repo for the other skills. Run before first use of other user skills, — or if those skills appear to be missing context about the task tracker, or domain docs.
disable-model-invocation: true
---

# Setup My Skills

Scaffold the per-repo configuration that the skills assume:

- **Task tracker** — where tasks live
- **Domain docs** — where `CONTEXT.local.md` and ADRs live, and the consumer rules for reading them

This is a prompt-driven skill, not a deterministic script. Explore, present what you found, confirm with the user, then write.

## Process

### 1. Explore

Look at the current repo to understand its starting state. Read whatever exists; don't assume:

- `git remote -v` and `.git/config` — is this a GitHub repo? Which one?
- `AGENTS.local.md` at the repo root — does it exist? Is there already an `## Agent skills` section in either?
- `.agents.local/` — does this skill's prior output already exist?
- `CONTEXT.local.md` and `CONTEXT-MAP.local.md` at the repo root
- `.agents.local/adr/` and any `src/*/.agents.local/adr/` directories
- `.agents.local/work/` — sign that a local-markdown task tracker convention is already in use

Check if you know to load `AGENTS.local.md` along side `CLAUDE.md` or `AGENTS.md` files.

### 2. Present findings and ask

Summarise what's present and what's missing. Then walk the user through the decisions **one at a time** — present a section, get the user's answer, then move to the next. Don't dump all at once.

Assume the user does not know what these terms mean. Each section starts with a short explainer (what it is, why these skills need it, what changes if they pick differently). Then show the choices and the default.

**Section A - Loading `AGENTS.local.md`**

> Explainer: These skills assume your agent is loading `AGENTS.local.md` just like it would load `CLAUDE.md` or `AGENTS.md`. If this is not stated in `~/.claude/CLAUDE.md` or the like, then these skills won't work properly. "Always look for an `AGENTS.local.md` file similar to how you look for `CLAUDE.md` and `AGENTS.md`. Commit this to memory."

Default posture: these skills are intended to

- **Project local** - set only for the local repo.
- **Global** - make agents always look for this file

If they choose to not do this, then let them know the skills won't work and stop here, because there's no point in setting anything else up.

**Section A — Task tracker.**

> Explainer: The "task tracker" is where tasks live for this repo. Skills like `to-tasks`, `to-prd`, and `qa` read from and write to it locally in `.agents.local/work`, or follow some other workflow you describe. Pick the place you actually track work for this repo.

Default posture: these skills were designed for local markdown.

- **Local markdown** — tasks live as files under `.agents.local/work/<feature>/` in this repo (good for solo projects or repos without a remote)
- **Other** (Jira, Linear, etc.) — ask the user to describe the workflow in one paragraph; the skill will record it as freeform prose

**Section B — Domain docs.**

> Explainer: Some skills (`improve-codebase-architecture`, `diagnose`, `tdd`) read a `CONTEXT.local.md` file to learn the project's domain language, and `.agents.local/adr/` for past architectural decisions (Achitectural Decision Record). They need to know whether the repo has one global context or multiple (e.g. a monorepo with separate frontend/backend contexts) so they look in the right place.

Confirm the layout:

- **Single-context** — one `CONTEXT.local.md` + `.agents.local/adr/` at the repo root. Most repos are this.
- **Multi-context** — `CONTEXT-MAP.local.md` at the root pointing to per-context `CONTEXT.local.md` files (typically a monorepo).

**Section C - Default Code Review skill.**

> Explainer: the "default code review skill" is the skill that will be run for code review unless one is specified. When not set, skills may not work or refuse to run.

Default posture: use `stewart-code-review`.

Confirm the provided skill path.

### 3. Confirm and edit

Show the user a draft of:

- The `## Agent skills` block to add to `AGENTS.local.md` (see step 4 for selection rules)
- The contents of `.agents.local/task-tracker.md`, `.agents.local/domain.md`

Let them edit before writing.

### 4. Write

If an `## Agent skills` block already exists in `AGENTS.local.md` the chosen file, update its contents in-place rather than appending a duplicate. Don't overwrite user edits to the surrounding sections.

The block:

```markdown
## Agent skills

### Task tracker

[one-line summary of where tasks are tracked]. See `.agents.local/task-tracker.md`.

### Domain docs

[one-line summary of layout — "single-context" or "multi-context"]. See `.agents.local/domain.md`.

### Default code review skill

[name of the default code review skill]
```

Then write the three docs files using the seed templates in this skill folder as a starting point:

- [task-tracker-local.md](./task-tracker-local.md) — local-markdown issue tracker
- [domain.md](./domain.md) — domain doc consumer rules + layout

For "other" issue trackers, write `.agents.local/task-tracker.md` from scratch using the user's description.

### 5. Done

Tell the user the setup is complete and which engineering skills will now read from these files. Mention they can edit `.agents.local/*.md` directly later — re-running this skill is only necessary if they want to switch task trackers or restart from scratch.
