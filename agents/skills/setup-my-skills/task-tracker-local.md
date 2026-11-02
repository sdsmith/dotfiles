# Task tracker: Local Markdown

Tasks and PRDs for this repo live as markdown files in `.agents.local/work/`.

## Conventions

- One feature per directory: `.agents.local/work/<date>_<feature-slug>/`
  - `<date>` is the PRD creation date in the form: YYYY-MM-DD
- The PRD is `.agents.local/work/<date>_<feature-slug>/PRD.md`
- Implementation tasks are `.agents.local/work/<date>_<feature-slug>/tasks/<NN>-<slug>.md`, numbered from `01`
- Comments and conversation history append to the bottom of the file under a `## Comments` heading

## When a skill says "publish to the task tracker"

Create a new file under `.agents.local/work/<date>_<feature-slug>/` (creating the directory if needed).

## When a skill says "fetch the relevant ticket"

Read the file at the referenced path. The user will normally pass the path or the task number directly.
