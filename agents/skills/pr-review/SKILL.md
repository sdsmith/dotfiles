---
name: pr-review
description: Review a GitHub PR in an isolated git worktree named `pr-<number>` under `<repo>.worktrees/pr-reviews/` next to the repo root, then run code reivew on it from inside that worktree. Use when the user gives a GitHub PR number or URL and asks for a review in a worktree, or wants a PR reviewed without touching the current checkout.
---

# PR review worktree

Review a GitHub PR without touching the current checkout: set up a dedicated worktree, then hand off to .

## 1. Resolve the PR

Accept either a PR number (`123`) or a GitHub PR URL (`https://github.com/<owner>/<repo>/pull/123`); extract the number. Confirm `gh pr view <number>` succeeds — a bad number, missing `gh`, or no auth stops the review here. While you have `gh` out, capture the PR's base branch (`gh pr view <number> --json baseRefName -q .baseRefName`) — it is the review's fixed point, so the reviewer never has to ask.

Rename the session to be `review-<repo>-pr-<pr_number>_<pr_title>`.

## 2. Resolve the code review skill

If the code review skill is specified as an argument, use that as the **code review skill**. Otherwise use the code review skill specified in AGENTS.local.md. If that file doesn't exist, prompt the user to run `setup-my-skills` skill to create it.

If the **code review skill** is still not known, ask the user which skill to use.

## 3. Create the worktree

Compute the paths from the repo root (`git rev-parse --show-toplevel`):

- Worktrees root: `<parent-of-root>/<root-name>.worktrees/pr-reviews/` — a sibling of the repo folder.
- Worktree path: `<worktrees-root>/pr-<number>/`.

If the worktree path already exists, reuse it; otherwise create it detached so branch-name collisions with the main checkout are impossible:

```bash
mkdir -p <worktrees-root>
git worktree add --detach <worktree-path>
```

Check out the PR head inside the worktree — fetch and checkout both run there, so branch-name collisions with the main checkout are impossible:

```bash
git -C <worktree-path> fetch origin pull/<number>/head
git -C <worktree-path> checkout --detach FETCH_HEAD
```

Then update the base branch's remote-tracking ref in the worktree — worktrees share the repo's refs, so a stale `origin/<base>` makes `git diff origin/<base>...HEAD` compute its merge-base against an old base and show unrelated master changes as if they were the PR's. A fetch that fails (network, auth) stops the review here:

```bash
git -C <worktree-path> fetch origin <base-branch>
```

Done when `git -C <worktree-path> rev-parse HEAD` resolves, `git -C <worktree-path> log -1` shows the PR's head commit, and `git -C <worktree-path> diff origin/<base-branch>...HEAD --stat` shows only files the PR plausibly touches (the PR's own commits — the noise symptom of a stale ref is hundreds of files that match no PR commit).

## 4. Move there, then review

Switch the working directory to the worktree (`change_dir`), and pass the worktree path as `cwd` on every later command — shell state does not persist across calls, so a stray command back in the main checkout silently reviews the wrong code.

Then run the **code review skill** with the PR number as its argument, from inside the worktree. Its handoff gets the facts already verified here: the PR number, the base branch as the fixed point, and the worktree path as the checkout to work in — it must not re-derive them.
