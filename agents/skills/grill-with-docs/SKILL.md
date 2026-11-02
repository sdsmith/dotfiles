---
name: grill-with-docs
description: Grilling session that challenges your plan against the existing domain model, sharpens terminology, and updates documentation (CONTEXT.local.md, ADRs) inline as decisions crystallise. Use when user wants to stress-test a plan against their project's language and documented decisions.
---

<what-to-do>

Interview me relentlessly about every aspect of this plan until we reach a shared understanding. Walk down each branch of the design tree, resolving dependencies between decisions one-by-one. For each question, provide your recommended answer.

Ask the questions one at a time, waiting for feedback on each question before continuing.

If a question can be answered by exploring the codebase, explore the codebase instead.

</what-to-do>

<supporting-info>

## Domain awareness

During codebase exploration, also look for existing documentation:

### File structure

Most repos have a single context:

```
/
├── CONTEXT.local.md
├── .agents.local/
│   └── adr/
│       ├── 0001-event-sourced-orders.md
│       └── 0002-postgres-for-write-model.md
└── src/
```

If a `CONTEXT-MAP.local.md` exists at the root, the repo has multiple contexts. The map points to where each one lives:

```
/
├── CONTEXT-MAP.local.md
├── .agents.local/
│   └── adr/                          ← system-wide decisions
├── src/
│   ├── ordering/
│   │   ├── CONTEXT.local.md
│   │   └── .agents.local/adr/              ← context-specific decisions
│   └── billing/
│       ├── CONTEXT.local.md
│       └── .agents.local/adr/
```

Create files lazily — only when you have something to write. If no `CONTEXT.local.md` exists, create one when the first term is resolved. If no `.agents.local/adr/` exists, create it when the first ADR is needed.

## During the session

### Challenge against the glossary

When the user uses a term that conflicts with the existing language in `CONTEXT.local.md`, call it out immediately. "Your glossary defines 'cancellation' as X, but you seem to mean Y — which is it?"

### Sharpen fuzzy language

When the user uses vague or overloaded terms, propose a precise canonical term. "You're saying 'account' — do you mean the Customer or the User? Those are different things."

### Discuss concrete scenarios

When domain relationships are being discussed, stress-test them with specific scenarios. Invent scenarios that probe edge cases and force the user to be precise about the boundaries between concepts.

### Cross-reference with code

When the user states how something works, check whether the code agrees. If you find a contradiction, surface it: "Your code cancels entire Orders, but you just said partial cancellation is possible — which is right?"

### Update CONTEXT.md inline

When a term is resolved, update `CONTEXT.local.md` right there. Don't batch these up — capture them as they happen. Use the format in [CONTEXT-FORMAT.md](./CONTEXT-FORMAT.md).

`CONTEXT.local.md` should be totally devoid of implementation details. Do not treat `CONTEXT.local.md` as a spec, a scratch pad, or a repository for implementation decisions. It is a glossary and nothing else.

Each entry is **one sentence** stating what the term IS. If the resolved content doesn't fit in one sentence — paths, mechanisms, rationales, behaviors, CLI flags, defaults — those belong in scratch (next section), not the glossary.

### Capture design notes in scratch

Architectural commitments crystallise faster than vocabulary during the grill, but they don't belong in the glossary. Give them a parallel home: `.agents.local/grill-session/`.

Create files lazily in scratch as decisions land — one file per topic, freeform prose, no required format. Examples: `branch-model.md`, `sandbox-mechanics.md`, `phase-lifecycle.md`. These notes become the raw material for the PRD at the end of the session.

grill-session is temporary. It exists only until the PRD is finalised. DO NOT DELETE IT UNTIL YOU HAVE EXPLICIT USER CONFIRMATION. Once the PRD lands, delete the `.agents.local/grill-session/` directory. The PRD and any ADRs are the durable records.

### Offer ADRs sparingly

Only offer to create an Architectual Decision Record (ADR) when all three are true:

1. **Hard to reverse** — the cost of changing your mind later is meaningful
2. **Surprising without context** — a future reader will wonder "why did they do it this way?"
3. **The result of a real trade-off** — there were genuine alternatives and you picked one for specific reasons

If any of the three is missing, skip the ADR. Use the format in [ADR-FORMAT.md](./ADR-FORMAT.md).

ADRs created here will be explicitly linked from the eventual PRD (both inline where the decision is described and in a consolidated ADR index) — write each ADR to stand on its own, since an implementer may read it without the surrounding context.

## End of session

Before declaring the grill done, do a cleanup pass on `CONTEXT.local.md`:

1. Read every entry. Does it meet the requirement of [CONTEXT-FORMAT.md](./CONTEXT-FORMAT.md)?
2. Strip anything that's a mechanism, path, CLI flag, default value, behavior, or rationale. Those things belong in the PRD or an ADR, not the glossary.
3. Move anything that survives the strip but doesn't fit one sentence into `.agents.local/grill-session/` (or directly into the PRD if it's being written next).

The goal: a future reader can scan the glossary in under two minutes to learn the vocabulary.

`.agents.local/grill-session/` stays on disk until the user explicitly confirms it can be deleted, even after the PRD is finalised. Do not delete it autonomously.

</supporting-info>
