---
name: context-to-prd
description: Turn the current conversation context into a PRD and publish it to the project task tracker. Use when user wants to create a PRD from the current context.
---

This skill takes the current conversation context and codebase understanding and produces a PRD. Do NOT interview the user — just synthesize what you already know.

The task tracker should have been provided to you — run `/setup-my-skills` if not.

**The PRD must stand alone.** Implementers will be given the PRD as their only artifact — no conversation transcript, no glossary, no grill-session notes. Anything an implementer needs to know must be in the PRD itself:

- Inline links (relative paths) to any relevant ADRs at the points where those decisions are described
- A consolidated **Architectural Decision Records** subsection listing every linked ADR with a one-line summary and the modules it touches
- Concrete contracts (CLI surface, prompt templates, file format schemas, operational defaults, naming conventions) — not just module names
- Anything captured in `.agents.local/grill-session/` that an implementer would need

Vague references like "see ADRs" or "see the glossary" are insufficient. Link explicitly with relative paths.

## Process

1. Explore the repo to understand the current state of the codebase, if you haven't already. Use the project's domain glossary vocabulary throughout the PRD, and respect any ADRs in the area you're touching.

2. Sketch out the major modules you will need to build or modify to complete the implementation. Actively look for opportunities to extract deep modules that can be tested in isolation.

A deep module (as opposed to a shallow module) is one which encapsulates a lot of functionality in a simple, testable interface which rarely changes.

Check with the user that these modules match their expectations. Check with the user which modules they want tests written for.

4. Divide the work into phases that represent logical, working, vertical slices of work. Each phase is intended to be one block of work towards the goal. Dependencies for one phase must be done in an earlier phase so they can be executed sequentially. Phases are labelled in increasing numerical order starting with 1.

Check with the user that these phases match their expectations.

5. Write the PRD using the template below, then publish it to the project issue tracker.

<prd-template>

## Problem Statement

The problem that the user is facing, from the user's perspective.

## Solution

The solution to the problem, from the user's perspective.

## User Stories

A LONG, numbered list of user stories. Each user story should be in the format of:

1. As an <actor>, I want a <feature>, so that <benefit>

<user-story-example>
1. As a mobile bank customer, I want to see balance on my accounts, so that I can make better informed decisions about my spending
</user-story-example>

This list of user stories should be extremely extensive and cover all aspects of the feature.

## Implementation Decisions

A list of implementation decisions that were made. This can include:

- The modules that will be built/modified
- The interfaces of those modules that will be modified
- Technical clarifications from the developer
- Architectural decisions
- Schema changes
- API contracts
- Specific interactions

Do NOT include specific file paths or code snippets. They may end up being outdated very quickly.

Exception: if a prototype produced a snippet that encodes a decision more precisely than prose can (state machine, reducer, schema, type shape), inline it within the relevant decision and note briefly that it came from a prototype. Trim to the decision-rich parts — not a working demo, just the important bits.

When describing a module whose design is governed by an ADR, link the ADR inline at that point — e.g. "the `sandbox` module (see [ADR 0005](../../adr/0005-...md))".

### Architectural Decision Records

A consolidated subsection listing every new ADR as a result of grilling with a relative-path link, a one-line summary of the decision, and which modules it touches. Implementers read this first to know which decisions are non-negotiable.

Format:

```md
- **[ADR NNNN — Short title](../../adr/NNNN-slug.md).** One-sentence summary of the decision. Touches: `module-a`, `module-b`.
```

## Testing Decisions

A list of testing decisions that were made. Include:

- A description of what makes a good test (only test external behavior, not implementation details)
- Which modules will be tested
- Prior art for the tests (i.e. similar types of tests in the codebase)

## Phases

The list of phases and a 1-3 sentence description that covers what should be completed in the phase.

## Out of Scope

A description of the things that are out of scope for this PRD.

## Further Notes

Any further notes about the feature.

</prd-template>

6. Review the PRD and make sure there is nothing missing from the grilling context. The PRD will be the only record of work for implementers, so verify:
   - Every new ADR in `.agents.local/adr/` is linked from the PRD at the module description (or other point) where its decision is being described.
   - The Implementation Decisions section contains a consolidated ADR index with relative-path links and one-line summaries.
   - Concrete contracts (CLI surface, prompt templates, file format schemas, operational defaults, naming conventions) appear inline in the PRD body — not as references to grill notes that the implementer won't have.
   - Anything in `.agents.local/grill-session/` that an implementer would need has been folded into the PRD.
