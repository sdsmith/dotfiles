# CONTEXT.local.md Format

## Structure

```md
# {Context Name}

{One or two sentence description of what this context is and why it exists.}

## Language

**Order**:
{A concise description of the term}
_Avoid_: Purchase, transaction

**Invoice**:
A request for payment sent to a customer after delivery.
_Avoid_: Bill, payment request

**Customer**:
A person or organization that places orders.
_Avoid_: Client, buyer, account

## Relationships

- An **Order** produces one or more **Invoices**
- An **Invoice** belongs to exactly one **Customer**

## Example dialogue

> **Dev:** "When a **Customer** places an **Order**, do we create the **Invoice** immediately?"
> **Domain expert:** "No — an **Invoice** is only generated once a **Fulfillment** is confirmed."

## Flagged ambiguities

- "account" was used to mean both **Customer** and **User** — resolved: these are distinct concepts.
```

## Rules

- **Be opinionated.** When multiple words exist for the same concept, pick the best one and list the others as aliases to avoid.
- **Flag conflicts explicitly.** If a term is used ambiguously, call it out in "Flagged ambiguities" with a clear resolution.
- **Keep definitions tight.** One sentence max. Define what it IS, not what it does.
- **Show relationships.** Use bold term names and express cardinality where obvious.
- **Only include terms specific to this project's context.** General programming concepts (timeouts, error types, utility patterns) don't belong even if the project uses them extensively. Before adding a term, ask: is this a concept unique to this context, or a general programming concept? Only the former belongs.
- **Group terms under subheadings** when natural clusters emerge. If all terms belong to a single cohesive area, a flat list is fine.
- **Write an example dialogue.** A conversation between a dev and a domain expert that demonstrates how the terms interact naturally and clarifies boundaries between related concepts.

## Anti-examples

Don't write entries like these — they're common drift patterns from grilling sessions.

### Too long — multi-paragraph with sub-bullets

```md
**Sandbox**:
The isolated execution environment for a Subagent, layered on top of its Worktree. Two backends:

- Docker backend (default): the Worktree is bind-mounted into a Docker container with restricted-by-default isolation — non-root host UID, all capabilities dropped, network allowlisted via the Network proxy. Auth is passed via the ANTHROPIC_API_KEY env var; ~/.claude/skills is bind-mounted read-only...
- Worktree-only backend: the Subagent runs natively in the Worktree with no container wrapper.

The tight container isolation in the Docker backend is what justifies running the agent with --dangerously-skip-permissions inside it...
```

**Right shape** — one sentence stating what it is:

```md
**Sandbox**:
The isolated execution environment for a **Subagent**, layered on its **Worktree**, with **Docker** and **Worktree-only** backends.
```

### Implementation details — paths, flags, mechanisms

```md
**State store**:
A SQLite database at `.agents.local/work/<feature-slug>/ralph/state.db`. Single source of truth for everything the Orchestrator persists: phase/task progress, Pause points, hook runs, agent runs, the Audit trail. Reads are cheap (no token cost), writes are atomic.
```

**Right shape** — strip the path, the technology, and the performance characteristics:

```md
**State store**:
The per-**Feature** persistent record of everything the **Orchestrator** tracks.
```

### Rationale included — explanation of why or how

```md
**AFK mode**:
A run-level flag (`--afk`) that tells the Orchestrator to drive the entire Feature autonomously: do not pause between Phases for human merge, treat each Phase's review hook in autonomous (iterate-until-pass) mode, and leave all Phase branches stacked at the end for human review. Without --afk, the Orchestrator pauses after each Phase.
```

**Right shape** — drop the behaviors and the contrast with the default:

```md
**AFK mode**:
A run-level mode (`--afk`) in which the **Orchestrator** drives the entire **Feature** without human gates.
```

The fix in all three cases is the same: state what the term IS. The how, why, where, and what-it-does belong elsewhere — in the PRD, in an ADR, or in `.agents.local/grill-session/`.

## Single vs multi-context repos

**Single context (most repos):** One `CONTEXT.local.md` at the repo root.

**Multiple contexts:** A `CONTEXT-MAP.local.md` at the repo root lists the contexts, where they live, and how they relate to each other:

```md
# Context Map

## Contexts

- [Ordering](./src/ordering/CONTEXT.local.md) — receives and tracks customer orders
- [Billing](./src/billing/CONTEXT.local.md) — generates invoices and processes payments
- [Fulfillment](./src/fulfillment/CONTEXT.local.md) — manages warehouse picking and shipping

## Relationships

- **Ordering → Fulfillment**: Ordering emits `OrderPlaced` events; Fulfillment consumes them to start picking
- **Fulfillment → Billing**: Fulfillment emits `ShipmentDispatched` events; Billing consumes them to generate invoices
- **Ordering ↔ Billing**: Shared types for `CustomerId` and `Money`
```

The skill infers which structure applies:

- If `CONTEXT-MAP.local.md` exists, read it to find contexts
- If only a root `CONTEXT.local.md` exists, single context
- If neither exists, create a root `CONTEXT.local.md` lazily when the first term is resolved

When multiple contexts exist, infer which one the current topic relates to. If unclear, ask.
