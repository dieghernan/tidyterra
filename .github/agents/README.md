# AI Agents & Skills

This folder contains the **AI‑assisted documentation review system** used in
this package. The system is built around two complementary concepts:

-   **Agents** — define workflow, scope, tools, and output format
-   **Skills** — provide domain expertise, style rules, and examples

Together, they ensure consistent, high‑quality documentation across the project.

--------------------------------------------------------------------------------

## 📁 Structure

```         
.github/
└── agents/
    ├── review-comments.agent.md   # Reviews roxygen2 + inline comments in R/
    ├── review-docs.agent.md       # Reviews vignettes, README, prose docs
    └── README.md                  # (this file)

└── skills/
    ├── proofread-comments/
    │   └── SKILL.md               # Expertise for roxygen2 + inline comments
    └── proofread-docs/
        └── SKILL.md               # Expertise for prose documentation
```

Each agent depends on one or more skills.

--------------------------------------------------------------------------------

## 🎯 Purpose

These agents and skills help maintain high-quality, consistent documentation
across the package by:

-   Enforcing package’s house style (especially **no Oxford comma** and **≤ 80
    characters per line**)
-   Improving clarity, accuracy, and user-friendliness
-   Preventing accidental changes to executable code
-   Producing structured, actionable feedback

--------------------------------------------------------------------------------

## How to Use

### Agents

-   `review-comments`: Focused exclusively on `.R` files (roxygen2 blocks and
    `#` comments)
-   `review-docs`: Focused on prose files (vignettes, README, `man/*.Rmd`, NEWS,
    etc.)

Both agents follow the same workflow:

1.  Discover relevant files
2.  Read content
3.  Evaluate using the corresponding **Skill**
4.  Produce a structured report with **Critical / Important / Polish**
    suggestions
5.  Wait for explicit user approval before making any changes

### Skills

Skills act as the **single source of truth** for style rules, review criteria,
tone, and examples.

-   `proofread-comments/SKILL.md` → Used by `review-comments`
-   `proofread-docs/SKILL.md` → Used by `review-docs`

--------------------------------------------------------------------------------

## Core Principles (Shared by All)

-   **Safety first**: Never modify executable code, function signatures, or
    behavior-affecting roxygen tags.
-   **Style rules**:
    -   No Oxford comma (serial comma)
    -   Maximum 80 characters per line
    -   Professional yet approachable tone
    -   Tidyverse style with package-specific exceptions
-   Always start feedback with positive observations when appropriate.
-   Prioritize: **Correctness → Clarity → Style**

--------------------------------------------------------------------------------

## Future Maintenance

### Adding a New Agent or Skill

1.  Follow the same structure as the existing ones.
2.  Keep agents lightweight and focused on workflow.
3.  Put all style rules and examples in the corresponding Skill.
4.  Use the same classification (`Critical` / `Important` / `Polish`) and output
    format when possible.

### Updating Style Rules

Update the relevant `SKILL.md` file. Both agents reference the skills, so
changes automatically propagate.

--------------------------------------------------------------------------------

## Related Tools

These files are designed to work with AI coding tools that support `.agent.md`
and skill-based prompting (such as Cursor, Continue.dev, or custom LLM
workflows).

--------------------------------------------------------------------------------

**Last updated:** April 2026 **Maintained by:** Diego Hernangómez

--------------------------------------------------------------------------------

**Questions or improvements?** Open an issue or edit the relevant `.agent.md` /
`SKILL.md` file.
