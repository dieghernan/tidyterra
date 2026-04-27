# GitHub Skills

This folder contains reusable skill definitions for package review workflows.

## Purpose

Each subfolder under `.github/skills/` contains a single skill, defined in a
`SKILL.md` file. Skills capture the domain expertise and style guidance used by
review agents.

## Current skills

- `proofread-comments` — Review roxygen2 documentation and inline comments in R
  source files.
- `proofread-docs` — Review narrative documentation prose in README, vignettes,
  and other top-level docs.

## Conventions

- Skill folders are named after the `name:` field in the skill manifest.
- Each skill folder must contain one `SKILL.md` file.
- Skill manifests use markdown with a YAML frontmatter section for metadata.
- Keep skill guidance focused on prose, documentation, and review guidance, not
  code execution.
- Use `name` and `description` consistently between the folder name and the
  skill metadata.

## Metadata conventions

- `SKILL.md` YAML frontmatter should include:
  - `name`: the skill identifier
  - `description`: a short summary of the skill's expertise
- Keep the skill folder name aligned with the `name` field.
- When adding a new skill, update this README and any agent README that
  references it.
- Agents may optionally use an explicit `skills:` declaration in `.agent.md` to
  list the skill names they rely on.

## Recommended workflow

1.  Add a new skill when a new review area is needed.
2.  Name the folder and `name:` field consistently.
3.  Keep skill guidance scoped and easy to follow.
4.  If an agent is created, reference the corresponding skill name in the agent
    manifest.

## Notes

- Agents are defined separately in `.github/agents/` and should point to the
  appropriate skill(s).
- This file is intended to help maintainers understand the `.github/skills/`
  layout and add new review skills consistently.
