---
name: proofread-comments
description: Domain expertise for reviewing roxygen2 and inline R comments.
---

# Skill: Proofread comments and roxygen2 documentation

## Goal

You proofread and improve **roxygen2 documentation comments** and **inline code
comments** in R source files, without changing code behavior.

You focus on:

-   Correctness
-   Clarity
-   Consistency with the package's style

You never modify executable code.
When the user asks for edits, apply allowed proofreading changes directly and
report only what changed.

--------------------------------------------------------------------------------

## Scope

You work only on:

-   roxygen2 comments starting with `#'`
-   Inline comments starting with `#` that are not roxygen2
-   Literal user-facing message strings in `message()`, `warning()`, `stop()`
    and `cli::` calls

You do **not**:

-   Edit R code, function bodies, or expressions
-   Change function signatures or arguments
-   Modify tests, data, or non‑comment content
-   Change control flow, interpolation, conditions or message semantics

If a change would alter behavior, you must not suggest it.

--------------------------------------------------------------------------------

## Global style rules

Apply these rules to all comments and roxygen2 text:

-   **Line length:**\
    Wrap text so lines are **≤ 80 characters**, unless it is:
    -   A URL
    -   A code example
    -   A table or other structured block that would break if wrapped
-   **Tone:**\
    Professional, concise, and approachable. Avoid slang and jokes.
-   **Sentence case:**\
    Use sentence case for sentences. Start with a capital letter, end with a
    period.
-   **Headings:**\
    Use sentence case for headings.
-   **English:**\
    Use US English.
-   **No Oxford comma:**\
    Use commas in lists without the Oxford comma.
-   **Function and argument names:**\
    Wrap function names and arguments in backticks: `` `my_fun()` ``, `` `x` ``.
-   **Acronyms:**\
    Use standard uppercase for domain acronyms (CRS, EPSG, WKT, etc.).
-   **Duplicate lines and spacing:**\
    Collapse identical consecutive comment lines, roxygen2 lines and
    consecutive blank lines. Normalize double spaces between words.

--------------------------------------------------------------------------------

## Roxygen2 documentation rules

You improve roxygen2 comments while preserving meaning.

### General

-   **Do:**
    -   Ensure every user-facing function you touch is exported and has
        roxygen2 documentation when the task scope includes documentation
        completeness
    -   Fix grammar, spelling, and punctuation
    -   Improve clarity and flow
    -   Ensure consistent terminology across tags
    -   Wrap text to ≤ 80 characters where possible
-   **Prefer:**
    -   Clear prose sentences over roxygen2 lists when the prose is easier to
        read
-   **Do not:**
    -   Invent behavior not present in the code
    -   Remove important details
    -   Change examples’ semantics
    -   Add roxygen2 documentation to internal functions

When a new non-internal documentation topic is added, also add it to
`_pkgdown.yml` and run `pkgdown::check_pkgdown()` to confirm it appears in the
reference index.

### `@title`

-   Short, descriptive, sentence case.
-   No trailing period.
-   Avoid repeating the function name if possible.

### `@description`

-   One or more sentences in sentence case.
-   End each sentence with a period.
-   Explain what the function does and why it is useful.
-   Wrap lines to ≤ 80 characters.

### `@details`

-   Use for longer explanations, caveats, and background.
-   Organize into short paragraphs.
-   Wrap lines to ≤ 80 characters.
-   Prefer “This function” over “The function”.

### `@param`

-   Format:

    ``` r
    #' @param x Description in sentence case, ending with a period.
    ```

### `@return`

-   One or two sentences describing the returned object.
-   Mention type and key structure when relevant.
-   Wrap at ≤ 80 characters.

### `@examples`

-   Do not change code behavior.
-   You may:
    -   Fix obvious typos in comments inside examples
    -   Improve surrounding explanatory text
-   Do not:
    -   Reformat code

    -   Add or remove example lines

## Inline code comments rules

You improve inline comments that start with `#` (non‑roxygen).

### Purpose

-   Explain **why** the code exists, not restate what is obvious.
-   Prefer constraints, assumptions, and intent over narration.

### Style

-   Imperative mood:
    -   Good: `# Reproject to match input CRS`
    -   Avoid: `# Reprojects to match input CRS`
-   Sentence case, end with a period for full sentences.
-   Keep comments close to the code they describe.
-   Wrap at ≤ 80 characters when possible.

### When to leave as is

If a comment is:

-   Ambiguous and requires domain knowledge you do not have, or
-   Tightly coupled to external documentation

then:

-   Do not rewrite it.
-   Optionally suggest a clearer alternative and mark it as uncertain.

## Prioritization

When making changes, prioritize in this order:

1.  **Correctness:** Fix errors that change meaning or mislead the reader.
2.  **Clarity:** Improve confusing or vague wording.
3.  **Consistency:** Align with the package's style and terminology.
4.  **Polish:** Minor phrasing, rhythm, or word choice improvements.

If a change would improve polish but risks altering meaning, do not apply it.

## Conflict and edge cases

-   **Unwrap‑resistant content:** If wrapping to ≤ 80 characters would break
    URLs, tables, or code‑like structures, leave them unwrapped.

-   **Ambiguous descriptions:** If you cannot safely infer the correct meaning,
    keep the original. For proofreading tasks, label it with a `FIXME`
    comment; otherwise optionally suggest a clearer alternative with a note that
    it may need author review.

-   **Multi‑line tags:** Ensure all lines in a multi‑line tag form a coherent
    paragraph and follow the same style rules.

## `NEWS.md` rules

-   Add a `NEWS.md` bullet for every user-facing change.
-   Do not add bullets for small documentation changes or internal refactors.
-   Each bullet should briefly describe the change for the end user and mention
    the related issue in parentheses.
-   Keep each bullet on one line; do not wrap it.
-   If a change is related to a function, put the function name early in the
    bullet.
-   Order bullets alphabetically by function name, with bullets that do not
    mention function names first.

## Proofreading workflow

When the user asks you to proofread a file:

-   Act as an expert proofreader and editor.
-   Work paragraph by paragraph.
-   Start by making a TODO list with one item for each top-level heading.
-   Fix spelling, grammar and other minor problems without asking.
-   Label unclear, confusing or ambiguous sentences with a `FIXME` comment.
-   Only report what changed.

## Non‑goals

You must not:

-   Change code behavior
-   Add new roxygen2 tags unless the user asks for documentation completeness or
    a user-facing documentation topic is missing
-   Remove existing tags unless they document internal-only code or are clearly
    obsolete
-   Reorder functions or sections
-   Modify non‑comment code except literal user-facing message strings
