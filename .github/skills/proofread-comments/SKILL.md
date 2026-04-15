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

--------------------------------------------------------------------------------

## Scope

You work only on:

-   roxygen2 comments starting with `#'`
-   Inline comments starting with `#` that are not roxygen2

You do **not**:

-   Edit R code, function bodies, or expressions
-   Change function signatures or arguments
-   Modify tests, data, or non‑comment content

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
-   **No Oxford comma:**\
    Use commas in lists without the Oxford comma.
-   **Function and argument names:**\
    Wrap function names and arguments in backticks: `` `my_fun()` ``, `` `x` ``.
-   **Acronyms:**\
    Use standard uppercase for domain acronyms (CRS, EPSG, WKT, etc.).

--------------------------------------------------------------------------------

## Roxygen2 documentation rules

You improve roxygen2 comments while preserving meaning.

### General

-   **Do:**
    -   Fix grammar, spelling, and punctuation
    -   Improve clarity and flow
    -   Ensure consistent terminology across tags
    -   Wrap text to ≤ 80 characters where possible
-   **Do not:**
    -   Invent behavior not present in the code
    -   Remove important details
    -   Change examples’ semantics

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
    keep the original and optionally suggest a clearer alternative with a note
    that it may need author review.

-   **Multi‑line tags:** Ensure all lines in a multi‑line tag form a coherent
    paragraph and follow the same style rules.

## Non‑goals

You must not:

-   Change code behavior
-   Add new roxygen2 tags
-   Remove existing tags
-   Reorder functions or sections
-   Modify non‑comment code
