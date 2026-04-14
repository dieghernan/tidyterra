---
name: proofread-comments
description: Review the roxygen2 and R comments of the source of this R package
---

## Purpose

This skill enables the agent to act as an expert reviewer of **roxygen2
comments** and **regular R comments** in R package source files.

The agent improves clarity, grammar, consistency, style, and roxygen2 best
practices while ensuring that **no executable code is ever modified**.

This skill is designed to work together with the `review-comments` agent.

--------------------------------------------------------------------------------

## What the Skill Does

When invoked, the agent:

1.  Explores the package (especially the `R/` directory) and identifies all
    roxygen2 and regular comments.
2.  Reviews them for clarity, grammar, consistency, and adherence to project
    style.
3.  Classifies each suggestion as **Critical**, **Major**, or **Minor**.
4.  Provides clear "Current" vs "Suggested" versions for every change.
5.  Outputs findings in a structured, easy-to-review format.
6.  Waits for explicit user confirmation before applying any edits.

--------------------------------------------------------------------------------

## What the Skill Must Not Do

The agent must **never**:

-   Modify executable R code, logic, pipes, or assignments
-   Change function signatures, default values, or parameter order
-   Add or remove roxygen2 tags (`@param`, `@return`, `@export`, etc.) unless
    explicitly instructed
-   Rewrite `@examples` in a way that changes their behavior
-   Exceed 80 characters per line
-   Alter indentation or structure outside of comment text
-   Apply any changes without explicit user approval

--------------------------------------------------------------------------------

## Review Criteria

### 1. Clarity

-   Comments must be easy to understand and unambiguous.
-   Descriptions must accurately reflect the actual function behavior.
-   Avoid vague or overly generic language.

### 2. Grammar and Style

-   Correct grammar, spelling, and punctuation.
-   Use sentence case for titles and first lines.
-   Avoid the Oxford comma.
-   Prefer commas over semicolons.
-   Maintain a concise, professional, and friendly tone.

### 3. Consistency

-   Use consistent terminology across the entire package.
-   Parameter descriptions should match function arguments.
-   Similar functions should have similarly structured documentation.

### 4. roxygen2-Specific Requirements

-   `@param` entries must correspond exactly to function parameters.
-   `@return` must clearly describe the output type and content.
-   Prefer `@inheritParams` and `@inheritDotParams` when appropriate.
-   `@examples` must be syntactically correct (wrap risky ones in `\dontrun{}`
    if needed).
-   Use proper markdown formatting inside roxygen blocks.
-   Title and description format (sentence case, no trailing period in titles)
-   Proper use of backticks, cross-links `[fun()]`, and package names
    `\pkg{pkg}`

### 5. Formatting Rules

-   Maximum **80 characters per line**.
-   Preserve all existing indentation and comment structure.
-   Remove redundant or obvious comments that restate the code.

--------------------------------------------------------------------------------

## Roxygen2 & Comment Guidelines (Project Specific)

-   Follow tidyverse documentation rules strictly, **except**:
    -   **Do not use the Oxford comma (serial comma)** anywhere. Remove it when
        found.
    -   Function titles: sentence case, **no trailing period**.
    -   Descriptions, parameters, and details should start with a capital letter
        and end with a full stop.
    -   Use `[function()]` for cross-links, `` `argument` `` for arguments,
        `\pkg{pkg}` for package names.
    -   Prefer sentence case everywhere.
-   Start `@param` descriptions with a verb or clear type indication when
    helpful.
-   Keep `@return` concise but informative (what the object is + content).
-   Prefer `@inheritParams` and `@inheritDotParams` when appropriate.
-   Wrap potentially slow or side-effect examples in `\dontrun{}`.
-   Prefer commas over semicolons in prose.
-   Keep a professional, clear, and friendly tone.
-   Remove redundant comments that restate obvious code.

## Additional Best Practices

-   Ensure all exported functions have complete `@param`, `@return`, and
    `@examples` (when appropriate).
-   Use proper markdown formatting inside roxygen blocks (lists, code, links).
-   Maintain consistency across the package in terminology and style.
-   Do not add new `@export` or similar structural tags.

## Behavior in Ambiguous Situations

-   When the existing style deviates from tidyverse conventions (except for the
    Oxford comma rule), politely note it and suggest alignment unless the
    maintainer has explicitly chosen a different style.
-   Actively remove or avoid the Oxford comma in all suggestions.
-   Favor clarity, friendliness, and consistency.
-   Always highlight excellent parts of the documentation, not just problems.
-   Ask the user for clarification on style preferences when needed.

--------------------------------------------------------------------------------

## Issue Severity Levels

-   **Critical** — misleading, incorrect, inconsistent, or missing essential
    documentation that could confuse users or developers.
-   **Major** — unclear, poorly written, or confusing text that significantly
    reduces readability.
-   **Minor** — stylistic, grammar, formatting, or optional improvements.

--------------------------------------------------------------------------------

## Expected Output Format

The agent must output for each file:

``` markdown
### File: `R/somefile.R`

**Summary**: X critical, Y important, Z polish suggestions.

#### Suggestions:

1. **Critical/Important/Polish** - Lines XXX-YYY

   **Current:**

     #' current text

   ## Suggested:

     #' improved text

   ## Reason:

   Brief explanation.
```

At the end of the review, always ask:

**Would you like me to apply these changes? (Yes / No / Only specific ones)**

--------------------------------------------------------------------------------

## Examples

### Good roxygen2

``` r
#' Normalize a numeric vector
#'
#' @param x Numeric vector to normalize.
#' @return A numeric vector scaled to the [0, 1] range.
#' @examples
#' normalize(c(1, 2, 3))
```

### Poor roxygen2

``` r
#' Normalize stuff
#' @param x The data
#' @return Something normalized
```

### Good regular comment

``` r
# Compute the scaling factor for normalization
```

### Poor regular comment

``` r
# do stuff here
```

## Behavior Summary

When this skill is active, the agent focuses exclusively on improving
documentation quality and consistency while maintaining absolute safety
regarding the underlying code.

It works as a precise, reliable partner to the `review-comments` agent.
