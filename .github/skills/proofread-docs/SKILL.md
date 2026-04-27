---
name: proofread-docs
description: |
  Domain expertise for reviewing prose documentation (vignettes, README, etc.).
---

# Skill: Proofread documentation files

## Goal

You proofread and improve **documentation prose** in:

- `vignettes/` (Quarto or R Markdown)
- `README` and other top‑level docs
- Package website content derived from these files

You focus on:

- Correctness
- Clarity
- Consistency with the package's style

You never modify executable code.

---

## Scope

You work on:

- Narrative text
- Section headings
- Lists and callouts
- Captions and inline explanations
- Comments inside code chunks (if present)

In `vignettes/`, when `basename.qmd`/`basename.Rmd` and `basename.qmd.orig`/
`basename.Rmd.orig` exist, review only the `*.orig` file and ignore the
generated non-`.orig` counterpart.

You do **not**:

- Change code examples’ behavior
- Modify data, configuration, or YAML semantics
- Reorganize content structure unless explicitly requested

---

## Global style rules

Apply these rules to all prose:

- **Line length:**\
  Wrap text so lines are **≤ 80 characters**, unless it is:
  - A URL
  - A code block
  - A table or other structured block that would break if wrapped
- **Tone:**\
  Professional, concise, and approachable. Avoid slang and jokes.
- **Sentence case:**\
  Use sentence case for sentences. Start with a capital letter, end with a
  period.
- **No Oxford comma:**\
  Use commas in lists without the Oxford comma.
- **Function and argument names:**\
  Wrap function names and arguments in backticks: `` `my_fun()` ``, `` `x` ``.
- **Package names:**\
  When a package is mentioned in prose, format it as bold markdown, not just inline code. For example: **jsonlite** package.
- **Acronyms:**\
  Use standard uppercase for domain acronyms (CRS, EPSG, WKT, etc.).

---

## Headings and structure

- Use clear, descriptive headings.
- Prefer sentence case for headings.
- Avoid redundant nesting (e.g., “Introduction” inside an “Introduction”).
- Keep heading text concise and informative.

If you see inconsistent heading levels, you may suggest a more coherent
structure, but do not reorder sections unless explicitly requested.

---

## Paragraphs and flow

- Keep paragraphs focused on a single idea.
- Use transitions to connect related paragraphs when needed.
- Avoid long, dense paragraphs; split when they exceed \~5 sentences.

You may:

- Reorder sentences within a paragraph for clarity
- Merge very short, fragmented sentences when it improves flow

---

## Lists

- Use lists for sequences, steps, or grouped items.
- Keep list items parallel in structure (all verbs, all nouns, etc.).
- For bullet items:
  - Start with a capital letter.
  - End with a period if the item is a full sentence.

---

## Callouts

For Quarto or R Markdown callouts (e.g., `::: {.callout-note}`):

- Use them for:
  - Notes
  - Tips
  - Warnings
  - Important caveats
- Wrap text inside callouts to ≤ 80 characters when possible.
- Keep the callout type aligned with its content (do not turn a warning into a
  note).

Do not change the callout type unless the current one is clearly wrong.

---

## Code chunks and inline code

- Do not change code behavior.
- You may:
  - Fix typos in comments inside code chunks
  - Improve surrounding explanatory text
- Do not:
  - Reformat code
  - Add or remove code lines
  - Change chunk options

Inline code in prose (e.g., `` `my_fun()` ``) should be:

- Used for function names, arguments, and short expressions
- Left unwrapped even if it exceeds 80 characters

---

## Tables

- Do not reflow or wrap table rows if it would break the table.
- You may:
  - Improve column headings for clarity
  - Fix typos in cell text
- If a table is very wide, you may suggest splitting it, but do not perform
  the split yourself.

---

## URLs, images, and YAML

- **URLs:**\
  Leave URLs as is, even if they exceed 80 characters. You may suggest
  reference‑style links in comments, but do not enforce them.

- **Images:**\
  You may improve alt text and captions for clarity and accessibility. Do not
  change file paths.

- **YAML headers:**\
  Do not modify YAML semantics. You may fix obvious typos in titles or
  descriptions, but do not rewrap or restructure YAML.

---

## Cross‑document consistency

When possible, keep:

- Terminology consistent across vignettes and README
- Function names and arguments consistently backticked
- Descriptions of the same concept aligned in meaning

If you notice conflicting descriptions of the same feature, you may point it out
and suggest harmonization.

---

## Prioritization

When making changes, prioritize in this order:

1.  **Correctness:** Fix errors that change meaning or mislead the reader.
2.  **Clarity:** Improve confusing or vague wording.
3.  **Consistency:** Align with the package's style and terminology.
4.  **Polish:** Minor phrasing, rhythm, or word choice improvements.

If a change would improve polish but risks altering meaning, do not apply it.

---

## Conflict and edge cases

- **Unwrap‑resistant content:**\
  If wrapping to ≤ 80 characters would break URLs, tables, code blocks, or
  YAML, leave them unwrapped.

- **Ambiguous explanations:**\
  If you cannot safely infer the correct meaning, keep the original and
  optionally suggest a clearer alternative with a note that it may need author
  review.

- **Legacy formats:**\
  If you see `.Rmd` and `.qmd` mixed, you may note the inconsistency, but do
  not suggest migration unless explicitly requested.

---

## Non‑goals

You must not:

- Change code behavior in examples
- Reorganize the document structure
- Add or remove major sections
- Modify build configuration or YAML semantics
