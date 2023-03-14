# slice errors if positive and negative indices mixed

    Code
      slice(empty, 1, -1)
    Error <rlang_error>
      Can't compute indices.
      Caused by error:
      ! Can't subset elements with `1`.
      x Negative and positive locations can't be mixed.
      i Subscript `1` has a positive value at location 1.

# slice errors if index is not numeric

    Code
      slice(empty, "a")
    Error <rlang_error>
      i In argument: `"a"`.
      Caused by error:
      ! Can't subset elements with `"a"`.
      x `"a"` must be numeric, not the string "a".

# user errors are correctly labelled

    Code
      slice(df, 1 + "")
    Error <rlang_error>
      i In argument: `1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      slice(group_by(df, x), 1 + "")
    Error <rlang_error>
      i In argument: `1 + ""`.
      i In group 1: `x = 1`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator

# `...` can't be named

    Code
      slice(df, 1, foo = g)
    Error <rlib_error_dots_named>
      Arguments in `...` must be passed by position, not name.
      x Problematic argument:
      * foo = g

# slice_helpers() call get_slice_size()

    Code
      slice_head(df, n = "a")
    Error <rlang_error>
      `n` must be a round number, not the string "a".
    Code
      slice_tail(df, n = "a")
    Error <rlang_error>
      `n` must be a round number, not the string "a".
    Code
      slice_min(df, x, n = "a")
    Error <rlang_error>
      `n` must be a round number, not the string "a".
    Code
      slice_max(df, x, n = "a")
    Error <rlang_error>
      `n` must be a round number, not the string "a".
    Code
      slice_sample(df, n = "a")
    Error <rlang_error>
      `n` must be a round number, not the string "a".

# slice_*() checks that `n=` is explicitly named and ... is empty

    Code
      slice_head(df, 5)
    Error <rlang_error>
      `n` must be explicitly named.
      i Did you mean `slice_head(n = 5)`?
    Code
      slice_tail(df, 5)
    Error <rlang_error>
      `n` must be explicitly named.
      i Did you mean `slice_tail(n = 5)`?
    Code
      slice_min(df, x, 5)
    Error <rlang_error>
      `n` must be explicitly named.
      i Did you mean `slice_min(n = 5)`?
    Code
      slice_max(df, x, 5)
    Error <rlang_error>
      `n` must be explicitly named.
      i Did you mean `slice_max(n = 5)`?
    Code
      slice_sample(df, 5)
    Error <rlang_error>
      `n` must be explicitly named.
      i Did you mean `slice_sample(n = 5)`?

---

    Code
      slice_head(df, 5, 2)
    Error <rlib_error_dots_nonempty>
      `...` must be empty.
      x Problematic arguments:
      * ..1 = 5
      * ..2 = 2
      i Did you forget to name an argument?
    Code
      slice_tail(df, 5, 2)
    Error <rlib_error_dots_nonempty>
      `...` must be empty.
      x Problematic arguments:
      * ..1 = 5
      * ..2 = 2
      i Did you forget to name an argument?
    Code
      slice_min(df, x, 5, 2)
    Error <rlib_error_dots_nonempty>
      `...` must be empty.
      x Problematic arguments:
      * ..1 = 5
      * ..2 = 2
      * ..3 = 5
      * ..4 = 2
      i Did you forget to name an argument?
    Code
      slice_max(df, x, 5, 2)
    Error <rlib_error_dots_nonempty>
      `...` must be empty.
      x Problematic arguments:
      * ..1 = 5
      * ..2 = 2
      * ..3 = 5
      * ..4 = 2
      i Did you forget to name an argument?
    Code
      slice_sample(df, 5, 2)
    Error <rlib_error_dots_nonempty>
      `...` must be empty.
      x Problematic arguments:
      * ..1 = 5
      * ..2 = 2
      i Did you forget to name an argument?

# slice_helper `by` errors use correct error context and correct `by_arg`

    Code
      slice_head(gdf, n = 1, by = x)
    Error <rlang_error>
      Can't supply `by` when `.data` is a grouped data frame.
    Code
      slice_tail(gdf, n = 1, by = x)
    Error <rlang_error>
      Can't supply `by` when `.data` is a grouped data frame.
    Code
      slice_min(gdf, order_by = x, by = x)
    Error <simpleError>
      formal argument "by" matched by multiple actual arguments
    Code
      slice_max(gdf, order_by = x, by = x)
    Error <simpleError>
      formal argument "by" matched by multiple actual arguments
    Code
      slice_sample(gdf, n = 1, by = x)
    Error <rlang_error>
      Can't supply `by` when `.data` is a grouped data frame.

# slice_min/max() check size of `order_by=`

    Code
      slice_min(df, 1:6)
    Error <rlang_error>
      Can't compute indices.
      Caused by error:
      ! `order_by` must have size 10, not size 6.
    Code
      slice_max(df, 1:6)
    Error <rlang_error>
      Can't compute indices.
      Caused by error:
      ! `order_by` must have size 10, not size 6.

