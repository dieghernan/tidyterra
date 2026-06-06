# group_modify() binds SpatVector group results

    Code
      group_modify(v, ~ mutate(.x, key = .y$grp))
    Condition
      Error in `group_modify()`:
      ! `group_modify.SpatVector()` requires `.f` to return <SpatVector> objects.

