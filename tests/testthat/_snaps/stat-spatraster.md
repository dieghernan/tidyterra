# stat_spatraster rejects invalid inputs

    Code
      writeLines(conditionMessage(err))
    Output
      argument "data" is missing, with no default

---

    Code
      ggplot() + stat_spatraster(data = v)
    Condition
      Error in `stat_spatraster()`:
      ! `tidyterra::stat_spatraster()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::rast()`.

---

    Code
      ggplot() + stat_spatraster(data = 1:3)
    Condition
      Error in `stat_spatraster()`:
      ! `tidyterra::stat_spatraster()` only works with <SpatRaster> objects, not <integer>. See `?terra::rast()`.

# stat_spatraster facets categorical layers with duplicated names

    Code
      layer <- stat_spatraster(data = r)
    Message
      i Layer(s) with duplicated or reserved names detected. See About layer/column
      names section on `tidyterra::as_tibble.SpatRaster()`
      ! Renaming columns and layers:
    Output
      * `passes` -> `passes.1`
      * `passes` -> `passes.2`

# stat_spatraster maps repaired names without name collisions

    Code
      layer <- stat_spatraster(data = r, aes(fill = layer.2, alpha = layer.1))
    Message
      i Layer(s) with duplicated or reserved names detected. See About layer/column
      names section on `tidyterra::as_tibble.SpatRaster()`
      ! Renaming columns and layers:
    Output
      * `layer` -> `layer.2`

