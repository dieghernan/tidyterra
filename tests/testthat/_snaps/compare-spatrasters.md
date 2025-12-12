# Error

    Code
      compare_spatrasters(x, terra::crs(x))
    Condition
      Error in `compare_spatrasters()`:
      ! `x` and `y` must be <SpatRaster>s. `x` is <SpatRaster>, `y` is <character>

---

    Code
      compare_spatrasters(1, "a")
    Condition
      Error in `compare_spatrasters()`:
      ! `x` and `y` must be <SpatRaster>s. `x` is <numeric>, `y` is <character>

# Different crs

    Code
      res <- compare_spatrasters(x, y)
    Message
      ! Results of `tidyterra::compare_spatrasters()`: 
      The following attributes are not equal:
      * crs

# Different extent

    Code
      res <- compare_spatrasters(x, y)
    Message
      ! Results of `tidyterra::compare_spatrasters()`: 
      The following attributes are not equal:
      * extent

# Different resolution

    Code
      res <- compare_spatrasters(x, y)
    Message
      ! Results of `tidyterra::compare_spatrasters()`: 
      The following attributes are not equal:
      * resolution

# All different

    Code
      res <- compare_spatrasters(x, y)
    Message
      ! Results of `tidyterra::compare_spatrasters()`: 
      The following attributes are not equal:
      * crs
      * extent
      * resolution

