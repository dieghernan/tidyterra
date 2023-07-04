# Return empty geom when no results

    Code
      empty <- drop_na(v)
    Message <cliMessage>
      ! All geometries dropped.
      Returning empty <SpatVector>

---

    Code
      emptycrs <- drop_na(nocrs)
    Message <cliMessage>
      ! All geometries dropped.
      Returning empty <SpatVector>

# Drop na with SpatRaster

    Code
      res <- compare_spatrasters(r, all)
    Message <cliMessage>
      ! Results of `tidyterra::compare_spatrasters()`: 
      The following attributes are not equal:
      * extent

