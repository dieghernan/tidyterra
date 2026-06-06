# Return empty geom when no results

    Code
      empty <- drop_na(v)
    Message
      ! All geometries dropped.
      Returning an empty <SpatVector>.

---

    Code
      emptycrs <- drop_na(nocrs)
    Message
      ! All geometries dropped.
      Returning an empty <SpatVector>.

# Drop na with SpatRaster

    Code
      res <- compare_spatrasters(r, all)
    Message
      ! Results of `tidyterra::compare_spatrasters()`: 
      The following attributes are not equal:
      * extent

