# SpatRaster mutate and check names

    Code
      fixed_names <- dplyr::mutate(spatrast, b2 = a + 100)
    Message <cliMessage>
      i Layer(s) with duplicated/reserved names detected. See `About layer/column names` section on `?as_tibble.SpatRaster` 
      ! Renaming columns:
    Message <simpleMessage>
      New column names:
      `a` -> `a.1`
      
      

---

    Code
      fixed_names2 <- dplyr::mutate(spatrast, b2 = x.1 + x.2 + y.1)
    Message <cliMessage>
      i Layer(s) with duplicated/reserved names detected. See `About layer/column names` section on `?as_tibble.SpatRaster` 
      ! Renaming columns:
    Message <simpleMessage>
      New column names:
      `x` -> `x.1`
      `x` -> `x.2`
      `y` -> `y.1`
      
      

