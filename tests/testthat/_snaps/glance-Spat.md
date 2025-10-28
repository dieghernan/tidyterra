# Glance SpatRasters

    Code
      glance(r)
    Output
      # A tibble: 1 x 16
         nrow  ncol  nlyr ncell  xres  yres     xmin     xmax     ymin     ymax crs   
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl> <chr> 
      1    87   118     3 10266 3881. 3881. -612335. -154347. 4283018. 4620687. World~
      # i 5 more variables: crs_units <chr>, source <chr>, has_rgb <lgl>,
      #   has_colors <lgl>, has_time <lgl>

# Stress SpatRaster

    Code
      glance(v)
    Output
      # A tibble: 1 x 16
         nrow  ncol  nlyr ncell  xres  yres  xmin  xmax  ymin  ymax crs      crs_units
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>    <chr>    
      1   126   212     1 26712 0.025 0.025 -7.08 -1.77  40.1  43.2 lon/lat~ degrees  
      # i 4 more variables: source <chr>, has_rgb <lgl>, has_colors <lgl>,
      #   has_time <lgl>

---

    Code
      glance(v2)
    Output
      # A tibble: 1 x 16
         nrow  ncol  nlyr ncell  xres  yres  xmin  xmax  ymin  ymax crs      crs_units
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>    <chr>    
      1   126   212     1 26712 0.025 0.025 -7.08 -1.78  40.1  43.2 lon/lat~ degrees  
      # i 4 more variables: source <chr>, has_rgb <lgl>, has_colors <lgl>,
      #   has_time <lgl>

---

    Code
      glance(v2)
    Output
      # A tibble: 1 x 15
         nrow  ncol  nlyr ncell  xres  yres  xmin  xmax  ymin  ymax crs         source
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>       <chr> 
      1   126   212     1 26712 0.025 0.025 -7.08 -1.78  40.1  43.2 Cartesian ~ NA    
      # i 3 more variables: has_rgb <lgl>, has_colors <lgl>, has_time <lgl>

---

    Code
      glance(empt)
    Output
      # A tibble: 1 x 16
         nrow  ncol  nlyr ncell    xres    yres      xmin    xmax    ymin   ymax crs  
        <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>     <dbl>   <dbl>   <dbl>  <dbl> <chr>
      1     9     4     2    36 987385. 987385. -2361760.  1.59e6 -7.82e6 1.07e6 Worl~
      # i 5 more variables: crs_units <chr>, source <chr>, has_rgb <lgl>,
      #   has_colors <lgl>, has_time <lgl>

# RGB SpatRaster

    Code
      glance(v)
    Output
      # A tibble: 1 x 16
         nrow  ncol  nlyr ncell  xres  yres     xmin     xmax     ymin     ymax crs   
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl> <chr> 
      1   212   261     3 55332 2446. 2446. -812067. -173665. 4852834. 5371383. WGS 8~
      # i 5 more variables: crs_units <chr>, source <chr>, has_rgb <lgl>,
      #   has_colors <lgl>, has_time <lgl>

# Coltab SpatRaster

    Code
      glance(v)
    Output
      # A tibble: 1 x 16
         nrow  ncol  nlyr  ncell  xres  yres     xmin     xmax     ymin     ymax crs  
        <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl> <chr>
      1   470   590     1 277300  1000  1000 -787641. -197641. 4878097. 5348097. WGS ~
      # i 5 more variables: crs_units <chr>, source <chr>, has_rgb <lgl>,
      #   has_colors <lgl>, has_time <lgl>

---

    Code
      glance(end)
    Output
      # A tibble: 1 x 16
         nrow  ncol  nlyr  ncell  xres  yres     xmin     xmax     ymin     ymax crs  
        <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl> <chr>
      1   470   590     2 277300  1000  1000 -787641. -197641. 4878097. 5348097. WGS ~
      # i 5 more variables: crs_units <chr>, source <chr>, has_rgb <lgl>,
      #   has_colors <lgl>, has_time <lgl>

---

    Code
      glance(twocoltabs)
    Output
      # A tibble: 1 x 16
         nrow  ncol  nlyr  ncell  xres  yres     xmin     xmax     ymin     ymax crs  
        <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl> <chr>
      1   470   590     3 277300  1000  1000 -787641. -197641. 4878097. 5348097. WGS ~
      # i 5 more variables: crs_units <chr>, source <chr>, has_rgb <lgl>,
      #   has_colors <lgl>, has_time <lgl>

# Time SpatRaster

    Code
      glance(v)
    Output
      # A tibble: 1 x 16
         nrow  ncol  nlyr ncell  xres  yres  xmin  xmax  ymin  ymax crs      crs_units
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>    <chr>    
      1   126   212     1 26712 0.025 0.025 -7.08 -1.77  40.1  43.2 lon/lat~ degrees  
      # i 4 more variables: source <chr>, has_rgb <lgl>, has_colors <lgl>,
      #   has_time <lgl>

---

    Code
      glance(v)
    Output
      # A tibble: 1 x 16
         nrow  ncol  nlyr ncell  xres  yres  xmin  xmax  ymin  ymax crs      crs_units
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>    <chr>    
      1   126   212     1 26712 0.025 0.025 -7.08 -1.77  40.1  43.2 lon/lat~ degrees  
      # i 4 more variables: source <chr>, has_rgb <lgl>, has_colors <lgl>,
      #   has_time <lgl>

# NA crs

    Code
      glance(r)
    Output
      # A tibble: 1 x 16
         nrow  ncol  nlyr ncell  xres  yres  xmin  xmax  ymin  ymax crs      crs_units
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>    <lgl>    
      1   180   360     1 64800     1     1  -180   180   -90    90 CRS: No~ NA       
      # i 4 more variables: source <chr>, has_rgb <lgl>, has_colors <lgl>,
      #   has_time <lgl>

---

    Code
      glance(v)
    Output
      # A tibble: 1 x 10
        geometry  nrow  ncol  xmin  xmax  ymin  ymax source crs              crs_units
        <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>  <chr>            <lgl>    
      1 points   64800     1 -180.  180. -89.5  89.5 NA     CRS: Not Define~ NA       

---

    Code
      glance(r1)
    Output
      # A tibble: 1 x 16
         nrow  ncol  nlyr ncell  xres  yres  xmin  xmax  ymin  ymax crs      crs_units
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>    <lgl>    
      1     4     3     1    12     1     1     0     3     0     4 CRS: No~ NA       
      # i 4 more variables: source <chr>, has_rgb <lgl>, has_colors <lgl>,
      #   has_time <lgl>

# Glance SpatVectors

    Code
      glance(v)
    Output
      # A tibble: 1 x 10
        geometry  nrow  ncol     xmin     xmax     ymin    ymax source crs   crs_units
        <chr>    <dbl> <dbl>    <dbl>    <dbl>    <dbl>   <dbl> <chr>  <chr> <chr>    
      1 polygons     9     3 2892687. 3341372. 2017622.  2.36e6 cyl.g~ ETRS~ meter    

# Stress SpatVector

    Code
      glance(v)
    Output
      # A tibble: 1 x 10
        geometry  nrow  ncol     xmin     xmax     ymin    ymax source crs   crs_units
        <chr>    <dbl> <dbl>    <dbl>    <dbl>    <dbl>   <dbl> <chr>  <chr> <chr>    
      1 polygons     9     3 2892687. 3341372. 2017622.  2.36e6 cyl.g~ ETRS~ meter    

---

    Code
      glance(v2)
    Output
      # A tibble: 1 x 10
        geometry  nrow  ncol  xmin  xmax  ymin  ymax source crs              crs_units
        <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>  <chr>            <chr>    
      1 polygons     9     3 -7.08 -1.78  40.1  43.2 NA     lon/lat WGS 84 ~ degrees  

---

    Code
      glance(v2)
    Output
      # A tibble: 1 x 9
        geometry  nrow  ncol  xmin  xmax  ymin  ymax source crs              
        <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>  <chr>            
      1 polygons     9     3 -7.08 -1.78  40.1  43.2 NA     Cartesian (Meter)

---

    Code
      glance(vnull)
    Output
      # A tibble: 1 x 10
        geometry  nrow  ncol     xmin     xmax     ymin    ymax source crs   crs_units
        <chr>    <dbl> <dbl>    <dbl>    <dbl>    <dbl>   <dbl> <chr>  <chr> <chr>    
      1 polygons     9     0 2892687. 3341372. 2017622.  2.36e6 cyl.g~ ETRS~ meter    

# Geometries SpatVector

    Code
      glance(v)
    Output
      # A tibble: 1 x 10
        geometry  nrow  ncol     xmin     xmax     ymin    ymax source crs   crs_units
        <chr>    <dbl> <dbl>    <dbl>    <dbl>    <dbl>   <dbl> <chr>  <chr> <chr>    
      1 polygons     9     3 2892687. 3341372. 2017622.  2.36e6 cyl.g~ ETRS~ meter    

---

    Code
      glance(l)
    Output
      # A tibble: 1 x 10
        geometry  nrow  ncol     xmin     xmax     ymin    ymax source crs   crs_units
        <chr>    <dbl> <dbl>    <dbl>    <dbl>    <dbl>   <dbl> <chr>  <chr> <chr>    
      1 lines        9     3 2892687. 3341372. 2017622.  2.36e6 cyl.g~ ETRS~ meter    

---

    Code
      glance(p)
    Output
      # A tibble: 1 x 10
        geometry  nrow  ncol     xmin     xmax     ymin    ymax source crs   crs_units
        <chr>    <dbl> <dbl>    <dbl>    <dbl>    <dbl>   <dbl> <chr>  <chr> <chr>    
      1 points    1492     3 2892687. 3341372. 2017622.  2.36e6 NA     ETRS~ meter    

