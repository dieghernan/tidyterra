# Glimpse SpatVectors

    Code
      glimpse(v)
    Output
      #  A SpatVector 9 x 3
      #  Geometry type: Polygons
      #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
      
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES~
      $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
      $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S~

---

    Code
      glimpse(v, geom = "WKT", width = 50)
    Output
      #  A SpatVector 9 x 3
      #  Geometry type: Polygons
      #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
      
      $ iso2     <chr> "ES-AV", "ES-BU", "ES-LE", "ES-~
      $ cpro     <chr> "05", "09", "24", "34", "37", "~
      $ name     <chr> "Avila", "Burgos", "Leon", "Pal~
      $ geometry <chr> "POLYGON ((3126360.2417 2066777~

---

    Code
      glimpse(v, width = 50, n = 2)
    Output
      #  A SpatVector 9 x 3
      #  Geometry type: Polygons
      #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
      
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", ~
      $ cpro <chr> "05", "09", "24", "34", "37", "40",~
      # i 1 more variable : name <chr>
      # i Use `tidyterra::glimpse(n = ...)` to see more columns

---

    Code
      glimpse(v, width = 50, n = 1)
    Output
      #  A SpatVector 9 x 3
      #  Geometry type: Polygons
      #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
      
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", ~
      # i 2 more variables : cpro <chr>, name <chr>
      # i Use `tidyterra::glimpse(n = ...)` to see more columns

# Stress SpatVector

    Code
      inv <- glimpse(v)
    Output
      #  A SpatVector 9 x 3
      #  Geometry type: Polygons
      #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
      
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES~
      $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
      $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S~

---

    Code
      inv <- glimpse(v2)
    Output
      #  A SpatVector 9 x 3
      #  Geometry type: Polygons
      #  Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
      #  Extent (x / y) : ([7° 4' 36.6" W / 1° 46' 58.08" W] , [40° 5' 7.8" N / 43° 14' 11.57" N])
      
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES~
      $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
      $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S~

---

    Code
      inv <- glimpse(v2)
    Output
      #  A SpatVector 9 x 3
      #  Geometry type: Polygons
      #  Projected CRS: Cartesian (Meter)
      #  Extent (x / y) : ([-7.076834 / -1.782799] , [40.085499 / 43.236547])
      
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES~
      $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
      $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S~

---

    Code
      inv <- glimpse(vnull)
    Output
      #  A SpatVector 9 x 0
      #  Geometry type: Polygons
      #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
      
      SpatVector with no attributes (only geometries)

# Geometries SpatVector

    Code
      glimpse(v)
    Output
      #  A SpatVector 9 x 3
      #  Geometry type: Polygons
      #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
      
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES~
      $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
      $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S~

---

    Code
      glimpse(l)
    Output
      #  A SpatVector 9 x 3
      #  Geometry type: Lines
      #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
      
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES~
      $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
      $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S~

---

    Code
      glimpse(p)
    Output
      #  A SpatVector 1,492 x 3
      #  Geometry type: Points
      #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
      
      $ iso2 <chr> "ES-AV", "ES-AV", "ES-AV", "ES-AV", "ES-AV", "ES-AV", "ES-AV", "E~
      $ cpro <chr> "05", "05", "05", "05", "05", "05", "05", "05", "05", "05", "05",~
      $ name <chr> "Avila", "Avila", "Avila", "Avila", "Avila", "Avila", "Avila", "A~

# Glimpse SpatRasters

    Code
      glimpse(r)
    Output
      #  A SpatRaster 87 x 118 x 3 layers (10,266 cells)
      #  Resolution (x / y): (3,881.255 / 3,881.255)
      #  Projected CRS: World_Robinson
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([ -612,335.4 /  -154,347.3] , [4,283,017.7 / 4,620,686.9])
      
      $ tavg_04 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
      $ tavg_05 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
      $ tavg_06 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~

---

    Code
      glimpse(r, xy = TRUE, width = 50)
    Output
      #  A SpatRaster 87 x 118 x 3 layers (10,266 cells)
      #  Resolution (x / y): (3,881.255 / 3,881.255)
      #  Projected CRS: World_Robinson
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([ -612,335.4 /  -154,347.3] , [4,283,017.7 / 4,620,686.9])
      
      $ tavg_04 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, ~
      $ tavg_05 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, ~
      $ tavg_06 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, ~

---

    Code
      glimpse(r, width = 50, n = 1)
    Output
      #  A SpatRaster 87 x 118 x 3 layers (10,266 cells)
      #  Resolution (x / y): (3,881.255 / 3,881.255)
      #  Projected CRS: World_Robinson
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([ -612,335.4 /  -154,347.3] , [4,283,017.7 / 4,620,686.9])
      
      $ tavg_04 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, ~
      # i 2 more layers : tavg_05 <dbl>, tavg_06 <dbl>
      # i Use `tidyterra::glimpse(n = ...)` to see more layers

---

    Code
      glimpse(r, width = 50, n = 2)
    Output
      #  A SpatRaster 87 x 118 x 3 layers (10,266 cells)
      #  Resolution (x / y): (3,881.255 / 3,881.255)
      #  Projected CRS: World_Robinson
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([ -612,335.4 /  -154,347.3] , [4,283,017.7 / 4,620,686.9])
      
      $ tavg_04 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, ~
      $ tavg_05 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, ~
      # i 1 more layer : tavg_06 <dbl>
      # i Use `tidyterra::glimpse(n = ...)` to see more layers

# Stress SpatRaster

    Code
      inv <- glimpse(v)
    Output
      #  A SpatRaster 126 x 212 x 1 layer (26,712 cells)
      #  Resolution (x / y): (1' 30" , 1' 30")
      #  Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
      #  Extent (x / y) : ([7° 4' 30" W / 1° 46' 30" W] , [40° 4' 60" N / 43° 13' 60" N])
      
      $ elevation_m <dbl> 700.2969, 780.3889, 706.1250, 568.9722, 584.9028, 506.7361~

---

    Code
      inv <- glimpse(v2)
    Output
      #  A SpatRaster 126 x 212 x 1 layer (26,712 cells)
      #  Resolution (x / y): (1' 30" , 1' 30")
      #  Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
      #  Extent (x / y) : ([7° 4' 30" W / 1° 46' 30" W] , [40° 4' 60" N / 43° 13' 60" N])
      
      $ elevation_m <dbl> 700.2969, 780.3889, 706.1250, 568.9722, 584.9028, 506.7361~

---

    Code
      inv <- glimpse(v2)
    Output
      #  A SpatRaster 126 x 212 x 1 layer (26,712 cells)
      #  Resolution (x / y): (0.025 / 0.025)
      #  Projected CRS: Cartesian (Meter)
      #  Extent (x / y) : ([-7.07500 / -1.77500] , [40.08333 / 43.23333])
      
      $ elevation_m <dbl> 700.2969, 780.3889, 706.1250, 568.9722, 584.9028, 506.7361~

---

    Code
      inv <- glimpse(empt)
    Output
      #  A SpatRaster 9 x 4 x 2 layers (36 cells)
      #  Resolution (x / y): (987,385.4 / 987,385.4)
      #  Projected CRS: World_Robinson (ESRI:54030)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([-2,361,760 /  1,587,782] , [-7,816,949 /  1,069,519])
      
      SpatRaster with no values

# RGB SpatRaster

    Code
      inv <- glimpse(v)
    Output
      #  A SpatRaster 212 x 261 x 3 layers (55,332 cells)
      #  Resolution (x / y): (2,445.985 / 2,445.985)
      #  Projected CRS: WGS 84 / Pseudo-Mercator (EPSG:3857)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([ -812,067.0 /  -173,664.9] , [4,852,834.1 / 5,371,382.9])
      #  SpatRaster with 3 RGB channels: cyl_tile_1 (Red), cyl_tile_2 (Green), cyl_tile_3 (Blue)
      
      $ cyl_tile_1 <int> 217, 234, 217, 217, 242, 200, 197, 197, 217, 209, 213, 205,~
      $ cyl_tile_2 <int> 229, 235, 229, 229, 239, 153, 180, 187, 229, 226, 228, 225,~
      $ cyl_tile_3 <int> 206, 224, 206, 206, 233, 169, 194, 193, 206, 198, 202, 194,~

---

    Code
      inv <- glimpse(v)
    Output
      #  A SpatRaster 212 x 261 x 3 layers (55,332 cells)
      #  Resolution (x / y): (2,445.985 / 2,445.985)
      #  Projected CRS: WGS 84 / Pseudo-Mercator (EPSG:3857)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([ -812,067.0 /  -173,664.9] , [4,852,834.1 / 5,371,382.9])
      #  SpatRaster with 3 RGB channels: cyl_tile_2 (Red), cyl_tile_3 (Green), cyl_tile_1 (Blue)
      
      $ cyl_tile_1 <int> 217, 234, 217, 217, 242, 200, 197, 197, 217, 209, 213, 205,~
      $ cyl_tile_2 <int> 229, 235, 229, 229, 239, 153, 180, 187, 229, 226, 228, 225,~
      $ cyl_tile_3 <int> 206, 224, 206, 206, 233, 169, 194, 193, 206, 198, 202, 194,~

---

    Code
      inv <- glimpse(v)
    Output
      #  A SpatRaster 212 x 261 x 4 layers (55,332 cells)
      #  Resolution (x / y): (2,445.985 / 2,445.985)
      #  Projected CRS: WGS 84 / Pseudo-Mercator (EPSG:3857)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([ -812,067.0 /  -173,664.9] , [4,852,834.1 / 5,371,382.9])
      #  SpatRaster with 4 RGB channels: aa (Red), cyl_tile_3 (Green), cyl_tile_2 (Blue), cyl_tile_1 (Alpha)
      
      $ cyl_tile_1 <int> 217, 234, 217, 217, 242, 200, 197, 197, 217, 209, 213, 205,~
      $ cyl_tile_2 <int> 229, 235, 229, 229, 239, 153, 180, 187, 229, 226, 228, 225,~
      $ cyl_tile_3 <int> 206, 224, 206, 206, 233, 169, 194, 193, 206, 198, 202, 194,~
      $ aa         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,~

# Coltab SpatRaster

    Code
      inv <- glimpse(v)
    Output
      #  A SpatRaster 470 x 590 x 1 layer (277,300 cells)
      #  Resolution (x / y): (1,000 / 1,000)
      #  Projected CRS: WGS 84 / Pseudo-Mercator (EPSG:3857)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([ -787,640.9 /  -197,640.9] , [4,878,097.0 / 5,348,097.0])
      #  SpatRaster with 1 color table in: era
      
      $ era <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~

---

    Code
      inv <- glimpse(end)
    Output
      #  A SpatRaster 470 x 590 x 2 layers (277,300 cells)
      #  Resolution (x / y): (1,000 / 1,000)
      #  Projected CRS: WGS 84 / Pseudo-Mercator (EPSG:3857)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([ -787,640.9 /  -197,640.9] , [4,878,097.0 / 5,348,097.0])
      #  SpatRaster with 1 color table in: era
      
      $ nocoltab <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~
      $ era      <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~

---

    Code
      inv <- glimpse(twocoltabs)
    Output
      #  A SpatRaster 470 x 590 x 3 layers (277,300 cells)
      #  Resolution (x / y): (1,000 / 1,000)
      #  Projected CRS: WGS 84 / Pseudo-Mercator (EPSG:3857)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([ -787,640.9 /  -197,640.9] , [4,878,097.0 / 5,348,097.0])
      #  SpatRaster with 2 color tables in: era, ihaveacoltab
      
      $ nocoltab     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
      $ era          <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
      $ ihaveacoltab <fct> S, W, S, S, W, S, S, W, S, S, W, S, S, W, S, S, W, S, S, ~

---

    Code
      glimpse(terra::subset(twocoltabs, c(1, 3, 2)))
    Output
      #  A SpatRaster 470 x 590 x 3 layers (277,300 cells)
      #  Resolution (x / y): (1,000 / 1,000)
      #  Projected CRS: WGS 84 / Pseudo-Mercator (EPSG:3857)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([ -787,640.9 /  -197,640.9] , [4,878,097.0 / 5,348,097.0])
      #  SpatRaster with 2 color tables in: ihaveacoltab, era
      
      $ nocoltab     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
      $ ihaveacoltab <fct> S, W, S, S, W, S, S, W, S, S, W, S, S, W, S, S, W, S, S, ~
      $ era          <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~

# Test formats

    Code
      decimal_to_degrees(-79.89, "lon")
    Output
      [1] "79° 53' 24\" W"

---

    Code
      decimal_to_degrees(-79.89, "lat")
    Output
      [1] "79° 53' 24\" S"

---

    Code
      decimal_to_degrees(79.89, "lon")
    Output
      [1] "79° 53' 24\" E"

---

    Code
      decimal_to_degrees(79.89, "lat")
    Output
      [1] "79° 53' 24\" N"

# NA crs

    Code
      glimpse(r)
    Output
      #  A SpatRaster 180 x 360 x 1 layer (64,800 cells)
      #  Resolution (x / y): (1 / 1)
      #  CRS: Not Defined / Empty
      #  Extent (x / y) : ([-180 /  180] , [ -90 /   90])
      
      $ lyr.1 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~

---

    Code
      glimpse(v)
    Output
      #  A SpatVector 64,800 x 1
      #  Geometry type: Points
      #  CRS: Not Defined / Empty
      #  Extent (x / y) : ([-179.5 /  179.5] , [ -89.5 /   89.5])
      
      $ lyr.1 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~

# Long geoms

    Code
      glimpse(a_rast, n = NULL, max_extra_cols = NULL)
    Output
      #  A SpatRaster 20 x 20 x 31 layers (400 cells)
      #  Resolution (x / y): (18 /  9)
      #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([-180 /  180] , [ -90 /   90])
      
      $ initial_name   <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_a  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_b  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_c  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_d  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_e  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_f  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_g  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_h  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_i  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_j  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_k  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_l  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_m  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_n  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname1_o  <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname2__a <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname2__b <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname2__c <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      $ A_longname2__d <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1~
      # i 11 more layers : A_longname2__e <dbl>, ...
      # i Use `tidyterra::glimpse(n = ...)` to see more layers

---

    Code
      glimpse(a_rast, n = -1, max_extra_cols = -1)
    Output
      #  A SpatRaster 20 x 20 x 31 layers (400 cells)
      #  Resolution (x / y): (18 /  9)
      #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      #  CRS projection units: meter <m>
      #  Extent (x / y) : ([-180 /  180] , [ -90 /   90])
      
      $ initial_name <dbl> 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 100~
      # i 30 more layers : A_longname1_a <dbl>, ...
      # i Use `tidyterra::glimpse(n = ...)` to see more layers

