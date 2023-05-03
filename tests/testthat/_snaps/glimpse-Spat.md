# Glimpse SpatVectors

    Code
      glimpse(v)
    Output
      Geometry type: Polygons
      Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      CRS projection units: m
      Extent (x , y) : [2,892,687 - 3,341,372] , [2,017,622 - 2,361,600]
      Rows: 9
      Columns: 3
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES~
      $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
      $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S~

---

    Code
      glimpse(v, geom = "WKT", width = 50)
    Output
      Geometry type: Polygons
      Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      CRS projection units: m
      Extent (x , y) : [2,892,687 - 3,341,372] , [2,017,622 - 2,361,600]
      Rows: 9
      Columns: 4
      $ iso2     <chr> "ES-AV", "ES-BU", "ES-LE", "ES-~
      $ cpro     <chr> "05", "09", "24", "34", "37", "~
      $ name     <chr> "Avila", "Burgos", "Leon", "Pal~
      $ geometry <chr> "POLYGON ((3126360.2417 2066777~

# Stress SpatVector

    Code
      inv <- glimpse(v)
    Output
      Geometry type: Polygons
      Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      CRS projection units: m
      Extent (x , y) : [2,892,687 - 3,341,372] , [2,017,622 - 2,361,600]
      Rows: 9
      Columns: 3
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES~
      $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
      $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S~

---

    Code
      inv <- glimpse(v2)
    Output
      Geometry type: Polygons
      Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
      Extent (x , y) : [7° 4' 36.6024" W - 1° 46' 58.078" W] , [40° 5' 7.7968" N - 43° 14' 11.5677" N]
      Rows: 9
      Columns: 3
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES~
      $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
      $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S~

---

    Code
      inv <- glimpse(v2)
    Output
      Geometry type: Polygons
      Projected CRS: Cartesian (Meter)
      Extent (x , y) : [-7.076834 - -1.782799] , [40.085499 - 43.236547]
      Rows: 9
      Columns: 3
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES~
      $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
      $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S~

# Geometries SpatVector

    Code
      glimpse(v)
    Output
      Geometry type: Polygons
      Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      CRS projection units: m
      Extent (x , y) : [2,892,687 - 3,341,372] , [2,017,622 - 2,361,600]
      Rows: 9
      Columns: 3
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES~
      $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
      $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S~

---

    Code
      glimpse(l)
    Output
      Geometry type: Lines
      Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      CRS projection units: m
      Extent (x , y) : [2,892,687 - 3,341,372] , [2,017,622 - 2,361,600]
      Rows: 9
      Columns: 3
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES~
      $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
      $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S~

---

    Code
      glimpse(p)
    Output
      Geometry type: Points
      Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
      CRS projection units: m
      Extent (x , y) : [2,892,687 - 3,341,372] , [2,017,622 - 2,361,600]
      Rows: 1,492
      Columns: 3
      $ iso2 <chr> "ES-AV", "ES-AV", "ES-AV", "ES-AV", "ES-AV", "ES-AV", "ES-AV", "E~
      $ cpro <chr> "05", "05", "05", "05", "05", "05", "05", "05", "05", "05", "05",~
      $ name <chr> "Avila", "Avila", "Avila", "Avila", "Avila", "Avila", "Avila", "A~

# Glimpse SpatRasters

    Code
      glimpse(r)
    Output
      Raster Rows: 87
      Raster Columns: 118
      Raster Layers: 3
      Raster Cells: 10,266
      Raster Resolution (x , y): 3,881.255 , 3,881.255
      Projected CRS: World_Robinson
      CRS projection units: m
      Extent (x , y) : [ -612,335.4 -  -154,347.3] , [4,283,017.7 - 4,620,686.9]
      Layers:
      $ tavg_04 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
      $ tavg_05 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
      $ tavg_06 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~

---

    Code
      glimpse(r, xy = TRUE, width = 50)
    Output
      Raster Rows: 87
      Raster Columns: 118
      Raster Layers: 3
      Raster Cells: 10,266
      Raster Resolution (x , y): 3,881.255 , 3,881.255
      Projected CRS: World_Robinson
      CRS projection units: m
      Extent (x , y) : [ -612,335.4 -  -154,347.3] , [4,283,017.7 - 4,620,686.9]
      Layers:
      $ x       <dbl> -610394.8, -606513.5, -602632.2,~
      $ y       <dbl> 4618746, 4618746, 4618746, 46187~
      $ tavg_04 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, ~
      $ tavg_05 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, ~
      $ tavg_06 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, ~

# Stress SpatRaster

    Code
      inv <- glimpse(v)
    Output
      Raster Rows: 126
      Raster Columns: 212
      Raster Layers: 1
      Raster Cells: 26,712
      Raster Resolution (x , y): 1' 30" , 1' 30"
      Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
      Extent (x , y) : [7° 4' 30" W - 1° 46' 30" W] , [40° 4' 60" N - 43° 13' 60" N]
      Layers:
      $ elevation_m <dbl> 700.2969, 780.3889, 706.1250, 568.9722, 584.9028, 506.7361~

---

    Code
      inv <- glimpse(v2)
    Output
      Raster Rows: 126
      Raster Columns: 212
      Raster Layers: 1
      Raster Cells: 26,712
      Raster Resolution (x , y): 1' 30" , 1' 30"
      Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
      Extent (x , y) : [7° 4' 30" W - 1° 46' 30" W] , [40° 4' 60" N - 43° 13' 60" N]
      Layers:
      $ elevation_m <dbl> 700.2969, 780.3889, 706.1250, 568.9722, 584.9028, 506.7361~

---

    Code
      inv <- glimpse(v2)
    Output
      Raster Rows: 126
      Raster Columns: 212
      Raster Layers: 1
      Raster Cells: 26,712
      Raster Resolution (x , y): 0.025 , 0.025
      Projected CRS: Cartesian (Meter)
      Extent (x , y) : [-7.07500 - -1.77500] , [40.08333 - 43.23333]
      Layers:
      $ elevation_m <dbl> 700.2969, 780.3889, 706.1250, 568.9722, 584.9028, 506.7361~

# RGB SpatRaster

    Code
      inv <- glimpse(v)
    Output
      Raster Rows: 212
      Raster Columns: 261
      Raster Layers: 3
      Raster Cells: 55,332
      Raster Resolution (x , y): 2,445.985 , 2,445.985
      Projected CRS: WGS 84 / Pseudo-Mercator (EPSG:3857)
      CRS projection units: m
      Extent (x , y) : [ -812,067.0 -  -173,664.9] , [4,852,834.1 - 5,371,382.9]
      Raster with 3 RGB channels: cyl_tile_1 (Red), cyl_tile_2 (Green), cyl_tile_3 (Blue)
      Layers:
      $ cyl_tile_1 <int> 217, 234, 217, 217, 242, 200, 197, 197, 217, 209, 213, 205,~
      $ cyl_tile_2 <int> 229, 235, 229, 229, 239, 153, 180, 187, 229, 226, 228, 225,~
      $ cyl_tile_3 <int> 206, 224, 206, 206, 233, 169, 194, 193, 206, 198, 202, 194,~

---

    Code
      inv <- glimpse(v)
    Output
      Raster Rows: 212
      Raster Columns: 261
      Raster Layers: 3
      Raster Cells: 55,332
      Raster Resolution (x , y): 2,445.985 , 2,445.985
      Projected CRS: WGS 84 / Pseudo-Mercator (EPSG:3857)
      CRS projection units: m
      Extent (x , y) : [ -812,067.0 -  -173,664.9] , [4,852,834.1 - 5,371,382.9]
      Raster with 3 RGB channels: cyl_tile_2 (Red), cyl_tile_3 (Green), cyl_tile_1 (Blue)
      Layers:
      $ cyl_tile_1 <int> 217, 234, 217, 217, 242, 200, 197, 197, 217, 209, 213, 205,~
      $ cyl_tile_2 <int> 229, 235, 229, 229, 239, 153, 180, 187, 229, 226, 228, 225,~
      $ cyl_tile_3 <int> 206, 224, 206, 206, 233, 169, 194, 193, 206, 198, 202, 194,~

---

    Code
      inv <- glimpse(v)
    Output
      Raster Rows: 212
      Raster Columns: 261
      Raster Layers: 4
      Raster Cells: 55,332
      Raster Resolution (x , y): 2,445.985 , 2,445.985
      Projected CRS: WGS 84 / Pseudo-Mercator (EPSG:3857)
      CRS projection units: m
      Extent (x , y) : [ -812,067.0 -  -173,664.9] , [4,852,834.1 - 5,371,382.9]
      Raster with 4 RGB channels: aa (Red), cyl_tile_3 (Green), cyl_tile_2 (Blue), cyl_tile_1 (Alpha)
      Layers:
      $ cyl_tile_1 <int> 217, 234, 217, 217, 242, 200, 197, 197, 217, 209, 213, 205,~
      $ cyl_tile_2 <int> 229, 235, 229, 229, 239, 153, 180, 187, 229, 226, 228, 225,~
      $ cyl_tile_3 <int> 206, 224, 206, 206, 233, 169, 194, 193, 206, 198, 202, 194,~
      $ aa         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,~

# Coltab SpatRaster

    Code
      inv <- glimpse(v)
    Output
      Raster Rows: 470
      Raster Columns: 590
      Raster Layers: 1
      Raster Cells: 277,300
      Raster Resolution (x , y): 1,000 , 1,000
      Projected CRS: WGS 84 / Pseudo-Mercator (EPSG:3857)
      CRS projection units: m
      Extent (x , y) : [ -787,640.9 -  -197,640.9] , [4,878,097.0 - 5,348,097.0]
      Raster with 1 color table in: era
      Layers:
      $ era <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~

---

    Code
      inv <- glimpse(end)
    Output
      Raster Rows: 470
      Raster Columns: 590
      Raster Layers: 2
      Raster Cells: 277,300
      Raster Resolution (x , y): 1,000 , 1,000
      Projected CRS: WGS 84 / Pseudo-Mercator (EPSG:3857)
      CRS projection units: m
      Extent (x , y) : [ -787,640.9 -  -197,640.9] , [4,878,097.0 - 5,348,097.0]
      Raster with 1 color table in: era
      Layers:
      $ nocoltab <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~
      $ era      <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~

---

    Code
      inv <- glimpse(twocoltabs)
    Output
      Raster Rows: 470
      Raster Columns: 590
      Raster Layers: 3
      Raster Cells: 277,300
      Raster Resolution (x , y): 1,000 , 1,000
      Projected CRS: WGS 84 / Pseudo-Mercator (EPSG:3857)
      CRS projection units: m
      Extent (x , y) : [ -787,640.9 -  -197,640.9] , [4,878,097.0 - 5,348,097.0]
      Raster with 2 color tables in: era, ihaveacoltab
      Layers:
      $ nocoltab     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
      $ era          <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
      $ ihaveacoltab <fct> S, W, S, S, W, S, S, W, S, S, W, S, S, W, S, S, W, S, S, ~

---

    Code
      glimpse(terra::subset(twocoltabs, c(1, 3, 2)))
    Output
      Raster Rows: 470
      Raster Columns: 590
      Raster Layers: 3
      Raster Cells: 277,300
      Raster Resolution (x , y): 1,000 , 1,000
      Projected CRS: WGS 84 / Pseudo-Mercator (EPSG:3857)
      CRS projection units: m
      Extent (x , y) : [ -787,640.9 -  -197,640.9] , [4,878,097.0 - 5,348,097.0]
      Raster with 2 color tables in: ihaveacoltab, era
      Layers:
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
      Raster Rows: 180
      Raster Columns: 360
      Raster Layers: 1
      Raster Cells: 64,800
      Raster Resolution (x , y): 1 , 1
      CRS: Not Defined / Empty
      Extent (x , y) : [-180 -  180] , [ -90 -   90]
      Layers:
      $ lyr.1 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~

---

    Code
      glimpse(v)
    Output
      Geometry type: Points
      CRS: Not Defined / Empty
      Extent (x , y) : [-179.5 -  179.5] , [ -89.5 -   89.5]
      Rows: 64,800
      Columns: 1
      $ lyr.1 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~

