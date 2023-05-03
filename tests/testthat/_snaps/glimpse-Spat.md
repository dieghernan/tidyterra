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
      Extent (x , y) : [2.892.687 - 3.341.372] , [2.017.622 - 2.361.600]
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
      Extent (x , y) : [7° 4' 36,6024" W - 1° 46' 58,078" W] , [40° 5' 7,7968" N - 43° 14' 11,5677" N]
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
      Extent (x , y) : [-7,076834 - -1,782799] , [40,085499 - 43,236547]
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
      Rows: 10,266
      Columns: 3
      $ tavg_04 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
      $ tavg_05 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
      $ tavg_06 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~

---

    Code
      glimpse(r, xy = TRUE, width = 50)
    Output
      Rows: 10,266
      Columns: 5
      $ x       <dbl> -610394.8, -606513.5, -602632.2,~
      $ y       <dbl> 4618746, 4618746, 4618746, 46187~
      $ tavg_04 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, ~
      $ tavg_05 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, ~
      $ tavg_06 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, ~

