# Glimpse SpatVectors

    Code
      glimpse(v)
    Output
      Rows: 9
      Columns: 3
      $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES~
      $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
      $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S~

---

    Code
      glimpse(v, geom = "WKT", width = 50)
    Output
      Rows: 9
      Columns: 4
      $ iso2     <chr> "ES-AV", "ES-BU", "ES-LE", "ES-~
      $ cpro     <chr> "05", "09", "24", "34", "37", "~
      $ name     <chr> "Avila", "Burgos", "Leon", "Pal~
      $ geometry <chr> "POLYGON ((3126360.2417 2066777~

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

