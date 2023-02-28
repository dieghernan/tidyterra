# Glimpse SpatVectors

    Code
      glimpse(v)
    Output
      Rows: 9
      Columns: 3
      $ iso2 [3m[38;5;246m<chr>[39m[23m "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES~
      $ cpro [3m[38;5;246m<chr>[39m[23m "05", "09", "24", "34", "37", "40", "42", "47", "49"
      $ name [3m[38;5;246m<chr>[39m[23m "Ávila", "Burgos", "León", "Palencia", "Salamanca", "Segovia", "S~

---

    Code
      glimpse(v, geom = "WKT", width = 50)
    Output
      Rows: 9
      Columns: 4
      $ iso2     [3m[38;5;246m<chr>[39m[23m "ES-AV", "ES-BU", "ES-LE", "ES-~
      $ cpro     [3m[38;5;246m<chr>[39m[23m "05", "09", "24", "34", "37", "~
      $ name     [3m[38;5;246m<chr>[39m[23m "Ávila", "Burgos", "León", "Pal~
      $ geometry [3m[38;5;246m<chr>[39m[23m "POLYGON ((3126360.2417 2066777~

# Glimpse SpatRasters

    Code
      glimpse(r)
    Output
      Rows: 10,266
      Columns: 3
      $ tavg_04 [3m[38;5;246m<dbl>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
      $ tavg_05 [3m[38;5;246m<dbl>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
      $ tavg_06 [3m[38;5;246m<dbl>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~

---

    Code
      glimpse(r, xy = TRUE, width = 50)
    Output
      Rows: 10,266
      Columns: 5
      $ x       [3m[38;5;246m<dbl>[39m[23m -610394.8, -606513.5, -602632.2,~
      $ y       [3m[38;5;246m<dbl>[39m[23m 4618746, 4618746, 4618746, 46187~
      $ tavg_04 [3m[38;5;246m<dbl>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, ~
      $ tavg_05 [3m[38;5;246m<dbl>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, ~
      $ tavg_06 [3m[38;5;246m<dbl>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, ~

