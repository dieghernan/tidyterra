## code to prepare `cross_blended_hypsometric_tints_db` dataset goes here

db <- tibble::tribble(
  ~pal, ~limit, ~r, ~g, ~b,
  "arid", 7000L, 245L, 245L, 245L,
  "arid", 6000L, 235L, 235L, 237L,
  "arid", 5000L, 220L, 220L, 220L,
  "arid", 4000L, 212L, 207L, 204L,
  "arid", 3000L, 212L, 193L, 179L,
  "arid", 2000L, 212L, 184L, 163L,
  "arid", 1000L, 212L, 201L, 180L,
  "arid", 600L, 202L, 190L, 174L,
  "arid", 200L, 180L, 170L, 158L,
  "arid", 50L, 170L, 160L, 150L,
  "arid", 0L, 160L, 152L, 141L,
  "arid", -400L, 146L, 136L, 129L,
  "warm_humid", 7000L, 245L, 245L, 245L,
  "warm_humid", 6000L, 235L, 235L, 237L,
  "warm_humid", 5000L, 220L, 220L, 220L,
  "warm_humid", 4000L, 212L, 207L, 204L,
  "warm_humid", 3000L, 212L, 193L, 179L,
  "warm_humid", 2000L, 212L, 184L, 163L,
  "warm_humid", 1000L, 212L, 201L, 180L,
  "warm_humid", 600L, 169L, 192L, 166L,
  "warm_humid", 200L, 134L, 184L, 159L,
  "warm_humid", 50L, 120L, 172L, 149L,
  "warm_humid", 0L, 114L, 164L, 141L,
  "warm_humid", -400L, 106L, 153L, 135L,
  "cold_humid", 7000L, 245L, 245L, 245L,
  "cold_humid", 6000L, 235L, 235L, 237L,
  "cold_humid", 5000L, 220L, 220L, 220L,
  "cold_humid", 4000L, 212L, 207L, 204L,
  "cold_humid", 3000L, 212L, 193L, 179L,
  "cold_humid", 2000L, 212L, 184L, 163L,
  "cold_humid", 1000L, 212L, 201L, 180L,
  "cold_humid", 600L, 180L, 192L, 180L,
  "cold_humid", 200L, 145L, 177L, 171L,
  "cold_humid", 50L, 130L, 165L, 159L,
  "cold_humid", 0L, 120L, 159L, 152L,
  "cold_humid", -400L, 112L, 147L, 141L,
  "polar", 4000L, 241L, 245L, 254L,
  "polar", 3000L, 239L, 243L, 252L,
  "polar", 2000L, 218L, 226L, 239L,
  "polar", 1000L, 201L, 214L, 231L,
  "polar", 600L, 185L, 201L, 224L,
  "polar", 200L, 171L, 192L, 213L,
  "polar", 50L, 164L, 180L, 203L,
  "polar", 0L, 149L, 169L, 196L
)

# Make hex color
db$hex <- rgb(db$r, db$g, db$b, maxColorValue = 255)

db <- db %>%
  arrange(pal, limit) %>%
  filter(limit >= 0)


cross_blended_hypsometric_tints_db <- db
names(cross_blended_hypsometric_tints_db) <- names(hypsometric_tints_db)

rm(db)

usethis::use_data(cross_blended_hypsometric_tints_db, overwrite = TRUE)

pals <- unique(cross_blended_hypsometric_tints_db$pal)

# Helper fun for plotting

npanels <- grDevices::n2mfrow(length(pals))
ncols <- 256
npals <- length(pals)

opar <- par(no.readonly = TRUE)

# db %>% filter(pal == "cold_humid") %>% pull(hex)
# db %>% filter(pal == "warm_humid") %>% pull(hex)

par(mfrow = npanels, mar = rep(1, 4), bg = "grey85")
for (i in pals) {
  cc <- cross_blended_hypsometric_tints_db %>%
    filter(pal == i) %>%
    pull(hex)
  ramp <- colorRampPalette(cc)

  image(
    x = seq(1, ncols),
    y = 1,
    z = as.matrix(seq(1, ncols)),
    col = ramp(ncols),
    main = i,
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    bty = "n"
  )
}
par(opar)
