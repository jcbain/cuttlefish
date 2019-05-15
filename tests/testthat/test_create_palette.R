context("Create Palettes")

img_file <- "https://jeroen.github.io/images/frink.png"

test_that("prominent.ord option works when turned on", {
  expect_equal(create_palette(img_file, 5, prominent.ord = T), c("#000000", "#ffffff", "#ffd521", "#f387b8", "#d8e3e9"))
})

set.seed(1)

test_that("prominent.ord option works when turned off and defaults to angle segmentations", {
  expect_equal(create_palette(img_file, 5), c("#FCF598", "#3B3A32", "#FCFDFE", "#DEE1E0", "#FDCE20"))
})
