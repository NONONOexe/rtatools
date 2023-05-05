test_that("latitude and longitude conversion", {
  expect_equal(convert_deg("40.3253820"), 40.5482833333333)
  expect_equal(convert_deg("140.3554682"), 140.598522777778)
})
