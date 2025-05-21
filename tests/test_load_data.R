source("R/functions.R")
test_that("mocked data is returned with CI", {
  withr::with_envvar(c(CI = "true"), {
    df <- load_data()
    expect_true(is.data.frame(df))
    expect_named(df, c("id", "name", "value"))
  })
})
