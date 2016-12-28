context("Test performance of the FARS functions")

library(dplyr)
library(maps)

setwd(system.file("data", package = "fars"))

test_that("fars_read() performs correctly", {
     expect_is(fars_read("accident_2014.csv.bz2"), "tbl_df")
     expect_error(fars_read("accident_2016.csv.bz2"))
})

test_that("fars_summarize_years() performs correctly", {
     expect_is(fars_summarize_years(2013:2015), "tbl_df")
     expect_equal(names(fars_summarize_years(2013:2015)), c("MONTH", 2013:2015))
})

test_that("fars_map_state() performs correctly", {
     expect_silent(fars_map_state(1, 2013))
     expect_error(fars_map_state(1000, 2013))
})
