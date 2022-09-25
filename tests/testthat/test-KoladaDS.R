pos_test <- get_kolda_data('kpi/n60026/year/2020,2019,2018')

test_that("get_kolda_data returns a list", {
  expect_equal(class(pos_test), "list")
})

test_that("get_kolda_data list has 2 elements", {
  expect_equal(names(pos_test), c("MunMaster", "FinalData"))
})

test_that("Municipality master data frame is available", {
  expect_gt(nrow(pos_test$MunMaster), 1)
})

test_that("Salary data frame is available", {
  expect_gt(nrow(pos_test$FinalData), 1)
})

test_that("Municipality data frame has proper structure", {
  expect_equal(names(pos_test$MunMaster), c("id", "title",
                                            "type"))
})

test_that("Salary data frame has proper structure", {
  expect_equal(names(pos_test$FinalData), c("kpi", "municipality",
                                         "period", "count", "gender",
                                         "value"))
})

test_that("get_kolda_data does not execute", {
  expect_error(get_kolda_data(''))
})

test_that("Year is passed in the url string", {
  expect_error(get_kolda_data('kpi/n60026'), )
})