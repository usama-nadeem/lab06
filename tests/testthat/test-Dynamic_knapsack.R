n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)
test_that("Correct object is returned", {
  expect_silent(gk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))
  expect_named(gk, c("value", "total"))
})

test_that("functions rejects errounous input.", {
  expect_error(knapsack_dynamic("hej", 3500))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  gk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(gk$value), 17277)
  expect_true(all(round(gk$total) %in% c(4,5,7)))
  
  gk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(gk$value), 19912)
  expect_true(all(round(gk$total) %in% c(5,7,10)))
  
  gk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(gk$value), 11498)
  expect_true(all(round(gk$total) %in% c(5,7)))
  
  gk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(gk$value), 13775)
  expect_true(all(round(gk$total) %in% c(7,10)))
  
  st <- system.time(gk <- knapsack_dynamic(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)
  
  gk <- knapsack_dynamic(x = knapsack_objects[1:800,], W = 3500)
  expect_equal(round(gk$value), 196220)
  
  gk <- knapsack_dynamic(x = knapsack_objects[1:1200,], W = 3500)
  expect_equal(round(gk$value), 252439)
})