set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)
test_that("Correct object is returned", {
  expect_silent(dk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))
  expect_named(dk, c("value", "elements"))
})
test_that("functions rejects errounous input.", {
  expect_error(knapsack_dynamic("hej", 3500))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], W = -3500))
})