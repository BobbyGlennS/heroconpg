context("test-hello")

test_that("Heroku CLI args are correctly created", {
  expect_equal(c("config:get", "hello", "-a", "world"),
               create_heroku_pgurl_args("hello", "world"))
})
