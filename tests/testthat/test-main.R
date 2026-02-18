box::use(
  shiny[testServer],
  testthat[expect_true, test_that],
)

box::use(
  app/main[server],
)

test_that("main server initializes without error", {
  testServer(server, {
    expect_true(TRUE)
  })
})
