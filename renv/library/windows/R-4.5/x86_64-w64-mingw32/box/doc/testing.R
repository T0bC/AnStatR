## ----eval = FALSE-------------------------------------------------------------
# if (is.null(box::name())) {
#     box::use(./`__tests__`)
# }

## ----eval = FALSE-------------------------------------------------------------
# box::use(testthat[...])
# 
# .on_load = function (ns) {
#     test_dir(box::file())
# }
# 
# box::export()

## ----eval = FALSE-------------------------------------------------------------
# box::use(../seq[...])

## ----eval = FALSE-------------------------------------------------------------
# test_that('valid sequences can be created', {
#     expect_error((s = seq('GATTACA')), NA)
#     expect_true(is_valid(s))
# 
#     expect_error(seq('cattaga'), NA)
# })
# 
# test_that('invalid sequences cannot be created', {
#     expect_error(seq('GATTXA'))
# })

