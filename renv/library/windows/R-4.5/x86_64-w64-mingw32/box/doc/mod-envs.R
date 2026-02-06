## ----eval = FALSE-------------------------------------------------------------
# #' @export
# f = function () 'c$f'

## ----eval = FALSE-------------------------------------------------------------
# #' @export
# f = function () 'b$f'
# 
# g = function () 'b$g'

## ----eval = FALSE-------------------------------------------------------------
# #' @export
# box::use(./b[g = f, ...])
# 
# box::use(./c[...])
# 
# #' @export
# box::use(./c)
# 
# #' @export
# f = function () 'a$f'
# 
# f_of_c1 = c$f
# f_of_c2 = get('f', parent.env(environment()))
# stopifnot(identical(f_of_c1, f_of_c2))

## ----eval = FALSE-------------------------------------------------------------
# box::use(a = ./a[f, g])

