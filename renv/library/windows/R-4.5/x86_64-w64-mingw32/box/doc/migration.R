## ----eval = FALSE-------------------------------------------------------------
# library(modules)
# 
# # …
# 
# modname = import('prefix/modname')
# pkgname = import_package('pkgname')

## ----eval = FALSE-------------------------------------------------------------
# box::use(prefix/modname)
# box::use(pkgname)

## ----eval = FALSE-------------------------------------------------------------
# box::use(./modname)

## ----eval = FALSE-------------------------------------------------------------
# box::use(mod_alias = prefix/modname)

## ----eval = FALSE-------------------------------------------------------------
# box::use(
#     prefix/mod1,
#     mod = prefix/mod2,
#     pkg
# )

## ----eval = FALSE-------------------------------------------------------------
# # ‘modules’:
# import('prefix/mod', attach = c('name1', 'name2'))
# 
# # ‘box’:
# box::use(prefix/mod[name1, name2])

## ----eval = FALSE-------------------------------------------------------------
# # ‘modules’:
# import('prefix/mod', attach = TRUE)
# 
# # ‘box’:
# box::use(prefix/mod[...])

## ----eval = FALSE-------------------------------------------------------------
# # ‘modules’:
# mod = import('prefix/mod', attach = c('name1', 'name2'))
# 
# # ‘box’:
# box::use(mod = prefix/mod[name1, name2])

## ----eval = FALSE-------------------------------------------------------------
# # Declare alias for one name, attach other name unchanged:
# box::use(prefix/mod[name_alias = name1, name2])
# 
# # Declare alias for two names, attach all other exported names unchanged:
# box::use(prefix/mod[name_alias1 = name1, name_alias2 = name2, ...])

## ----eval = FALSE-------------------------------------------------------------
# # This function is not exported:
# f1 = function () {}
# 
# # This nested module is not exported
# box::use(./nested1)
# 
# # This function is exported:
# #' @export
# f2 = function () {}
# 
# # This nested module is exported
# #' @export
# box::use(./nested2)

## ----eval = FALSE-------------------------------------------------------------
# #' @export
# box::use(
#     pkg_alias = pkg,
#     prefix/mod,
#     prefix/mod2[a, b, c]
# )

## ----eval = FALSE-------------------------------------------------------------
# box::export()

## ----eval = FALSE-------------------------------------------------------------
# box::use(r/core[...])

