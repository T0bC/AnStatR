## ----eval = FALSE-------------------------------------------------------------
# build_shared_lib = function (object_names) {
#     # Change working directory so R finds the Makevars.
#     old_dir = setwd(box::file())
#     on.exit(setwd(old_dir))
#     rbin = file.path(R.home('bin'), 'R')
#     exitcode = system2(rbin, c('CMD', 'SHLIB', paste0(object_names, '.c')))
#     stopifnot(exitcode == 0L)
# }
# 
# build_shared_lib('hello')

## ----eval = FALSE-------------------------------------------------------------
# box::use(./c/`__setup__`)

## ----eval = FALSE-------------------------------------------------------------
# libname = function (name) {
#     box::file(paste0(name, .Platform$dynlib.ext))
# }

## ----eval = FALSE-------------------------------------------------------------
# .on_load = function (ns) {
#     ns$dll = dyn.load(libname('hello'))
# }

## ----eval = FALSE-------------------------------------------------------------
# .on_unload = function (ns) {
#     dyn.unload(libname('hello'))
# }

## ----eval = FALSE-------------------------------------------------------------
# #' @export
# hello_world = function (name) {
#     .Call(dll$hello_world, name)
# }

## ----eval = FALSE-------------------------------------------------------------
# box::use(./c)
# c$hello_world('Rthur')

