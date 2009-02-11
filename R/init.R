.First.lib <- function(lib, pkg) {
    library.dynam("pspearman", pkg, lib)
}

.Last.lib <- function(libpath) {
    library.dynam.unload("pspearman", libpath)
}

