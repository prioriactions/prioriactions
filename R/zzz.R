
.pkgenv <- new.env(parent = emptyenv())

.onUnload <- function(libpath) {
  library.dynam.unload("prioriactions", libpath)
}

.onAttach <- function(libname, pkgname) {
  # define message generator function
  msg <- function() {
    packageStartupMessage(paste(rep("-", 30), collapse = ""))
    packageStartupMessage(
      "You have loaded both prioriactions and prioritizr - ",
      "this is likely to cause serious issues.\n",
      "You should only have one of these packages loaded at a time,\n",
      "please unload one prioriactions or prioritizr using one of the commands below:\n",
      "  detach(\"package:prioriactions\", unload = TRUE) # unload prioriactions package\n",
      "  detach(\"package:prioritizr\", unload = TRUE) # unload prioritizr",
      "package\n",
      "and then reload the desired package."
    )
    packageStartupMessage(paste(rep("-", 30), collapse = ""))
  }
  # print message if prioritizr already loaded
  if ("prioritizr" %in% .packages()) {
    msg()
  }
}
