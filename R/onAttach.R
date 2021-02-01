.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "attrib",
    utils::packageDescription("attrib")$Version,
    "https://folkehelseinstituttet.github.io/attrib"
  ))
}
