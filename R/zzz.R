
# On loading, the package needs to know its root.  This saves out the
#   root dir.
.onLoad = function(libname, pkgname){
  template_rmd = system.file("templates", "acoustic_stationary_report.Rmd", package = "nabatr")
  nabat_logo   = system.file("templates", "nabat_logo.png", package = "nabatr")

  # Shows message for path to project root
  # packageStartupMessage(template_rmd)
  # packageStartupMessage(nabat_logo)
}
