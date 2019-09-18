
# On loading, the package needs to know its root.  This saves out the
#   root dir to the options as 'nabat_path'.
.onLoad = function(libname, pkgname){
  # project_dir = rprojroot::find_root_file('R', "acoustic_stationary_report.Rmd", criterion = has_file('DESCRIPTION'))
  template_rmd = system.file("templates", "acoustic_stationary_report.Rmd", package = "nabatr")
  nabat_logo   = system.file("templates", "nabat_logo.png", package = "nabatr")

  # Shows message for path to project root
  packageStartupMessage(template_rmd)
  packageStartupMessage(nabat_logo)
}
