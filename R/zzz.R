
# On loading, the package needs to know its root.  This saves out the
#   root dir.
.onLoad = function(libname, pkgname){
  template_rmd = system.file("templates", "acoustic_stationary_report.Rmd", package = "nabatr")
  nabat_logo   = system.file("templates", "nabat_logo.png", package = "nabatr")

  # pkg.env = new.env(parent = emptyenv())
  # pkg.env$bats_df = NULL
  # message(getwd())
  # tryCatch({
  #   message('Loading in bat species range shapefiles')
  #   pkg.env$species_ranges = rgdal::readOGR('./data/bat_species_ranges/')[,1:4]
  # },error = function(cond) {
  #   message('Failed to load in bat species ranges')
  #   message(cond)
  #   return(NULL)
  # })

  # Shows message for path to project root
  # packageStartupMessage(template_rmd)
  # packageStartupMessage(nabat_logo)
}
