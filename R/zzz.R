
# On loading, the package needs to know its root.  This saves out the
#   root dir to the options as 'nabat_path'.
.onLoad = function(libname, pkgname){
  project_dir = rprojroot::find_root(has_file("DESCRIPTION"))
  op = options()
  op_nabat = list(
    nabat_path = project_dir
  )
  toset = !(names(op_nabat) %in% names(op))
  if(any(toset)) options(op_nabat[toset])
  invisible()

  # Shows message for path to project root
  packageStartupMessage(getOption('nabat_path'))
}
