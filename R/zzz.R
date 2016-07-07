.onAttach <- function(libname, pkgname)
{
  f <- read.dcf(file.path(libname, pkgname, "DESCRIPTION"),
                c("Version", "Date"))
  packageStartupMessage('Successfully loaded changepoint.np package version ',
                        f[1,1])
}