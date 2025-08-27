## Convert your shiny app into all the assets for running the app 
## the docs is a requirement of GitHub
## in the browser
shinylive::export(
  appdir = "HEX-Colour-Studio",
  destdir = "docs"
  )
 

## with development version of httpuv, run shinylive app locally
## remotes::install_github("rstudio/httpuv")
httpuv::runStaticServer(
  dir = "docs",
  port = 8888
  )
