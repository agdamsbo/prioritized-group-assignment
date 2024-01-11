# Typical shiny
shiny::runApp(appDir = here::here("R/"),launch.browser = TRUE)


# Shinylive version
shinylive::export(appdir = "R", destdir = "docs")

httpuv::runStaticServer(dir = "docs")
