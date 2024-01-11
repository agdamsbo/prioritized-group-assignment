# First try with shinylive

Reviving an old project to use shinylive with R


## Shortcoming

This project requires ROI.plugin.symphony to solve the problem, which depends on the RSymphony project (which again adapts SYMPHONY MILP), which is not [compiled for webR](https://repo.r-wasm.org/).

Clone the project and run the solver with
```
shiny::runApp(appDir = here::here("R/"),launch.browser = TRUE)
```

