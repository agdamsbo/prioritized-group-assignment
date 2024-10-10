# Typical shiny
# shiny::runApp(appDir = here::here("app/"),launch.browser = TRUE)


# pak::pak("agdamsbo/project.aid")

# merge_scripts(path=here::here("app/functions.R"),files=list.files("R/",pattern = ".R$",full.names = TRUE))

# styler::style_file("app/functions.R")

project.aid::merge_scripts(
    files = c(
        list.files("R/", pattern = ".R$", full.names = TRUE),
        "app/server_raw.R"
    ),
    dest = here::here("app/server.R")
)

shiny::runApp(appDir = "app",launch.browser = TRUE)

project.aid::deploy_shiny(
    files = c("server.R", "ui.R"),
    account.name = "agdamsbo",
    name.app = "prioritized-grouping",
    name.token = "rsconnect_agdamsbo_token",
    name.secret = "rsconnect_agdamsbo_secret"
)
