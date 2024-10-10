# Checks for publishing

# Rhub v2

# rhub::rhub_setup()

# rhub::rhub_doctor()

rhub::rhub_check(platforms = c(
    "linux",
    "macos",
    "macos-arm64",
    "windows"))

# rhub::rhub_setup(overwrite = TRUE)
# rhub::rhub_platforms()

# SPELLING - dont use usethis::use_spell_check() - tests create notes on CRAN - not good
devtools::spell_check()
spelling::update_wordlist()


# Release on CRAN - confirm e-mail
devtools::release()

# Use the following to publish latest release to GitHub
usethis::use_github_release()


## Testing
##
# ds <- data.frame(id=paste0("id",1:100),matrix(replicate(100,sample(c(1:5,rep(NA,15)),10)),ncol=10,byrow = TRUE))
#
# ls <- ds|> prioritized_grouping(seed = 7)
#
# ls$export |> tibble::as.tibble() |> dplyr::right_join(ds,by = c("ID"="id")) |> View()
