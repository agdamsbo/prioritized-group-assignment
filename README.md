# Group allocation based on individual subject prioritization

This project was initially developed after a co-worker told me, he would be grouping students by individual priorities by hand and was preparing to spend most of a day doing so. I insisted on having a go at solving that problem in *R*. Now some of the some functionality is baked into Teams (that is what I am told at least), though that is not because of me. 

The functionality has later been further extended to allow for the so-called "pedagogical redraw", where some students are manually grouped in consideration of other factors than only the given priorities before the algorithmic assignment. This single addition makes it quite useful in schools and other educational institutions, as some customisation is allowed before the grouping.

## Shiny version

I am hosting a shiny version of this package, that allows to use the majority of functions.

[Use the app here](https://agdamsbo.shinyapps.io/prioritized-grouping/)

I have also attempted to package this as a shinylive program, but the "Symphony" solver has not been build to run in `{webr}`. When/if this happens, I will have another go.

## Install the `{prioritized.grouping}` package

```
pak::pak("agdamsbo/prioritized.grouping")
```

## Code of Conduct

Please note that the prioritized.grouping project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
