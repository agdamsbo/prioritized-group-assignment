# Version 25.1.1

UI tweaks and first release to Zenodo.

Added options to interpret Google and Microsoft forms data formats.

# Version 24.10.2

Working to allow direct import of online form results

* NEW: `prio2groups()` is a helper function to widen and format to apply with
valid format for `prioritized_grouping()` when importing the spreadsheet.

Wish list of supported platforms:

- Google Forms [WIP]
- Teams
- REDCap Survey


# Version 24.10.1

First proper public version. The package is mainly build for an easy to use 
shiny-interface, but can as easily be used directly in *R* with the main 
`prioritized_grouping()` function. This function is mainly a wrapper around the `ROI`
package and the `ROI.plugin.symphony` plugin. 

File types accepted in the shiny-app are .csv, .xls(x) and .ods.

This project was initially developed after a co-worker told me, he would be 
grouping students by individual priorities by hand and was preparing to spend 
most of a day doing so. I insisted on having a go at solving that problem in 
*R*. Now some of the some functionality is baked into Teams (that is what I am 
told at least), though that is not because of me. I still think is small app has 
its value, as it allows for the so-called "pedagogical redraw", where some 
students are manually grouped in consideration of other factors than only the 
given priorities before the algorithmic assignment.

This, as said, is a first official version and working example. 
More documentation is needed and will be added after further feedback. And 
please, feedback is most welcome!

