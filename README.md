# marr Shiny App

R Shiny app interface for the Bioconductor package [`marr`](https://bioconductor.org/packages/3.14/bioc/html/marr.html).
Hosted instances of
the application are available through ShinyApps.io and Binder.

[![](https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue)](https://maxmcgrath.shinyapps.io/marr/)
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Ghoshlab/marrShiny/HEAD?urlpath=shiny/shiny/)

### About

The Maximum Rank Reproducibility (MaRR) method measures the reproducibility of
features per sample pair and
sample pairs per feature in high-dimensional biological replicate
experiments. The `marr` Shiny app provides an easy way to access the
functionality of the `marr` R package with no programming experience needed. The app has
four tabs: `Introduction`, `Example`, `Analysis`, and `Further Information`.

The `Introduction` tab provides information on the app and the MaRR method. The
`Example` tab provides a guide to using the app with example data from
the [`MSPrep`](https://bioconductor.org/packages/3.14/bioc/html/MSPrep.html)
R package. Here you can play around with input options and outputs to discover
the functionality of `marr`. Just hover your mouse over any section of the `Example` tab and helpful information will be provided. The
`Analysis` tab allows you to apply the MaRR method to your own data by uploading CSV files, viewing
reproducibility diagnostics, and downloading data that has been filtered
according to reproducibility. Lastly, the `Further Information` tab provides
information on the `marr` package, the MaRR method, and other useful
information.
