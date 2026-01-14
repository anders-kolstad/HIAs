# Source files for the manuscript titled: Bias Correction and Uncertainty Quantification for Small-Sample Ecosystem Condition Assessments

[![DOI](https://zenodo.org/badge/718768356.svg)](https://zenodo.org/badge/latestdoi/718768356)

On this repository you can find the material to recreate the analyses and the manuscript titled "Bias Correction and Uncertainty Quantification for Small-Sample Ecosystem Condition Assessments".
The exeption is the large nature type survey data which was downloaded from [here](https://kartkatalog.miljodirektoratet.no/Dataset/Details/2031).

The project uses the renv package manager. After cloing the repo, use `renv::restore` to recreate the environmnt. 

The analysis and the rendering of the manuscript and appendix files, are coded as a `targets` workflow. If unfamiliar with targets, please see [here](https://books.ropensci.org/targets/).
