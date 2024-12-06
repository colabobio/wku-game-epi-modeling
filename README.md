# Mathematical epidemiological analysis of the WKU game data

This repository contains all the code necessary to reproduce the results in the paper "The effect of Behavioral Factors and Intervention Strategies on Pathogen Transmission: Insights from a Two-Week Epidemic Game at Wenzhou-Kean University in China", by Musa et al.

## Getting started

Download the histories.csv file from the following Zenodo repository:

https://zenodo.org/records/10674401

and place it under data. Then you should be able to run the full-analysis.Rmd notebook from [RStudio Desktop](https://posit.co/download/rstudio-desktop/).

The code was tested with R software version 4.4.1, and it requires the following packages:

* dplyr
* tidyr
* ggplot2
* reshape2
* scales
* patchwork
* cowplot
* lubridate
* zoo
* patchwork
* deSolve
* sfsmisc
