# Mathematical epidemiological analysis of the WKU game data

This repository contains all the code necessary to reproduce the results in the paper "The effect of Behavioral Factors and Intervention Strategies on Pathogen Transmission: Insights from a Two-Week Epidemic Game at Wenzhou-Kean University in China", by Musa et al.

## Getting started

Download the histories.csv file from the following Zenodo repository:

https://zenodo.org/records/10674401

and place it under data. Then you should be able to run the main-epi-analysis.Rmd notebook from [RStudio Desktop](https://posit.co/download/rstudio-desktop/).

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

## Super spreader analysis

The super spreader analysis following the methods from [Taube et al.](https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001685) can be run in [Jupyter Lab](https://jupyterlab.readthedocs.io/en/latest/) using the super-spreader-analysis.ipynb notebook. 

The notebook was tested with python version 3.13.0, and it requires the following packages:

* pandas
* numpy
* scipy
* pickle
* matplotlib
* networkx
* pygraphviz