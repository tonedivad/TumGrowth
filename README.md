TumGrowth
=============

Source code for the analysis of tumour growth experiments.

installation
------------

Before running the app you will need to have 1) R and 2) RStudio installed:
 - https://cran.rstudio.com/
 - https://www.rstudio.com/products/rstudio/download/

Open RStudio and run these lines in the console to install a few packages:
 - install.packages(c("shiny","shinyFiles","devtools","nlme","xtable","multcomp","survival","doBy"))
 - install.packages(c("RColorBrewer","car","coxphf","beeswarm","RSVGTipsDevice"))
 - library(devtools)
 - install_github('ramnathv/rCharts')

Open RStudio and run this line:
 - shiny:::runGitHub("TumGrowth", "tonedivad")

Your web browser will open the web app.
