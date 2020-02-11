library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(VulnToolkit)
library(viridis)

sink("bibliography.bib")
out <- sapply(names(sessionInfo()$otherPkgs), 
              function(x) print(citation(x), style = "Bibtex"))

out

sink("bibliography.txt")
out <- sapply(names(sessionInfo()$otherPkgs), 
              function(x) print(citation(x), style = "text"))

out