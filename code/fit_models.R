################################################################################
## Purpose: Read in data and fit preliminary models
## Date created: 
## Date modified:
## Author: Austin Carter (aucarter@uw.edu) & Victoria Sass
## Run instructions: 
## Notes:
################################################################################

### Setup
rm(list=ls())
windows <- Sys.info()[1]=="Windows"

## Packages
library(data.table);library(ggplot2);library(lme4)

### Paths
root <- ifelse(windows,"C:/Users/aucarter/Repos/soc560_proj/","")
code.dir <- paste0(root, "code/")
data.dir <- paste0(root, "data/")
output.dir <- paste0(root, "")

### Functions
comp.models <- function(models) {
  ## Input a list of models and output a table of the BIC's
  bic.list <- sapply(models, function(model) {
    BIC <- BIC(logLik(get(model)))
    return(BIC)
  })
  out.dt <- data.table(model = names(bic.list), bic = bic.list)
  min.bic <- out.dt[bic == min(bic), bic]
  out.dt[, min := min.bic]
  out.dt[, diff := bic - min.bic]
  out.dt[, min := NULL]
  print(out.dt)
}

### Code
## Read in and prep data
dt <- fread(paste0(data.dir, "full_data_long.csv"))

## Summarize data
state.means <- dt[, lapply(.SD, mean), by = .(STATEFP10), .SDcols = c("emissions", "POVRAT")]
county.means <- dt[, lapply(.SD, mean), by = .(COUNTYFP10), .SDcols = c("emissions", "POVRAT")]

## Fit models
M1 <- lmer(emissions ~ POVRAT  + (1 | COUNTYFP10), REML = F, data = dt)
M2 <- lmer(emissions ~ POVRAT  + (1  + POVRAT| COUNTYFP10), REML = F, data = dt)
model.list <- c("M1", "M2")
comp.models(model.list)

### End