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
user <- Sys.getenv("USERNAME")

## Packages
library(data.table);library(ggplot2);library(lme4);library(dplyr)

### Paths
root <- ifelse(user == "aucarter","C:/Users/aucarter/Repos/soc560_proj/", "~/Desktop/University of Washington/Coursework/Winter 2017/SOC 560 Hierarchical Modeling for the Social Sciences/Project/soc560_proj/")
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
dt_long <- read.csv(paste0(data.dir, "cross_sec_data.csv"))
dt1990 <- dt_long  %>% filter(!is.na(emissions)) %>% filter(time == 1990)
dt2000 <- dt_long  %>% filter(!is.na(emissions)) %>% filter(time == 2000)
dt2010 <- dt_long  %>% filter(!is.na(emissions)) %>% filter(time == 2010)
dt_change1 <- read.csv(paste0(data.dir, "change_1990_2000.csv"))
dt_change2 <- read.csv(paste0(data.dir, "change_2000_2010.csv"))

dt_change1 <- dt_change1 %>%
  mutate(avg_white = (SHRNHW9 + SHRNHW0)/2,
         avg_black = (SHRNHB9 + SHRNHB0)/2, 
         avg_hispanic = (SHRHSP9 + SHRHSP0)/2,
         avg_other = (prop_other_race_all_90 + prop_other_race_all_00)/2,
         avg_povrat = (POVRAT9 + POVRAT0)/2,
         avg_income = (MDFAMY9 + MDFAMY0)/2, 
         avg_hs_educ = ((EDUC129/EDUCPP9) + (EDUC120/EDUCPP0))/2,
         avg_col_educ = ((EDUC169/EDUCPP9) + (EDUC160/EDUCPP0))/2,
         avg_PC1 = (PC1_1990 + PC1_2000)/2, 
         avg_PC2 = (PC2_1990 + PC2_2000)/2, 
         avg_PC3 = (PC3_1990 + PC3_2000)/2, 
         avg_log_PC1 = (Log_PC1_1990 + Log_PC1_2000)/2, 
         avg_log_PC2 = (Log_PC2_1990 + Log_PC2_2000)/2, 
         avg_log_PC3 = (Log_PC3_1990 + Log_PC3_2000)/2, 
         avg_manuf = (prop_manuf_90 + prop_manuf_00)/2,
         avg_pop_density = (pop_density_90 + pop_density_00)/2)

dt_change2 <- dt_change2 %>%
  mutate(avg_white = (SHRNHW0 + SHRNHW1)/2,
         avg_black = (SHRNHB0 + SHRNHB1)/2, 
         avg_hispanic = (SHRHSP0 + SHRHSP1)/2,
         avg_other = (prop_other_race_all_00 + prop_other_race_all_10)/2,
         avg_povrat = (POVRAT0 + POVRAT1A)/2,
         avg_income = (MDFAMY0 + MDFAMY1A)/2, 
         avg_hs_educ = ((EDUC120/EDUCPP0) + (EDUC121A/EDUCPP1A))/2,
         avg_col_educ = ((EDUC160/EDUCPP0) + (EDUC161A/EDUCPP1A))/2,
         avg_PC1 = (PC1_2000 + PC1_2010)/2, 
         avg_PC2 = (PC2_2000 + PC2_2010)/2, 
         avg_PC3 = (PC3_2000 + PC3_2010)/2, 
         avg_log_PC1 = (Log_PC1_2000 + Log_PC1_2010)/2, 
         avg_log_PC2 = (Log_PC2_2000 + Log_PC2_2010)/2, 
         avg_log_PC3 = (Log_PC3_2000 + Log_PC3_2010)/2, 
         avg_manuf = (prop_manuf_00 + prop_manuf_10)/2,
         avg_pop_density = (pop_density_00 + pop_density_10)/2)

dt1990 <- dt1990 %>%
  mutate(hs_educ = EDUC12/EDUCPP,
         col_educ = EDUC16/EDUCPP)

dt2000 <- dt2000 %>%
  mutate(hs_educ = EDUC12/EDUCPP,
         col_educ = EDUC16/EDUCPP)

dt2010 <- dt2010 %>%
  mutate(hs_educ = EDUC12/EDUCPP,
         col_educ = EDUC16/EDUCPP)

## Summarize data
state.means.1990 <- dt1990 %>% group_by(STATEFP10) %>% summarize_all(funs(mean, sd))
county.means.1990 <- dt1990 %>% group_by(COUNTYFP10) %>% summarize_all(funs(mean, sd))
tract.means.1990 <- dt1990 %>% group_by(TRACTCE10) %>% summarize_all(funs(mean, sd))

state.means.2000 <- dt2000 %>% group_by(STATEFP10) %>% summarize_all(funs(mean, sd))
county.means.2000 <- dt2000 %>% group_by(COUNTYFP10) %>% summarize_all(funs(mean, sd))
tract.means.2000 <- dt2000 %>% group_by(TRACTCE10) %>% summarize_all(funs(mean, sd))

state.means.2010 <- dt2010 %>% group_by(STATEFP10) %>% summarize_all(funs(mean, sd))
county.means.2010 <- dt2010 %>% group_by(COUNTYFP10) %>% summarize_all(funs(mean, sd))
tract.means.2010 <- dt2010 %>% group_by(TRACTCE10) %>% summarize_all(funs(mean, sd))

## Plotting the state-ordered emissions

# 1990
nj <- tapply(state.means.1990$emissions_mean, state.means.1990$STATEFP10, length)
statemeans <- state.means.1990$emissions_mean
statesd <- state.means.1990$emissions_sd
nstate <- length(nj)
lower <- statemeans - 1.96* statesd
upper <- statemeans + 1.96* statesd
ymax <- max (upper)
ymin <- min (lower)


plot (1:nstate, statemeans[order(statemeans)], pch=19,ylim=c(ymin,ymax),
      xlab="State rank by mean emmisions in 1990", ylab="Emissions", 
      main = "Ordered State Emmissions (1990)")
for (j in 1:nstate) {
  jj <- order(statemeans)[j]
  lines (x=c(j,j),y=c(lower[jj],upper[jj]))
}
abline (h=0,col="red")
text(46.5, -10000, labels = "IN, TN, RI", col = "blue")
text(3, 3000, labels = "DC, NV, VT", col = "blue")

# 2000
nj <- tapply(state.means.2000$emissions_mean, state.means.2000$STATEFP10, length)
statemeans <- state.means.2000$emissions_mean
statesd <- state.means.2000$emissions_sd
nstate <- length(nj)
lower <- statemeans - 1.96* statesd
upper <- statemeans + 1.96* statesd
ymax <- max (upper)
ymin <- min (lower)

plot (1:nstate, statemeans[order(statemeans)], pch=19,ylim=c(ymin,ymax),
      xlab="State rank by mean emmisions in 2000", ylab="Emissions", 
      main = "Ordered State Emmissions (2000)")
for (j in 1:nstate) {
  jj <- order(statemeans)[j]
  lines (x=c(j,j),y=c(lower[jj],upper[jj]))
}
abline (h=0,col="red")
text(46.5, -4000, labels = "MI, IN, VA", col = "blue")
text(4, 1000, labels = "VT, NM, NV", col = "blue")

# 2010
nj <- tapply(state.means.2010$emissions_mean, state.means.2010$STATEFP10, length)
statemeans <- state.means.2010$emissions_mean
statesd <- state.means.2010$emissions_sd
nstate <- length(nj)
lower <- statemeans - 1.96* statesd
upper <- statemeans + 1.96* statesd
ymax <- max (upper)
ymin <- min (lower)

plot (1:nstate, statemeans[order(statemeans)], pch=19,ylim=c(ymin,ymax),
      xlab="State rank by mean emmisions in 2010", ylab="Emissions", 
      main = "Ordered State Emmissions (2010)")
for (j in 1:nstate) {
  jj <- order(statemeans)[j]
  lines (x=c(j,j),y=c(lower[jj],upper[jj]))
}
abline (h=0,col="red")
text(46.5, -3000, labels = "KY, WV, LA", col = "blue")
text(4, 800, labels = "VT, DC, NM", col = "blue")

# Plotting change in emmissions
change1 <- cbind.data.frame(State = state.means.2000$STATEFP10, 
                            Emissions = state.means.1990$emissions_mean,
                            Change = state.means.2000$emissions_mean - state.means.1990$emissions_mean)
change2 <- cbind.data.frame(State = state.means.2010$STATEFP10, 
                            Emissions = state.means.2000$emissions_mean,
                            Change = state.means.2010$emissions_mean - state.means.2000$emissions_mean)
change3 <- cbind.data.frame(State = state.means.2010$STATEFP10, 
                            Emissions = state.means.1990$emissions_mean,
                            Change = state.means.2010$emissions_mean - state.means.1990$emissions_mean)
nj <- tapply(state.means.2010$emissions_mean, state.means.2010$STATEFP10, length)
nstate <- length(nj)
ymax <- max (change1$Change)
ymin <- min (change1$Change)

# 1990 - 2000
plot (1:nstate, change1$Change[order(change1$Change)], pch=19,ylim=c(ymin,ymax),
      xlab="State rank by raw change in mean emmissions between 1990 and 2000", ylab="Change in Emissions", 
      main = "Ordered State Emmissions Change (1990 - 2000)")
abline (h=0,col="red")
text(46.5, 55, labels = "MT, ND, DC", col = "blue")
text(4, -800, labels = "IL, TN, RI", col = "blue")

# 2000 - 2010
ymax <- max (change2$Change)
ymin <- min (change2$Change)
plot (1:nstate, change2$Change[order(change2$Change)], pch=19,ylim=c(ymin,ymax),
      xlab="State rank by raw change in mean emmissions between 2000 and 2010", ylab="Change in Emissions", 
      main = "Ordered State Emmissions Change (2000 - 2010)")
abline (h=0,col="red")
text(46.5, 15, labels = "ND, WY, MT", col = "blue")
text(4, -294, labels = "NJ, VA, DE", col = "blue")

# 1990 - 2010
ymax <- max (change3$Change)
ymin <- min (change3$Change)
plot (1:nstate, change3$Change[order(change3$Change)], pch=19,ylim=c(ymin,ymax),
      xlab="State rank by raw change in mean emmissions between 1990 and 2010", ylab="Change in Emissions", 
      main = "Ordered State Emmissions Change (1990 - 2010)")
abline (h=0,col="red")
text(46.5, -160, labels = "DC, MT, ND", col = "blue")
text(4, -900, labels = "RI, TN, IN", col = "blue")

# Plotting relative change
overallmean1990 <- mean(state.means.1990$emissions_mean)
overallmean2000 <- mean(state.means.2000$emissions_mean)
overallmean2010 <- mean(state.means.2010$emissions_mean)
change1 <- change1 %>% mutate(rel_change = Change/overallmean1990*100)
change2 <- change2 %>% mutate(rel_change = Change/overallmean2000*100)
change3 <- change3 %>% mutate(rel_change = Change/overallmean1990*100)

# 1990 - 2000
ymax <- max (change1$rel_change)
ymin <- min (change1$rel_change)
plot (1:nstate, change1$rel_change[order(change1$rel_change)], pch=19,ylim=c(ymin,ymax),
      xlab="State rank by percentage change in mean emmissions between 1990 and 2000", ylab="% Change in Emissions", 
      main = "Ordered State Emmissions Change (1990 - 2000)")
abline (h=0,col="red")
text(46.5, -50, labels = "DC, MT, ND", col = "blue")
text(4, -250, labels = "IN, TN, RI", col = "blue")

# 2000 - 2010
ymax <- max (change2$rel_change)
ymin <- min (change2$rel_change)
plot (1:nstate, change2$rel_change[order(change2$rel_change)], pch=19,ylim=c(ymin,ymax),
      xlab="State rank by percentage change in mean emmissions between 2000 and 2010", ylab="% Change in Emissions", 
      main = "Ordered State Emmissions Change (2000 - 2010)")
abline (h=0,col="red")
text(46.5, -25, labels = "MT, WY, ND", col = "blue")
text(4, -150, labels = "NJ, VA, DE", col = "blue")

# 1990 - 2010
ymax <- max (change3$rel_change)
ymin <- min (change3$rel_change)
plot (1:nstate, change3$rel_change[order(change3$rel_change)], pch=19,ylim=c(ymin,ymax),
      xlab="State rank by percentage change in mean emmissions between 1990 and 2010", ylab="% Change in Emissions", 
      main = "Ordered State Emmissions Change (1990 - 2010)")
abline (h=0,col="red")
text(46.5, -35, labels = "DC, MT, ND", col = "blue")
text(4, -250, labels = "RI, TN, IN", col = "blue")




## Fit models for change 1990 - 2000
M1 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
             (1 | STATEFP10), REML = F, data = dt_change1)
M2 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change + (1 | STATEFP10), REML = F, data = dt_change1)
M3 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change + avg_income + 
             (1 | STATEFP10), REML = F, data = dt_change1)
M4 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change + avg_hs_educ + avg_col_educ + 
             (1 | STATEFP10), REML = F, data = dt_change1)
M5 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change + avg_income + avg_hs_educ + avg_col_educ + 
             (1 | STATEFP10), REML = F, data = dt_change1)
M6 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
             avg_povrat + (1 | STATEFP10), REML = F, data = dt_change1)
M7 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
             avg_povrat + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)
M8 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
             avg_povrat + avg_manuf + avg_pop_density + (1 | STATEFP10), REML = F, data = dt_change1)
M9 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
             avg_povrat + avg_manuf + avg_pop_density + avg_PC1 + avg_PC2 + avg_PC3 + 
             (1 | STATEFP10), REML = F, data = dt_change1)
M10 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
             avg_povrat + avg_manuf + avg_pop_density + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 + 
             (1 | STATEFP10), REML = F, data = dt_change1)
M11 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 + 
              (1 | STATEFP10), REML = F, data = dt_change1)
M12 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 + 
              (1 + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 | STATEFP10), REML = F, data = dt_change1)
M13 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density +
               (1 | STATEFP10), REML = F, data = dt_change1)
<<<<<<< HEAD
M14 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 + 
              (1 | STATEFP10/TRACTCE10), REML = F, data = dt_change1)
M15 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 + 
              (1 | STATEFP10:TRACTCE10), REML = F, data = dt_change1)
M16 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 + 
               (1 + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 | STATEFP10), REML = F, data = dt_change1)
M17 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 + 
              (1 | STATEFP10), REML = F, data = dt_change1)
model.list <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", 
                "M9", "M10", "M11", "M12", "M13", "M14", "M15", 
                "M16", "M17")
=======
model.list <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", 
                "M9", "M10","M12", "M13")
>>>>>>> 7aa1aa134f718ae2bf0c602d43b1461ed5109d93
comp.models(model.list)

# Model 11 seems to be the best out of the above for 1990 to 2000


## Fit models for change 2000 - 2010
M1 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
             (1 | STATEFP10), REML = F, data = dt_change2)
M2 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change + (1 | STATEFP10), REML = F, data = dt_change2)
M3 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change + avg_income + 
             (1 | STATEFP10), REML = F, data = dt_change2)
M4 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change + avg_hs_educ + avg_col_educ + 
             (1 | STATEFP10), REML = F, data = dt_change2)
M5 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change + avg_income + avg_hs_educ + avg_col_educ + 
             (1 | STATEFP10), REML = F, data = dt_change2)
M6 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
             avg_povrat + (1 | STATEFP10), REML = F, data = dt_change2)
M7 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
             avg_povrat + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change2)
M8 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
             avg_povrat + avg_manuf + avg_pop_density + (1 | STATEFP10), REML = F, data = dt_change2)
M9 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
             avg_povrat + avg_manuf + avg_pop_density + avg_PC1 + avg_PC2 + avg_PC3 + 
             (1 | STATEFP10), REML = F, data = dt_change2)
M10 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 + 
              (1 | STATEFP10), REML = F, data = dt_change2)
M11 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 + 
              (1 | STATEFP10), REML = F, data = dt_change2)
M12 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 + 
              (1 + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 | STATEFP10), REML = F, data = dt_change2)
M13 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density
              (1 | STATEFP10), REML = F, data = dt_change2)
M14 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density + avg_PC1 + avg_PC2 + avg_PC3 + 
              (1 | STATEFP10), REML = F, data = dt_change2)
M15 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
              black_change + latino_change + other_change  + avg_income + 
              avg_hs_educ + avg_col_educ + avg_povrat + avg_manuf + avg_pop_density + 
              (1 | STATEFP10), REML = F, data = dt_change2)



model.list <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", 
                "M9", "M10", "M11", "M12", "M13", "M14", "M15")
comp.models(model.list)

# Model 4 seems to be the best out of the above for 2000 to 2010

##############################
### Cross sectional models ###
##############################

M1 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all + 
             (1 | TRACTCE10), REML = F, data = dt1990)
M2 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
             MDFAMY + (1 | TRACTCE10), REML = F, data = dt1990)
M3 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
             MDFAMY + hs_educ + col_educ + (1 | TRACTCE10), REML = F, data = dt1990)
M4 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
             MDFAMY + hs_educ + col_educ + POVRAT + (1 | TRACTCE10), REML = F, data = dt1990)
M5 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
             MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + 
             (1 | TRACTCE10), REML = F, data = dt1990)
M6 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
             MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + pop_density +
             (1 | TRACTCE10), REML = F, data = dt1990)
M7 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
             MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + pop_density +
             
             (1 | TRACTCE10), REML = F, data = dt1990)




M9 <- lmer(emissions ~ emissions_1990 + SHRNHB + SHRHSP + prop_other_race_all + 
             black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
             avg_povrat + avg_manuf + avg_pop_density + avg_PC1 + avg_PC2 + avg_PC3 + 
             (1 | TRACTCE10), REML = F, data = dt1990)
M10 <- lmer(emissions ~ emissions_1990 + SHRNHB + SHRHSP + prop_other_race_all + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 + 
              (1 | TRACTCE10), REML = F, data = dt1990)
M11 <- lmer(emissions ~ emissions_1990 + SHRNHB + SHRHSP + prop_other_race_all + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 + 
              (1 | TRACTCE10) + (1 | STATEFP10), REML = F, data = dt1990)
M12 <- lmer(emissions ~ emissions_1990 + SHRNHB + SHRHSP + prop_other_race_all + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 + 
              (1 | TRACTCE10) + (1 + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 | STATEFP10), REML = F, data = dt1990)



###################
Presentation models
###################


library(texreg)
M11 <- lmer(emissions_decrease ~ emissions_1990 + avg_black + avg_hispanic + avg_other + 
              black_change + latino_change + other_change  + avg_hs_educ + avg_col_educ + 
              avg_povrat + avg_manuf + avg_pop_density + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 + 
              (1 | STATEFP10), REML = F, data = dt_change1)
texreg(M11)

M4 <- lmer(emissions_decrease ~ emissions_2000 + avg_black + avg_hispanic + avg_other + 
             black_change + latino_change + other_change + avg_hs_educ + avg_col_educ + 
             (1 | STATEFP10), REML = F, data = dt_change2)
texreg(M4)
### End