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
options(scipen = 999)

## Packages
library(data.table);library(ggplot2);library(lme4);library(dplyr);library(BMA)

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
  mutate(race_1990 = SHRNHW9 + SHRNHB9 + SHRHSP9 + prop_other_race_all_90,
         race_2000 = SHRNHW0 + SHRNHB0 + SHRHSP0 + prop_other_race_all_00,
         SHRNHW9 = ifelse(race_1990 > 0, SHRNHW9/race_1990, 0),
         SHRNHW0 = ifelse(race_2000 > 0, SHRNHW0/race_2000, 0),
         SHRNHB9 = ifelse(race_1990 > 0, SHRNHB9/race_1990, 0),
         SHRNHB0 = ifelse(race_2000 > 0, SHRNHB0/race_2000, 0),
         SHRHSP9 = ifelse(race_1990 > 0, SHRHSP9/race_1990, 0),
         SHRHSP0 = ifelse(race_2000 > 0, SHRHSP0/race_2000, 0),
         prop_other_race_all_90 = ifelse(race_1990 > 0, prop_other_race_all_90/race_1990, 0),
         prop_other_race_all_00 = ifelse(race_2000 > 0, prop_other_race_all_00/race_2000, 0),
         white_change = SHRNHW0 - SHRNHW9,
         black_change = SHRNHB0 - SHRNHB9,
         latino_change = SHRHSP0 - SHRHSP9,
         other_change = prop_other_race_all_00 - prop_other_race_all_90, 
         hs_educ_90 = ifelse(EDUCPP9 > 0, EDUC129/EDUCPP9, 0),
         hs_educ_00 = ifelse(EDUCPP0 > 0, EDUC120/EDUCPP0, 0),
         col_educ_90 = ifelse(EDUCPP9 > 0, EDUC169/EDUCPP9, 0),
         col_educ_00 = ifelse(EDUCPP0 > 0, EDUC160/EDUCPP0, 0),
         avg_white = (SHRNHW9 + SHRNHW0)/2,
         avg_black = (SHRNHB9 + SHRNHB0)/2, 
         avg_hispanic = (SHRHSP9 + SHRHSP0)/2,
         avg_other = (prop_other_race_all_90 + prop_other_race_all_00)/2,
         avg_povrat = (POVRAT9 + POVRAT0)/2,
         avg_income = (MDFAMY9 + MDFAMY0)/2, 
         avg_hs_educ = (hs_educ_90 + hs_educ_00)/2,
         avg_col_educ = (col_educ_90 + col_educ_00)/2,
         avg_PC1 = (PC1_1990 + PC1_2000)/2, 
         avg_PC2 = (PC2_1990 + PC2_2000)/2, 
         avg_PC3 = (PC3_1990 + PC3_2000)/2, 
         avg_log_PC1 = (Log_PC1_1990 + Log_PC1_2000)/2, 
         avg_log_PC2 = (Log_PC2_1990 + Log_PC2_2000)/2, 
         avg_log_PC3 = (Log_PC3_1990 + Log_PC3_2000)/2, 
         avg_manuf = (prop_manuf_90 + prop_manuf_00)/2,
         avg_pop_density = (pop_density_90 + pop_density_00)/2) %>%
  filter(!is.na(emissions_1990) & !is.na(emissions_2000))

dt_change2 <- dt_change2 %>%
  mutate(race_2000 = SHRNHW0 + SHRNHB0 + SHRHSP0 + prop_other_race_all_00,
         race_2010 = SHRNHW1 + SHRNHB1 + SHRHSP1 + prop_other_race_all_10,
         SHRNHW0 = ifelse(race_2000 > 0, SHRNHW0/race_2000, 0),
         SHRNHW1 = ifelse(race_2010 > 0, SHRNHW1/race_2010, 0),
         SHRNHB0 = ifelse(race_2000 > 0, SHRNHB0/race_2000, 0),
         SHRNHB1 = ifelse(race_2010 > 0, SHRNHB1/race_2010, 0),
         SHRHSP0 = ifelse(race_2000 > 0, SHRHSP0/race_2000, 0),
         SHRHSP1 = ifelse(race_2010 > 0, SHRHSP1/race_2010, 0),
         prop_other_race_all_00 = ifelse(race_2000 > 0, prop_other_race_all_00/race_2000, 0),
         prop_other_race_all_10 = ifelse(race_2010 > 0, prop_other_race_all_10/race_2010, 0),
         white_change = SHRNHW1 - SHRNHW0,
         black_change = SHRNHB1 - SHRNHB0,
         latino_change = SHRHSP1 - SHRHSP0,
         other_change = prop_other_race_all_10 - prop_other_race_all_00, 
         hs_educ_00 = ifelse(EDUCPP0 > 0, EDUC120/EDUCPP0, 0),
         hs_educ_10 = ifelse(EDUCPP1A > 0, EDUC121A/EDUCPP1A, 0),
         col_educ_00 = ifelse(EDUCPP0 > 0, EDUC160/EDUCPP0, 0),
         col_educ_10 = ifelse(EDUCPP1A > 0, EDUC161A/EDUCPP1A, 0),
         avg_white = (SHRNHW0 + SHRNHW1)/2,
         avg_black = (SHRNHB0 + SHRNHB1)/2, 
         avg_hispanic = (SHRHSP0 + SHRHSP1)/2,
         avg_other = (prop_other_race_all_00 + prop_other_race_all_10)/2,
         avg_povrat = (POVRAT0 + POVRAT1A)/2,
         avg_income = (MDFAMY0 + MDFAMY1A)/2, 
         avg_hs_educ = (hs_educ_00 + hs_educ_10)/2,
         avg_col_educ = (col_educ_00 + col_educ_10)/2,
         avg_PC1 = (PC1_2000 + PC1_2010)/2, 
         avg_PC2 = (PC2_2000 + PC2_2010)/2, 
         avg_PC3 = (PC3_2000 + PC3_2010)/2, 
         avg_log_PC1 = (Log_PC1_2000 + Log_PC1_2010)/2, 
         avg_log_PC2 = (Log_PC2_2000 + Log_PC2_2010)/2, 
         avg_log_PC3 = (Log_PC3_2000 + Log_PC3_2010)/2, 
         avg_manuf = (prop_manuf_00 + prop_manuf_10)/2,
         avg_pop_density = (pop_density_00 + pop_density_10)/2) %>%
  filter(!is.na(emissions_2000) & !is.na(emissions_2010))

dt1990 <- dt1990 %>%
  mutate(hs_educ = ifelse(EDUCPP > 0, EDUC12/EDUCPP, 0),
         col_educ = ifelse(EDUCPP > 0, EDUC16/EDUCPP, 0),
         race_total = SHRNHW + SHRNHB + SHRHSP + prop_other_race_all, 
         SHRNHW = ifelse(race_total > 0, SHRNHW/race_total, 0),
         SHRNHB = ifelse(race_total > 0 ,SHRNHB/race_total, 0),
         SHRHSP = ifelse(race_total > 0, SHRHSP/race_total, 0),
         prop_other_race_all = ifelse(race_total > 0, 
                                      prop_other_race_all/race_total,
                                      0))

dt2000 <- dt2000 %>%
  mutate(hs_educ = ifelse(EDUCPP > 0, EDUC12/EDUCPP, 0),
         col_educ = ifelse(EDUCPP > 0, EDUC16/EDUCPP, 0),
         race_total = SHRNHW + SHRNHB + SHRHSP + prop_other_race_all, 
         SHRNHW = ifelse(race_total > 0, SHRNHW/race_total, 0),
         SHRNHB = ifelse(race_total > 0 ,SHRNHB/race_total, 0),
         SHRHSP = ifelse(race_total > 0, SHRHSP/race_total, 0),
         prop_other_race_all = ifelse(race_total > 0, 
                                      prop_other_race_all/race_total,
                                      0))

dt2010 <- dt2010 %>%
  mutate(hs_educ = ifelse(EDUCPP > 0, EDUC12/EDUCPP, 0),
         col_educ = ifelse(EDUCPP > 0, EDUC16/EDUCPP, 0),
         race_total = SHRNHW + SHRNHB + SHRHSP + prop_other_race_all, 
         SHRNHW = ifelse(race_total > 0, SHRNHW/race_total, 0),
         SHRNHB = ifelse(race_total > 0 ,SHRNHB/race_total, 0),
         SHRHSP = ifelse(race_total > 0, SHRHSP/race_total, 0),
         prop_other_race_all = ifelse(race_total > 0, 
                                      prop_other_race_all/race_total,
                                      0))

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

##############################################
## Plotting the state-ordered raw emissions ##
##############################################

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

########################################################
## Plotting the state-ordered change in raw emissions ##
########################################################

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

#############################################################
## Plotting the state-ordered change in relative emissions ##
#############################################################

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

################################################################
## Plotting the state-ordered evaluation/compliance variables ##
################################################################
pca_vars <- dt_long %>%
  group_by(STUSAB, time) %>%
  summarise(log_PC1 = first(Log_PC1),
            log_PC2 = first(Log_PC2),
            log_PC3 = first(Log_PC3),
            PC1 = first(PC1),
            PC2 = first(PC2),
            PC3 = first(PC3)) %>%
  ungroup() %>%
  mutate(STUSAB = as.character(STUSAB))
pca_1990 <- pca_vars %>%
  filter(time == 1990, !is.na(log_PC1))
pca_2000 <- pca_vars %>%
  filter(time == 2000)
pca_2010 <- pca_vars %>%
  filter(time == 2010)


# Setup
nj <- tapply(pca_2000$log_PC1, 
             pca_2000$STUSAB, length)
statemeans_PC1_1990 <- pca_1990$log_PC1
statemeans_PC1_2000 <- pca_2000$log_PC1
statemeans_PC1_2010 <- pca_2010$log_PC1
statemeans_PC2_1990 <- pca_1990$log_PC2
statemeans_PC2_2000 <- pca_2000$log_PC2
statemeans_PC2_2010 <- pca_2010$log_PC2
statemeans_PC3_1990 <- pca_1990$log_PC3
statemeans_PC3_2000 <- pca_2000$log_PC3
statemeans_PC3_2010 <- pca_2010$log_PC3
nstate <- length(nj)
ymax1 <- max (pca_2010$log_PC1)
ymin1 <- min (pca_2000$log_PC1)
ymax2 <- max (pca_2010$log_PC2)
ymin2 <- min (pca_2000$log_PC2)
ymax3 <- max (pca_1990$log_PC3)
ymin3 <- min (pca_2000$log_PC3)
mat <- matrix(c(1,2,3,4,5,6,7,8,9), 3)

par(mfrow=c(3,3))
layout(mat, c(2,2,2), c(2,2,2))
# PCA 1
plot (1:48, statemeans_PC1_1990[order(statemeans_PC1_1990)], 
      pch=19,ylim=c(ymin1,ymax1), col = "red",
      xlab="State rank in 1990", ylab = "",
      main = "Ordered State Compliance Measure (PC1)")
text(6, -1, labels = "ND, NV, VT", col = "red")
text(43, 3, labels = "WI, OH, PA", col = "red")
plot(1:nstate, statemeans_PC1_2000[order(statemeans_PC1_2000)],
     xlab="State rank in 2000", ylab = "", pch=19, col = "blue", 
     ylim=c(ymin1,ymax1))
text(6, 0, labels = "DC, VT, ND", col = "blue")
text(43, 4, labels = "CA, IN, PA", col = "blue")
plot(1:nstate, statemeans_PC1_2010[order(statemeans_PC1_2010)], 
     xlab="State rank in 2010", ylab = "", pch=19, col = "purple", 
     ylim=c(ymin1,ymax1))
text(6, 0.5, labels = "DC, VT, ND", col = "purple")
text(43, 2, labels = "OH, CA, TX", col = "purple")
# PCA 2
plot (1:48, statemeans_PC2_1990[order(statemeans_PC2_1990)], 
      pch=19,ylim=c(ymin2,ymax2), col = "red",
      xlab="State rank in 1990", ylab = "",
      main = "Ordered State Compliance Measure (PC2)")
text(6, -1, labels = "GA, PA, NY", col = "red")
text(43, 1.3, labels = "WI, NV, ND", col = "red")
plot(1:nstate, statemeans_PC2_2000[order(statemeans_PC2_2000)],
     xlab="State rank in 2000", ylab = "", pch=19, col = "blue", 
     ylim=c(ymin2,ymax2))
text(6, -.9, labels = "TX, GA, IL", col = "blue")
text(43, 2.2, labels = "MN, WI, DC", col = "blue")
plot(1:nstate, statemeans_PC2_2010[order(statemeans_PC2_2010)], 
     xlab="State rank in 2010", ylab = "", pch=19, col = "purple", 
     ylim=c(ymin2,ymax2))
text(6, -1.5, labels = "NY, MA, WV", col = "purple")
text(43, 1, labels = "WA, NE, NV", col = "purple")
# PCA 3
plot (1:48, statemeans_PC3_1990[order(statemeans_PC3_1990)], 
      pch=19,ylim=c(ymin3,ymax3), col = "red",
      xlab="State rank in 1990", ylab = "",
      main = "Ordered State Compliance Measure (PC3)")
text(6, -1, labels = "ID, WY, ND", col = "red")
text(43, 0.5, labels = "TN, NC, TX", col = "red")
plot(1:nstate, statemeans_PC3_2000[order(statemeans_PC3_2000)],
     xlab="State rank in 2000", ylab = "", pch=19, col = "blue", 
     ylim=c(ymin3,ymax3))
text(6, -.8, labels = "UT, KY, DC", col = "blue")
text(43, 1.2, labels = "OH, RI, AZ", col = "blue")
plot(1:nstate, statemeans_PC3_2010[order(statemeans_PC3_2010)], 
     xlab="State rank in 2010", ylab = "", pch=19, col = "purple", 
     ylim=c(ymin3,ymax3))
text(6, -1.6, labels = "DC, WY, NC", col = "purple")
text(43, 1.4, labels = "GA, UT, MI", col = "purple")


#####################
### Change models ###
#####################

## Fit models for change 1990 - 2000
# Assess the significance of controls
dt1 <- as.data.table(dt_change1)
controls.list <- c("emissions_1990", "avg_povrat", "avg_income", "avg_hs_educ", "avg_col_educ", "avg_manuf", "avg_pop_density")
test <- bic.glm(dt1[, controls.list, with = F], dt1$emissions_decrease, glm.family = gaussian)
par(mfrow = c(1,1), oma = c(0,3,0,0))
imageplot.bma(test)
# Should include emissions_1990, povrat, hs_educ, avg_manuf in all models

# Baseline (No random intercepts)
M0 <- lm(emissions_decrease ~ emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf, data = dt_change1)

# Baseline (Random intercepts)
M1 <- lmer(emissions_decrease ~ emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# Average race
M2 <- lmer(emissions_decrease ~ avg_black + avg_hispanic + avg_other + 
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# Change race
M3 <- lmer(emissions_decrease ~ black_change + latino_change + other_change +
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# Average and change race
M4 <- lmer(emissions_decrease ~ avg_black + avg_hispanic + avg_other + black_change + latino_change + other_change +
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA
M5 <- lmer(emissions_decrease ~ avg_log_PC1 + avg_log_PC2 + avg_log_PC3 +
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# Average and change in race with PCA
M6 <- lmer(emissions_decrease ~ avg_black + avg_hispanic + avg_other + black_change + latino_change + other_change + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 +
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with average race interaction
M7 <- lmer(emissions_decrease ~ avg_log_PC1*avg_black + avg_log_PC2*avg_black + avg_log_PC3*avg_black + avg_log_PC1*avg_hispanic + avg_log_PC2*avg_hispanic + avg_log_PC3*avg_hispanic + avg_log_PC1*avg_other + avg_log_PC2*avg_other + avg_log_PC3*avg_other +
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with black interaction
M8 <- lmer(emissions_decrease ~ avg_log_PC1*avg_black + avg_log_PC2*avg_black + avg_log_PC3*avg_black + 
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with white interaction
M9 <- lmer(emissions_decrease ~ avg_log_PC1*avg_white + avg_log_PC2*avg_white + avg_log_PC3*avg_white + 
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with hispanic interaction
M10 <- lmer(emissions_decrease ~ avg_log_PC1*avg_hispanic + avg_log_PC2*avg_hispanic + avg_log_PC3*avg_hispanic +
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with other interaction
M11 <- lmer(emissions_decrease ~ avg_log_PC1*avg_other + avg_log_PC2*avg_other + avg_log_PC3*avg_other +
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with race change interaction
M12 <- lmer(emissions_decrease ~ avg_log_PC1*black_change + avg_log_PC2*black_change + avg_log_PC3*black_change + avg_log_PC1*latino_change + avg_log_PC2*latino_change + avg_log_PC3*latino_change + avg_log_PC1*other_change + avg_log_PC2*other_change + avg_log_PC3*other_change +
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with black change interaction
M13 <- lmer(emissions_decrease ~ avg_log_PC1*black_change + avg_log_PC2*black_change + avg_log_PC3*black_change + 
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with white change interaction
M14 <- lmer(emissions_decrease ~ avg_log_PC1*white_change + avg_log_PC2*white_change + avg_log_PC3*white_change + 
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with hispanic change interaction
M15 <- lmer(emissions_decrease ~ avg_log_PC1*latino_change + avg_log_PC2*latino_change + avg_log_PC3*latino_change + 
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with other change interaction
M16 <- lmer(emissions_decrease ~ avg_log_PC1*other_change + avg_log_PC2*other_change + avg_log_PC3*other_change + 
           emissions_1990 + avg_povrat + avg_hs_educ + avg_manuf + (1 | STATEFP10), REML = F, data = dt_change1)

comp.models(paste0("M", 0:16))

# Model 9 seems to be the best out of the above for 1990 to 2000

## Fit models for change 2000 - 2010
# Assess the significance of controls
dt2 <- as.data.table(dt_change2)
controls.list <- c("emissions_2000", "avg_povrat", "avg_income", "avg_hs_educ", "avg_col_educ", "avg_manuf", "avg_pop_density")
test2 <- bic.glm(dt2[, controls.list, with = F], dt2$emissions_decrease, glm.family = gaussian)
par(mfrow = c(1,1), oma = c(0,3,0,0))
imageplot.bma(test2)
# Should include emissions_2000, avg_hs_educ, avg_col_educ in all models

# Baseline (No random intercepts)
M0 <- lm(emissions_decrease ~ emissions_2000 + avg_hs_educ + avg_col_educ, data = dt_change1)

# Baseline (Random intercepts)
M1 <- lmer(emissions_decrease ~ emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# Average race
M2 <- lmer(emissions_decrease ~ avg_black + avg_hispanic + avg_other + 
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# Change race
M3 <- lmer(emissions_decrease ~ black_change + latino_change + other_change +
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# Average and change race
M4 <- lmer(emissions_decrease ~ avg_black + avg_hispanic + avg_other + black_change + latino_change + other_change +
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA
M5 <- lmer(emissions_decrease ~ avg_log_PC1 + avg_log_PC2 + avg_log_PC3 +
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# Average and change in race with PCA
M6 <- lmer(emissions_decrease ~ avg_black + avg_hispanic + avg_other + black_change + latino_change + other_change + avg_log_PC1 + avg_log_PC2 + avg_log_PC3 +
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with average race interaction
M7 <- lmer(emissions_decrease ~ avg_log_PC1*avg_black + avg_log_PC2*avg_black + avg_log_PC3*avg_black + avg_log_PC1*avg_hispanic + avg_log_PC2*avg_hispanic + avg_log_PC3*avg_hispanic + avg_log_PC1*avg_other + avg_log_PC2*avg_other + avg_log_PC3*avg_other +
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with black interaction
M8 <- lmer(emissions_decrease ~ avg_log_PC1*avg_black + avg_log_PC2*avg_black + avg_log_PC3*avg_black + 
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with white interaction
M9 <- lmer(emissions_decrease ~ avg_log_PC1*avg_white + avg_log_PC2*avg_white + avg_log_PC3*avg_white + 
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with hispanic interaction
M10 <- lmer(emissions_decrease ~ avg_log_PC1*avg_hispanic + avg_log_PC2*avg_hispanic + avg_log_PC3*avg_hispanic +
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with other interaction
M11 <- lmer(emissions_decrease ~ avg_log_PC1*avg_other + avg_log_PC2*avg_other + avg_log_PC3*avg_other +
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with race change interaction
M12 <- lmer(emissions_decrease ~ avg_log_PC1*black_change + avg_log_PC2*black_change + avg_log_PC3*black_change + avg_log_PC1*latino_change + avg_log_PC2*latino_change + avg_log_PC3*latino_change + avg_log_PC1*other_change + avg_log_PC2*other_change + avg_log_PC3*other_change +
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with black change interaction
M13 <- lmer(emissions_decrease ~ avg_log_PC1*black_change + avg_log_PC2*black_change + avg_log_PC3*black_change + 
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with white change interaction
M14 <- lmer(emissions_decrease ~ avg_log_PC1*white_change + avg_log_PC2*white_change + avg_log_PC3*white_change + 
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with hispanic change interaction
M15 <- lmer(emissions_decrease ~ avg_log_PC1*latino_change + avg_log_PC2*latino_change + avg_log_PC3*latino_change + 
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

# PCA with other change interaction
M16 <- lmer(emissions_decrease ~ avg_log_PC1*other_change + avg_log_PC2*other_change + avg_log_PC3*other_change + 
           emissions_2000 + avg_hs_educ + avg_col_educ + (1 | STATEFP10), REML = F, data = dt_change1)

comp.models(paste0("M", 0:16))

##############################
### Cross sectional models ###
##############################

### 1990 ###
# Assess the significance of controls
dt90 <- as.data.table(dt1990)
controls.list <- c("MDFAMY", "hs_educ", "col_educ", "POVRAT", "prop_manuf", "pop_density")
test3 <- bic.glm(dt90[, controls.list, with = F], dt90$emissions, glm.family = gaussian)
par(mfrow = c(1,1), oma = c(0,3,0,0))
imageplot.bma(test3)
# Should include "hs_educ", "col_educ", "POVRAT", "prop_manuf", "pop_density" in all models

# Baseline (No random intercepts)
M0 <- lm(emissions ~ hs_educ + col_educ + POVRAT + prop_manuf + pop_density, data = dt1990)

# Baseline (Random intercepts)
M1 <- lmer(emissions ~ hs_educ + col_educ + POVRAT + prop_manuf + pop_density + (1 | STATEFP10), REML = F, data = dt1990)

# Race
M2 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
           hs_educ + col_educ + POVRAT + prop_manuf + pop_density + (1 | STATEFP10), REML = F, data = dt1990)
# PCA
M3 <- lmer(emissions ~ Log_PC1 + Log_PC2 + Log_PC3 +
           hs_educ + col_educ + POVRAT + prop_manuf + pop_density + (1 | STATEFP10), REML = F, data = dt1990)

# Race and PCA
M4 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all + Log_PC1 + Log_PC2 + Log_PC3 +
           hs_educ + col_educ + POVRAT + prop_manuf + pop_density + (1 | STATEFP10), REML = F, data = dt1990)

# Race and PCA interaction
M5 <- lmer(emissions ~ Log_PC1*SHRNHB + Log_PC2*SHRNHB + Log_PC3*SHRNHB + Log_PC1*SHRHSP + Log_PC2*SHRHSP+ Log_PC3*SHRHSP + Log_PC1*prop_other_race_all + Log_PC2*prop_other_race_all + Log_PC3*prop_other_race_all +
           hs_educ + col_educ + POVRAT + prop_manuf + pop_density + (1 | STATEFP10), REML = F, data = dt1990)

# Black and PCA interaction
M6 <- lmer(emissions ~ Log_PC1*SHRNHB + Log_PC2*SHRNHB + Log_PC3*SHRNHB +
           hs_educ + col_educ + POVRAT + prop_manuf + pop_density + (1 | STATEFP10), REML = F, data = dt1990)

# White and PCA interaction
M7 <- lmer(emissions ~ Log_PC1*SHRNHW + Log_PC2*SHRNHW + Log_PC3*SHRNHW +
           hs_educ + col_educ + POVRAT + prop_manuf + pop_density + (1 | STATEFP10), REML = F, data = dt1990)

# Hispanic and PCA interaction
M8 <- lmer(emissions ~ Log_PC1*SHRHSP + Log_PC2*SHRHSP+ Log_PC3*SHRHSP +
           hs_educ + col_educ + POVRAT + prop_manuf + pop_density + (1 | STATEFP10), REML = F, data = dt1990)

# Other and PCA interaction
M9 <- lmer(emissions ~ Log_PC1*prop_other_race_all + Log_PC2*prop_other_race_all + Log_PC3*prop_other_race_all +
           hs_educ + col_educ + POVRAT + prop_manuf + pop_density + (1 | STATEFP10), REML = F, data = dt1990)

comp.models(paste0("M", 0:9))

# Model 8 seems to be the best for 1990
summary(M8)

# Visualizing the state random effects for the best fitting model 
lattice::dotplot(ranef(M8, condVar = TRUE))

### 2000 ###
# Assess the significance of controls
dt00 <- as.data.table(dt2000)
controls.list <- c("MDFAMY", "hs_educ", "col_educ", "POVRAT", "prop_manuf", "pop_density")
test4 <- bic.glm(dt00[, controls.list, with = F], dt00$emissions, glm.family = gaussian)
par(mfrow = c(1,1), oma = c(0,3,0,0))
imageplot.bma(test4)
# Should include "hs_educ", "col_educ", "POVRAT", "prop_manuf" in all models

# Baseline (No random intercepts)
M0 <- lm(emissions ~ hs_educ + col_educ + POVRAT + prop_manuf, data = dt2000)

# Baseline (Random intercepts)
M1 <- lmer(emissions ~ hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2000)

# Race
M2 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
           hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2000)
# PCA
M3 <- lmer(emissions ~ Log_PC1 + Log_PC2 + Log_PC3 +
           hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2000)

# Race and PCA
M4 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all + Log_PC1 + Log_PC2 + Log_PC3 +
           hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2000)

# Race and PCA interaction
M5 <- lmer(emissions ~ Log_PC1*SHRNHB + Log_PC2*SHRNHB + Log_PC3*SHRNHB + Log_PC1*SHRHSP + Log_PC2*SHRHSP+ Log_PC3*SHRHSP + Log_PC1*prop_other_race_all + Log_PC2*prop_other_race_all + Log_PC3*prop_other_race_all +
           hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2000)

# Black and PCA interaction
M6 <- lmer(emissions ~ Log_PC1*SHRNHB + Log_PC2*SHRNHB + Log_PC3*SHRNHB +
           hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2000)

# White and PCA interaction
M7 <- lmer(emissions ~ Log_PC1*SHRNHW + Log_PC2*SHRNHW + Log_PC3*SHRNHW +
           hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2000)

# Hispanic and PCA interaction
M8 <- lmer(emissions ~ Log_PC1*SHRHSP + Log_PC2*SHRHSP+ Log_PC3*SHRHSP +
           hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2000)

# Other and PCA interaction
M9 <- lmer(emissions ~ Log_PC1*prop_other_race_all + Log_PC2*prop_other_race_all + Log_PC3*prop_other_race_all +
           hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2000)

comp.models(paste0("M", 0:9))

# Model 1 seems to be the best for 2000

# Visualizing the state random effects for the best fitting model 
lattice::dotplot(ranef(M1, condVar = TRUE))


### 2010 ###
# Assess the significance of controls
dt10 <- as.data.table(dt2010)
controls.list <- c("MDFAMY", "hs_educ", "col_educ", "POVRAT", "prop_manuf", "pop_density")
test5 <- bic.glm(dt10[, controls.list, with = F], dt10$emissions, glm.family = gaussian)
par(mfrow = c(1,1), oma = c(0,3,0,3))
imageplot.bma(test5)
# Should include "hs_educ", "col_educ", "POVRAT", "prop_manuf" in all models

# Baseline (No random intercepts)
M0 <- lm(emissions ~ MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf, data = dt2010)

# Baseline (Random intercepts)
M1 <- lmer(emissions ~ MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2010)

# Race
M2 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
           MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2010)
# PCA
M3 <- lmer(emissions ~ Log_PC1 + Log_PC2 + Log_PC3 +
           MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2010)

# Race and PCA
M4 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all + Log_PC1 + Log_PC2 + Log_PC3 +
           MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2010)

# Race and PCA interaction
M5 <- lmer(emissions ~ Log_PC1*SHRNHB + Log_PC2*SHRNHB + Log_PC3*SHRNHB + Log_PC1*SHRHSP + Log_PC2*SHRHSP+ Log_PC3*SHRHSP + Log_PC1*prop_other_race_all + Log_PC2*prop_other_race_all + Log_PC3*prop_other_race_all +
           MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2010)

# Black and PCA interaction
M6 <- lmer(emissions ~ Log_PC1*SHRNHB + Log_PC2*SHRNHB + Log_PC3*SHRNHB +
           MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2010)

# White and PCA interaction
M7 <- lmer(emissions ~ Log_PC1*SHRNHW + Log_PC2*SHRNHW + Log_PC3*SHRNHW +
           MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2010)

# Hispanic and PCA interaction
M8 <- lmer(emissions ~ Log_PC1*SHRHSP + Log_PC2*SHRHSP+ Log_PC3*SHRHSP +
           MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2010)

# Other and PCA interaction
M9 <- lmer(emissions ~ Log_PC1*prop_other_race_all + Log_PC2*prop_other_race_all + Log_PC3*prop_other_race_all +
           MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + (1 | STATEFP10), REML = F, data = dt2010)

comp.models(paste0("M", 0:9))

# Model 2 seems to be the best for 2010

# Visualizing the state random effects for the best fitting model 
lattice::dotplot(ranef(M1, condVar = TRUE))

### Year Comparisons ###
# 1990 #
M11_1 <- lmer(emissions ~ SHRNHB*Log_PC1 + SHRHSP*Log_PC1 + prop_other_race_all*Log_PC1 + 
                SHRNHB*Log_PC2 + SHRHSP*Log_PC2 + prop_other_race_all*Log_PC2 + 
                SHRNHB*Log_PC3 + SHRHSP*Log_PC3 + prop_other_race_all*Log_PC3 +
                MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + pop_density +
                (1 | STUSAB), REML = F, data = dt1990)
M11_2 <- lmer(emissions ~ SHRNHB*Log_PC1 + SHRHSP*Log_PC1 + prop_other_race_all*Log_PC1 + 
                SHRNHB*Log_PC2 + SHRHSP*Log_PC2 + prop_other_race_all*Log_PC2 + 
                SHRNHB*Log_PC3 + SHRHSP*Log_PC3 + prop_other_race_all*Log_PC3 +
                MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + pop_density +
                (1 | STUSAB), REML = F, data = dt2000)
M11_3 <- lmer(emissions ~ SHRNHB*Log_PC1 + SHRHSP*Log_PC1 + prop_other_race_all*Log_PC1 + 
                SHRNHB*Log_PC2 + SHRHSP*Log_PC2 + prop_other_race_all*Log_PC2 + 
                SHRNHB*Log_PC3 + SHRHSP*Log_PC3 + prop_other_race_all*Log_PC3 +
                MDFAMY + hs_educ + col_educ + POVRAT + prop_manuf + pop_density +
                (1 | STUSAB), REML = F, data = dt2010)
summary(M11_1)
summary(M11_2)
summary(M11_3)
lattice::dotplot(ranef(M11_1, condVar = TRUE))
lattice::dotplot(ranef(M11_2, condVar = TRUE))
lattice::dotplot(ranef(M11_3, condVar = TRUE))

# 2000 #
M12_1 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
              hs_educ + col_educ + POVRAT + prop_manuf + 
              (1 | STUSAB), REML = F, data = dt1990)
M12_2 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
              hs_educ + col_educ + POVRAT + prop_manuf + 
              (1 | STUSAB), REML = F, data = dt2000)
M12_3 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
              hs_educ + col_educ + POVRAT + prop_manuf + 
              (1 | STUSAB), REML = F, data = dt2010)
summary(M12_1)
summary(M12_2)
summary(M12_3)
lattice::dotplot(ranef(M12_1, condVar = TRUE))
lattice::dotplot(ranef(M12_2, condVar = TRUE))
lattice::dotplot(ranef(M12_3, condVar = TRUE))

# 2010 #
M4_1 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
             MDFAMY + hs_educ + col_educ + POVRAT + (1 | STUSAB), 
             REML = F, data = dt1990)
M4_2 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
             MDFAMY + hs_educ + col_educ + POVRAT + (1 | STUSAB), 
             REML = F, data = dt2000)
M4_3 <- lmer(emissions ~ SHRNHB + SHRHSP + prop_other_race_all +
             MDFAMY + hs_educ + col_educ + POVRAT + (1 | STUSAB), 
             REML = F, data = dt2010)
summary(M4_1)
summary(M4_2)
summary(M4_3)
lattice::dotplot(ranef(M4_1, condVar = TRUE))
lattice::dotplot(ranef(M4_2, condVar = TRUE))
lattice::dotplot(ranef(M4_3, condVar = TRUE))




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