rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(lfe)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
wdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(wdir, showWarnings = FALSE, recursive = TRUE) 

setwd(wdir)

dt <- data.table(readRDS('./_data_for_correlates.rds'))
dt <- dt[major_religion != 'No Religion' & major_religion != 'Other']
dt[, muslim:= as.integer(major_religion=='Muslim')]
dt[, traditional:= as.integer(major_religion=='Traditional')]

dt[, smod1960 := sman1960+sser1960]


ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA",
         "SEN", "SLE", "TGO", "UGA", "ZAF",
         "ZMB")

pop <- fread('pop_world.csv')

pop <- pop[iso %in% ISO & year == 1980]
pop[, shrpop := pop / sum(pop)]
pop <- pop[, c('iso', 'shrpop')]

dt[, nobsc := .N, by = iso]
dt[, shrobs := nobsc / .N]
dt[, nobsc := NULL]
dt <- merge(dt, pop, by='iso')
dt[, wt := shrpop / shrobs]

weights <- dt[, c('iso', 'shrpop', 'shrobs', 'wt')]
weights <- unique(weights, by='iso')

RHS <- c('intOIL',
         'intDIA',
         'ln_distCAP',
         'ln_distBORD',
         'ln_distCOAST',
         'zsSUIT',
         'zsMAL',
         'zsTR',
         'ln_pdHYDE50',
         'ln_distRRD',
         'ln_distIMP60',
         'ln_distMIC',
         'ln_distMIP',
         'ln_distEBRQC',
         'ln_distEMURC',
         'surb1960',
         'sagr1960',
         'smod1960')

RHSNSMS <- c("oil field dummy",
             "diamond mine dummy",
             "log(distance to capital)",
             "log(distance to national border)",
             "log(distance to coast)",
             "log(agricultural suitability)",
             "log(stability of malaria transmission)",
             "log(terrain ruggedness)" ,
             "HYDE population density in 1950",
             "log(distance to closest colonial railroad)",
             "log(distance to closest improved or better road in 1960)",
             "log(distance to closest Catholic mission (Nunn only))",
             "log(distance to closest Protestant mission)",
             "log(distance to closest precolonial empire (Besley and Reynal-Querol))",
             "log(distance to closest precolonial state (Murdock))",
             "urban share",
             "agricultural labour share",
             "manufacturing + services labour share")

OUT <- data.table(matrix(NA, length(RHS)*2, 5))
names(OUT) <- c('standardized', 'covariate', 'b', 'se', 'N')
OUT[, standardized := as.character(standardized)]
OUT[, covariate := as.character(covariate)]
OUT[, b := as.numeric(b)]
OUT[, se := as.numeric(se)]
OUT[, N := as.integer(N)]

OUT[, standardized := sort(rep(c('no', 'yes'), length(RHS)))]
OUT[, covariate := rep(RHS,2)]

OUTM <- copy(OUT)
OUTT <- copy(OUT)

for (i in 1:NROW(OUT)) {
  
  standardized <- OUT[i,1]$standardized
  covariate <- OUT[i,2]$covariate
  
  print(paste0(i, ': ', '################ ', standardized, ' ', covariate, ' ################'))
  
  dte <- na.omit(dt[, c(c('cbd', 'province', 'wt', 'muslim', 'traditional'), c(covariate)), with=FALSE])
  dte <- dte[!is.infinite(get(covariate))]
  
  if (standardized == 'yes') {
    mx <- mean(dte[, get(covariate)])
    sx <- sd(dte[, get(covariate)])
    dte[, `:=`(scov = (get(covariate)-mx)/sx)]
    f <- sprintf('scov ~ muslim + traditional | cbd | 0 | cbd+province')
  } else {
    f <- sprintf('%s ~ muslim + traditional | cbd | 0 | cbd+province', covariate)
  }                 
  
  m <- felm(as.formula(f), data=dte, weights=dte[,wt])
  OUTM[i, 3] <- m$coefficients[1]
  OUTM[i, 4] <- sqrt(m$clustervcv[1,1])
  OUTM[i, 5] <- m$N      
  
  OUTT[i, 3] <- m$coefficients[2]
  OUTT[i, 4] <- sqrt(m$clustervcv[2,2])
  OUTT[i, 5] <- m$N  
  
}


###########################################################
###########################################################
###########################################################
###########################################################

for (i in 1:length(RHS)) {
  OUTM[covariate == RHS[i], covariate := RHSNSMS[i]]
  OUTT[covariate == RHS[i], covariate := RHSNSMS[i]]
}

OUTM[, majrel := 'muslim']
OUTT[, majrel := 'traditional']

OUT <- rbind(OUTM, OUTT)
setcolorder(OUT, c('standardized', 'covariate', 'majrel'))
fwrite(OUT, '_simple_meantests_covariates.csv')

