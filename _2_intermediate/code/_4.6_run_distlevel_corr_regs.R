rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(lfe)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
wdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(wdir, showWarnings = FALSE, recursive = TRUE) 

setwd(wdir)

dt <- fread('_data_for_distlevel_corrregs.csv')

dt[, c:= as.integer(as.factor(iso))]
dt[, smod1960 := sman1960+sser1960]


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
         'smod1960',
         'popshare_c',
         'popshare_m',
         'popshare_t',
         'frag_n')

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
             "log(distance to closest precolonial state (Murdock))" ,
             "urban share",
             "agricultural labour share",
             "manufacturing + services labour share",
             "population share christian",
             "population share muslim",
             "population share traditional",
             "religious fragmentation")

check <- data.table(RHS, RHSNSMS)


OUT <- data.table(matrix(NA, length(RHS)*3*2*2*2, 8))
names(OUT) <- c('majrel', 'min10', 'direction', 'lito', 'variable', 'b', 'se', 'N')
OUT[, majrel := as.character(majrel)]
OUT[, min10 := as.character(min10)]
OUT[, direction := as.character(direction)]
OUT[, lito := as.character(lito)]
OUT[, variable := as.character(variable)]
OUT[, b := as.numeric(b)]
OUT[, se := as.numeric(se)]
OUT[, N := as.integer(N)]

OUT[, majrel := sort(rep(c('Christian', 'Muslim', 'Traditional'), 2*2*2*length(RHS)))]
OUT[, min10 := rep(sort(rep(c('no', 'yes'), length(RHS)*2*2)), 3)]
OUT[, direction := rep(sort(rep(c('up', 'down'), length(RHS)*2)), 3*2)]
OUT[, lito := rep(sort(rep(c('no', 'yes'), length(RHS))), 3*2*2)]
OUT[, variable := rep(RHS, 3*2*2*2)]

i <- 1

for (i in 1:NROW(OUT)) {
  
  mr <- OUT[i,1]$majrel
  min10 <- OUT[i,2]$min10
  updn <- OUT[i,3]$direction
  lito <- OUT[i,4]$lito
  rhs <- OUT[i,5]$variable
  
  print(paste0(i, ': ', '################ ', mr, ' ', min10, ' ', updn, ' ', lito, ' ', rhs, ' ################'))
  
  if (updn == 'up') {
    lhs <- 'im'
    nlhs <- 'nim'
  } else {
    lhs <- 'imdw'
    nlhs <- 'nimdw'
  }
  
  if (lito == 'yes') {
    dte <- na.omit(dt[major_religion==mr, c(c('c', 'province'), c(lhs, nlhs, rhs, 'lito')), with=FALSE])
  } else {
    dte <- na.omit(dt[major_religion==mr, c(c('c', 'province'), c(lhs, nlhs, rhs)), with=FALSE])
  }
  
  dte <- dte[!is.infinite(get(rhs))]
  
  if (min10 == 'yes') {
    dte <- dte[get(nlhs) >= 10]  
  }
  
  mx <- mean(dte[, get(rhs)])
  sx <- sd(dte[, get(rhs)])
  my <- mean(dte[, get(lhs)])
  sy <- sd(dte[, get(lhs)])
  
  if (lito == 'yes') {
    mxx <- mean(dte[, lito])
    sxx <- sd(dte[, lito])
    
    dte[, `:=`(slhs = (get(lhs)-my)/sy,
               srhs = (get(rhs)-mx)/sx,
               srhs0 = (lito-mxx)/sxx)]
    f <- sprintf('slhs ~ srhs + srhs0 | c | 0 | c')
  } else {
    dte[, `:=`(slhs = (get(lhs)-my)/sy,
               srhs = (get(rhs)-mx)/sx)]
    f <- sprintf('slhs ~ srhs | c | 0 | c')
  }
  
  m <- felm(as.formula(f), data=dte)
  
  OUT[i, 6] <- m$coefficients[1]
  OUT[i, 7] <- sqrt(m$clustervcv[1,1])
  OUT[i, 8] <- m$N   
  
}

for (i in 1:length(RHS)) {
  OUT[variable == RHS[i], variable := RHSNSMS[i]]
}


fwrite(OUT, '_dist_level_correlates_results.csv')








