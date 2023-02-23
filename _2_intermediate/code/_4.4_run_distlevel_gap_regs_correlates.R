rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(lfe)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
wdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(wdir, showWarnings = FALSE, recursive = TRUE) 

setwd(wdir)

dt <- fread('_data_for_distlevel_gapregs.csv')

# plot(dt$litoc, dt$imc)

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


OUT <- data.table(matrix(NA, length(RHS)*2*2*2, 7))
names(OUT) <- c('gap', 'lit0', 'min10', 'variable', 'b', 'se', 'N')
OUT[, lit0 := as.character(lit0)]
OUT[, gap := as.character(gap)]
OUT[, min10 := as.character(min10)]
OUT[, variable := as.character(variable)]
OUT[, b := as.numeric(b)]
OUT[, se := as.numeric(se)]
OUT[, N := as.integer(N)]

OUT[, lit0 := rep(sort(rep(c('no', 'yes'), 2*length(RHS))),2)]
OUT[, gap := sort(rep(c('christian-muslim', 'christian-traditional'), 2*2*length(RHS)))]
OUT[, min10 := rep(sort(rep(c('no', 'yes'), length(RHS))), 2*2)]
OUT[, variable := rep(RHS, 2*2*2)]

i <- 24

for (i in 1:NROW(OUT)) {
  
  gap <- OUT[i,1]$gap
  lit0 <- OUT[i,2]$lit0
  min10 <- OUT[i,3]$min10
  rhs <- OUT[i,4]$variable

  print(paste0(i, ': ', '################ ', gap, ' ', min10, ' ', rhs, ' ################'))
  
  if (gap == 'christian-muslim') {
    lhs2 <- 'imm'
    nlhs2 <- 'nimm'
    if (lit0 == 'yes') {
      rhs01 <- 'litoc'
      rhs02 <- 'litom' 
    }
  } else {
    lhs2 <- 'imt'
    nlhs2 <- 'nimt'
    if (lit0 == 'yes') {
      rhs01 <- 'litoc'
      rhs02 <- 'litot' 
    }
  }
  
  if (lit0 == 'yes') {
    dte <- na.omit(dt[, c(c('c', 'province', 'imc', 'nimc'), c(lhs2, nlhs2, rhs01, rhs02, rhs)), with=FALSE])
  } else {
    dte <- na.omit(dt[, c(c('c', 'province', 'imc', 'nimc'), c(lhs2, nlhs2, rhs)), with=FALSE])
  }
  dte <- dte[!is.infinite(get(rhs))]
  
  dte[, lhs := imc - get(lhs2)]
  
  if (min10 == 'yes') {
    dte <- dte[nimc >= 10 & (nlhs2) >= 10]  
  }
  
  mx <- mean(dte[, get(rhs)])
  sx <- sd(dte[, get(rhs)])
  my <- mean(dte[, lhs])
  sy <- sd(dte[, lhs])
  
  dte[, `:=`(slhs = (lhs-my)/sy,
             srhs = (get(rhs)-mx)/sx)]
  
  if (lit0 == 'yes') {
    mx01 <- mean(dte[, get(rhs01)])
    sx01 <- sd(dte[, get(rhs01)])
    mx02 <- mean(dte[, get(rhs02)])
    sx02 <- sd(dte[, get(rhs02)])
    
    dte[, `:=`(srhs01 = (get(rhs01)-mx01)/sx01,
               srhs02 = (get(rhs02)-mx02)/sx02)]
  
    f <- sprintf('slhs ~ srhs + srhs01 + srhs02 | c | 0 | c')
  } else {
    f <- sprintf('slhs ~ srhs | c | 0 | c')
  }
  
  m <- felm(as.formula(f), data=dte)
  
  OUT[i, 5] <- m$coefficients[1]
  OUT[i, 6] <- sqrt(m$clustervcv[1,1])
  OUT[i, 7] <- m$N   
  
}

for (i in 1:length(RHS)) {
  OUT[variable == RHS[i], variable := RHSNSMS[i]]
}
  

fwrite(OUT, '_dist_level_gap_regs_correlates_results.csv')
  
  
  
  




