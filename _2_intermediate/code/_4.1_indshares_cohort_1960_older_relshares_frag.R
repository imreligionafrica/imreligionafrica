rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
wdir <- file.path(rootdir, "_2_intermediate", "data")
indir_dis <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts")
indir_io <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "ind_occ")
indir_pwt <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "perwt")
indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
indir_mig <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "migdists")
indir_mys <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "migyrs")
indir_bas <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "basedat")

dir.create(wdir, showWarnings = FALSE, recursive = TRUE) 

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA",
         "SEN", "SLE", "UGA", "ZAF", "ZMB",
         "TGO")


for (isoc in ISO) {
  
  print(isoc)
  
  setwd(indir_io)
  ind <- data.table(readRDS(sprintf("new_ind_occ_%s.rds", isoc)))
  ind <- ind[, c("year", "serial", "pernum", "empind")]
  setwd(indir_edu)
  age <- data.table(readRDS(sprintf("aey_old_mompop_unc_%s.rds", isoc)))
  age <- age[, c("year", "serial", "pernum", "age")]
  setwd(indir_dis)
  dis <- data.table(readRDS(sprintf("districts_%s.rds", isoc)))
  dis <- dis[, c("year", "serial", "pernum", "district")]
  setwd(indir_pwt)
  pwt <- data.table(readRDS(sprintf("perwt_%s.rds", isoc)))
  setwd(indir_rel)
  rel <- data.table(readRDS(sprintf("religions_%s.rds", isoc)))
  rel <- rel[, c("year", "serial", "pernum", "major_religion")]
  setwd(indir_bas)
  bas <- data.table(readRDS(sprintf("basedat_%s.rds", isoc)))
  bas <- bas[, c("year", "serial", "pernum", "urban")]
  
  ind <- merge(ind, dis, by=c("year", "serial", "pernum"))
  ind <- merge(ind, age, by=c("year", "serial", "pernum"))
  rel <- merge(rel, dis, by=c("year", "serial", "pernum"))
  rel <- merge(rel, pwt, by=c("year", "serial", "pernum"))
  
  ####################################################
  # INDUSTRY OF EMPLOYMENT OF COHORT 1960 AND OLDER
  ####################################################
  
  ind[, bd:=floor((year-age)/10)*10]
  ind <- ind[!is.na(bd) & !is.na(empind) & bd<=1960]
  ind[, `:=`(agr = as.numeric(empind==1),
            min = as.numeric(empind==2),
            man = as.numeric(empind==3),
            ser = as.numeric(empind==4 | empind==5 | empind==6)
            )]
  
  # 1 -> 10: agriculture, fishing, forestry
  # 2 -> 20: mining and extraction
  # 3 -> 30: manufacturing
  # 4 -> 40: electricity, gas, water, waste management
  # 5 -> 50: construction
  # 6 -> 60: wholesale and retail trade
  #   -> 70: hotels and restaurants
  #   -> 80: transportation, storage and communications
  #   -> 90: financial services and insurance
  #   -> 100: public admin and defense
  #   -> 110: services, not specified
  #   -> 111: business and real estate
  #   -> 112: education
  #   -> 113: health and social work
  #   -> 114: other services
  #   -> 120: private household services
  
  ind <- ind[, .(nagr = sum(agr),
               nmin = sum(min),
               nman = sum(man),
               nser = sum(ser),
               n = .N),
           by=district]
  ind[
    , `:=`(sagr = nagr/n,
           smin = nmin/n,
           sman = nman/n,
           sser = nser/n)
  ][, c("nagr", "nmin", "nman", "nser"):=NULL]
  
  ind[, iso := isoc]
  setcolorder(ind, "iso")
  
  
  # adding urban share
  
  bas <- merge(bas, age, by=c("year", "serial", "pernum"))
  bas <- merge(bas, dis, by=c("year", "serial", "pernum"))
  bas[, bd:=floor((year-age)/10)*10]
  bas <- bas[!is.na(bd) & !is.na(urban) & bd<=1960]
  bas <- bas[, .(surb = mean(urban)), by=district]
  ind <- merge(ind, bas, by="district")
  

  ######################################
  # RELIGION SHARES, FRAGMENTATION ETC.
  ######################################
  
  rel <- rel[!is.na(major_religion) & major_religion != '']
  
  all_n <- rel[, .(all_n = .N, all_wt = sum(perwt, na.rm=TRUE)), by=c('district')]
  rel_n <- rel[, .(rel_n = .N, rel_wt = sum(perwt, na.rm=TRUE)), by=c('district', 'major_religion')]
  all3_n <- rel[major_religion != 'No Religion' & major_religion != 'Other'
             , .(all_n3 = .N, all_wt3 = sum(perwt, na.rm=TRUE)), by=c('district')]
  rel3_n <- rel[major_religion != 'No Religion' & major_religion != 'Other',
             .(rel_n3 = .N, rel_wt3 = sum(perwt, na.rm=TRUE)), by=c('district', 'major_religion')]
  
  all <- merge(all_n, all3_n, by = c('district'), all=TRUE)
  rel <- merge(rel_n, rel3_n, by = c('district', 'major_religion'), all=TRUE)
  shr <- merge(all, rel, by = c('district'), all=TRUE)
  
  rm(all, rel, all3_n, rel3_n, all_n, rel_n)
  
  shr[, `:=`(shr_n = rel_n / all_n,
             shr_wt = rel_wt / all_wt,
             shr_n3 = rel_n3 / all_n3,
             shr_wt3 = rel_wt3 / all_wt3
  )]
  shr <- shr[, c('district', 'major_religion', 'shr_n', 'shr_wt', 'shr_n3', 'shr_wt3')]
  
  shr[, `:=`(frag_n = 1-sum(shr_n^2, na.rm=TRUE), 
             frag_wt = 1-sum(shr_wt^2, na.rm=TRUE), 
             pol_n = 1-sum((((0.5 - shr_n)/0.5)^2) * shr_n, na.rm=TRUE),
             pol_wt = 1-sum((((0.5 - shr_wt)/0.5)^2) * shr_wt, na.rm=TRUE),
             frag_n3 = 1-sum(shr_n3^2, na.rm=TRUE), 
             frag_wt3 = 1-sum(shr_wt3^2, na.rm=TRUE), 
             pol_n3 = 1-sum((((0.5 - shr_n3)/0.5)^2) * shr_n3, na.rm=TRUE),
             pol_wt3 = 1-sum((((0.5 - shr_wt3)/0.5)^2) * shr_wt3, na.rm=TRUE)), 
      by=c('district')]
  
  # plot(shr$pol_n3, shr$frag_n3)
  # plot(shr$pol_n, shr$frag_n)
  
  shr <- shr[, c('district', 'major_religion',
                 'shr_n', 'shr_wt', 'shr_n3', 'shr_wt3',
                 'frag_n', 'frag_wt', 'frag_n3', 'frag_wt3',
                 'pol_n', 'pol_wt', 'pol_n3', 'pol_wt3')]
  
  
  if (isoc == "BEN") {
    IND <- ind
    SHR <- shr
  } else {
    IND <- rbind(IND, ind)
    SHR <- rbind(SHR, shr)
  }
  
}

setwd(wdir)

fwrite(IND, "indshares_district_cohort_1960_and_older.csv")
fwrite(SHR, "relshares_relfrag_replpol_district.csv")

IND[, x:= sagr + sman + sser + smin]
table(IND$x)

plot(SHR$pol_n3, SHR$frag_n3)
plot(SHR$pol_n, SHR$frag_n)

