rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
indir_pro <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "provinces")
indir_dis <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts")
indir_bas <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "basedat")
indir_mig <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "migdists")
outdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)  
  
assign_mg <- function(DT, varsuff, minag, maxag) {
    
  # Function to assign whether a household is multigen or not
  # depends on
  # 1) concept of old education (0, mom, pop, etc.)
  # 2) maximum age of the kids in the sample
  
  varvec <- c('year', 'serial', 'pernum', 'gencode', 'age', 'eckid', sprintf('ec%s', varsuff))
  x <- DT[, ..varvec]
  x1 <- x[!is.na(eckid) & !is.na(get(sprintf('ec%s', varsuff))) & !is.na(gencode) & age >= minag & age <= maxag]
  x1 <- unique(x1, by = c('year', 'serial', 'gencode'))[, ng := .N, by = c('year', 'serial')]
  x1 <- unique(x1, by = c('year', 'serial'))[, c('year', 'serial', 'ng')]
  x1[, (sprintf('mgec%s%s%s', varsuff, minag, maxag)) := as.integer(ng > 1)][, ng:=NULL]
  x <- x[, c('year', 'serial', 'pernum')]
  x <- merge(x, x1, by = c('year', 'serial'), all.x=TRUE)
  x[is.na(get(sprintf('mgec%s%s%s', varsuff, minag, maxag))), (sprintf('mgec%s%s%s', varsuff, minag, maxag)):= 0]
  
  DT <- merge(DT, x, c('year', 'serial', 'pernum'), all.x=TRUE)
  
  return(DT)
    
}
  
  
ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA",
         "SEN", "SLE", "UGA", "ZAF", "ZMB",
         "TGO")
  
for (isoc in ISO) {
  
  print(sprintf('%s: loading data', isoc))
  
  setwd(indir_rel)
  reli <- data.table(readRDS(sprintf('religions_%s.rds', isoc)))
  setwd(indir_edu)
  educ <- data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', isoc)))
  setwd(indir_bas)
  base <- data.table(readRDS(sprintf('basedat_%s.rds', isoc)))
  setwd(indir_dis)
  dist <- data.table(readRDS(sprintf('districts_%s.rds', isoc)))
  setwd(indir_pro)
  prov <- data.table(readRDS(sprintf('provinces_%s.rds', isoc)))
  setwd(indir_mig)
  migr <- data.table(readRDS(sprintf('migdists_%s.rds', isoc)))

  
  reli <- reli[,c('year', 'serial', 'pernum', 'major_religion')]
  base <- base[,c('year', 'serial', 'pernum','urban')]  
  educ <- educ[,c('year', 'serial', 'pernum', 'gencode',
                  'age', 'ysc', 'eckid', 'ec0')]
  
  
  print(sprintf('%s: merging data', isoc))
  
  dat <- merge(base, educ, by = c('year', 'serial', 'pernum'))
  dat <- merge(dat, reli, by = c('year', 'serial', 'pernum'))
  dat <- merge(dat, dist, by = c('year', 'serial', 'pernum'))
  dat <- merge(dat, prov, by = c('year', 'serial', 'pernum'))
  dat <- merge(dat, migr, by = c('year', 'serial', 'pernum'))
  
  rm(base, reli, educ, dist, prov, migr)

  print(sprintf('%s: assigning cohort and migrant dummies', isoc))
  
  dat[, byr := year-age]
  dat[, bch10 := 10*(byr %/% 10)]
  dat[, bch5 := 5*(byr %/% 5)]

  if (isoc != "NGA") {
    dat[!is.na(migdistr) & !is.na(migdistr_bplmatch),
        migr := as.numeric(migdistr != migdistr_bplmatch)]
  }
  
  for (maxag in c(18, 25, 100)) {
    print(sprintf('assigning MG status, concept = %s, maxag = %s', '0', maxag))
    dat <- assign_mg(dat, '0', 14, maxag)
  }
  
  
  dat[, district_ur := paste0(district, '_', urban)]
  

  # ################################################################
  # ################################################################
  # ################################################################
  # # country-level
  # ################################################################
  # ################################################################
  # ################################################################
  
  # ################################################################
  # # country-level literacy and years of schooling
  # ################################################################
  
  print(sprintf('%s: country-level literacy', isoc))
  
  lit_14_10 <- dat[age >= 14 & !is.na(age) &
                     !is.na(eckid) & major_religion!="",
                   .(lit_14 = mean(eckid >= 1, na.rm = TRUE),
                     n_lit_14 = .N), by = c('bch10', 'major_religion')]
  lit_14_5 <- dat[age >= 14 & !is.na(age) &
                    !is.na(eckid) & major_religion!="",
                  .(lit_14 = mean(eckid >= 1, na.rm = TRUE),
                    n_lit_14 = .N), by = c('bch5', 'major_religion')]
  lit_18_10 <- dat[age >= 18 & !is.na(age) &
                     !is.na(eckid) & major_religion!="",
                   .(lit_18 = mean(eckid >= 1, na.rm = TRUE),
                     n_lit_18 = .N), by = c('bch10', 'major_religion')]
  lit_18_5 <- dat[age >= 18 & !is.na(age) &
                    !is.na(eckid) & major_religion!="",
                  .(lit_18 = mean(eckid >= 1, na.rm = TRUE),
                    n_lit_18 = .N), by = c('bch5', 'major_religion')]
  lit_25_10 <- dat[age >= 25 & !is.na(age) &
                     !is.na(eckid) & major_religion!="",
                   .(lit_25 = mean(eckid >= 1, na.rm = TRUE),
                     n_lit_25 = .N), by = c('bch10', 'major_religion')]
  lit_25_5 <- dat[age >= 25 & !is.na(age) &
                    !is.na(eckid) & major_religion!="",
                  .(lit_25 = mean(eckid >= 1, na.rm = TRUE),
                    n_lit_25 = .N), by = c('bch5', 'major_religion')]
  
  
  print(sprintf('%s: country-level years of schooling', isoc))
  
  ysc_14_10 <- dat[age >= 14 & !is.na(age) &
                     !is.na(ysc) & major_religion!="",
                   .(ysc_14 = mean(ysc, na.rm = TRUE),
                     n_ysc_14 = .N), by = c('bch10', 'major_religion')]
  ysc_14_5 <- dat[age >= 14 & !is.na(age) &
                    !is.na(ysc) & major_religion!="",
                  .(ysc_14 = mean(ysc, na.rm = TRUE),
                    n_ysc_14 = .N), by = c('bch5', 'major_religion')]
  ysc_18_10 <- dat[age >= 18 & !is.na(age) &
                     !is.na(ysc) & major_religion!="",
                   .(ysc_18 = mean(ysc, na.rm = TRUE),
                     n_ysc_18 = .N), by = c('bch10', 'major_religion')]
  ysc_18_5 <- dat[age >= 18 & !is.na(age) &
                    !is.na(ysc) & major_religion!="",
                  .(ysc_18 = mean(ysc, na.rm = TRUE),
                    n_ysc_18 = .N), by = c('bch5', 'major_religion')]
  ysc_25_10 <- dat[age >= 25 & !is.na(age) &
                     !is.na(ysc) & major_religion!="",
                   .(ysc_25 = mean(ysc, na.rm = TRUE),
                     n_ysc_25 = .N), by = c('bch10', 'major_religion')]
  ysc_25_5 <- dat[age >= 25 & !is.na(age) &
                    !is.na(ysc) & major_religion!="",
                  .(ysc_25 = mean(ysc, na.rm = TRUE),
                    n_ysc_25 = .N), by = c('bch5', 'major_religion')]
  
  
  # ################################################################
  # # country-level IM, with and without MG households
  # ################################################################
  
  print(sprintf('%s: country-level up-im, with mg hhs', isoc))
  
  immg_100_10 <- dat[age >= 14 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="",
                     .(immg_100 = mean(eckid >= 1, na.rm = TRUE),
                       n_immg_100 = .N), by = c('bch10', 'major_religion')]
  immg_100_5 <- dat[age >= 14 & !is.na(age) &
                      !is.na(eckid) & ec0 == 0 &
                      major_religion!="",
                    .(immg_100 = mean(eckid >= 1, na.rm = TRUE),
                      n_immg_100 = .N), by = c('bch5', 'major_religion')]
  immg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                      !is.na(eckid) & ec0 == 0 &
                      major_religion!="",
                    .(immg_25 = mean(eckid >= 1, na.rm = TRUE),
                      n_immg_25 = .N), by = c('bch10', 'major_religion')]
  immg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                     !is.na(eckid) & ec0 == 0 &
                     major_religion!="",
                   .(immg_25 = mean(eckid >= 1, na.rm = TRUE),
                     n_immg25 = .N), by = c('bch5', 'major_religion')]
  immg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                      !is.na(eckid) & ec0 == 0 &
                      major_religion!="",
                    .(immg_18 = mean(eckid >= 1, na.rm = TRUE),
                      n_immg_18 = .N), by = c('bch10', 'major_religion')]
  immg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                     !is.na(eckid) & ec0 == 0 &
                     major_religion!="",
                   .(immg_18 = mean(eckid >= 1, na.rm = TRUE),
                     n_immg_18 = .N), by = c('bch5', 'major_religion')]
  
  print(sprintf('%s: country-level up-im, no mg hhs', isoc))
  
  imnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & mgec014100 == 0,
                       .(imnomg_100 = mean(eckid >= 1, na.rm = TRUE),
                         n_imnomg_100 = .N), by = c('bch10', 'major_religion')]
  imnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & mgec014100 == 0,
                      .(imnomg_100 = mean(eckid >= 1, na.rm = TRUE),
                        n_imnomg_100 = .N), by = c('bch5', 'major_religion')]
  imnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & mgec01425 == 0,
                      .(imnomg_25 = mean(eckid >= 1, na.rm = TRUE),
                        n_imnomg_25 = .N), by = c('bch10', 'major_religion')]
  imnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="" & mgec01425 == 0,
                     .(imnomg_25 = mean(eckid >= 1, na.rm = TRUE),
                       n_imnomg_25 = .N), by = c('bch5', 'major_religion')]
  imnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & mgec01418 == 0,
                      .(imnomg_18 = mean(eckid >= 1, na.rm = TRUE),
                        n_imnomg_18 = .N), by = c('bch10', 'major_religion')]
  imnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="" & mgec01418 == 0,
                     .(imnomg_18 = mean(eckid >= 1, na.rm = TRUE),
                       n_imnomg_18 = .N), by = c('bch5', 'major_religion')]
  
  print(sprintf('%s: country-level down-im, with mg hhs', isoc))
  
  imdwmg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="",
                       .(imdwmg_100 = mean(eckid == 0, na.rm = TRUE),
                         n_imdwmg_100 = .N), by = c('bch10', 'major_religion')]
  imdwmg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & ec0 >= 1 &
                        major_religion!="",
                      .(imdwmg_100 = mean(eckid == 0, na.rm = TRUE),
                        n_imdwmg_100 = .N), by = c('bch5', 'major_religion')]
  imdwmg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & ec0 >= 1 &
                        major_religion!="",
                      .(imdwmg_25 = mean(eckid == 0, na.rm = TRUE),
                        n_imdwmg_25 = .N), by = c('bch10', 'major_religion')]
  imdwmg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & ec0 >= 1 &
                       major_religion!="",
                     .(imdwmg_25 = mean(eckid == 0, na.rm = TRUE),
                       n_imdwmg25 = .N), by = c('bch5', 'major_religion')]
  imdwmg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & ec0 >= 1 &
                        major_religion!="",
                      .(imdwmg_18 = mean(eckid == 0, na.rm = TRUE),
                        n_imdwmg_18 = .N), by = c('bch10', 'major_religion')]
  imdwmg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & ec0 >= 1 &
                       major_religion!="",
                     .(imdwmg_18 = mean(eckid == 0, na.rm = TRUE),
                       n_imdwmg_18 = .N), by = c('bch5', 'major_religion')]
  
  print(sprintf('%s: country-level down-im, no mg hhs', isoc))
  
  imdwnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & mgec014100 == 0,
                         .(imdwnomg_100 = mean(eckid == 0, na.rm = TRUE),
                           n_imdwnomg_100 = .N), by = c('bch10', 'major_religion')]
  imdwnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & mgec014100 == 0,
                        .(imdwnomg_100 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwnomg_100 = .N), by = c('bch5', 'major_religion')]
  imdwnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & mgec01425 == 0,
                        .(imdwnomg_25 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwnomg_25 = .N), by = c('bch10', 'major_religion')]
  imdwnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="" & mgec01425 == 0,
                       .(imdwnomg_25 = mean(eckid == 0, na.rm = TRUE),
                         n_imdwnomg_25 = .N), by = c('bch5', 'major_religion')]
  imdwnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & mgec01418 == 0,
                        .(imdwnomg_18 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwnomg_18 = .N), by = c('bch10', 'major_religion')]
  imdwnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="" & mgec01418 == 0,
                       .(imdwnomg_18 = mean(eckid == 0, na.rm = TRUE),
                         n_imdwnomg_18 = .N), by = c('bch5', 'major_religion')]
  
  
  # ################################################################
  # # country-level parental literacy, with and without MG households
  # ################################################################
  
  print(sprintf('%s: country-level parental literacy, with mg hhs', isoc))
  
  litomg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="",
                       .(litomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                         n_litomg_100 = .N), by = c('bch10', 'major_religion')]
  litomg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & !is.na(ec0) &
                        major_religion!="",
                      .(litomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                        n_litomg_100 = .N), by = c('bch5', 'major_religion')]
  litomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & !is.na(ec0) &
                        major_religion!="",
                      .(litomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                        n_litomg_25 = .N), by = c('bch10', 'major_religion')]
  litomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & !is.na(ec0) &
                       major_religion!="",
                     .(litomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                       n_litomg_25 = .N), by = c('bch5', 'major_religion')]
  litomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & !is.na(ec0) &
                        major_religion!="",
                      .(litomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                        n_litomg_18 = .N), by = c('bch10', 'major_religion')]
  litomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & !is.na(ec0) &
                       major_religion!="",
                     .(litomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                       n_litomg_18 = .N), by = c('bch5', 'major_religion')]
  
  print(sprintf('%s: country-level parental literacy, no mg hhs', isoc))
  
  litonomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & mgec014100 == 0,
                         .(litonomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                           n_litonomg_100 = .N), by = c('bch10', 'major_religion')]
  litonomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & mgec014100 == 0,
                        .(litonomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litonomg_100 = .N), by = c('bch5', 'major_religion')]
  litonomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & mgec01425 == 0,
                        .(litonomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litonomg_25 = .N), by = c('bch10', 'major_religion')]
  litonomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="" & mgec01425 == 0,
                       .(litonomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                         n_litonomg_25 = .N), by = c('bch5', 'major_religion')]
  litonomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & mgec01418 == 0,
                        .(litonomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litonomg_18 = .N), by = c('bch10', 'major_religion')]
  litonomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="" & mgec01418 == 0,
                       .(litonomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                         n_litonomg_18 = .N), by = c('bch5', 'major_religion')]
  
  
  ctry_10 <- merge(lit_14_10, lit_18_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, lit_25_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, ysc_14_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, ysc_18_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, ysc_25_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, immg_100_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, immg_25_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, immg_18_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, imnomg_100_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, imnomg_25_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, imnomg_18_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, imdwmg_100_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, imdwmg_25_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, imdwmg_18_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, imdwnomg_100_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, imdwnomg_25_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, imdwnomg_18_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, litomg_100_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, litomg_25_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, litomg_18_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, litonomg_100_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, litonomg_25_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_10 <- merge(ctry_10, litonomg_18_10, by = c('bch10', 'major_religion'), all.x = TRUE, all.y = TRUE)
  
  ctry_5 <- merge(lit_14_5, lit_18_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, lit_25_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, ysc_14_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, ysc_18_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, ysc_25_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, immg_100_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, immg_25_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, immg_18_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, imnomg_100_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, imnomg_25_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, imnomg_18_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, imdwmg_100_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, imdwmg_25_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, imdwmg_18_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, imdwnomg_100_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, imdwnomg_25_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, imdwnomg_18_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, litomg_100_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, litomg_25_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, litomg_18_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, litonomg_100_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, litonomg_25_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  ctry_5 <- merge(ctry_5, litonomg_18_5, by = c('bch5', 'major_religion'), all.x = TRUE, all.y = TRUE)
  
  rm(lit_14_10, lit_18_10, lit_25_10, ysc_14_10, ysc_18_10, ysc_25_10,
     immg_100_10, immg_25_10, immg_18_10, imnomg_100_10, imnomg_25_10, imnomg_18_10,
     imdwmg_100_10, imdwmg_25_10, imdwmg_18_10, imdwnomg_100_10, imdwnomg_25_10, imdwnomg_18_10,
     litomg_100_10, litomg_25_10, litomg_18_10, litonomg_100_10, litonomg_25_10, litonomg_18_10)
  rm(lit_14_5, lit_18_5, lit_25_5, ysc_14_5, ysc_18_5, ysc_25_5,
     immg_100_5, immg_25_5, immg_18_5, imnomg_100_5, imnomg_25_5, imnomg_18_5,
     imdwmg_100_5, imdwmg_25_5, imdwmg_18_5, imdwnomg_100_5, imdwnomg_25_5, imdwnomg_18_5,
     litomg_100_5, litomg_25_5, litomg_18_5, litonomg_100_5, litonomg_25_5, litonomg_18_5)
  
  # ################################################################
  # ################################################################
  # ################################################################
  # # province-level, including migrants
  # ################################################################
  # ################################################################
  # ################################################################
  
  
  # ################################################################
  # # province-level literacy and years of schooling
  # ################################################################
  
  print(sprintf('%s: province-level literacy, incl. migrants', isoc))
  
  lit_14_10 <- dat[age >= 14 & !is.na(age) &
                     !is.na(eckid) & major_religion!="",
                   .(lit_14 = mean(eckid >= 1, na.rm = TRUE),
                     n_lit_14 = .N), by = c('bch10', 'major_religion', 'province')]
  lit_14_5 <- dat[age >= 14 & !is.na(age) &
                    !is.na(eckid) & major_religion!="",
                  .(lit_14 = mean(eckid >= 1, na.rm = TRUE),
                    n_lit_14 = .N), by = c('bch5', 'major_religion', 'province')]
  lit_18_10 <- dat[age >= 18 & !is.na(age) &
                     !is.na(eckid) & major_religion!="",
                   .(lit_18 = mean(eckid >= 1, na.rm = TRUE),
                     n_lit_18 = .N), by = c('bch10', 'major_religion', 'province')]
  lit_18_5 <- dat[age >= 18 & !is.na(age) &
                    !is.na(eckid) & major_religion!="",
                  .(lit_18 = mean(eckid >= 1, na.rm = TRUE),
                    n_lit_14 = .N), by = c('bch5', 'major_religion', 'province')]
  lit_25_10 <- dat[age >= 25 & !is.na(age) &
                     !is.na(eckid) & major_religion!="",
                   .(lit_25 = mean(eckid >= 1, na.rm = TRUE),
                     n_lit_25 = .N), by = c('bch10', 'major_religion', 'province')]
  lit_25_5 <- dat[age >= 25 & !is.na(age) &
                    !is.na(eckid) & major_religion!="",
                  .(lit_25 = mean(eckid >= 1, na.rm = TRUE),
                    n_lit_25 = .N), by = c('bch5', 'major_religion', 'province')]
  
  
  print(sprintf('%s: province-level years of schooling, incl. migrants', isoc))
  
  ysc_14_10 <- dat[age >= 14 & !is.na(age) &
                     !is.na(ysc) & major_religion!="",
                   .(ysc_14 = mean(ysc, na.rm = TRUE),
                     n_ysc_14 = .N), by = c('bch10', 'major_religion', 'province')]
  ysc_14_5 <- dat[age >= 14 & !is.na(age) &
                    !is.na(ysc) & major_religion!="",
                  .(ysc_14 = mean(ysc, na.rm = TRUE),
                    n_ysc_14 = .N), by = c('bch5', 'major_religion', 'province')]
  ysc_18_10 <- dat[age >= 18 & !is.na(age) &
                     !is.na(ysc) & major_religion!="",
                   .(ysc_18 = mean(ysc, na.rm = TRUE),
                     n_ysc_18 = .N), by = c('bch10', 'major_religion', 'province')]
  ysc_18_5 <- dat[age >= 18 & !is.na(age) &
                    !is.na(ysc) & major_religion!="",
                  .(ysc_18 = mean(ysc, na.rm = TRUE),
                    n_ysc_18 = .N), by = c('bch5', 'major_religion', 'province')]
  ysc_25_10 <- dat[age >= 25 & !is.na(age) &
                     !is.na(ysc) & major_religion!="",
                   .(ysc_25 = mean(ysc, na.rm = TRUE),
                     n_ysc_25 = .N), by = c('bch10', 'major_religion', 'province')]
  ysc_25_5 <- dat[age >= 25 & !is.na(age) &
                    !is.na(ysc) & major_religion!="",
                  .(ysc_25 = mean(ysc, na.rm = TRUE),
                    n_ysc_25 = .N), by = c('bch5', 'major_religion', 'province')]
  
  
  # ################################################################
  # # province-level IM, with and without MG households
  # ################################################################
  
  print(sprintf('%s: province-level up-im, with mg hhs, incl. migrants', isoc))
  
  immg_100_10 <- dat[age >= 14 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="",
                     .(immg_100 = mean(eckid >= 1, na.rm = TRUE),
                       n_immg_100 = .N), by = c('bch10', 'major_religion', 'province')]
  immg_100_5 <- dat[age >= 14 & !is.na(age) &
                      !is.na(eckid) & ec0 == 0 &
                      major_religion!="",
                    .(immg_100 = mean(eckid >= 1, na.rm = TRUE),
                      n_immg_100 = .N), by = c('bch5', 'major_religion', 'province')]
  immg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                      !is.na(eckid) & ec0 == 0 &
                      major_religion!="",
                    .(immg_25 = mean(eckid >= 1, na.rm = TRUE),
                      n_immg_25 = .N), by = c('bch10', 'major_religion', 'province')]
  immg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                     !is.na(eckid) & ec0 == 0 &
                     major_religion!="",
                   .(immg_25 = mean(eckid >= 1, na.rm = TRUE),
                     n_immg_25 = .N), by = c('bch5', 'major_religion', 'province')]
  immg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                      !is.na(eckid) & ec0 == 0 &
                      major_religion!="",
                    .(immg_18 = mean(eckid >= 1, na.rm = TRUE),
                      n_immg_18 = .N), by = c('bch10', 'major_religion', 'province')]
  immg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                     !is.na(eckid) & ec0 == 0 &
                     major_religion!="",
                   .(immg_18 = mean(eckid >= 1, na.rm = TRUE),
                     n_immg_18 = .N), by = c('bch5', 'major_religion', 'province')]
  
  print(sprintf('%s: province-level up-im, no mg hhs, incl. migrants', isoc))
  
  imnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & mgec014100 == 0,
                       .(imnomg_100 = mean(eckid >= 1, na.rm = TRUE),
                         n_imnomg_100 = .N), by = c('bch10', 'major_religion', 'province')]
  imnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & mgec014100 == 0,
                      .(imnomg_100 = mean(eckid >= 1, na.rm = TRUE),
                        n_imnomg_100 = .N), by = c('bch5', 'major_religion', 'province')]
  imnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & mgec01425 == 0,
                      .(imnomg_25 = mean(eckid >= 1, na.rm = TRUE),
                        n_imnomg_25 = .N), by = c('bch10', 'major_religion', 'province')]
  imnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="" & mgec01425 == 0,
                     .(imnomg_25 = mean(eckid >= 1, na.rm = TRUE),
                       n_imnomg_25 = .N), by = c('bch5', 'major_religion', 'province')]
  imnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & mgec01418 == 0,
                      .(imnomg_18 = mean(eckid >= 1, na.rm = TRUE),
                        n_imnomg_18 = .N), by = c('bch10', 'major_religion', 'province')]
  imnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="" & mgec01418 == 0,
                     .(imnomg_18 = mean(eckid >= 1, na.rm = TRUE),
                       n_imnomg_18 = .N), by = c('bch5', 'major_religion', 'province')]
  
  
  print(sprintf('%s: province-level down-im, with mg hhs, incl. migrants', isoc))
  
  imdwmg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="",
                       .(imdwmg_100 = mean(eckid == 0, na.rm = TRUE),
                         n_imdwmg_100 = .N), by = c('bch10', 'major_religion', 'province')]
  imdwmg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & ec0 >= 1 &
                        major_religion!="",
                      .(imdwmg_100 = mean(eckid == 0, na.rm = TRUE),
                        n_imdwmg_100 = .N), by = c('bch5', 'major_religion', 'province')]
  imdwmg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & ec0 >= 1 &
                        major_religion!="",
                      .(imdwmg_25 = mean(eckid == 0, na.rm = TRUE),
                        n_imdwmg_25 = .N), by = c('bch10', 'major_religion', 'province')]
  imdwmg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & ec0 >= 1 &
                       major_religion!="",
                     .(imdwmg_25 = mean(eckid == 0, na.rm = TRUE),
                       n_imdwmg_25 = .N), by = c('bch5', 'major_religion', 'province')]
  imdwmg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & ec0 >= 1 &
                        major_religion!="",
                      .(imdwmg_18 = mean(eckid == 0, na.rm = TRUE),
                        n_imdwmg_18 = .N), by = c('bch10', 'major_religion', 'province')]
  imdwmg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & ec0 >= 1 &
                       major_religion!="",
                     .(imdwmg_18 = mean(eckid == 0, na.rm = TRUE),
                       n_imdwmg_18 = .N), by = c('bch5', 'major_religion', 'province')]
  
  print(sprintf('%s: province-level down-im, no mg hhs, incl. migrants', isoc))
  
  imdwnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & mgec014100 == 0,
                         .(imdwnomg_100 = mean(eckid == 0, na.rm = TRUE),
                           n_imdwnomg_100 = .N), by = c('bch10', 'major_religion', 'province')]
  imdwnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & mgec014100 == 0,
                        .(imdwnomg_100 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwnomg_100 = .N), by = c('bch5', 'major_religion', 'province')]
  imdwnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & mgec01425 == 0,
                        .(imdwnomg_25 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwnomg_25 = .N), by = c('bch10', 'major_religion', 'province')]
  imdwnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="" & mgec01425 == 0,
                       .(imdwnomg_25 = mean(eckid == 0, na.rm = TRUE),
                         n_imdwnomg_25 = .N), by = c('bch5', 'major_religion', 'province')]
  imdwnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & mgec01418 == 0,
                        .(imdwnomg_18 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwnomg_18 = .N), by = c('bch10', 'major_religion', 'province')]
  imdwnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="" & mgec01418 == 0,
                       .(imdwnomg_18 = mean(eckid == 0, na.rm = TRUE),
                         n_imdwnomg_18 = .N), by = c('bch5', 'major_religion', 'province')]
  
  
  # ################################################################
  # # province-level parental literacy, with and without MG households
  # ################################################################
  
  print(sprintf('%s: province-level parental literacy, with mg hhs, incl. migrants', isoc))
  
  litomg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="",
                       .(litomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                         n_litomg_100 = .N), by = c('bch10', 'major_religion', 'province')]
  litomg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & !is.na(ec0) &
                        major_religion!="",
                      .(litomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                        n_litomg_100 = .N), by = c('bch5', 'major_religion', 'province')]
  litomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & !is.na(ec0) &
                        major_religion!="",
                      .(litomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                        n_litomg_25 = .N), by = c('bch10', 'major_religion', 'province')]
  litomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & !is.na(ec0) &
                       major_religion!="",
                     .(litomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                       n_litomg_25 = .N), by = c('bch5', 'major_religion', 'province')]
  litomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & !is.na(ec0) &
                        major_religion!="",
                      .(litomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                        n_litomg_18 = .N), by = c('bch10', 'major_religion', 'province')]
  litomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & !is.na(ec0) &
                       major_religion!="",
                     .(litomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                       n_litomg_18 = .N), by = c('bch5', 'major_religion', 'province')]
  
  print(sprintf('%s: province-level parental literacy, no mg hhs, incl. migrants', isoc))
  
  litonomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & mgec014100 == 0,
                         .(litonomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                           n_litonomg_100 = .N), by = c('bch10', 'major_religion', 'province')]
  litonomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & mgec014100 == 0,
                        .(litonomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litonomg_100 = .N), by = c('bch5', 'major_religion', 'province')]
  litonomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & mgec01425 == 0,
                        .(litonomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litonomg_25 = .N), by = c('bch10', 'major_religion', 'province')]
  litonomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="" & mgec01425 == 0,
                       .(litonomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                         n_litonomg_25 = .N), by = c('bch5', 'major_religion', 'province')]
  litonomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & mgec01418 == 0,
                        .(litonomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litonomg_18 = .N), by = c('bch10', 'major_religion', 'province')]
  litonomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="" & mgec01418 == 0,
                       .(litonomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                         n_litonomg_18 = .N), by = c('bch5', 'major_religion', 'province')]
  
  
  prov_10 <- merge(lit_14_10, lit_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, lit_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, ysc_14_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, ysc_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, ysc_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, immg_100_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, immg_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, immg_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, imnomg_100_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, imnomg_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, imnomg_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, imdwmg_100_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, imdwmg_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, imdwmg_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, imdwnomg_100_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, imdwnomg_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, imdwnomg_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, litomg_100_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, litomg_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, litomg_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, litonomg_100_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, litonomg_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_10 <- merge(prov_10, litonomg_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  
  prov_5 <- merge(lit_14_5, lit_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, lit_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, ysc_14_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, ysc_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, ysc_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, immg_100_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, immg_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, immg_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, imnomg_100_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, imnomg_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, imnomg_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, imdwmg_100_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, imdwmg_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, imdwmg_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, imdwnomg_100_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, imdwnomg_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, imdwnomg_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, litomg_100_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, litomg_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, litomg_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, litonomg_100_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, litonomg_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  prov_5 <- merge(prov_5, litonomg_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
  
  
  rm(lit_14_10, lit_18_10, lit_25_10, ysc_14_10, ysc_18_10, ysc_25_10,
     immg_100_10, immg_25_10, immg_18_10, imnomg_100_10, imnomg_25_10, imnomg_18_10,
     imdwmg_100_10, imdwmg_25_10, imdwmg_18_10, imdwnomg_100_10, imdwnomg_25_10, imdwnomg_18_10,
     litomg_100_10, litomg_25_10, litomg_18_10, litonomg_100_10, litonomg_25_10, litonomg_18_10)
  rm(lit_14_5, lit_18_5, lit_25_5, ysc_14_5, ysc_18_5, ysc_25_5,
     immg_100_5, immg_25_5, immg_18_5, imnomg_100_5, imnomg_25_5, imnomg_18_5,
     imdwmg_100_5, imdwmg_25_5, imdwmg_18_5, imdwnomg_100_5, imdwnomg_25_5, imdwnomg_18_5,
     litomg_100_5, litomg_25_5, litomg_18_5, litonomg_100_5, litonomg_25_5, litonomg_18_5)

    
  # ################################################################
  # ################################################################
  # ################################################################
  # # province-level, excluding migrants
  # ################################################################
  # ################################################################
  # ################################################################
  
  
  # ################################################################
  # # province-level literacy and years of schooling
  # ################################################################
  

  if (isoc != "NGA") {

    print(sprintf('%s: province-level literacy, excl. migrants', isoc))
    
    lit_14_10 <- dat[age >= 14 & !is.na(age) &
                       !is.na(eckid) & major_religion!="" & migr == 0,
                     .(lit_14_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_lit_14_nm = .N), by = c('bch10', 'major_religion', 'province')]
    lit_14_5 <- dat[age >= 14 & !is.na(age) &
                      !is.na(eckid) & major_religion!="" & migr == 0,
                    .(lit_14_nm = mean(eckid >= 1, na.rm = TRUE),
                      n_lit_14_nm = .N), by = c('bch5', 'major_religion', 'province')]
    lit_18_10 <- dat[age >= 18 & !is.na(age) &
                       !is.na(eckid) & major_religion!="" & migr == 0,
                     .(lit_18_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_lit_18_nm = .N), by = c('bch10', 'major_religion', 'province')]
    lit_18_5 <- dat[age >= 18 & !is.na(age) &
                      !is.na(eckid) & major_religion!="" & migr == 0,
                    .(lit_18_nm = mean(eckid >= 1, na.rm = TRUE),
                      n_lit_18_nm = .N), by = c('bch5', 'major_religion', 'province')]
    lit_25_10 <- dat[age >= 25 & !is.na(age) &
                       !is.na(eckid) & major_religion!="" & migr == 0,
                     .(lit_25_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_lit_25_nm = .N), by = c('bch10', 'major_religion', 'province')]
    lit_25_5 <- dat[age >= 25 & !is.na(age) &
                      !is.na(eckid) & major_religion!="" & migr == 0,
                    .(lit_25_nm = mean(eckid >= 1, na.rm = TRUE),
                      n_lit_25_nm = .N), by = c('bch5', 'major_religion', 'province')]
    
    
    print(sprintf('%s: province-level years of schooling, excl. migrants', isoc))
    
    ysc_14_10 <- dat[age >= 14 & !is.na(age) &
                       !is.na(ysc) & major_religion!="" & migr == 0,
                     .(ysc_14_nm = mean(ysc, na.rm = TRUE),
                       n_ysc_14_nm = .N), by = c('bch10', 'major_religion', 'province')]
    ysc_14_5 <- dat[age >= 14 & !is.na(age) &
                      !is.na(ysc) & major_religion!="" & migr == 0,
                    .(ysc_14_nm = mean(ysc, na.rm = TRUE),
                      n_ysc_14_nm = .N), by = c('bch5', 'major_religion', 'province')]
    ysc_18_10 <- dat[age >= 18 & !is.na(age) &
                       !is.na(ysc) & major_religion!="" & migr == 0,
                     .(ysc_18_nm = mean(ysc, na.rm = TRUE),
                       n_ysc_18_nm = .N), by = c('bch10', 'major_religion', 'province')]
    ysc_18_5 <- dat[age >= 18 & !is.na(age) &
                      !is.na(ysc) & major_religion!="" & migr == 0,
                    .(ysc_18_nm = mean(ysc, na.rm = TRUE),
                      n_ysc_18_nm = .N), by = c('bch5', 'major_religion', 'province')]
    ysc_25_10 <- dat[age >= 25 & !is.na(age) &
                       !is.na(ysc) & major_religion!="" & migr == 0,
                     .(ysc_25_nm = mean(ysc, na.rm = TRUE),
                       n_ysc_25_nm = .N), by = c('bch10', 'major_religion', 'province')]
    ysc_25_5 <- dat[age >= 25 & !is.na(age) &
                      !is.na(ysc) & major_religion!="" & migr == 0,
                    .(ysc_25_nm = mean(ysc, na.rm = TRUE),
                      n_ysc_25_nm = .N), by = c('bch5', 'major_religion', 'province')]
    
    
  #   ################################################################
  #   # province-level IM, with and without MG households
  #   ################################################################
    
    print(sprintf('%s: province-level up-im, with mg hhs, excl. migrants', isoc))
    
    immg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & migr == 0,
                       .(immg_100_nm = mean(eckid >= 1, na.rm = TRUE),
                         n_immg_100_nm = .N), by = c('bch10', 'major_religion', 'province')]
    immg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & migr == 0,
                      .(immg_100_nm = mean(eckid >= 1, na.rm = TRUE),
                        n_immg_100_nm = .N), by = c('bch5', 'major_religion', 'province')]
    immg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & migr == 0,
                      .(immg_25_nm = mean(eckid >= 1, na.rm = TRUE),
                        n_immg_25_nm = .N), by = c('bch10', 'major_religion', 'province')]
    immg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="" & migr == 0,
                     .(immg_25_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_immg_25_nm = .N), by = c('bch5', 'major_religion', 'province')]
    immg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & migr == 0,
                      .(immg_18_nm = mean(eckid >= 1, na.rm = TRUE),
                        n_immg_18_nm = .N), by = c('bch10', 'major_religion', 'province')]
    immg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="" & migr == 0,
                     .(immg_18_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_immg_18_nm = .N), by = c('bch5', 'major_religion', 'province')]
    
    print(sprintf('%s: province-level up-im, no mg hhs, excl. migrants', isoc))
    
    imnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & ec0 == 0 &
                           major_religion!="" & mgec014100 == 0 & migr == 0,
                         .(imnomg_100_nm = mean(eckid >= 1, na.rm = TRUE),
                           n_imnomg_100_nm = .N), by = c('bch10', 'major_religion', 'province')]
    imnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 &
                          major_religion!="" & mgec014100 == 0 & migr == 0,
                        .(imnomg_100_nm = mean(eckid >= 1, na.rm = TRUE),
                          n_imnomg_100_nm = .N), by = c('bch5', 'major_religion', 'province')]
    imnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 &
                          major_religion!="" & mgec01425 == 0 & migr == 0,
                        .(imnomg_25_nm = mean(eckid >= 1, na.rm = TRUE),
                          n_imnomg_25_nm = .N), by = c('bch10', 'major_religion', 'province')]
    imnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & mgec01425 == 0 & migr == 0,
                       .(imnomg_25_nm = mean(eckid >= 1, na.rm = TRUE),
                         n_imnomg_25_nm = .N), by = c('bch5', 'major_religion', 'province')]
    imnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 &
                          major_religion!="" & mgec01418 == 0 & migr == 0,
                        .(imnomg_18_nm = mean(eckid >= 1, na.rm = TRUE),
                          n_imnomg_18_nm = .N), by = c('bch10', 'major_religion', 'province')]
    imnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & mgec01418 == 0 & migr == 0,
                       .(imnomg_18_nm = mean(eckid >= 1, na.rm = TRUE),
                         n_imnomg_18_nm = .N), by = c('bch5', 'major_religion', 'province')]
    
    
    print(sprintf('%s: province-level down-im, with mg hhs, excl. migrants', isoc))
    
    imdwmg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & migr == 0,
                         .(imdwmg_100_nm = mean(eckid == 0, na.rm = TRUE),
                           n_imdwmg_100_nm = .N), by = c('bch10', 'major_religion', 'province')]
    imdwmg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & migr == 0,
                        .(imdwmg_100_nm = mean(eckid == 0, na.rm = TRUE),
                          n_imdwmg_100_nm = .N), by = c('bch5', 'major_religion', 'province')]
    imdwmg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & migr == 0,
                        .(imdwmg_25_nm = mean(eckid == 0, na.rm = TRUE),
                          n_imdwmg_25_nm = .N), by = c('bch10', 'major_religion', 'province')]
    imdwmg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="" & migr == 0,
                       .(imdwmg_25_nm = mean(eckid == 0, na.rm = TRUE),
                         n_imdwmg_25_nm = .N), by = c('bch5', 'major_religion', 'province')]
    imdwmg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & migr == 0,
                        .(imdwmg_18_nm = mean(eckid == 0, na.rm = TRUE),
                          n_imdwmg_18_nm = .N), by = c('bch10', 'major_religion', 'province')]
    imdwmg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="" & migr == 0,
                       .(imdwmg_18_nm = mean(eckid == 0, na.rm = TRUE),
                         n_imdwmg_18_nm = .N), by = c('bch5', 'major_religion', 'province')]
    
    print(sprintf('%s: province-level down-im, no mg hhs, excl. migrants', isoc))
    
    imdwnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                             !is.na(eckid) & ec0 >= 1 &
                             major_religion!="" & mgec014100 == 0 & migr == 0,
                           .(imdwnomg_100_nm = mean(eckid == 0, na.rm = TRUE),
                             n_imdwnomg_100_nm = .N), by = c('bch10', 'major_religion', 'province')]
    imdwnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 &
                            major_religion!="" & mgec014100 == 0 & migr == 0,
                          .(imdwnomg_100_nm = mean(eckid == 0, na.rm = TRUE),
                            n_imdwnomg_100_nm = .N), by = c('bch5', 'major_religion', 'province')]
    imdwnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 &
                            major_religion!="" & mgec01425 == 0 & migr == 0,
                          .(imdwnomg_25_nm = mean(eckid == 0, na.rm = TRUE),
                            n_imdwnomg_25_nm = .N), by = c('bch10', 'major_religion', 'province')]
    imdwnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & mgec01425 == 0 & migr == 0,
                         .(imdwnomg_25_nm = mean(eckid == 0, na.rm = TRUE),
                           n_imdwnomg_25_nm = .N), by = c('bch5', 'major_religion', 'province')]
    imdwnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 &
                            major_religion!="" & mgec01418 == 0 & migr == 0,
                          .(imdwnomg_18_nm = mean(eckid == 0, na.rm = TRUE),
                            n_imdwnomg_18_nm = .N), by = c('bch10', 'major_religion', 'province')]
    imdwnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & mgec01418 == 0 & migr == 0,
                         .(imdwnomg_18_nm = mean(eckid == 0, na.rm = TRUE),
                           n_imdwnomg_18_nm = .N), by = c('bch5', 'major_religion', 'province')]
    
    
  #   ################################################################
  #   # province-level parental literacy, with and without MG households
  #   ################################################################
    
    print(sprintf('%s: province-level parental literacy, with mg hhs, excl. migrants', isoc))
    
    litomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & migr == 0,
                         .(litomg_100_nm = mean(ec0 >= 1, na.rm = TRUE),
                           n_litomg_100_nm = .N), by = c('bch10', 'major_religion', 'province')]
    litomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & migr == 0,
                        .(litomg_100_nm = mean(ec0 >= 1, na.rm = TRUE),
                          n_litomg_100_nm = .N), by = c('bch5', 'major_religion', 'province')]
    litomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & migr == 0,
                        .(litomg_25_nm = mean(ec0 >= 1, na.rm = TRUE),
                          n_litomg_25_nm = .N), by = c('bch10', 'major_religion', 'province')]
    litomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="" & migr == 0,
                       .(litomg_25_nm = mean(ec0 >= 1, na.rm = TRUE),
                         n_litomg_25_nm = .N), by = c('bch5', 'major_religion', 'province')]
    litomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & migr == 0,
                        .(litomg_18_nm = mean(ec0 >= 1, na.rm = TRUE),
                          n_litomg_18_nm = .N), by = c('bch10', 'major_religion', 'province')]
    litomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="" & migr == 0,
                       .(litomg_18_nm = mean(ec0 >= 1, na.rm = TRUE),
                         n_litomg_18_nm = .N), by = c('bch5', 'major_religion', 'province')]
    
    print(sprintf('%s: province-level parental literacy, no mg hhs, excl. migrants', isoc))
    
    litonomg_100_10 <- dat[age >= 14 & !is.na(age) &
                             !is.na(eckid) & !is.na(ec0) &
                             major_religion!="" & mgec014100 == 0 & migr == 0,
                           .(litonomg_100_nm = mean(ec0 >= 1, na.rm = TRUE),
                             n_litonomg_100_nm = .N), by = c('bch10', 'major_religion', 'province')]
    litonomg_100_5 <- dat[age >= 14 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) &
                            major_religion!="" & mgec014100 == 0 & migr == 0,
                          .(litonomg_100_nm = mean(ec0 >= 1, na.rm = TRUE),
                            n_litonomg_100_nm = .N), by = c('bch5', 'major_religion', 'province')]
    litonomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) &
                            major_religion!="" & mgec01425 == 0 & migr == 0,
                          .(litonomg_25_nm = mean(ec0 >= 1, na.rm = TRUE),
                            n_litonomg_25_nm = .N), by = c('bch10', 'major_religion', 'province')]
    litonomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & mgec01425 == 0 & migr == 0,
                         .(litonomg_25_nm = mean(ec0 >= 1, na.rm = TRUE),
                           n_litonomg_25_nm = .N), by = c('bch5', 'major_religion', 'province')]
    litonomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) &
                            major_religion!="" & mgec01418 == 0 & migr == 0,
                          .(litonomg_18_nm = mean(ec0 >= 1, na.rm = TRUE),
                            n_litonomg_18_nm = .N), by = c('bch10', 'major_religion', 'province')]
    litonomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & mgec01418 == 0 & migr == 0,
                         .(litonomg_18_nm = mean(ec0 >= 1, na.rm = TRUE),
                           n_litonomg_18_nm = .N), by = c('bch5', 'major_religion', 'province')]
    
    
    prov_10 <- merge(prov_10, lit_14_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, lit_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, lit_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, ysc_14_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, ysc_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, ysc_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, immg_100_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, immg_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, immg_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, imnomg_100_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, imnomg_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, imnomg_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, imdwmg_100_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, imdwmg_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, imdwmg_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, imdwnomg_100_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, imdwnomg_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, imdwnomg_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, litomg_100_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, litomg_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, litomg_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, litonomg_100_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, litonomg_25_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_10 <- merge(prov_10, litonomg_18_10, by = c('bch10', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    
    prov_5 <- merge(prov_5, lit_14_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, lit_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, lit_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, ysc_14_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, ysc_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, ysc_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, immg_100_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, immg_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, immg_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, imnomg_100_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, imnomg_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, imnomg_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, imdwmg_100_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, imdwmg_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, imdwmg_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, imdwnomg_100_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, imdwnomg_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, imdwnomg_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, litomg_100_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, litomg_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, litomg_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, litonomg_100_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, litonomg_25_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    prov_5 <- merge(prov_5, litonomg_18_5, by = c('bch5', 'major_religion', 'province'), all.x = TRUE, all.y = TRUE)
    
    rm(lit_14_10, lit_18_10, lit_25_10, ysc_14_10, ysc_18_10, ysc_25_10,
       immg_100_10, immg_25_10, immg_18_10, imnomg_100_10, imnomg_25_10, imnomg_18_10,
       imdwmg_100_10, imdwmg_25_10, imdwmg_18_10, imdwnomg_100_10, imdwnomg_25_10, imdwnomg_18_10,
       litomg_100_10, litomg_25_10, litomg_18_10, litonomg_100_10, litonomg_25_10, litonomg_18_10)
    rm(lit_14_5, lit_18_5, lit_25_5, ysc_14_5, ysc_18_5, ysc_25_5,
       immg_100_5, immg_25_5, immg_18_5, imnomg_100_5, imnomg_25_5, imnomg_18_5,
       imdwmg_100_5, imdwmg_25_5, imdwmg_18_5, imdwnomg_100_5, imdwnomg_25_5, imdwnomg_18_5,
       litomg_100_5, litomg_25_5, litomg_18_5, litonomg_100_5, litonomg_25_5, litonomg_18_5)
  
  }
  
  ################################################################
  ################################################################
  ################################################################
  # district-level, including migrants
  ################################################################
  ################################################################
  ################################################################


  ################################################################
  # district-level literacy and years of schooling
  ################################################################

  print(sprintf('%s: district-level literacy, incl. migrants', isoc))

  n_10 <- dat[major_religion!="",
              .(n = .N),
              by = c('bch10', 'major_religion', 'district')]
  n_5 <- dat[major_religion!="",
             .(n = .N),
             by = c('bch5', 'major_religion', 'district')]

  lit_14_10 <- dat[age >= 14 & !is.na(age) &
                     !is.na(eckid) & major_religion!="",
                   .(lit_14 = mean(eckid >= 1, na.rm = TRUE),
                     n_lit_14 = .N), by = c('bch10', 'major_religion', 'district')]
  lit_14_5 <- dat[age >= 14 & !is.na(age) &
                    !is.na(eckid) & major_religion!="",
                  .(lit_14 = mean(eckid >= 1, na.rm = TRUE),
                    n_lit_14 = .N), by = c('bch5', 'major_religion', 'district')]
  lit_18_10 <- dat[age >= 18 & !is.na(age) &
                     !is.na(eckid) & major_religion!="",
                   .(lit_18 = mean(eckid >= 1, na.rm = TRUE),
                     n_lit_18 = .N), by = c('bch10', 'major_religion', 'district')]
  lit_18_5 <- dat[age >= 18 & !is.na(age) &
                    !is.na(eckid) & major_religion!="",
                  .(lit_18 = mean(eckid >= 1, na.rm = TRUE),
                    n_lit_18 = .N), by = c('bch5', 'major_religion', 'district')]
  lit_25_10 <- dat[age >= 25 & !is.na(age) &
                     !is.na(eckid) & major_religion!="",
                   .(lit_25 = mean(eckid >= 1, na.rm = TRUE),
                     n_lit_25 = .N), by = c('bch10', 'major_religion', 'district')]
  lit_25_5 <- dat[age >= 25 & !is.na(age) &
                    !is.na(eckid) & major_religion!="",
                  .(lit_25 = mean(eckid >= 1, na.rm = TRUE),
                    n_lit_25 = .N), by = c('bch5', 'major_religion', 'district')]


  print(sprintf('%s: district-level years of schooling, incl. migrants', isoc))

  ysc_14_10 <- dat[age >= 14 & !is.na(age) &
                     !is.na(ysc) & major_religion!="",
                   .(ysc_14 = mean(ysc, na.rm = TRUE),
                     n_ysc_14 = .N), by = c('bch10', 'major_religion', 'district')]
  ysc_14_5 <- dat[age >= 14 & !is.na(age) &
                    !is.na(ysc) & major_religion!="",
                  .(ysc_14 = mean(ysc, na.rm = TRUE),
                    n_ysc_14 = .N), by = c('bch5', 'major_religion', 'district')]
  ysc_18_10 <- dat[age >= 18 & !is.na(age) &
                     !is.na(ysc) & major_religion!="",
                   .(ysc_18 = mean(ysc, na.rm = TRUE),
                     n_ysc_18 = .N), by = c('bch10', 'major_religion', 'district')]
  ysc_18_5 <- dat[age >= 18 & !is.na(age) &
                    !is.na(ysc) & major_religion!="",
                  .(ysc_18 = mean(ysc, na.rm = TRUE),
                    n_ysc_18 = .N), by = c('bch5', 'major_religion', 'district')]
  ysc_25_10 <- dat[age >= 25 & !is.na(age) &
                     !is.na(ysc) & major_religion!="",
                   .(ysc_25 = mean(ysc, na.rm = TRUE),
                     n_ysc_25 = .N), by = c('bch10', 'major_religion', 'district')]
  ysc_25_5 <- dat[age >= 25 & !is.na(age) &
                    !is.na(ysc) & major_religion!="",
                  .(ysc_25 = mean(ysc, na.rm = TRUE),
                    n_ysc_25 = .N), by = c('bch5', 'major_religion', 'district')]


  ################################################################
  # district-level IM, with and without MG households
  ################################################################

  print(sprintf('%s: district-level up-im, with mg hhs, incl. migrants', isoc))

  immg_100_10 <- dat[age >= 14 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="",
                     .(immg_100 = mean(eckid >= 1, na.rm = TRUE),
                       n_immg_100 = .N), by = c('bch10', 'major_religion', 'district')]
  immg_100_5 <- dat[age >= 14 & !is.na(age) &
                      !is.na(eckid) & ec0 == 0 &
                      major_religion!="",
                    .(immg_100 = mean(eckid >= 1, na.rm = TRUE),
                      n_immg_100 = .N), by = c('bch5', 'major_religion', 'district')]
  immg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                      !is.na(eckid) & ec0 == 0 &
                      major_religion!="",
                    .(immg_25 = mean(eckid >= 1, na.rm = TRUE),
                      n_immg_25 = .N), by = c('bch10', 'major_religion', 'district')]
  immg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                     !is.na(eckid) & ec0 == 0 &
                     major_religion!="",
                   .(immg_25 = mean(eckid >= 1, na.rm = TRUE),
                     n_immg_25 = .N), by = c('bch5', 'major_religion', 'district')]
  immg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                      !is.na(eckid) & ec0 == 0 &
                      major_religion!="",
                    .(immg_18 = mean(eckid >= 1, na.rm = TRUE),
                      n_immg_18 = .N), by = c('bch10', 'major_religion', 'district')]
  immg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                     !is.na(eckid) & ec0 == 0 &
                     major_religion!="",
                   .(immg_18 = mean(eckid >= 1, na.rm = TRUE),
                     n_immg_18 = .N), by = c('bch5', 'major_religion', 'district')]

  print(sprintf('%s: district-level up-im, no mg hhs, incl. migrants', isoc))

  imnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & mgec014100 == 0,
                       .(imnomg_100 = mean(eckid >= 1, na.rm = TRUE),
                         n_imnomg_100 = .N), by = c('bch10', 'major_religion', 'district')]
  imnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & mgec014100 == 0,
                      .(imnomg_100 = mean(eckid >= 1, na.rm = TRUE),
                        n_imnomg_100 = .N), by = c('bch5', 'major_religion', 'district')]
  imnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & mgec01425 == 0,
                      .(imnomg_25 = mean(eckid >= 1, na.rm = TRUE),
                        n_imnomg_25 = .N), by = c('bch10', 'major_religion', 'district')]
  imnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="" & mgec01425 == 0,
                     .(imnomg_25 = mean(eckid >= 1, na.rm = TRUE),
                       n_imnomg_25 = .N), by = c('bch5', 'major_religion', 'district')]
  imnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & mgec01418 == 0,
                      .(imnomg_18 = mean(eckid >= 1, na.rm = TRUE),
                        n_imnomg_18 = .N), by = c('bch10', 'major_religion', 'district')]
  imnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="" & mgec01418 == 0,
                     .(imnomg_18 = mean(eckid >= 1, na.rm = TRUE),
                       n_imnomg_18 = .N), by = c('bch5', 'major_religion', 'district')]


  print(sprintf('%s: district-level down-im, with mg hhs, incl. migrants', isoc))

  imdwmg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="",
                       .(imdwmg_100 = mean(eckid == 0, na.rm = TRUE),
                         n_imdwmg_100 = .N), by = c('bch10', 'major_religion', 'district')]
  imdwmg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & ec0 >= 1 &
                        major_religion!="",
                      .(imdwmg_100 = mean(eckid == 0, na.rm = TRUE),
                        n_imdwmg_100 = .N), by = c('bch5', 'major_religion', 'district')]
  imdwmg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & ec0 >= 1 &
                        major_religion!="",
                      .(imdwmg_25 = mean(eckid == 0, na.rm = TRUE),
                        n_imdwmg_25 = .N), by = c('bch10', 'major_religion', 'district')]
  imdwmg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & ec0 >= 1 &
                       major_religion!="",
                     .(imdwmg_25 = mean(eckid == 0, na.rm = TRUE),
                       n_imdwmg_25 = .N), by = c('bch5', 'major_religion', 'district')]
  imdwmg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & ec0 >= 1 &
                        major_religion!="",
                      .(imdwmg_18 = mean(eckid == 0, na.rm = TRUE),
                        n_imdwmg_18 = .N), by = c('bch10', 'major_religion', 'district')]
  imdwmg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & ec0 >= 1 &
                       major_religion!="",
                     .(imdwmg_18 = mean(eckid == 0, na.rm = TRUE),
                       n_imdwmg_18 = .N), by = c('bch5', 'major_religion', 'district')]

  print(sprintf('%s: district-level down-im, no mg hhs, incl. migrants', isoc))

  imdwnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & mgec014100 == 0,
                         .(imdwnomg_100 = mean(eckid == 0, na.rm = TRUE),
                           n_imdwnomg_100 = .N), by = c('bch10', 'major_religion', 'district')]
  imdwnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & mgec014100 == 0,
                        .(imdwnomg_100 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwnomg_100 = .N), by = c('bch5', 'major_religion', 'district')]
  imdwnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & mgec01425 == 0,
                        .(imdwnomg_25 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwnomg_25 = .N), by = c('bch10', 'major_religion', 'district')]
  imdwnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="" & mgec01425 == 0,
                       .(imdwnomg_25 = mean(eckid == 0, na.rm = TRUE),
                         n_imdwnomg_25 = .N), by = c('bch5', 'major_religion', 'district')]
  imdwnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & mgec01418 == 0,
                        .(imdwnomg_18 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwnomg_18 = .N), by = c('bch10', 'major_religion', 'district')]
  imdwnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="" & mgec01418 == 0,
                       .(imdwnomg_18 = mean(eckid == 0, na.rm = TRUE),
                         n_imdwnomg_18 = .N), by = c('bch5', 'major_religion', 'district')]


  ################################################################
  # district-level parental literacy, with and without MG households
  ################################################################

  print(sprintf('%s: district-level parental literacy, with mg hhs, incl. migrants', isoc))

  litomg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="",
                       .(litomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                         n_litomg_100 = .N), by = c('bch10', 'major_religion', 'district')]
  litomg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & !is.na(ec0) &
                        major_religion!="",
                      .(litomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                        n_litomg_100 = .N), by = c('bch5', 'major_religion', 'district')]
  litomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & !is.na(ec0) &
                        major_religion!="",
                      .(litomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                        n_litomg_25 = .N), by = c('bch10', 'major_religion', 'district')]
  litomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & !is.na(ec0) &
                       major_religion!="",
                     .(litomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                       n_litomg_25 = .N), by = c('bch5', 'major_religion', 'district')]
  litomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & !is.na(ec0) &
                        major_religion!="",
                      .(litomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                        n_litomg_18 = .N), by = c('bch10', 'major_religion', 'district')]
  litomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & !is.na(ec0) &
                       major_religion!="",
                     .(litomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                       n_litomg_18 = .N), by = c('bch5', 'major_religion', 'district')]

  print(sprintf('%s: district-level parental literacy, no mg hhs, incl. migrants', isoc))

  litonomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & mgec014100 == 0,
                         .(litonomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                           n_litonomg_100 = .N), by = c('bch10', 'major_religion', 'district')]
  litonomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & mgec014100 == 0,
                        .(litonomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litonomg_100 = .N), by = c('bch5', 'major_religion', 'district')]
  litonomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & mgec01425 == 0,
                        .(litonomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litonomg_25 = .N), by = c('bch10', 'major_religion', 'district')]
  litonomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="" & mgec01425 == 0,
                       .(litonomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                         n_litonomg_25 = .N), by = c('bch5', 'major_religion', 'district')]
  litonomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & mgec01418 == 0,
                        .(litonomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litonomg_18 = .N), by = c('bch10', 'major_religion', 'district')]
  litonomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="" & mgec01418 == 0,
                       .(litonomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                         n_litonomg_18 = .N), by = c('bch5', 'major_religion', 'district')]


  dist_10 <- merge(n_10, lit_14_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, lit_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, lit_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, ysc_14_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, ysc_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, ysc_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, immg_100_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, immg_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, immg_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, imnomg_100_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, imnomg_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, imnomg_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, imdwmg_100_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, imdwmg_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, imdwmg_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, imdwnomg_100_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, imdwnomg_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, imdwnomg_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, litomg_100_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, litomg_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, litomg_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, litonomg_100_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, litonomg_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_10 <- merge(dist_10, litonomg_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)

  dist_5 <- merge(n_5, lit_14_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, lit_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, lit_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, ysc_14_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, ysc_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, ysc_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, immg_100_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, immg_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, immg_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, imnomg_100_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, imnomg_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, imnomg_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, imdwmg_100_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, imdwmg_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, imdwmg_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, imdwnomg_100_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, imdwnomg_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, imdwnomg_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, litomg_100_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, litomg_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, litomg_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, litonomg_100_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, litonomg_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
  dist_5 <- merge(dist_5, litonomg_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)

  rm(n_10, lit_14_10, lit_18_10, lit_25_10, ysc_14_10, ysc_18_10, ysc_25_10,
     immg_100_10, immg_25_10, immg_18_10, imnomg_100_10, imnomg_25_10, imnomg_18_10,
     imdwmg_100_10, imdwmg_25_10, imdwmg_18_10, imdwnomg_100_10, imdwnomg_25_10, imdwnomg_18_10,
     litomg_100_10, litomg_25_10, litomg_18_10, litonomg_100_10, litonomg_25_10, litonomg_18_10)
  rm(n_5, lit_14_5, lit_18_5, lit_25_5, ysc_14_5, ysc_18_5, ysc_25_5,
     immg_100_5, immg_25_5, immg_18_5, imnomg_100_5, imnomg_25_5, imnomg_18_5,
     imdwmg_100_5, imdwmg_25_5, imdwmg_18_5, imdwnomg_100_5, imdwnomg_25_5, imdwnomg_18_5,
     litomg_100_5, litomg_25_5, litomg_18_5, litonomg_100_5, litonomg_25_5, litonomg_18_5)
  
  
  ################################################################
  ################################################################
  ################################################################
  # district-level, excluding migrants
  ################################################################
  ################################################################
  ################################################################
  
  ################################################################
  # district-level literacy and years of schooling
  ################################################################

  if (isoc != "NGA") {
  
    print(sprintf('%s: district-level literacy, excl. migrants', isoc))

    n_10 <- dat[major_religion!="" & migr == 0,
                .(n_nm = .N),
                by = c('bch10', 'major_religion', 'district')]
    n_5 <- dat[major_religion!="" & migr == 0,
               .(n_nm = .N),
               by = c('bch5', 'major_religion', 'district')]
    
    lit_14_10 <- dat[age >= 14 & !is.na(age) &
                       !is.na(eckid) & major_religion!="" & migr == 0,
                     .(lit_14_nm = mean(eckid >= 1, na.rm = TRUE), 
                       n_lit_14_nm = .N), by = c('bch10', 'major_religion', 'district')]
    lit_14_5 <- dat[age >= 14 & !is.na(age) &
                      !is.na(eckid) & major_religion!="" & migr == 0,
                    .(lit_14_nm = mean(eckid >= 1, na.rm = TRUE), 
                      n_lit_14_nm = .N), by = c('bch5', 'major_religion', 'district')]
    lit_18_10 <- dat[age >= 18 & !is.na(age) &
                       !is.na(eckid) & major_religion!="" & migr == 0,
                     .(lit_18_nm = mean(eckid >= 1, na.rm = TRUE), 
                       n_lit_18_nm = .N), by = c('bch10', 'major_religion', 'district')]
    lit_18_5 <- dat[age >= 18 & !is.na(age) &
                      !is.na(eckid) & major_religion!="" & migr == 0,
                    .(lit_18_nm = mean(eckid >= 1, na.rm = TRUE), 
                      n_lit_18_nm = .N), by = c('bch5', 'major_religion', 'district')]
    lit_25_10 <- dat[age >= 25 & !is.na(age) &
                       !is.na(eckid) & major_religion!="" & migr == 0,
                     .(lit_25_nm = mean(eckid >= 1, na.rm = TRUE), 
                       n_lit_25_nm = .N), by = c('bch10', 'major_religion', 'district')]
    lit_25_5 <- dat[age >= 25 & !is.na(age) &
                      !is.na(eckid) & major_religion!="" & migr == 0,
                    .(lit_25_nm = mean(eckid >= 1, na.rm = TRUE), 
                      n_lit_25_nm = .N), by = c('bch5', 'major_religion', 'district')]
    
    
    print(sprintf('%s: district-level years of schooling, excl. migrants', isoc))
    
    ysc_14_10 <- dat[age >= 14 & !is.na(age) &
                       !is.na(ysc) & major_religion!="" & migr == 0,
                     .(ysc_14_nm = mean(ysc, na.rm = TRUE), 
                       n_ysc_14_nm = .N), by = c('bch10', 'major_religion', 'district')]
    ysc_14_5 <- dat[age >= 14 & !is.na(age) &
                      !is.na(ysc) & major_religion!="" & migr == 0,
                    .(ysc_14_nm = mean(ysc, na.rm = TRUE), 
                      n_ysc_14_nm = .N), by = c('bch5', 'major_religion', 'district')]
    ysc_18_10 <- dat[age >= 18 & !is.na(age) &
                       !is.na(ysc) & major_religion!="" & migr == 0,
                     .(ysc_18_nm = mean(ysc, na.rm = TRUE), 
                       n_ysc_18_nm = .N), by = c('bch10', 'major_religion', 'district')]
    ysc_18_5 <- dat[age >= 18 & !is.na(age) &
                      !is.na(ysc) & major_religion!="" & migr == 0,
                    .(ysc_18_nm = mean(ysc, na.rm = TRUE), 
                      n_ysc_18_nm = .N), by = c('bch5', 'major_religion', 'district')]
    ysc_25_10 <- dat[age >= 25 & !is.na(age) &
                       !is.na(ysc) & major_religion!="" & migr == 0,
                     .(ysc_25_nm = mean(ysc, na.rm = TRUE), 
                       n_ysc_25_nm = .N), by = c('bch10', 'major_religion', 'district')]
    ysc_25_5 <- dat[age >= 25 & !is.na(age) &
                      !is.na(ysc) & major_religion!="" & migr == 0,
                    .(ysc_25_nm = mean(ysc, na.rm = TRUE), 
                      n_ysc_25_nm = .N), by = c('bch5', 'major_religion', 'district')]
    
    
    ################################################################
    # district-level IM, with and without MG households
    ################################################################
    
    print(sprintf('%s: district-level up-im, with mg hhs, excl. migrants', isoc))
    
    immg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 & 
                         major_religion!="" & migr == 0,
                       .(immg_100_nm = mean(eckid >= 1, na.rm = TRUE), 
                         n_immg_100_nm = .N), by = c('bch10', 'major_religion', 'district')]
    immg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 & 
                        major_religion!="" & migr == 0,
                      .(immg_100_nm = mean(eckid >= 1, na.rm = TRUE), 
                        n_immg_100_nm = .N), by = c('bch5', 'major_religion', 'district')]
    immg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 & 
                        major_religion!="" & migr == 0,
                      .(immg_25_nm = mean(eckid >= 1, na.rm = TRUE), 
                        n_immg_25_nm = .N), by = c('bch10', 'major_religion', 'district')]
    immg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 & 
                       major_religion!="" & migr == 0,
                     .(immg_25_nm = mean(eckid >= 1, na.rm = TRUE), 
                       n_immg_25_nm = .N), by = c('bch5', 'major_religion', 'district')]
    immg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 & 
                        major_religion!="" & migr == 0,
                      .(immg_18_nm = mean(eckid >= 1, na.rm = TRUE), 
                        n_immg_18_nm = .N), by = c('bch10', 'major_religion', 'district')]
    immg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 & 
                       major_religion!="" & migr == 0,
                     .(immg_18_nm = mean(eckid >= 1, na.rm = TRUE), 
                       n_immg_18_nm = .N), by = c('bch5', 'major_religion', 'district')]
    
    print(sprintf('%s: district-level up-im, no mg hhs, excl. migrants', isoc))
    
    imnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & ec0 == 0 & 
                           major_religion!="" & mgec014100 == 0 & migr == 0,
                         .(imnomg_100_nm = mean(eckid >= 1, na.rm = TRUE), 
                           n_imnomg_100_nm = .N), by = c('bch10', 'major_religion', 'district')]
    imnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 & 
                          major_religion!="" & mgec014100 == 0 & migr == 0,
                        .(imnomg_100_nm = mean(eckid >= 1, na.rm = TRUE), 
                          n_imnomg_100_nm = .N), by = c('bch5', 'major_religion', 'district')]
    imnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 & 
                          major_religion!="" & mgec01425 == 0 & migr == 0,
                        .(imnomg_25_nm = mean(eckid >= 1, na.rm = TRUE), 
                          n_imnomg_25_nm = .N), by = c('bch10', 'major_religion', 'district')]
    imnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 & 
                         major_religion!="" & mgec01425 == 0 & migr == 0,
                       .(imnomg_25_nm = mean(eckid >= 1, na.rm = TRUE), 
                         n_imnomg_25_nm = .N), by = c('bch5', 'major_religion', 'district')]
    imnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 & 
                          major_religion!="" & mgec01418 == 0 & migr == 0,
                        .(imnomg_18_nm = mean(eckid >= 1, na.rm = TRUE), 
                          n_imnomg_18_nm = .N), by = c('bch10', 'major_religion', 'district')]
    imnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 & 
                         major_religion!="" & mgec01418 == 0 & migr == 0,
                       .(imnomg_18_nm = mean(eckid >= 1, na.rm = TRUE), 
                         n_imnomg_18_nm = .N), by = c('bch5', 'major_religion', 'district')]
    
    
    print(sprintf('%s: district-level down-im, with mg hhs, excl. migrants', isoc))
    
    imdwmg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 & 
                           major_religion!="" & migr == 0,
                         .(imdwmg_100_nm = mean(eckid == 0, na.rm = TRUE), 
                           n_imdwmg_100_nm = .N), by = c('bch10', 'major_religion', 'district')]
    imdwmg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 & 
                          major_religion!="" & migr == 0,
                        .(imdwmg_100_nm = mean(eckid == 0, na.rm = TRUE), 
                          n_imdwmg_100_nm = .N), by = c('bch5', 'major_religion', 'district')]
    imdwmg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 & 
                          major_religion!="" & migr == 0,
                        .(imdwmg_25_nm = mean(eckid == 0, na.rm = TRUE), 
                          n_imdwmg_25_nm = .N), by = c('bch10', 'major_religion', 'district')]
    imdwmg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 & 
                         major_religion!="" & migr == 0,
                       .(imdwmg_25_nm = mean(eckid == 0, na.rm = TRUE), 
                         n_imdwmg_25_nm = .N), by = c('bch5', 'major_religion', 'district')]
    imdwmg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 & 
                          major_religion!="" & migr == 0,
                        .(imdwmg_18_nm = mean(eckid == 0, na.rm = TRUE), 
                          n_imdwmg_18_nm = .N), by = c('bch10', 'major_religion', 'district')]
    imdwmg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 & 
                         major_religion!="" & migr == 0,
                       .(imdwmg_18_nm = mean(eckid == 0, na.rm = TRUE), 
                         n_imdwmg_18_nm = .N), by = c('bch5', 'major_religion', 'district')]
    
    print(sprintf('%s: district-level down-im, no mg hhs, excl. migrants', isoc))
    
    imdwnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                             !is.na(eckid) & ec0 >= 1 & 
                             major_religion!="" & mgec014100 == 0 & migr == 0,
                           .(imdwnomg_100_nm = mean(eckid == 0, na.rm = TRUE), 
                             n_imdwnomg_100_nm = .N), by = c('bch10', 'major_religion', 'district')]
    imdwnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 & 
                            major_religion!="" & mgec014100 == 0 & migr == 0,
                          .(imdwnomg_100_nm = mean(eckid == 0, na.rm = TRUE), 
                            n_imdwnomg_100_nm = .N), by = c('bch5', 'major_religion', 'district')]
    imdwnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 & 
                            major_religion!="" & mgec01425 == 0 & migr == 0,
                          .(imdwnomg_25_nm = mean(eckid == 0, na.rm = TRUE), 
                            n_imdwnomg_25_nm = .N), by = c('bch10', 'major_religion', 'district')]
    imdwnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 & 
                           major_religion!="" & mgec01425 == 0 & migr == 0,
                         .(imdwnomg_25_nm = mean(eckid == 0, na.rm = TRUE), 
                           n_imdwnomg_25_nm = .N), by = c('bch5', 'major_religion', 'district')]
    imdwnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 & 
                            major_religion!="" & mgec01418 == 0 & migr == 0,
                          .(imdwnomg_18_nm = mean(eckid == 0, na.rm = TRUE), 
                            n_imdwnomg_18_nm = .N), by = c('bch10', 'major_religion', 'district')]
    imdwnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 & 
                           major_religion!="" & mgec01418 == 0 & migr == 0,
                         .(imdwnomg_18_nm = mean(eckid == 0, na.rm = TRUE), 
                           n_imdwnomg_18_nm = .N), by = c('bch5', 'major_religion', 'district')]                     
    
    ################################################################
    # district-level parental literacy, with and without MG households
    ################################################################
    
    print(sprintf('%s: district-level parental literacy, with mg hhs, excl. migrants', isoc))
    
    litomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) & 
                           major_religion!="" & migr == 0,
                         .(litomg_100_nm = mean(ec0 >= 1, na.rm = TRUE), 
                           n_litomg_100_nm = .N), by = c('bch10', 'major_religion', 'district')]
    litomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) & 
                          major_religion!="" & migr == 0,
                        .(litomg_100_nm = mean(ec0 >= 1, na.rm = TRUE), 
                          n_litomg_100_nm = .N), by = c('bch5', 'major_religion', 'district')]
    litomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) & 
                          major_religion!="" & migr == 0,
                        .(litomg_25_nm = mean(ec0 >= 1, na.rm = TRUE), 
                          n_litomg_25_nm = .N), by = c('bch10', 'major_religion', 'district')]
    litomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) & 
                         major_religion!="" & migr == 0,
                       .(litomg_25_nm = mean(ec0 >= 1, na.rm = TRUE), 
                         n_litomg_25_nm = .N), by = c('bch5', 'major_religion', 'district')]
    litomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) & 
                          major_religion!="" & migr == 0,
                        .(litomg_18_nm = mean(ec0 >= 1, na.rm = TRUE), 
                          n_litomg_18_nm = .N), by = c('bch10', 'major_religion', 'district')]
    litomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) & 
                         major_religion!="" & migr == 0,
                       .(litomg_18_nm = mean(ec0 >= 1, na.rm = TRUE), 
                         n_litomg_18_nm = .N), by = c('bch5', 'major_religion', 'district')]
    
    print(sprintf('%s: district-level parental literacy, no mg hhs, excl. migrants', isoc))
    
    litonomg_100_10 <- dat[age >= 14 & !is.na(age) &
                             !is.na(eckid) & !is.na(ec0) & 
                             major_religion!="" & mgec014100 == 0 & migr == 0,
                           .(litonomg_100_nm = mean(ec0 >= 1, na.rm = TRUE), 
                             n_litonomg_100_nm = .N), by = c('bch10', 'major_religion', 'district')]
    litonomg_100_5 <- dat[age >= 14 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) & 
                            major_religion!="" & mgec014100 == 0 & migr == 0,
                          .(litonomg_100_nm = mean(ec0 >= 1, na.rm = TRUE), 
                            n_litonomg_100_nm = .N), by = c('bch5', 'major_religion', 'district')]
    litonomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) & 
                            major_religion!="" & mgec01425 == 0 & migr == 0,
                          .(litonomg_25_nm = mean(ec0 >= 1, na.rm = TRUE), 
                            n_litonomg_25_nm = .N), by = c('bch10', 'major_religion', 'district')]
    litonomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) & 
                           major_religion!="" & mgec01425 == 0 & migr == 0,
                         .(litonomg_25_nm = mean(ec0 >= 1, na.rm = TRUE), 
                           n_litonomg_25_nm = .N), by = c('bch5', 'major_religion', 'district')]
    litonomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) & 
                            major_religion!="" & mgec01418 == 0 & migr == 0,
                          .(litonomg_18_nm = mean(ec0 >= 1, na.rm = TRUE), 
                            n_litonomg_18_nm = .N), by = c('bch10', 'major_religion', 'district')]
    litonomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) & 
                           major_religion!="" & mgec01418 == 0 & migr == 0,
                         .(litonomg_18_nm = mean(ec0 >= 1, na.rm = TRUE), 
                           n_litonomg_18_nm = .N), by = c('bch5', 'major_religion', 'district')]  
    
    
    dist_10 <- merge(dist_10, n_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, lit_14_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, lit_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, lit_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, ysc_14_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, ysc_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, ysc_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, immg_100_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, immg_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, immg_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, imnomg_100_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, imnomg_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, imnomg_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)  
    dist_10 <- merge(dist_10, imdwmg_100_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, imdwmg_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, imdwmg_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, imdwnomg_100_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, imdwnomg_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, imdwnomg_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE) 
    dist_10 <- merge(dist_10, litomg_100_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, litomg_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, litomg_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, litonomg_100_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, litonomg_25_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_10 <- merge(dist_10, litonomg_18_10, by = c('bch10', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)    
    
    dist_5 <- merge(dist_5, n_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, lit_14_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, lit_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, lit_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, ysc_14_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, ysc_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, ysc_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, immg_100_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, immg_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, immg_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, imnomg_100_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, imnomg_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, imnomg_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)  
    dist_5 <- merge(dist_5, imdwmg_100_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, imdwmg_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, imdwmg_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, imdwnomg_100_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, imdwnomg_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, imdwnomg_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)  
    dist_5 <- merge(dist_5, litomg_100_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, litomg_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, litomg_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, litonomg_100_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, litonomg_25_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)
    dist_5 <- merge(dist_5, litonomg_18_5, by = c('bch5', 'major_religion', 'district'), all.x = TRUE, all.y = TRUE)  
    
    rm(n_10, lit_14_10, lit_18_10, lit_25_10, ysc_14_10, ysc_18_10, ysc_25_10,
       immg_100_10, immg_25_10, immg_18_10, imnomg_100_10, imnomg_25_10, imnomg_18_10,
       imdwmg_100_10, imdwmg_25_10, imdwmg_18_10, imdwnomg_100_10, imdwnomg_25_10, imdwnomg_18_10,
       litomg_100_10, litomg_25_10, litomg_18_10, litonomg_100_10, litonomg_25_10, litonomg_18_10)
    rm(n_5, lit_14_5, lit_18_5, lit_25_5, ysc_14_5, ysc_18_5, ysc_25_5,
       immg_100_5, immg_25_5, immg_18_5, imnomg_100_5, imnomg_25_5, imnomg_18_5,
       imdwmg_100_5, imdwmg_25_5, imdwmg_18_5, imdwnomg_100_5, imdwnomg_25_5, imdwnomg_18_5,
       litomg_100_5, litomg_25_5, litomg_18_5, litonomg_100_5, litonomg_25_5, litonomg_18_5)
    
  }
  
  # ################################################################
  # ################################################################
  # ################################################################
  # # district_ur-level, including migrants
  # ################################################################
  # ################################################################
  # ################################################################
  
  
  # ################################################################
  # # district_ur-level literacy and years of schooling
  # ################################################################
  
  print(sprintf('%s: district_ur-level literacy, incl. migrants', isoc))
  
  lit_14_10 <- dat[age >= 14 & !is.na(age) &
                     !is.na(eckid) & major_religion!="",
                   .(lit_14 = mean(eckid >= 1, na.rm = TRUE),
                     n_lit_14 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  lit_14_5 <- dat[age >= 14 & !is.na(age) &
                    !is.na(eckid) & major_religion!="",
                  .(lit_14 = mean(eckid >= 1, na.rm = TRUE),
                    n_lit_14 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  lit_18_10 <- dat[age >= 18 & !is.na(age) &
                     !is.na(eckid) & major_religion!="",
                   .(lit_18 = mean(eckid >= 1, na.rm = TRUE),
                     n_lit_18 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  lit_18_5 <- dat[age >= 18 & !is.na(age) &
                    !is.na(eckid) & major_religion!="",
                  .(lit_18 = mean(eckid >= 1, na.rm = TRUE),
                    n_lit_18 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  lit_25_10 <- dat[age >= 25 & !is.na(age) &
                     !is.na(eckid) & major_religion!="",
                   .(lit_25 = mean(eckid >= 1, na.rm = TRUE),
                     n_lit_25 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  lit_25_5 <- dat[age >= 25 & !is.na(age) &
                    !is.na(eckid) & major_religion!="",
                  .(lit_25 = mean(eckid >= 1, na.rm = TRUE),
                    n_lit_25 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  
  
  print(sprintf('%s: district_ur-level years of schooling, incl. migrants', isoc))
  
  ysc_14_10 <- dat[age >= 14 & !is.na(age) &
                     !is.na(ysc) & major_religion!="",
                   .(ysc_14 = mean(ysc, na.rm = TRUE),
                     n_ysc_14 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  ysc_14_5 <- dat[age >= 14 & !is.na(age) &
                    !is.na(ysc) & major_religion!="",
                  .(ysc_14 = mean(ysc, na.rm = TRUE),
                    n_ysc_14 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  ysc_18_10 <- dat[age >= 18 & !is.na(age) &
                     !is.na(ysc) & major_religion!="",
                   .(ysc_18 = mean(ysc, na.rm = TRUE),
                     n_ysc_18 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  ysc_18_5 <- dat[age >= 18 & !is.na(age) &
                    !is.na(ysc) & major_religion!="",
                  .(ysc_18 = mean(ysc, na.rm = TRUE),
                    n_ysc_18 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  ysc_25_10 <- dat[age >= 25 & !is.na(age) &
                     !is.na(ysc) & major_religion!="",
                   .(ysc_25 = mean(ysc, na.rm = TRUE),
                     n_ysc_25 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  ysc_25_5 <- dat[age >= 25 & !is.na(age) &
                    !is.na(ysc) & major_religion!="",
                  .(ysc_25 = mean(ysc, na.rm = TRUE),
                    n_ysc_25 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  
  
  # ################################################################
  # # district_ur-level IM, with and without MG households
  # ################################################################
  
  print(sprintf('%s: district_ur-level up-im, with mg hhs, incl. migrants', isoc))
  
  immg_100_10 <- dat[age >= 14 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="",
                     .(immg_100 = mean(eckid >= 1, na.rm = TRUE),
                       n_immg_100 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  immg_100_5 <- dat[age >= 14 & !is.na(age) &
                      !is.na(eckid) & ec0 == 0 &
                      major_religion!="",
                    .(immg_100 = mean(eckid >= 1, na.rm = TRUE),
                      n_immg_100 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  immg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                      !is.na(eckid) & ec0 == 0 &
                      major_religion!="",
                    .(immg_25 = mean(eckid >= 1, na.rm = TRUE),
                      n_immg_25 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  immg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                     !is.na(eckid) & ec0 == 0 &
                     major_religion!="",
                   .(immg_25 = mean(eckid >= 1, na.rm = TRUE),
                     n_immg_25 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  immg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                      !is.na(eckid) & ec0 == 0 &
                      major_religion!="",
                    .(immg_18 = mean(eckid >= 1, na.rm = TRUE),
                      n_immg_18 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  immg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                     !is.na(eckid) & ec0 == 0 &
                     major_religion!="",
                   .(immg_18 = mean(eckid >= 1, na.rm = TRUE),
                     n_immg_18 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  
  print(sprintf('%s: district_ur-level up-im, no mg hhs, incl. migrants', isoc))
  
  imnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & mgec014100 == 0,
                       .(imnomg_100 = mean(eckid >= 1, na.rm = TRUE),
                         n_imnomg_100 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  imnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & mgec014100 == 0,
                      .(imnomg_100 = mean(eckid >= 1, na.rm = TRUE),
                        n_imnomg_100 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  imnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & mgec01425 == 0,
                      .(imnomg_25 = mean(eckid >= 1, na.rm = TRUE),
                        n_imnomg_25 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  imnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="" & mgec01425 == 0,
                     .(imnomg_25 = mean(eckid >= 1, na.rm = TRUE),
                       n_imnomg_25 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  imnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & mgec01418 == 0,
                      .(imnomg_18 = mean(eckid >= 1, na.rm = TRUE),
                        n_imnomg_18 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  imnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="" & mgec01418 == 0,
                     .(imnomg_18 = mean(eckid >= 1, na.rm = TRUE),
                       n_imnomg_18 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  
  
  print(sprintf('%s: district_ur-level down-im, with mg hhs, incl. migrants', isoc))
  
  imdwmg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="",
                       .(imdwmg_100 = mean(eckid == 0, na.rm = TRUE),
                         n_imdwmg_100 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  imdwmg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & ec0 >= 1 &
                        major_religion!="",
                      .(imdwmg_100 = mean(eckid == 0, na.rm = TRUE),
                        n_imdwmg_100 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  imdwmg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & ec0 >= 1 &
                        major_religion!="",
                      .(imdwmg_25 = mean(eckid == 0, na.rm = TRUE),
                        n_imdwmg_25 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  imdwmg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & ec0 >= 1 &
                       major_religion!="",
                     .(imdwmg_25 = mean(eckid == 0, na.rm = TRUE),
                       n_imdwmg_25 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  imdwmg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & ec0 >= 1 &
                        major_religion!="",
                      .(imdwmg_18 = mean(eckid == 0, na.rm = TRUE),
                        n_imdwmg_18 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  imdwmg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & ec0 >= 1 &
                       major_religion!="",
                     .(imdwmg_18 = mean(eckid == 0, na.rm = TRUE),
                       n_imdwmg_18 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  
  print(sprintf('%s: district_ur-level down-im, no mg hhs, incl. migrants', isoc))
  
  imdwnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & mgec014100 == 0,
                         .(imdwnomg_100 = mean(eckid == 0, na.rm = TRUE),
                           n_imdwnomg_100 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  imdwnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & mgec014100 == 0,
                        .(imdwnomg_100 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwnomg_100 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  imdwnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & mgec01425 == 0,
                        .(imdwnomg_25 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwnomg_25 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  imdwnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="" & mgec01425 == 0,
                       .(imdwnomg_25 = mean(eckid == 0, na.rm = TRUE),
                         n_imdwnomg_25 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  imdwnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & mgec01418 == 0,
                        .(imdwnomg_18 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwnomg_18 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  imdwnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="" & mgec01418 == 0,
                       .(imdwnomg_18 = mean(eckid == 0, na.rm = TRUE),
                         n_imdwnomg_18 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  
  
  # ################################################################
  # # district_ur-level parental literacy, with and without MG households
  # ################################################################
  
  print(sprintf('%s: district_ur-level parental literacy, with mg hhs, incl. migrants', isoc))
  
  litomg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="",
                       .(litomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                         n_litomg_100 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  litomg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & !is.na(ec0) &
                        major_religion!="",
                      .(litomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                        n_litomg_100 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  litomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & !is.na(ec0) &
                        major_religion!="",
                      .(litomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                        n_litomg_25 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  litomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & !is.na(ec0) &
                       major_religion!="",
                     .(litomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                       n_litomg_25 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  litomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & !is.na(ec0) &
                        major_religion!="",
                      .(litomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                        n_litomg_18 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  litomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & !is.na(ec0) &
                       major_religion!="",
                     .(litomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                       n_litomg_18 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  
  print(sprintf('%s: district_ur-level parental literacy, no mg hhs, incl. migrants', isoc))
  
  litonomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & mgec014100 == 0,
                         .(litonomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                           n_litonomg_100 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  litonomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & mgec014100 == 0,
                        .(litonomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litonomg_100 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  litonomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & mgec01425 == 0,
                        .(litonomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litonomg_25 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  litonomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="" & mgec01425 == 0,
                       .(litonomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                         n_litonomg_25 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  litonomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & mgec01418 == 0,
                        .(litonomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litonomg_18 = .N), by = c('bch10', 'major_religion', 'district_ur')]
  litonomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="" & mgec01418 == 0,
                       .(litonomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                         n_litonomg_18 = .N), by = c('bch5', 'major_religion', 'district_ur')]
  
  distur_10 <- merge(lit_14_10, lit_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, lit_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, ysc_14_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, ysc_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, ysc_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, immg_100_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, immg_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, immg_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, imnomg_100_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, imnomg_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, imnomg_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, imdwmg_100_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, imdwmg_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, imdwmg_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, imdwnomg_100_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, imdwnomg_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, imdwnomg_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, litomg_100_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, litomg_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, litomg_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, litonomg_100_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, litonomg_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_10 <- merge(distur_10, litonomg_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  
  distur_5 <- merge(lit_14_5, lit_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, lit_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, ysc_14_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, ysc_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, ysc_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, immg_100_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, immg_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, immg_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, imnomg_100_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, imnomg_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, imnomg_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, imdwmg_100_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, imdwmg_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, imdwmg_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, imdwnomg_100_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, imdwnomg_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, imdwnomg_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, litomg_100_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, litomg_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, litomg_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, litonomg_100_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, litonomg_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  distur_5 <- merge(distur_5, litonomg_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
  
  rm(lit_14_10, lit_18_10, lit_25_10, ysc_14_10, ysc_18_10, ysc_25_10,
     immg_100_10, immg_25_10, immg_18_10, imnomg_100_10, imnomg_25_10, imnomg_18_10,
     imdwmg_100_10, imdwmg_25_10, imdwmg_18_10, imdwnomg_100_10, imdwnomg_25_10, imdwnomg_18_10,
     litomg_100_10, litomg_25_10, litomg_18_10, litonomg_100_10, litonomg_25_10, litonomg_18_10)
  rm(lit_14_5, lit_18_5, lit_25_5, ysc_14_5, ysc_18_5, ysc_25_5,
     immg_100_5, immg_25_5, immg_18_5, imnomg_100_5, imnomg_25_5, imnomg_18_5,
     imdwmg_100_5, imdwmg_25_5, imdwmg_18_5, imdwnomg_100_5, imdwnomg_25_5, imdwnomg_18_5,
     litomg_100_5, litomg_25_5, litomg_18_5, litonomg_100_5, litonomg_25_5, litonomg_18_5)
  
  # ################################################################
  # ################################################################
  # ################################################################
  # # district_ur-level, excluding migrants
  # ################################################################
  # ################################################################
  # ################################################################
  
  
  # ################################################################
  # # district_ur-level literacy and years of schooling
  # ################################################################
  
  if (isoc != "NGA") {

    print(sprintf('%s: district_ur-level literacy, excl. migrants', isoc))
    
    lit_14_10 <- dat[age >= 14 & !is.na(age) &
                       !is.na(eckid) & major_religion!="" & migr == 0,
                     .(lit_14_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_lit_14_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    lit_14_5 <- dat[age >= 14 & !is.na(age) &
                      !is.na(eckid) & major_religion!="" & migr == 0,
                    .(lit_14_nm = mean(eckid >= 1, na.rm = TRUE),
                      n_lit_14_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    lit_18_10 <- dat[age >= 18 & !is.na(age) &
                       !is.na(eckid) & major_religion!="" & migr == 0,
                     .(lit_18_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_lit_18_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    lit_18_5 <- dat[age >= 18 & !is.na(age) &
                      !is.na(eckid) & major_religion!="" & migr == 0,
                    .(lit_18_nm = mean(eckid >= 1, na.rm = TRUE),
                      n_lit_18_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    lit_25_10 <- dat[age >= 25 & !is.na(age) &
                       !is.na(eckid) & major_religion!="" & migr == 0,
                     .(lit_25_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_lit_25_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    lit_25_5 <- dat[age >= 25 & !is.na(age) &
                      !is.na(eckid) & major_religion!="" & migr == 0,
                    .(lit_25_nm = mean(eckid >= 1, na.rm = TRUE),
                      n_lit_25_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    
    
    print(sprintf('%s: district_ur-level years of schooling, excl. migrants', isoc))
    
    ysc_14_10 <- dat[age >= 14 & !is.na(age) &
                       !is.na(ysc) & major_religion!="" & migr == 0,
                     .(ysc_14_nm = mean(ysc, na.rm = TRUE),
                       n_ysc_14_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    ysc_14_5 <- dat[age >= 14 & !is.na(age) &
                      !is.na(ysc) & major_religion!="" & migr == 0,
                    .(ysc_14_nm = mean(ysc, na.rm = TRUE),
                      n_ysc_14_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    ysc_18_10 <- dat[age >= 18 & !is.na(age) &
                       !is.na(ysc) & major_religion!="" & migr == 0,
                     .(ysc_18_nm = mean(ysc, na.rm = TRUE),
                       n_ysc_18_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    ysc_18_5 <- dat[age >= 18 & !is.na(age) &
                      !is.na(ysc) & major_religion!="" & migr == 0,
                    .(ysc_18_nm = mean(ysc, na.rm = TRUE),
                      n_ysc_18_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    ysc_25_10 <- dat[age >= 25 & !is.na(age) &
                       !is.na(ysc) & major_religion!="" & migr == 0,
                     .(ysc_25_nm = mean(ysc, na.rm = TRUE),
                       n_ysc_25_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    ysc_25_5 <- dat[age >= 25 & !is.na(age) &
                      !is.na(ysc) & major_religion!="" & migr == 0,
                    .(ysc_25_nm = mean(ysc, na.rm = TRUE),
                      n_ysc_25_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    
    
  #   ################################################################
  #   # district_ur-level IM, with and without MG households
  #   ################################################################
    
    print(sprintf('%s: district_ur-level up-im, with mg hhs, excl. migrants', isoc))
    
    immg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & migr == 0,
                       .(immg_100_nm = mean(eckid >= 1, na.rm = TRUE),
                         n_immg_100_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    immg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & migr == 0,
                      .(immg_100_nm = mean(eckid >= 1, na.rm = TRUE),
                        n_immg_100_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    immg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & migr == 0,
                      .(immg_25_nm = mean(eckid >= 1, na.rm = TRUE),
                        n_immg_25_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    immg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="" & migr == 0,
                     .(immg_25_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_immg_25_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    immg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & migr == 0,
                      .(immg_18_nm = mean(eckid >= 1, na.rm = TRUE),
                        n_immg_18_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    immg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="" & migr == 0,
                     .(immg_18_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_immg_18_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    
    print(sprintf('%s: district_ur-level up-im, no mg hhs, excl. migrants', isoc))
    
    imnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & ec0 == 0 &
                           major_religion!="" & mgec014100 == 0 & migr == 0,
                         .(imnomg_100_nm = mean(eckid >= 1, na.rm = TRUE),
                           n_imnomg_100_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    imnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 &
                          major_religion!="" & mgec014100 == 0 & migr == 0,
                        .(imnomg_100_nm = mean(eckid >= 1, na.rm = TRUE),
                          n_imnomg_100_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    imnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 &
                          major_religion!="" & mgec01425 == 0 & migr == 0,
                        .(imnomg_25_nm = mean(eckid >= 1, na.rm = TRUE),
                          n_imnomg_25_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    imnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & mgec01425 == 0 & migr == 0,
                       .(imnomg_25_nm = mean(eckid >= 1, na.rm = TRUE),
                         n_imnomg_25_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    imnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 &
                          major_religion!="" & mgec01418 == 0 & migr == 0,
                        .(imnomg_18_nm = mean(eckid >= 1, na.rm = TRUE),
                          n_imnomg_18_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    imnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & mgec01418 == 0 & migr == 0,
                       .(imnomg_18_nm = mean(eckid >= 1, na.rm = TRUE),
                         n_imnomg_18_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    
    
    print(sprintf('%s: district_ur-level down-im, with mg hhs, excl. migrants', isoc))
    
    imdwmg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & migr == 0,
                         .(imdwmg_100_nm = mean(eckid == 0, na.rm = TRUE),
                           n_imdwmg_100_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    imdwmg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & migr == 0,
                        .(imdwmg_100_nm = mean(eckid == 0, na.rm = TRUE),
                          n_imdwmg_100_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    imdwmg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & migr == 0,
                        .(imdwmg_25_nm = mean(eckid == 0, na.rm = TRUE),
                          n_imdwmg_25_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    imdwmg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="" & migr == 0,
                       .(imdwmg_25_nm = mean(eckid == 0, na.rm = TRUE),
                         n_imdwmg_25_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    imdwmg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & migr == 0,
                        .(imdwmg_18_nm = mean(eckid == 0, na.rm = TRUE),
                          n_imdwmg_18_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    imdwmg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="" & migr == 0,
                       .(imdwmg_18_nm = mean(eckid == 0, na.rm = TRUE),
                         n_imdwmg_18_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    
    print(sprintf('%s: district_ur-level down-im, no mg hhs, excl. migrants', isoc))
    
    imdwnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                             !is.na(eckid) & ec0 >= 1 &
                             major_religion!="" & mgec014100 == 0 & migr == 0,
                           .(imdwnomg_100_nm = mean(eckid == 0, na.rm = TRUE),
                             n_imdwnomg_100_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    imdwnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 &
                            major_religion!="" & mgec014100 == 0 & migr == 0,
                          .(imdwnomg_100_nm = mean(eckid == 0, na.rm = TRUE),
                            n_imdwnomg_100_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    imdwnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 &
                            major_religion!="" & mgec01425 == 0 & migr == 0,
                          .(imdwnomg_25_nm = mean(eckid == 0, na.rm = TRUE),
                            n_imdwnomg_25_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    imdwnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & mgec01425 == 0 & migr == 0,
                         .(imdwnomg_25_nm = mean(eckid == 0, na.rm = TRUE),
                           n_imdwnomg_25_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    imdwnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 &
                            major_religion!="" & mgec01418 == 0 & migr == 0,
                          .(imdwnomg_18_nm = mean(eckid == 0, na.rm = TRUE),
                            n_imdwnomg_18_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    imdwnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & mgec01418 == 0 & migr == 0,
                         .(imdwnomg_18_nm = mean(eckid == 0, na.rm = TRUE),
                           n_imdwnomg_18_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    
  #   ################################################################
  #   # district_ur-level parental literacy, with and without MG households
  #   ################################################################
    
    print(sprintf('%s: district_ur-level parental literacy, with mg hhs, excl. migrants', isoc))
    
    litomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & migr == 0,
                         .(litomg_100_nm = mean(ec0 >= 1, na.rm = TRUE),
                           n_litomg_100_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    litomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & migr == 0,
                        .(litomg_100_nm = mean(ec0 >= 1, na.rm = TRUE),
                          n_litomg_100_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    litomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & migr == 0,
                        .(litomg_25_nm = mean(ec0 >= 1, na.rm = TRUE),
                          n_litomg_25_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    litomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="" & migr == 0,
                       .(litomg_25_nm = mean(ec0 >= 1, na.rm = TRUE),
                         n_litomg_25_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    litomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & migr == 0,
                        .(litomg_18_nm = mean(ec0 >= 1, na.rm = TRUE),
                          n_litomg_18_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    litomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="" & migr == 0,
                       .(litomg_18_nm = mean(ec0 >= 1, na.rm = TRUE),
                         n_litomg_18_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    
    print(sprintf('%s: district_ur-level parental literacy, no mg hhs, excl. migrants', isoc))
    
    litonomg_100_10 <- dat[age >= 14 & !is.na(age) &
                             !is.na(eckid) & !is.na(ec0) &
                             major_religion!="" & mgec014100 == 0 & migr == 0,
                           .(litonomg_100_nm = mean(ec0 >= 1, na.rm = TRUE),
                             n_litonomg_100_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    litonomg_100_5 <- dat[age >= 14 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) &
                            major_religion!="" & mgec014100 == 0 & migr == 0,
                          .(litonomg_100_nm = mean(ec0 >= 1, na.rm = TRUE),
                            n_litonomg_100_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    litonomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) &
                            major_religion!="" & mgec01425 == 0 & migr == 0,
                          .(litonomg_25_nm = mean(ec0 >= 1, na.rm = TRUE),
                            n_litonomg_25_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    litonomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & mgec01425 == 0 & migr == 0,
                         .(litonomg_25_nm = mean(ec0 >= 1, na.rm = TRUE),
                           n_litonomg_25_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    litonomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) &
                            major_religion!="" & mgec01418 == 0 & migr == 0,
                          .(litonomg_18_nm = mean(ec0 >= 1, na.rm = TRUE),
                            n_litonomg_18_nm = .N), by = c('bch10', 'major_religion', 'district_ur')]
    litonomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & mgec01418 == 0 & migr == 0,
                         .(litonomg_18_nm = mean(ec0 >= 1, na.rm = TRUE),
                           n_litonomg_18_nm = .N), by = c('bch5', 'major_religion', 'district_ur')]
    
    
    distur_10 <- merge(distur_10, lit_14_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, lit_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, lit_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, ysc_14_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, ysc_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, ysc_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, immg_100_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, immg_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, immg_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, imnomg_100_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, imnomg_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, imnomg_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, imdwmg_100_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, imdwmg_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, imdwmg_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, imdwnomg_100_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, imdwnomg_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, imdwnomg_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, litomg_100_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, litomg_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, litomg_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, litonomg_100_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, litonomg_25_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_10 <- merge(distur_10, litonomg_18_10, by = c('bch10', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    
    distur_5 <- merge(distur_5, lit_14_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, lit_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, lit_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, ysc_14_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, ysc_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, ysc_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, immg_100_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, immg_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, immg_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, imnomg_100_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, imnomg_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, imnomg_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, imdwmg_100_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, imdwmg_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, imdwmg_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, imdwnomg_100_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, imdwnomg_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, imdwnomg_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, litomg_100_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, litomg_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, litomg_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, litonomg_100_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, litonomg_25_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    distur_5 <- merge(distur_5, litonomg_18_5, by = c('bch5', 'major_religion', 'district_ur'), all.x = TRUE, all.y = TRUE)
    
    rm(lit_14_10, lit_18_10, lit_25_10, ysc_14_10, ysc_18_10, ysc_25_10,
       immg_100_10, immg_25_10, immg_18_10, imnomg_100_10, imnomg_25_10, imnomg_18_10,
       imdwmg_100_10, imdwmg_25_10, imdwmg_18_10, imdwnomg_100_10, imdwnomg_25_10, imdwnomg_18_10,
       litomg_100_10, litomg_25_10, litomg_18_10, litonomg_100_10, litonomg_25_10, litonomg_18_10)
    rm(lit_14_5, lit_18_5, lit_25_5, ysc_14_5, ysc_18_5, ysc_25_5,
       immg_100_5, immg_25_5, immg_18_5, imnomg_100_5, imnomg_25_5, imnomg_18_5,
       imdwmg_100_5, imdwmg_25_5, imdwmg_18_5, imdwnomg_100_5, imdwnomg_25_5, imdwnomg_18_5,
       litomg_100_5, litomg_25_5, litomg_18_5, litonomg_100_5, litonomg_25_5, litonomg_18_5)

  }


  # ################################################################
  # ################################################################
  # ################################################################
  # # migidstr-level, including migrants
  # ################################################################
  # ################################################################
  # ################################################################
  
  
  # ################################################################
  # # migidstr-level literacy and years of schooling
  # ################################################################
  
  if (isoc != "NGA") {
    
    n_10 <- dat[major_religion!="",
                .(n = .N),
                by = c('bch10', 'major_religion', 'migdistr')]
    n_5 <- dat[major_religion!="",
                .(n = .N),
                by = c('bch5', 'major_religion', 'migdistr')]

    print(sprintf('%s: migdistr-level literacy, incl. migrants', isoc))
    
    lit_14_10 <- dat[age >= 14 & !is.na(age) &
                       !is.na(eckid) & major_religion!="",
                     .(lit_14 = mean(eckid >= 1, na.rm = TRUE),
                       n_lit_14 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    lit_14_5 <- dat[age >= 14 & !is.na(age) &
                      !is.na(eckid) & major_religion!="",
                    .(lit_14 = mean(eckid >= 1, na.rm = TRUE),
                      n_lit_14 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    lit_18_10 <- dat[age >= 18 & !is.na(age) &
                       !is.na(eckid) & major_religion!="",
                     .(lit_18 = mean(eckid >= 1, na.rm = TRUE),
                       n_lit_18 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    lit_18_5 <- dat[age >= 18 & !is.na(age) &
                      !is.na(eckid) & major_religion!="",
                    .(lit_18 = mean(eckid >= 1, na.rm = TRUE),
                      n_lit_18 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    lit_25_10 <- dat[age >= 25 & !is.na(age) &
                       !is.na(eckid) & major_religion!="",
                     .(lit_25 = mean(eckid >= 1, na.rm = TRUE),
                       n_lit_25 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    lit_25_5 <- dat[age >= 25 & !is.na(age) &
                      !is.na(eckid) & major_religion!="",
                    .(lit_25 = mean(eckid >= 1, na.rm = TRUE),
                      n_lit_25 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    
    print(sprintf('%s: migdistr-level years of schooling, incl. migrants', isoc))
    
    ysc_14_10 <- dat[age >= 14 & !is.na(age) &
                       !is.na(ysc) & major_religion!="",
                     .(ysc_14 = mean(ysc, na.rm = TRUE),
                       n_ysc_14 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    ysc_14_5 <- dat[age >= 14 & !is.na(age) &
                      !is.na(ysc) & major_religion!="",
                    .(ysc_14 = mean(ysc, na.rm = TRUE),
                      n_ysc_14 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    ysc_18_10 <- dat[age >= 18 & !is.na(age) &
                       !is.na(ysc) & major_religion!="",
                     .(ysc_18 = mean(ysc, na.rm = TRUE),
                       n_ysc_18 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    ysc_18_5 <- dat[age >= 18 & !is.na(age) &
                      !is.na(ysc) & major_religion!="",
                    .(ysc_18 = mean(ysc, na.rm = TRUE),
                      n_ysc_18 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    ysc_25_10 <- dat[age >= 25 & !is.na(age) &
                       !is.na(ysc) & major_religion!="",
                     .(ysc_25 = mean(ysc, na.rm = TRUE),
                       n_ysc_25 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    ysc_25_5 <- dat[age >= 25 & !is.na(age) &
                      !is.na(ysc) & major_religion!="",
                    .(ysc_25 = mean(ysc, na.rm = TRUE),
                      n_ysc_25 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    
  #   ################################################################
  #   # migdistr-level IM, with and without MG households
  #   ################################################################
    
    print(sprintf('%s: migdistr-level up-im, with mg hhs, incl. migrants', isoc))
    
    immg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="",
                       .(immg_100 = mean(eckid >= 1, na.rm = TRUE),
                         n_immg_100 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    immg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="",
                      .(immg_100 = mean(eckid >= 1, na.rm = TRUE),
                        n_immg_100 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    immg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="",
                      .(immg_25 = mean(eckid >= 1, na.rm = TRUE),
                        n_immg_25 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    immg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="",
                     .(immg_25 = mean(eckid >= 1, na.rm = TRUE),
                       n_immg_25 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    immg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="",
                      .(immg_18 = mean(eckid >= 1, na.rm = TRUE),
                        n_immg_18 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    immg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="",
                     .(immg_18 = mean(eckid >= 1, na.rm = TRUE),
                       n_immg_18 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    print(sprintf('%s: migdistr-level up-im, no mg hhs, incl. migrants', isoc))
    
    imnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & ec0 == 0 &
                           major_religion!="" & mgec014100 == 0,
                         .(imnomg_100 = mean(eckid >= 1, na.rm = TRUE),
                           n_imnomg_100 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 &
                          major_religion!="" & mgec014100 == 0,
                        .(imnomg_100 = mean(eckid >= 1, na.rm = TRUE),
                          n_imnomg_100 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    imnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 &
                          major_religion!="" & mgec01425 == 0,
                        .(imnomg_25 = mean(eckid >= 1, na.rm = TRUE),
                          n_imnomg_25 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & mgec01425 == 0,
                       .(imnomg_25 = mean(eckid >= 1, na.rm = TRUE),
                         n_imnomg_25 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    imnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 &
                          major_religion!="" & mgec01418 == 0,
                        .(imnomg_18 = mean(eckid >= 1, na.rm = TRUE),
                          n_imnomg_18 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & mgec01418 == 0,
                       .(imnomg_18 = mean(eckid >= 1, na.rm = TRUE),
                         n_imnomg_18 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    
    print(sprintf('%s: migdistr-level down-im, with mg hhs, incl. migrants', isoc))
    
    imdwmg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="",
                         .(imdwmg_100 = mean(eckid == 0, na.rm = TRUE),
                           n_imdwmg_100 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imdwmg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="",
                        .(imdwmg_100 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwmg_100 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    imdwmg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="",
                        .(imdwmg_25 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwmg_25 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imdwmg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="",
                       .(imdwmg_25 = mean(eckid == 0, na.rm = TRUE),
                         n_imdwmg_25 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    imdwmg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="",
                        .(imdwmg_18 = mean(eckid == 0, na.rm = TRUE),
                          n_imdwmg_18 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imdwmg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="",
                       .(imdwmg_18 = mean(eckid == 0, na.rm = TRUE),
                         n_imdwmg_18 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    print(sprintf('%s: migdistr-level down-im, no mg hhs, incl. migrants', isoc))
    
    imdwnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                             !is.na(eckid) & ec0 >= 1 &
                             major_religion!="" & mgec014100 == 0,
                           .(imdwnomg_100 = mean(eckid == 0, na.rm = TRUE),
                             n_imdwnomg_100 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imdwnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 &
                            major_religion!="" & mgec014100 == 0,
                          .(imdwnomg_100 = mean(eckid == 0, na.rm = TRUE),
                            n_imdwnomg_100 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    imdwnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 &
                            major_religion!="" & mgec01425 == 0,
                          .(imdwnomg_25 = mean(eckid == 0, na.rm = TRUE),
                            n_imdwnomg_25 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imdwnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & mgec01425 == 0,
                         .(imdwnomg_25 = mean(eckid == 0, na.rm = TRUE),
                           n_imdwnomg_25 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    imdwnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 &
                            major_religion!="" & mgec01418 == 0,
                          .(imdwnomg_18 = mean(eckid == 0, na.rm = TRUE),
                            n_imdwnomg_18 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imdwnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & mgec01418 == 0,
                         .(imdwnomg_18 = mean(eckid == 0, na.rm = TRUE),
                           n_imdwnomg_18 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    
  #   ################################################################
  #   # migdistr-level parental literacy, with and without MG households
  #   ################################################################
    
    print(sprintf('%s: migdistr-level parental literacy, with mg hhs, incl. migrants', isoc))
    
    litomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="",
                         .(litomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                           n_litomg_100 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    litomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="",
                        .(litomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litomg_100 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    litomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="",
                        .(litomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litomg_25 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    litomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="",
                       .(litomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                         n_litomg_25 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    litomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="",
                        .(litomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                          n_litomg_18 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    litomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="",
                       .(litomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                         n_litomg_18 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    print(sprintf('%s: migdistr-level parental literacy, no mg hhs, incl. migrants', isoc))
    
    litonomg_100_10 <- dat[age >= 14 & !is.na(age) &
                             !is.na(eckid) & !is.na(ec0) &
                             major_religion!="" & mgec014100 == 0,
                           .(litonomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                             n_litonomg_100 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    litonomg_100_5 <- dat[age >= 14 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) &
                            major_religion!="" & mgec014100 == 0,
                          .(litonomg_100 = mean(ec0 >= 1, na.rm = TRUE),
                            n_litonomg_100 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    litonomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) &
                            major_religion!="" & mgec01425 == 0,
                          .(litonomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                            n_litonomg_25 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    litonomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & mgec01425 == 0,
                         .(litonomg_25 = mean(ec0 >= 1, na.rm = TRUE),
                           n_litonomg_25 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    litonomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) &
                            major_religion!="" & mgec01418 == 0,
                          .(litonomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                            n_litonomg_18 = .N), by = c('bch10', 'major_religion', 'migdistr')]
    litonomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & mgec01418 == 0,
                         .(litonomg_18 = mean(ec0 >= 1, na.rm = TRUE),
                           n_litonomg_18 = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    migdist_10 <- merge(n_10, lit_14_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, lit_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, lit_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, ysc_14_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, ysc_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, ysc_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, immg_100_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, immg_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, immg_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imnomg_100_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imnomg_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imnomg_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imdwmg_100_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imdwmg_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imdwmg_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imdwnomg_100_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imdwnomg_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imdwnomg_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, litomg_100_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, litomg_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, litomg_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, litonomg_100_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, litonomg_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, litonomg_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    
    migdist_5 <- merge(n_5, lit_14_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, lit_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, lit_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, ysc_14_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, ysc_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, ysc_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, immg_100_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, immg_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, immg_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imnomg_100_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imnomg_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imnomg_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imdwmg_100_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imdwmg_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imdwmg_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imdwnomg_100_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imdwnomg_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imdwnomg_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, litomg_100_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, litomg_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, litomg_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, litonomg_100_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, litonomg_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, litonomg_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    
    rm(n_10, lit_14_10, lit_18_10, lit_25_10, ysc_14_10, ysc_18_10, ysc_25_10,
       immg_100_10, immg_25_10, immg_18_10, imnomg_100_10, imnomg_25_10, imnomg_18_10,
       imdwmg_100_10, imdwmg_25_10, imdwmg_18_10, imdwnomg_100_10, imdwnomg_25_10, imdwnomg_18_10,
       litomg_100_10, litomg_25_10, litomg_18_10, litonomg_100_10, litonomg_25_10, litonomg_18_10)
    rm(n_5, lit_14_5, lit_18_5, lit_25_5, ysc_14_5, ysc_18_5, ysc_25_5,
       immg_100_5, immg_25_5, immg_18_5, imnomg_100_5, imnomg_25_5, imnomg_18_5,
       imdwmg_100_5, imdwmg_25_5, imdwmg_18_5, imdwnomg_100_5, imdwnomg_25_5, imdwnomg_18_5,
       litomg_100_5, litomg_25_5, litomg_18_5, litonomg_100_5, litonomg_25_5, litonomg_18_5)

  }
  
  
  # ################################################################
  # ################################################################
  # ################################################################
  # # migdistr-level, excluding migrants
  # ################################################################
  # ################################################################
  # ################################################################
  
  # ################################################################
  # # migdistr-level literacy and years of schooling
  # ################################################################

  if (isoc != "NGA") {
  
    n_10 <- dat[major_religion!="" & migr == 0,
                .(n_nm = .N),
                by = c('bch10', 'major_religion', 'migdistr')]
    n_5 <- dat[major_religion!="" & migr == 0,
               .(n_nm = .N),
               by = c('bch5', 'major_religion', 'migdistr')]
    
    print(sprintf('%s: migdistr-level literacy, excl. migrants', isoc))
    
    lit_14_10 <- dat[age >= 14 & !is.na(age) &
                       !is.na(eckid) & major_religion!="" & migr == 0,
                     .(lit_14_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_lit_14_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    lit_14_5 <- dat[age >= 14 & !is.na(age) &
                      !is.na(eckid) & major_religion!="" & migr == 0,
                    .(lit_14_nm = mean(eckid >= 1, na.rm = TRUE),
                      n_lit_14_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    lit_18_10 <- dat[age >= 18 & !is.na(age) &
                       !is.na(eckid) & major_religion!="" & migr == 0,
                     .(lit_18_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_lit_18_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    lit_18_5 <- dat[age >= 18 & !is.na(age) &
                      !is.na(eckid) & major_religion!="" & migr == 0,
                    .(lit_18_nm = mean(eckid >= 1, na.rm = TRUE),
                      n_lit_18_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    lit_25_10 <- dat[age >= 25 & !is.na(age) &
                       !is.na(eckid) & major_religion!="" & migr == 0,
                     .(lit_25_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_lit_25_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    lit_25_5 <- dat[age >= 25 & !is.na(age) &
                      !is.na(eckid) & major_religion!="" & migr == 0,
                    .(lit_25_nm = mean(eckid >= 1, na.rm = TRUE),
                      n_lit_25_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    
    print(sprintf('%s: migdistr-level years of schooling, excl. migrants', isoc))
    
    ysc_14_10 <- dat[age >= 14 & !is.na(age) &
                       !is.na(ysc) & major_religion!="" & migr == 0,
                     .(ysc_14_nm = mean(ysc, na.rm = TRUE),
                       n_ysc_14_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    ysc_14_5 <- dat[age >= 14 & !is.na(age) &
                      !is.na(ysc) & major_religion!="" & migr == 0,
                    .(ysc_14_nm = mean(ysc, na.rm = TRUE),
                      n_ysc_14_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    ysc_18_10 <- dat[age >= 18 & !is.na(age) &
                       !is.na(ysc) & major_religion!="" & migr == 0,
                     .(ysc_18_nm = mean(ysc, na.rm = TRUE),
                       n_ysc_18_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    ysc_18_5 <- dat[age >= 18 & !is.na(age) &
                      !is.na(ysc) & major_religion!="" & migr == 0,
                    .(ysc_18_nm = mean(ysc, na.rm = TRUE),
                      n_ysc_18_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    ysc_25_10 <- dat[age >= 25 & !is.na(age) &
                       !is.na(ysc) & major_religion!="" & migr == 0,
                     .(ysc_25_nm = mean(ysc, na.rm = TRUE),
                       n_ysc_25_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    ysc_25_5 <- dat[age >= 25 & !is.na(age) &
                      !is.na(ysc) & major_religion!="" & migr == 0,
                    .(ysc_25_nm = mean(ysc, na.rm = TRUE),
                      n_ysc_25_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    
  #   ################################################################
  #   # migdistr-level IM, with and without MG households
  #   ################################################################
    
    print(sprintf('%s: migdistr-level up-im, with mg hhs, excl. migrants', isoc))
    
    immg_100_10 <- dat[age >= 14 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & migr == 0,
                       .(immg_100_nm = mean(eckid >= 1, na.rm = TRUE),
                         n_immg_100_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    immg_100_5 <- dat[age >= 14 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & migr == 0,
                      .(immg_100_nm = mean(eckid >= 1, na.rm = TRUE),
                        n_immg_100_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    immg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & migr == 0,
                      .(immg_25_nm = mean(eckid >= 1, na.rm = TRUE),
                        n_immg_25_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    immg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="" & migr == 0,
                     .(immg_25_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_immg_25_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    immg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                        !is.na(eckid) & ec0 == 0 &
                        major_religion!="" & migr == 0,
                      .(immg_18_nm = mean(eckid >= 1, na.rm = TRUE),
                        n_immg_18_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    immg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                       !is.na(eckid) & ec0 == 0 &
                       major_religion!="" & migr == 0,
                     .(immg_18_nm = mean(eckid >= 1, na.rm = TRUE),
                       n_immg_18_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    print(sprintf('%s: migdistr-level up-im, no mg hhs, excl. migrants', isoc))
    
    imnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & ec0 == 0 &
                           major_religion!="" & mgec014100 == 0 & migr == 0,
                         .(imnomg_100_nm = mean(eckid >= 1, na.rm = TRUE),
                           n_imnomg_100_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 &
                          major_religion!="" & mgec014100 == 0 & migr == 0,
                        .(imnomg_100_nm = mean(eckid >= 1, na.rm = TRUE),
                          n_imnomg_100_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    imnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 &
                          major_religion!="" & mgec01425 == 0 & migr == 0,
                        .(imnomg_25_nm = mean(eckid >= 1, na.rm = TRUE),
                          n_imnomg_25_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & mgec01425 == 0 & migr == 0,
                       .(imnomg_25_nm = mean(eckid >= 1, na.rm = TRUE),
                         n_imnomg_25_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    imnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & ec0 == 0 &
                          major_religion!="" & mgec01418 == 0 & migr == 0,
                        .(imnomg_18_nm = mean(eckid >= 1, na.rm = TRUE),
                          n_imnomg_18_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & ec0 == 0 &
                         major_religion!="" & mgec01418 == 0 & migr == 0,
                       .(imnomg_18_nm = mean(eckid >= 1, na.rm = TRUE),
                         n_imnomg_18_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    
    print(sprintf('%s: migdistr-level down-im, with mg hhs, excl. migrants', isoc))
    
    imdwmg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & migr == 0,
                         .(imdwmg_100_nm = mean(eckid == 0, na.rm = TRUE),
                           n_imdwmg_100_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imdwmg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & migr == 0,
                        .(imdwmg_100_nm = mean(eckid == 0, na.rm = TRUE),
                          n_imdwmg_100_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    imdwmg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & migr == 0,
                        .(imdwmg_25_nm = mean(eckid == 0, na.rm = TRUE),
                          n_imdwmg_25_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imdwmg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="" & migr == 0,
                       .(imdwmg_25_nm = mean(eckid == 0, na.rm = TRUE),
                         n_imdwmg_25_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    imdwmg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & ec0 >= 1 &
                          major_religion!="" & migr == 0,
                        .(imdwmg_18_nm = mean(eckid == 0, na.rm = TRUE),
                          n_imdwmg_18_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imdwmg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & ec0 >= 1 &
                         major_religion!="" & migr == 0,
                       .(imdwmg_18_nm = mean(eckid == 0, na.rm = TRUE),
                         n_imdwmg_18_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    print(sprintf('%s: migdistr-level down-im, no mg hhs, excl. migrants', isoc))
    
    imdwnomg_100_10 <- dat[age >= 14 & !is.na(age) &
                             !is.na(eckid) & ec0 >= 1 &
                             major_religion!="" & mgec014100 == 0 & migr == 0,
                           .(imdwnomg_100_nm = mean(eckid == 0, na.rm = TRUE),
                             n_imdwnomg_100_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imdwnomg_100_5 <- dat[age >= 14 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 &
                            major_religion!="" & mgec014100 == 0 & migr == 0,
                          .(imdwnomg_100_nm = mean(eckid == 0, na.rm = TRUE),
                            n_imdwnomg_100_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    imdwnomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 &
                            major_religion!="" & mgec01425 == 0 & migr == 0,
                          .(imdwnomg_25_nm = mean(eckid == 0, na.rm = TRUE),
                            n_imdwnomg_25_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imdwnomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & mgec01425 == 0 & migr == 0,
                         .(imdwnomg_25_nm = mean(eckid == 0, na.rm = TRUE),
                           n_imdwnomg_25_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    imdwnomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                            !is.na(eckid) & ec0 >= 1 &
                            major_religion!="" & mgec01418 == 0 & migr == 0,
                          .(imdwnomg_18_nm = mean(eckid == 0, na.rm = TRUE),
                            n_imdwnomg_18_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    imdwnomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                           !is.na(eckid) & ec0 >= 1 &
                           major_religion!="" & mgec01418 == 0 & migr == 0,
                         .(imdwnomg_18_nm = mean(eckid == 0, na.rm = TRUE),
                           n_imdwnomg_18_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
  #   ################################################################
  #   # migdistr-level parental literacy, with and without MG households
  #   ################################################################
    
    print(sprintf('%s: migdistr-level parental literacy, with mg hhs, excl. migrants', isoc))
    
    litomg_100_10 <- dat[age >= 14 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & migr == 0,
                         .(litomg_100_nm = mean(ec0 >= 1, na.rm = TRUE),
                           n_litomg_100_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    litomg_100_5 <- dat[age >= 14 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & migr == 0,
                        .(litomg_100_nm = mean(ec0 >= 1, na.rm = TRUE),
                          n_litomg_100_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    litomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & migr == 0,
                        .(litomg_25_nm = mean(ec0 >= 1, na.rm = TRUE),
                          n_litomg_25_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    litomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="" & migr == 0,
                       .(litomg_25_nm = mean(ec0 >= 1, na.rm = TRUE),
                         n_litomg_25_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    litomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                          !is.na(eckid) & !is.na(ec0) &
                          major_religion!="" & migr == 0,
                        .(litomg_18_nm = mean(ec0 >= 1, na.rm = TRUE),
                          n_litomg_18_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    litomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                         !is.na(eckid) & !is.na(ec0) &
                         major_religion!="" & migr == 0,
                       .(litomg_18_nm = mean(ec0 >= 1, na.rm = TRUE),
                         n_litomg_18_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    print(sprintf('%s: migdistr-level parental literacy, no mg hhs, excl. migrants', isoc))
    
    litonomg_100_10 <- dat[age >= 14 & !is.na(age) &
                             !is.na(eckid) & !is.na(ec0) &
                             major_religion!="" & mgec014100 == 0 & migr == 0,
                           .(litonomg_100_nm = mean(ec0 >= 1, na.rm = TRUE),
                             n_litonomg_100_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    litonomg_100_5 <- dat[age >= 14 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) &
                            major_religion!="" & mgec014100 == 0 & migr == 0,
                          .(litonomg_100_nm = mean(ec0 >= 1, na.rm = TRUE),
                            n_litonomg_100_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    litonomg_25_10 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) &
                            major_religion!="" & mgec01425 == 0 & migr == 0,
                          .(litonomg_25_nm = mean(ec0 >= 1, na.rm = TRUE),
                            n_litonomg_25_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    litonomg_25_5 <- dat[age >= 14 & age <= 25 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & mgec01425 == 0 & migr == 0,
                         .(litonomg_25_nm = mean(ec0 >= 1, na.rm = TRUE),
                           n_litonomg_25_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    litonomg_18_10 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                            !is.na(eckid) & !is.na(ec0) &
                            major_religion!="" & mgec01418 == 0 & migr == 0,
                          .(litonomg_18_nm = mean(ec0 >= 1, na.rm = TRUE),
                            n_litonomg_18_nm = .N), by = c('bch10', 'major_religion', 'migdistr')]
    litonomg_18_5 <- dat[age >= 14 & age <= 18 & !is.na(age) &
                           !is.na(eckid) & !is.na(ec0) &
                           major_religion!="" & mgec01418 == 0 & migr == 0,
                         .(litonomg_18_nm = mean(ec0 >= 1, na.rm = TRUE),
                           n_litonomg_18_nm = .N), by = c('bch5', 'major_religion', 'migdistr')]
    
    migdist_10 <- merge(migdist_10, n_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, lit_14_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, lit_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, lit_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, ysc_14_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, ysc_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, ysc_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, immg_100_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, immg_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, immg_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imnomg_100_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imnomg_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imnomg_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imdwmg_100_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imdwmg_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imdwmg_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imdwnomg_100_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imdwnomg_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, imdwnomg_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, litomg_100_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, litomg_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, litomg_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, litonomg_100_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, litonomg_25_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_10 <- merge(migdist_10, litonomg_18_10, by = c('bch10', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    
    migdist_5 <- merge(migdist_5, n_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, lit_14_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, lit_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, lit_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, ysc_14_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, ysc_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, ysc_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, immg_100_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, immg_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, immg_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imnomg_100_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imnomg_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imnomg_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imdwmg_100_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imdwmg_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imdwmg_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imdwnomg_100_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imdwnomg_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, imdwnomg_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, litomg_100_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, litomg_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, litomg_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, litonomg_100_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, litonomg_25_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)
    migdist_5 <- merge(migdist_5, litonomg_18_5, by = c('bch5', 'major_religion', 'migdistr'), all.x = TRUE, all.y = TRUE)

    rm(n_10, lit_14_10, lit_18_10, lit_25_10, ysc_14_10, ysc_18_10, ysc_25_10,
       immg_100_10, immg_25_10, immg_18_10, imnomg_100_10, imnomg_25_10, imnomg_18_10,
       imdwmg_100_10, imdwmg_25_10, imdwmg_18_10, imdwnomg_100_10, imdwnomg_25_10, imdwnomg_18_10,
       litomg_100_10, litomg_25_10, litomg_18_10, litonomg_100_10, litonomg_25_10, litonomg_18_10)
    rm(n_5, lit_14_5, lit_18_5, lit_25_5, ysc_14_5, ysc_18_5, ysc_25_5,
       immg_100_5, immg_25_5, immg_18_5, imnomg_100_5, imnomg_25_5, imnomg_18_5,
       imdwmg_100_5, imdwmg_25_5, imdwmg_18_5, imdwnomg_100_5, imdwnomg_25_5, imdwnomg_18_5,
       litomg_100_5, litomg_25_5, litomg_18_5, litonomg_100_5, litonomg_25_5, litonomg_18_5)

  }


  rm(dat)
  
  ctry_10[, iso := isoc]
  ctry_5[, iso := isoc]
  prov_10[, iso := isoc]
  prov_5[, iso := isoc]
  dist_10[, iso := isoc]
  dist_5[, iso := isoc]
  distur_10[, iso := isoc]
  distur_5[, iso := isoc]
  if (isoc != "NGA") {
    migdist_10[, iso := isoc]
    migdist_5[, iso := isoc]
  }

  setcolorder(ctry_10, 'iso')
  setcolorder(ctry_5, 'iso')
  setcolorder(prov_10, 'iso')
  setcolorder(prov_5, 'iso')
  setcolorder(dist_10, 'iso')
  setcolorder(dist_5, 'iso')
  setcolorder(distur_10, 'iso')
  setcolorder(distur_5, 'iso')
  if (isoc != "NGA") {
    setcolorder(migdist_10, 'iso')
    setcolorder(migdist_5, 'iso')
  }

  if (isoc == "BEN") {
    
    country_10 <- ctry_10
    country_5 <- ctry_5
    province_10 <- prov_10
    province_5 <- prov_5
    district_10 <- dist_10
    district_5 <- dist_5  
    districtur_10 <- distur_10
    districtur_5 <- distur_5
    migdistrict_10 <- migdist_10
    migdistrict_5 <- migdist_5
    
  } else {
    country_10 <- rbindlist(list(country_10, ctry_10), fill = TRUE)
    country_5 <- rbindlist(list(country_5, ctry_5), fill = TRUE)
    province_10 <- rbindlist(list(province_10, prov_10), fill = TRUE)
    province_5 <- rbindlist(list(province_5, prov_5), fill = TRUE)
    district_10 <- rbindlist(list(district_10, dist_10), fill = TRUE)
    district_5 <- rbindlist(list(district_5, dist_5), fill = TRUE)
    districtur_10 <- rbindlist(list(districtur_10, distur_10), fill = TRUE)
    districtur_5 <- rbindlist(list(districtur_5, distur_5), fill = TRUE)
    
    if (isoc != "NGA") {
      migdistrict_10 <- rbindlist(list(migdistrict_10, migdist_10), fill = TRUE)
      migdistrict_5 <- rbindlist(list(migdistrict_5, migdist_5), fill = TRUE)
    }

  }
    
  rm(ctry_10, ctry_5, prov_10, prov_5, dist_10, dist_5, distur_10, distur_5)
  rm(dist_10, dist_5)
  if (isoc != "NGA") {
    rm(migdist_10, migdist_5)
  }
}
  
setwd(outdir)
fwrite(country_10, '_E_ctry_religion_bch10.csv')
fwrite(country_5, '_E_ctry_religion_bch5.csv')
fwrite(province_10, '_F_prov_religion_bch10.csv')
fwrite(province_5, '_F_prov_religion_bch5.csv')
fwrite(district_10, '_F_dist_religion_bch10.csv')
fwrite(district_5, '_F_dist_religion_bch5.csv')
fwrite(districtur_10, '_F_distur_religion_bch10.csv')
fwrite(districtur_5, '_F_distur_religion_bch5.csv')
fwrite(migdistrict_10, '_F_migdist_religion_bch10.csv')
fwrite(migdistrict_5, '_F_migdist_religion_bch5.csv')
