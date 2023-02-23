rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
outpath <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outpath, showWarnings = FALSE, recursive = TRUE)

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA",
         "SEN", "SLE", "TGO", "UGA", "ZAF",
         "ZMB")

for (iiso in 1:21) {
  
  iso <- ISO[iiso]
  
  print(sprintf('###################### %s ######################', iso))
  
  setwd(indir_edu)
  dt <- as.data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', iso)))
  dt <- dt[, c('year', 'serial', 'pernum', 'age', 'eckid', 'ec0')]
  setwd(indir_rel)
  rel <- as.data.table(readRDS(sprintf('religions_%s.rds', iso)))
  rel <- rel[, c('year', 'serial', 'pernum', 'major_religion')]
  dt <- merge(dt, rel, by=c('year', 'serial', 'pernum'), all=TRUE)
  rm(rel)
  
  dt <- dt[age>= 14 & !is.na(major_religion) & major_religion != "" & !is.na(eckid)]
  dt[, byr := year-age]
  dt[, bd := 10 * (byr %/% 10)]
  

  all <- dt[, .(n = .N), by=.(year)]
  all50 <- dt[bd==1950, .(n50 = .N), by=.(year)]
  all80 <- dt[bd==1980, .(n80 = .N), by=.(year)]
  im18 <- dt[!is.na(ec0) & age <= 18, .(nim18 = .N), by=.(year)]
  im25 <- dt[!is.na(ec0) & age <= 25, .(nim25 = .N), by=.(year)]
  
  out <- merge(all, all50, by='year', all=TRUE)
  out <- merge(out, all80, by='year', all=TRUE)
  out <- merge(out, im18, by='year', all=TRUE)
  out <- merge(out, im25, by='year', all=TRUE)
  
  out[, iso:= ISO[iiso]]
  setcolorder(out, 'iso')

  if (iiso == 1) {
    OUT <- out
  } else {
    OUT <- rbind(OUT, out)
  }
  
}

setwd(outpath)

df <- data.table(readRDS('_mig_for_exposure_newecold.rds'))

alo <- 14
ahi <- 25

cols <- c('iso3', 'year', 'major_religion', 'male', 'migdistr', 'migdistr_bplmatch',
          'mobo', 'age', 'ageatmig', 'bdy', 'serial_new',
          sprintf('yo_%s%s_bp', alo, ahi), sprintf('d_yo_%s%s', alo, ahi),
          sprintf('yor_%s%s_bp', alo, ahi), sprintf('d_yor_%s%s', alo, ahi))
df <- df[,cols, with=FALSE]
df <- df[major_religion!='Other' & major_religion!='No Religion']

df[, `:=`(Dod_mob = get(sprintf('d_yo_%s%s', alo, ahi)),
          Dod_mobr = get(sprintf('d_yor_%s%s', alo, ahi)),
          mob = mobo)]

df <- df[ageatmig <= 18 & ageatmig >= 1 & age >= alo & age <= ahi & !is.na(mobo) & !is.na(d_yo_1425)]
df[, iso := iso3]
df <- df[, .(nexposure = .N), by = .(iso, year)]


OUT <- merge(OUT, df, by = c('iso', 'year'), all=TRUE)


fwrite(OUT, '_nobs_cens_condensed.csv')

