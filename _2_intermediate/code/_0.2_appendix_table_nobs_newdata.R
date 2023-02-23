rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_dis <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts")
indir_pro <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "provinces")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
indir_bas <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "basedat")
outpath <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outpath, showWarnings = FALSE, recursive = TRUE)

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA",
         "SEN", "SLE", "TGO", "UGA", "ZAF",
         "ZMB")

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

for (iiso in 1:21) {
  
  iso <- ISO[iiso]
  
  print(sprintf('################ %s ################', iso))
  
  setwd(indir_edu)
  dt <- as.data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', iso)))
  dt <- dt[, c('year', 'serial', 'pernum', 'age', 'eckid', 'ec0')]
  setwd(indir_bas)
  base <- as.data.table(readRDS(sprintf('basedat_%s.rds', iso)))
  base <- base[, c('year', 'serial', 'pernum', 'urban')]
  setwd(indir_dis)
  districts <- as.data.table(readRDS(sprintf('districts_%s.rds', iso)))
  setwd(indir_pro)
  provinces <- as.data.table(readRDS(sprintf('provinces_%s.rds', iso)))
  setwd(indir_rel)
  rel <- as.data.table(readRDS(sprintf('religions_%s.rds', iso)))
  rel[, c('harmonized_religion', 'religiond', 'religiond_str'):= NULL]
  
  dt <- merge(dt, base, by = c('year', 'serial', 'pernum'))
  dt <- merge(dt, districts, by = c('year', 'serial', 'pernum'))
  dt <- merge(dt, provinces, by = c('year', 'serial', 'pernum'))
  dt <- merge(dt, rel, by = c('year', 'serial', 'pernum'))
  rm(base, districts, provinces, rel)
  
  dt[, haverel:= sum(   !is.na(major_religion) & major_religion != ""    ), by=year]
  dt <- dt[haverel>0]
  dt[,haverel := NULL]
  
  # dt <- assign_mg(dt, '0', 14, 18)
  # dt <- assign_mg(dt, '0', 14, 25)
  # dt <- assign_mg(dt, '0', 14, 100)
  # dt[, gencode:=NULL]
  
  ni <- dt[, .(ni = .N), by=.(year)]
  ni_r <- dt[!is.na(major_religion) & major_religion != "" , .(ni_r = .N), by=.(year)]
  ni_a <- dt[!is.na(major_religion) & major_religion != ""  & !is.na(age), .(ni_a = .N), by=.(year)]
  ni_e <- dt[!is.na(major_religion) & major_religion != ""  & !is.na(age) & !is.na(eckid), .(ni_e = .N), by=.(year)]

  dt <- dt[age>=14]
  hh <- unique(dt, by=c('year', 'serial'))
  nhh <- hh[, .(nhh = .N), by=.(year)]
  
  ni100 <- dt[, .(ni100 = .N), by=.(year)]
  # ni100_nomg <- dt[mgec014100==0, .(ni100_nomg = .N), by=.(year)]
  ni100_r <- dt[!is.na(major_religion) & major_religion != "" , .(ni100_r = .N), by=.(year)]
  ni100_a <- dt[!is.na(major_religion) & major_religion != ""  & !is.na(age), .(ni100_a = .N), by=.(year)]
  ni100_e <- dt[!is.na(major_religion) & major_religion != ""  & !is.na(age) & !is.na(eckid), .(ni100_e = .N), by=.(year)]
  ni100_ec0 <- dt[!is.na(major_religion) & major_religion != ""  & !is.na(age) & !is.na(eckid) & !is.na(ec0), .(ni100_ec0 = .N), by=.(year)]
  # ni100_ec0_nomg <- dt[!is.na(age) & !is.na(eckid) & !is.na(ec0) & mgec014100==0, .(ni100_ec0_nomg = .N), by=.(year)]
  
  ni25 <- dt[age <= 25, .(ni25 = .N), by=.(year)]
  # ni25_nomg <- dt[age <= 25 & mgec01425==0, .(ni25_nomg = .N), by=.(year)]
  ni25_r <- dt[age <= 25 & !is.na(major_religion) & major_religion != "" , .(ni25_r = .N), by=.(year)]
  ni25_a <- dt[age <= 25 & !is.na(major_religion) & major_religion != ""  & !is.na(age), .(ni25_a = .N), by=.(year)]
  ni25_e <- dt[age <= 25 & !is.na(major_religion) & major_religion != ""  &  !is.na(age) & !is.na(eckid), .(ni25_e = .N), by=.(year)]
  ni25_ec0 <- dt[age <= 25 & !is.na(major_religion) & major_religion != ""  & !is.na(age) & !is.na(eckid) & !is.na(ec0), .(ni25_ec0 = .N), by=.(year)]
  # ni25_ec0_nomg <- dt[age <= 25 & !is.na(age) & !is.na(eckid) & !is.na(ec0) & mgec01425==0, .(ni25_ec0_nomg = .N), by=.(year)]
  
  ni18 <- dt[age <= 18, .(ni18 = .N), by=.(year)]
  # ni18_nomg <- dt[age <= 18 & mgec01418==0, .(ni18_nomg = .N), by=.(year)]
  ni18_r <- dt[age <= 18 & !is.na(major_religion) & major_religion != "" , .(ni18_r = .N), by=.(year)]
  ni18_a <- dt[age <= 18 & !is.na(major_religion) & major_religion != ""  & !is.na(age), .(ni18_a = .N), by=.(year)]
  ni18_e <- dt[age <= 18 & !is.na(major_religion) & major_religion != ""  &  !is.na(age) & !is.na(eckid), .(ni18_e = .N), by=.(year)]
  ni18_ec0 <- dt[age <= 18 & !is.na(major_religion) & major_religion != ""  & !is.na(age) & !is.na(eckid) & !is.na(ec0), .(ni18_ec0 = .N), by=.(year)]
  # ni18_ec0_nomg <- dt[age <= 18 & !is.na(age) & !is.na(eckid) & !is.na(ec0) & mgec01418==0, .(ni18_ec0_nomg = .N), by=.(year)]
  
  ur <- dt[, .(urban = max(!is.na(urban))), by = .(year)]
  
  hh <- unique(dt, by=c('year', 'serial'))
  nhh100 <- hh[, .(nhh100 = .N), by=.(year)]
  # nhh100_nomg <- hh[mgec014100==0, .(nhh100_nomg = .N), by=.(year)]
  hh <- unique(dt[!is.na(major_religion) & major_religion != ""  & !is.na(eckid) & !is.na(ec0)], by=c('year', 'serial'))
  nhh100_ec0 <- hh[!is.na(major_religion) & major_religion != ""  & !is.na(age) & !is.na(eckid) & !is.na(ec0), .(nhh100_ec0 = .N), by=.(year)]
  # nhh100_ec0_nomg <- hh[!is.na(eckid) & !is.na(ec0) & mgec014100==0, .(nhh100_ec0_nomg = .N), by=.(year)]
  
  hh <- unique(dt[age<=25], by=c('year', 'serial'))
  nhh25 <- hh[age<=25, .(nhh25 = .N), by=.(year)]
  # nhh25_nomg <- hh[age <= 25 & mgec01425==0, .(nhh25_nomg = .N), by=.(year)]
  hh <- unique(dt[!is.na(major_religion) & major_religion != ""  & !is.na(age) & !is.na(eckid) & !is.na(ec0) & age<=25], by=c('year', 'serial'))
  nhh25_ec0 <- hh[age <= 25 & !is.na(major_religion) & major_religion != ""  & !is.na(eckid) & !is.na(ec0), .(nhh25_ec0 = .N), by=.(year)]
  # nhh25_ec0_nomg <- hh[age <= 25 & !is.na(eckid) & !is.na(ec0) & mgec01425==0, .(nhh25_ec0_nomg = .N), by=.(year)]  
  
  hh <- unique(dt[age<=18], by=c('year', 'serial'))
  nhh18 <- hh[age <= 18, .(nhh18 = .N), by=.(year)]
  # nhh18_nomg <- hh[age <= 18 & mgec01418==0, .(nhh18_nomg = .N), by=.(year)]
  hh <- unique(dt[!is.na(major_religion) & major_religion != ""  & !is.na(age) & !is.na(eckid) & !is.na(ec0) & age<=18], by=c('year', 'serial'))
  nhh18_ec0 <- hh[age <= 18 & !is.na(major_religion) & major_religion != ""  & !is.na(eckid) & !is.na(ec0), .(nhh18_ec0 = .N), by=.(year)]
  # nhh18_ec0_nomg <- hh[age <= 18 & !is.na(eckid) & !is.na(ec0) & mgec01418==0, .(nhh18_ec0_nomg = .N), by=.(year)]  
  
  prov <- unique(dt, by = c('year', 'province'))
  dist <- unique(dt, by = c('year', 'district'))
  
  nprov <- prov[, .(nprov = .N), by = .(year)]
  ndist <- dist[, .(ndist = .N), by = .(year)]
  
  out <- merge(ni, ni_r, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, ni_a, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, ni_e, by = c('year'), all.x=TRUE, all.y=TRUE)
  
  out <- merge(out, ni100, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, ni25, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, ni18, by = c('year'), all.x=TRUE, all.y=TRUE)
  
  # out <- merge(out, ni100_nomg, by = c('year'), all.x=TRUE, all.y=TRUE)
  # out <- merge(out, ni25_nomg, by = c('year'), all.x=TRUE, all.y=TRUE)
  # out <- merge(out, ni18_nomg, by = c('year'), all.x=TRUE, all.y=TRUE)
  
  out <- merge(out, ni100_a, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, ni25_a, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, ni18_a, by = c('year'), all.x=TRUE, all.y=TRUE)
  
  out <- merge(out, ni100_r, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, ni25_r, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, ni18_r, by = c('year'), all.x=TRUE, all.y=TRUE)
  
  out <- merge(out, ni100_e, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, ni25_e, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, ni18_e, by = c('year'), all.x=TRUE, all.y=TRUE)
  
  out <- merge(out, ni100_ec0, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, ni25_ec0, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, ni18_ec0, by = c('year'), all.x=TRUE, all.y=TRUE)
  
  # out <- merge(out, ni100_ec0_nomg, by = c('year'), all.x=TRUE, all.y=TRUE)
  # out <- merge(out, ni25_ec0_nomg, by = c('year'), all.x=TRUE, all.y=TRUE)
  # out <- merge(out, ni18_ec0_nomg, by = c('year'), all.x=TRUE, all.y=TRUE)
  
  
  rm(ni, ni_r, ni_a, ni_e)
  rm(ni100, ni100_ec0, ni100_a, ni100_e)
  rm(ni25, ni25_ec0, ni25_a, ni25_e)
  rm(ni18, ni18_ec0, ni18_a, ni18_e)
  #rm(ni100_nomg, ni100_ec0_nomg, ni25_nomg, ni25_ec0_nomg, ni18_nomg, ni18_ec0_nomg)
  
  out <- merge(out, nhh, by = c('year'), all.x=TRUE, all.y=TRUE)
  
  out <- merge(out, nhh100, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, nhh25, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, nhh18, by = c('year'), all.x=TRUE, all.y=TRUE)
  
  # out <- merge(out, nhh100_nomg, by = c('year'), all.x=TRUE, all.y=TRUE)
  # out <- merge(out, nhh25_nomg, by = c('year'), all.x=TRUE, all.y=TRUE)
  # out <- merge(out, nhh18_nomg, by = c('year'), all.x=TRUE, all.y=TRUE)
  
  out <- merge(out, nhh100_ec0, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, nhh25_ec0, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, nhh18_ec0, by = c('year'), all.x=TRUE, all.y=TRUE)
  
  # out <- merge(out, nhh100_ec0_nomg, by = c('year'), all.x=TRUE, all.y=TRUE)
  # out <- merge(out, nhh25_ec0_nomg, by = c('year'), all.x=TRUE, all.y=TRUE)
  # out <- merge(out, nhh18_ec0_nomg, by = c('year'), all.x=TRUE, all.y=TRUE)
  
  rm(nhh)
  rm(nhh100, nhh100_ec0)
  rm(nhh25, nhh25_ec0)
  rm(nhh18, nhh18_ec0)
  #rm(nhh100_nomg, nhh100_ec0_nomg, nhh25_nomg, nhh25_ec0_nomg, nhh18_nomg, nhh18_ec0_nomg)
  
  out <- merge(out, nprov, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, ndist, by = c('year'), all.x=TRUE, all.y=TRUE)
  out <- merge(out, ur, by = c('year'), all.x=TRUE, all.y=TRUE)
  
  rm(nprov, ndist, ur, prov, dist, hh, dt)
  
  out[, iso := ISO[iiso]]
  setcolorder(out, 'iso')
  
  if (iiso == 1) {
    OUT <- out
  } else {
    OUT <- rbind(OUT, out)
  }
  
}

rm(out)

setwd(outpath)
OUT <- OUT[order(iso, year)]
fwrite(OUT, '_nobstab_app_newdata.csv')
