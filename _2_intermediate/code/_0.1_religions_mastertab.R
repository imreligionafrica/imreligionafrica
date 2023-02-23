rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)
library(tools)
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
  
  isoc <- ISO[iiso]
  print(isoc)
  
  setwd(indir_rel)
  rel <- readRDS(sprintf('religions_%s.rds', isoc))
  setwd(indir_edu)
  edu <- readRDS(sprintf('aey_old_mompop_unc_%s.rds', isoc))
  rel <- rel[, c('year', 'serial', 'pernum', 'religiond_str', 'major_religion')]
  edu <- edu[, c('year', 'serial', 'pernum', 'ec0')]
  dt <- merge(rel, edu, by=c('year', 'serial', 'pernum'))
  rm(edu, rel)
  
  dt <- dt[religiond_str != '' &
           religiond_str != 'niu (not in universe)' & 
           religiond_str != 'unknown']
  
  org <- dt[, .(nrorg = .N), by=c('religiond_str', 'major_religion')]
  org[, tot := sum(nrorg)][
    , shrorg := nrorg/tot][
      , nr:=sum(nrorg), by=c('major_religion')][
        ,shr:=nr/tot][
          , tot:=NULL]
  orge <- dt[!is.na(ec0), .(nrorge = .N), by=c('religiond_str', 'major_religion')]
  orge[, tot := sum(nrorge)][
    , shrorge := nrorge/tot][
      , nre:=sum(nrorge), by=c('major_religion')][
        ,shre:=nre/tot][
          , tot:=NULL]
  
  conciso <- merge(org, orge, by=c('religiond_str', 'major_religion'), all=TRUE)
  conciso[, religiond_str := toTitleCase(religiond_str)]
  
  conciso[, iso := isoc]
  setcolorder(conciso, 'iso')

  if (iiso == 1) {
    conc <- conciso
  } else {
    conc <- rbind(conc, conciso)
  }
  
}

conc <- conc[order(iso, -shr)]

setwd(outpath)
fwrite(conc, 'religions__mastertab.csv')