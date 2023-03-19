rm(list=ls()) # this clears all variables from the work-space
gc()
cat('\014')   # this clears the console

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_pwt <- file.path(rootdir, '_1_preprocessing', 'data', 'standardized', 'perwt')
indir_dis <- file.path(rootdir, '_1_preprocessing', 'data', 'standardized', 'districts')
indir_rel <- file.path(rootdir, '_1_preprocessing', 'data', 'standardized', 'rel')
outdir <- file.path(rootdir, '_2_intermediate', 'data')

dir.create(outdir, showWarnings = FALSE, recursive = TRUE) 

ISO <- c('BEN', 'BWA', 'BFA', 'CMR', 'EGY',
         'ETH', 'GHA', 'GIN', 'LBR', 'MWI', 
         'MLI', 'MUS', 'MOZ','NGA', 'RWA', 
         'SEN', 'SLE', 'TGO', 'UGA',
         'ZAF', 'ZMB')

for (iiso in 1:length(ISO)) {
  
  isoc <- ISO[iiso]
  
  print(isoc)
  
  setwd(indir_pwt)
  perwt <- data.table(readRDS(sprintf('perwt_%s.rds', isoc)))[,c('year','serial','pernum','perwt')]
  
  setwd(indir_dis)
  dis <- data.table(readRDS(sprintf('districts_%s.rds', isoc)))[,c('year','serial','pernum','district')]
  
  setwd(indir_rel)
  rel <- data.table(readRDS(sprintf('religions_%s.rds', isoc)))[,c('year','serial','pernum','major_religion')]
  
  dt <- merge(dis, perwt, by = c('year','serial','pernum'))
  dt <- merge(dt, rel, by = c('year','serial','pernum'))
  
  dt[,iso:=isoc]
  maxyr <- max(dt[!is.na(major_religion) & major_religion != '', year])
  
  dt <- dt[year == maxyr & ! is.na(district)
    ,
    c('iso','year','perwt','district', 'major_religion')
  ]
  
  ndis <- uniqueN(dt[!is.na(major_religion) & major_religion != '', district])
  
  pop <- dt[, .(pop = sum(perwt)), by=.(iso, year, district)]
  out <- pop[
      ,
      .(
        sum_pop = sum(pop),
        mean_pop = mean(pop),
        med_pop = median(pop),
        std_pop = sd(pop)
      ),
      by=.(iso, year)
    ][
      , ndist := ndis
    ]
  setcolorder(out, c('iso', 'year', 'ndist'))

  if (iiso == 1) {
    OUT <- out
  } else {
    OUT <- rbind(OUT, out)   
  }
}  

OUT <- rbind(OUT, data.frame(iso = 'Total', t(colSums(OUT[,c('ndist')]))), fill=TRUE)
names(OUT) <- c(names(OUT)[1], 'census_year', names(OUT)[3:7])

setwd(outdir)
fwrite(OUT, 'pop_n_land_size_of_districts.csv')
