rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

# this needs more than 16GB of ram to run.
# it works on a 32GB machine. the expensive part 
# is in the assign_mg function when the data are 
# reshaped (dcast) from long to wide
# to make this run on a machine with less memory, 
# one would have to split the task into pieces, e.g.
# divide up the observations into 10 roughly equal
# sized chunks by HOUSEHOLD (important to keep 
# households together) and process the data chunk by
# chunk

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
indir_rl0 <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "old_rel")
indir_dis <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts")
indir_pro <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "provinces")
indir_pwt <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "perwt")
indir_bas <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "basedat")
indir_coh <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "cohabtypes")
indir_ioc <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "ind_occ")
indir_mig <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "migdists")
indir_eth <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "eth")
outdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE) 

assign_mg <- function(dt) {
  
  # Function to assign whether a household is multigen or not
  
  varvec <- c('year', 'serial', 'pernum', 'age', 'famcode', 'gencode', 'momloc', 'poploc')
  x <- dt[, ..varvec]

  # 1) number of generations (we can assign a gencode)
  x1 <- unique(x[!is.na(gencode)], by=c('year', 'serial', 'gencode'))
  x1[, ngen:= .N, by=c('year', 'serial')]
  x1 <- x1[, c('year', 'serial', 'ngen')]
  x1 <- unique(x1, by=c('year', 'serial'))
  x <- merge.data.table(x, x1, by=c('year', 'serial'), all.x=TRUE)
  
  # 2) number of generations (we can assign a gencode), momloc and poploc available
  x1 <- unique(x[!is.na(gencode) & ((!is.na(momloc) & momloc!=0) |(!is.na(poploc) & poploc!=0))], by=c('year', 'serial', 'gencode'))
  x1[, ngenmompop:= .N, by=c('year', 'serial')]
  x1 <- x1[, c('year', 'serial', 'ngenmompop')]
  x1 <- unique(x1, by=c('year', 'serial'))
  x <- merge.data.table(x, x1, by=c('year', 'serial'), all.x=TRUE)
  
  # 3) number of individuals who are family members (by relhead), and who live with 
  # another family member 15-40 years older
  x1 <- x[, c('year', 'serial', 'age', 'pernum', 'famcode')]
  x1[, minag := age+15]
  x1[, maxag := age+40]
  x1[, fam_1540 := 0]
  x2 <- x[, c('year', 'serial', 'age', 'pernum', 'famcode')]
  x2 <- x2[order(year, serial, pernum)]
  x2 <- dcast(x2, year + serial ~ pernum, value.var = c('famcode', 'age'))
  x1 <- merge.data.table(x1, x2, by=c('year', 'serial'), all=TRUE)
  maxsz <- (NCOL(x2)-2)/2
  print('finding other family member age')
  x1[, n_fam_1540 := 0]
  for (j in 1:maxsz) {
    x1[, aux := as.integer(famcode == 1 &
                           (!is.na(get(sprintf('famcode_%s', j))) & get(sprintf('famcode_%s', j)) == 1) &
                           (!is.na(get(sprintf('age_%s', j))) & get(sprintf('age_%s', j)) >= minag & get(sprintf('age_%s', j)) <= maxag)
                          )
       ]
    print(j)
    x1[aux == 1, n_fam_1540 := n_fam_1540+1]
    x1[fam_1540 < aux, fam_1540:= aux]
  }
  x1 = x1[, c('year', 'serial', 'pernum', 'age', 'fam_1540', 'n_fam_1540')]
  # 3.1) since this is a "pure age" definition of mg, we need to come up with
  # gen-coding based on age. Here we set g0 to start at the minage in the household
  # and then make buckets of 20 years starting from there
  # then we assign all individuals in the household to one of these age buckets
  x1[, minag:=min(age, na.rm = TRUE), by=c('year', 'serial')]
  x1[, gencodenum := NA]
  x1[, gencodenum := as.integer(gencodenum)]
  for (g in 0:10) {
    x1[age >= minag + g*20 & age <= minag + (g+1)*20, gencodenum := g]
  }
  # 3.2 finally count the unique number of age buckets in the household
  # for individuals that live with at least one other family member 15-40 years older
  x2 <- unique(x1[!is.na(gencodenum) & fam_1540==1], by=c('year', 'serial', 'gencodenum'))
  x2[, ngen1540:= .N, by=c('year', 'serial')]
  x2 <- x2[, c('year', 'serial', 'ngen1540')]
  x2 <- unique(x2, by=c('year', 'serial'))
  x <- merge.data.table(x, x2, by=c('year', 'serial'), all.x=TRUE)
  
  # AS EXAMPLE FOR HOW THIS BEHAVES, CONSIDER BWA, year==2011, serial==47436000

  # we are now ready to assign the multigen variable
  x[, mg:= NA]
  x[, mg:= as.integer(mg)]
  # >= 3 generations
  x[!is.na(ngen) & ngen >= 3, mg:= 1]
  x[!is.na(ngen) & ngen < 3, mg:= 0]
  # >= 2 generations with momloc or poploc available
  x[!is.na(ngenmompop) & ngenmompop >= 2, mg:= 1]
  x[!is.na(ngenmompop) & ngenmompop < 2, mg:= 0]
  # >= 2 generations that belong to the family that live 
  # with at least 1 member also of the family who is between 15 and 40 years older
  x[!is.na(ngen1540) & ngen1540 >= 2, mg:= 1]
  x[!is.na(ngen1540) & ngen1540 < 2, mg:= 0]
  
  x <- x[, c('year', 'serial', 'pernum', 'mg')]
  dt <- merge.data.table(dt, x, by=c('year', 'serial', 'pernum'), all.x = TRUE)
  
  
  ##################################################################################
  # creating more variables
  # 1) number of members in same generation as the child
  # 2) number of members in old generation
  # 3) number of members in old generation other than parents
  
  x <- dt[, c('year', 'serial', 'pernum', 'famcode', 'gencode', 'momloc', 'poploc', 'age')]
  x1 <- x1[, c('year', 'serial', 'pernum', 'n_fam_1540')]
  x <- merge.data.table(x, x1, by=c('year', 'serial','pernum'))
  
  # age based generation code
  x[, minag:=min(age, na.rm = TRUE), by=c('year', 'serial')]
  x[, gencodenum := NA]
  x[, gencodenum := as.integer(gencodenum)]
  for (g in 0:10) {
    x[age >= minag + g*20 & age <= minag + (g+1)*20, gencodenum := g]
  }
  gcr <- x[!is.na(gencode) & famcode==1, .(n_sameg_r = .N), by = c('year', 'serial', 'gencode')]
  gcn <- x[!is.na(gencodenum) & famcode==1, .(n_sameg_n = .N), by = c('year', 'serial', 'gencodenum')]
  x <- merge.data.table(x, gcr, by=c('year', 'serial', 'gencode'), all.x = TRUE)
  x <- merge.data.table(x, gcn, by=c('year', 'serial', 'gencodenum'), all.x = TRUE)
  gcr[, gencode := gencode+1]

  names(gcr) <- c('year', 'serial', 'gencode', 'n_prevg_r')
  x <- merge.data.table(x, gcr, by=c('year', 'serial', 'gencode'), all.x = TRUE)

  x[is.na(n_sameg_r), n_sameg_r := 1]
  x[is.na(n_sameg_n), n_sameg_n := 1]
  x[, n_prevg_1540 := n_fam_1540]
  x[, n_fam_1540 :=NULL]
  
  x[famcode==0&momloc!=0&!is.na(momloc), n_prevg_r := 1]
  x[famcode==0&momloc!=0&!is.na(momloc), n_prevg_1540 := 1]
  x[famcode==0&poploc!=0&!is.na(poploc)&(momloc==0|is.na(momloc)), n_prevg_r := 1]
  x[famcode==0&poploc!=0&!is.na(poploc)&(momloc==0|is.na(momloc)), n_prevg_1540 := 1]
  x[famcode==0&poploc!=0&!is.na(poploc)&momloc!=0&is.na(momloc), n_prevg_r := 2]
  x[famcode==0&poploc!=0&!is.na(poploc)&momloc!=0&is.na(momloc), n_prevg_1540 := 2]
  
  x[, `:=`(n_prevg_r_nomompop = n_prevg_r, n_prevg_1540_nomompop = n_prevg_1540)]
  x[!is.na(momloc)&momloc!=0, n_prevg_r_nomompop := n_prevg_r_nomompop-1]
  x[!is.na(poploc)&poploc!=0, n_prevg_r_nomompop := n_prevg_r_nomompop-1]
  x[!is.na(momloc)&momloc!=0, n_prevg_1540_nomompop := n_prevg_1540_nomompop-1]
  x[!is.na(poploc)&poploc!=0, n_prevg_1540_nomompop := n_prevg_1540_nomompop-1]
  x[n_prevg_r_nomompop<0, n_prevg_r_nomompop := 0]
  x[n_prevg_1540_nomompop<0, n_prevg_1540_nomompop := 0]
  
  # AS EXAMPLE FOR HOW THIS BEHAVES, CONSIDER BWA, year==2001, serial==22770000
  # a <- x[year==2001& serial==22770000]
  # this is an interesting example for the following reason
  # there are many members in this household, and a lot of them have 
  # previous generation education. they are assigned this because
  # they dont have a gencode based on relationship to head (being other relatives)
  # but through the 15-40 older rule. at the same time, they are all part of 
  # numeric generation 0, which starts at age 0. Note though that the 
  # individual that gets them their old education is 45 years old, so
  # is two numeric generations above them. 
  # this is an edge case
  

  x <- x[, c('year', 'serial', 'pernum', 'n_sameg_r', 'n_sameg_n',
             'n_prevg_r', 'n_prevg_1540', 'n_prevg_r_nomompop', 'n_prevg_1540_nomompop')]
  x[is.na(n_prevg_r), `:=`(n_prevg_r = 0, n_prevg_r_nomompop = 0)]

  
  dt <- merge.data.table(dt, x, by=c('year', 'serial', 'pernum'), all=TRUE)
  
  return(dt)
  
}

assign_relcat <- function(dt) {
  
  relstrs <- c("Head", "Spouse/partner", "Spouse", "Child",
               "Biological child", "Adopted child", "Stepchild",
               "Other relative", "Grandchild", "Grandchild or great grandchild", 
               "Parent/parent-in-law", "Parent", "Parent-in-law", 
               "Child-in-law", "Sibling/sibling-in-law", 
               "Sibling", "Sibling-in-law", "Grandparent",
               "Parent/grandparent/ascendant", "Aunt/uncle", 
               "Nephew/niece", "Cousin", "Other relative, not elsewhere classified", 
               "Non-relative", "Friend/guest/visitor/partner", "Visitor", 
               "Employee", "Domestic employee", "Foster child",
               "Group quarters", "Non-relative, n.e.c.", "Other relative or non-relative",
               "Unknown")
  
  relcodes <- c(1000, 2000, 2100, 3000, 3100, 3200, 3300, 4000,
                4100, 4110, 4200, 4210, 4220, 4300, 4400,
                4410, 4430, 4500, 4600, 4700, 4810, 4820,
                4900, 5000, 5100, 5120, 5200, 5210, 5330,
                5600, 5900, 6000, 9999)
  
  dt[, relstr := '']
  for (i in 1:length(relstrs)) {
    dt[related==relcodes[i], relstr:=relstrs[i]]
  }
  
  dt[, relcat := '']
  dt[relstr=='Child', relcat := 'child']
  dt[relstr=='Other relative, not elsewhere classified', relcat := 'other relative']
  dt[relstr=='Spouse/partner', relcat := 'spouse']
  dt[relstr=='Grandchild', relcat := 'grandchild']
  dt[relstr=='Parent-in-law', relcat := 'other relative']
  dt[relstr=='Head', relcat := 'head']
  dt[relstr=='Non-relative, n.e.c.', relcat := 'non-relative']
  dt[relstr=='Parent', relcat := 'other relative']
  dt[relstr=='Foster child', relcat := 'other child']
  dt[relstr=='Biological child', relcat := 'biological child']
  dt[relstr=='Child-in-law', relcat := 'other relative']
  dt[relstr=='Sibling', relcat := 'sibling']
  dt[relstr=='Adopted child', relcat := 'other child']
  dt[relstr=='Non-relative', relcat := 'non-relative']
  dt[relstr=='Nephew/niece', relcat := 'other relative']
  dt[relstr=='Cousin', relcat := 'other relative']
  dt[relstr=='Aunt/uncle', relcat := 'other relative']
  dt[relstr=='Stepchild', relcat := 'other child']
  dt[relstr=='Grandparent', relcat := 'other relative']
  dt[relstr=='Visitor', relcat := 'non-relative']
  dt[relstr=='Group quarters', relcat := 'non-relative']
  dt[relstr=='Sibling-in-law', relcat := 'other relative']
  dt[relstr=='Parent/grandparent/ascendant', relcat := 'other relative']
  dt[relstr=='Employee', relcat := 'non-relative']
  dt[relstr=='Sibling/sibling-in-law', relcat := 'sibling']
  dt[relstr=='Parent/parent-in-law', relcat := 'other relative']
  dt[relstr=='Other relative or non-relative', relcat := 'other relative']
  dt[relstr=='Other relative', relcat := 'other relative']
  dt[relstr=='Domestic employee', relcat := 'non-relative']
  dt[relstr=='Unknown', relcat := 'non-relative']
  dt[relstr=='Friend/guest/visitor/partner', relcat := 'non-relative']
  dt[relstr=='Grandchild or great grandchild', relcat := 'grandchild']
  dt[relstr=='Spouse', relcat := 'spouse']
  
  dt[, relstr:= NULL]
  
  return(dt)
  
}

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA", 
         "SEN", "SLE", "UGA", "ZAF", "ZMB", 
         "TGO")

ISOE <- c("BEN", "BFA", "BWA", "ETH", "GHA",
          "GIN", "LBR", "MAR", "MLI", "MOZ",
          "MWI", "NGA", "SEN", "SLE", "TGO", 
          "UGA", "ZAF", "ZMB")

for (iiso in 1:21) {
  
  # iiso <- 3
  
  isoc <- ISO[iiso]
  
  print(sprintf('################ %s ################', isoc))
  
  setwd(indir_edu)
  dt <- as.data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', isoc)))
  dt <- dt[, c('year', 'serial', 'pernum', 'related', 'famcode', 'gencode', 'momloc', 'poploc', 'age', 'eckid', 'ec0', 'a0', 'amom', 'apop', 'ecmom', 'ecpop')]
  setwd(indir_bas)
  base <- as.data.table(readRDS(sprintf('basedat_%s.rds', isoc)))
  base <- base[, c('year', 'serial', 'pernum', 'male', 'urban')]
  setwd(indir_dis)
  districts <- as.data.table(readRDS(sprintf('districts_%s.rds', isoc)))
  districts <- districts[, c('year', 'serial', 'pernum', 'district')]
  setwd(indir_pro)
  provinces <- as.data.table(readRDS(sprintf('provinces_%s.rds', isoc)))
  setwd(indir_rel)
  rel <- as.data.table(readRDS(sprintf('religions_%s.rds', isoc)))
  rel[, harmonized_religion:= NULL]
  setwd(indir_rl0)
  rl0 <- as.data.table(readRDS(sprintf('old_rel_%s.rds', isoc))) 
  rl0[, c("major_religion", "major_religion_pop", "major_religion_mom"):=NULL]
  setwd(indir_ioc)
  indocc <- as.data.table(readRDS(sprintf('new_ind_occ_%s.rds', isoc)))
  indocc[, c('empind', 'empocc'):=NULL]
  setwd(indir_coh)
  cohabt <- as.data.table(readRDS(sprintf('cohabtypes_%s.rds', isoc)))
  cohabt[, `:=`(extended = momothers+popothers+mompopothers)]
  cohabt[, c('related', 'unrecorded', 'momothers', 'popothers', 'mompopothers'):=NULL]
  cohabt[, check := momonly+poponly+mompoponly+othersonly+extended]

  dt <- merge(dt, base, by = c('year', 'serial', 'pernum'))
  dt <- merge(dt, districts, by = c('year', 'serial', 'pernum'))
  dt <- merge(dt, provinces, by = c('year', 'serial', 'pernum'))
  dt <- merge(dt, rel, by = c('year', 'serial', 'pernum'))
  dt <- merge(dt, rl0, by = c('year', 'serial', 'pernum'))
  dt <- merge(dt, indocc, by = c('year', 'serial', 'pernum'))
  dt <- merge(dt, cohabt, by = c('year', 'serial', 'pernum'))
  if (isoc %in% ISOE) {
    setwd(indir_eth)
    eth <- as.data.table(readRDS(sprintf('ethnicities_%s.rds', isoc)))
    dt <- merge(dt, eth, by = c('year', 'serial', 'pernum'))
    rm(eth)
  } else {
    dt[, eth_hrm := NA]
    dt[, eth_hrm := as.character(eth_hrm)]
  }

  rm(base, districts, provinces, rel, indocc, cohabt)
  
  dt[, lit0 := as.numeric(ec0>0)]
  dt[is.na(ec0), lit0 := NA]
  dt[, bd := 10*floor((year-age)/10)]
  dt[, shrlit0rel := mean(lit0, na.rm=TRUE), by=c('district', 'bd', 'major_religion')]
  dt[age>=14&age<=18, shrlit0rel1418 := mean(lit0, na.rm=TRUE), by=c('district', 'bd', 'major_religion')]
  dt[is.na(bd), shrlit0rel:=NA]
  dt[is.na(bd), shrlit0rel1418:=NA]
  dt[, c('lit0', 'bd'):=NULL]

  dt[, haverel:= sum(major_religion!=''), by=year]
  dt <- dt[haverel>0]
  dt[,haverel := NULL]

  dt <- assign_mg(dt)

  dt[, c('famcode', 'gencode', 'momloc', 'poploc'):=NULL]
  dt[, nhhmemb := .N, by=c('year', 'serial')]

  dt <- dt[age>=14&age<=18&(major_religion!='')&!is.na(eckid)&!is.na(ec0)]

  dt[, iso:=isoc]
  setcolorder(dt, 'iso')
  print(table(dt$check))
  
  setwd(indir_mig)
  mig <- as.data.table(readRDS(sprintf('migdists_%s.rds', isoc)))
  dt <- merge(dt, mig, by = c('year', 'serial', 'pernum'), all.x=TRUE)
  rm(mig)
    
  dt[, c('serial', 'pernum', 'check'):=NULL]

  dt <- assign_relcat(dt)

  if (iiso == 1) {
    DT <- dt
  } else {
    DT <- rbind(DT, dt, fill=TRUE)
  }
}

setwd(outdir)

saveRDS(DT, '_ilevel_data_for_famchars_regs.rds')