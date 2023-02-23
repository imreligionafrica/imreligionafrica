rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(lfe)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
outdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE) 

setwd(outdir)
dt <- data.table(readRDS('_ilevel_data_for_famchars_regs.rds'))
dt[, c('eth_hrm'):=NULL]
dt <- dt[major_religion != 'No Religion' & major_religion != 'Other']
dt[, muslim:= as.integer(major_religion=='Muslim')]
dt[, traditional:= as.integer(major_religion=='Traditional')]
dt[, c('religiond', 'religiond_str', 'major_religion'):=NULL]
table(dt[, muslim])


ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA", 
         "SEN", "SLE", "UGA", "ZAF", "ZMB", 
         "TGO")

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

#########################################################
# construct binned versions of variables
#########################################################
# block 1
# household size - enter as is
dt[, nhhmemb := nhhmemb]
# number of houehold members same, previous, previous no mom pop 
# in different ways
# all enter as they are (no nas need to be replaced)
dt[, n_sameg_r := n_sameg_r]
dt[, n_sameg_n := n_sameg_n]
dt[, n_prevg_r := n_prevg_r]
dt[, n_prevg_1540 := n_prevg_1540]
dt[, n_prevg_r_nomompop := n_prevg_r_nomompop]
dt[, n_prevg_1540_nomompop := n_prevg_1540_nomompop]


# multigen hh - is already binary
dt[, mg := mg]
dt[is.na(mg), mg:= -9999]

# block2
# household structure
dt[, momonly := momonly]
dt[, poponly := poponly]
dt[, mompoponly := mompoponly]
dt[, othersonly := othersonly]
dt[, extended := extended]

# relationship to head
dt[, rh_child := as.integer(relcat == 'child')]
dt[, rh_biochild := as.integer(relcat == 'biological child')]
dt[, rh_othchild := as.integer(relcat == 'other child')]
dt[, rh_grndchild := as.integer(relcat == 'grandchild')]
dt[, rh_head := as.integer(relcat == 'head"')]
dt[, rh_spouse := as.integer(relcat == 'spouse')]
dt[, rh_sibl := as.integer(relcat == 'sibling')]
dt[, rh_othrel := as.integer(relcat == 'other relative')]
dt[, rh_nonrel:= as.integer(relcat == 'non-relative')]

# block3 
# previous generation age at birth
pgags <- c('a0', 'amom', 'apop')

for (vpg in pgags) {
  
  vpgstr <- paste0(vpg, '_birth')
  
  dt[, auxxx := get(vpg)-age]
  dt[, (vpgstr) := NA]
  dt[, (vpgstr) := as.character(get(vpgstr))]
  dt[auxxx < 5, (vpgstr):='0-4']
  dt[auxxx >=5 & auxxx <= 10, (vpgstr):='5_10']
  for (i in seq(21, 35, 5)) {
    dt[auxxx >= i & auxxx < i+5, (vpgstr):= sprintf('%s_%s', i, i+4)]
    print(sprintf('%s_%s', i, i+4))
  }
  for (i in seq(36, 90, 10)) {
    dt[auxxx >= i & auxxx < i+10, (vpgstr):= sprintf('%s_%s', i, i+9)]
    print(sprintf('%s_%s', i, i+9))
  }
  dt[is.na(get(vpgstr)), (vpgstr) := as.character(auxxx)]
  dt[is.na(get(vpgstr)), (vpgstr):= '-9999']
  dt[, auxxx := NULL]
}

# block4
# urban - is already binary
dt[, urban:= urban]
dt[is.na(urban), urban:= -9999]
# previous generation industry - already category
dt[, ind0:= ind0]
dt[is.na(ind0), ind0:= -9999]
# previous generation occupation - already category
dt[, occ0:= occ0]
dt[is.na(occ0), occ0:= -9999]

# block6
# district - will enter as fixed effect
# but need to make country-specific to deal with NAs
dt[, district:= as.integer(as.factor( paste0(iso, '_' ,as.character(district) )))]
# same for dur
dt[, dur:= as.integer(as.factor( paste0(iso, '_' ,as.character(district), '_', as.character(urban) )))]


##############################################################################
# checking for missing
##############################################################################
varlist <- c('nhhmemb', 'mg', 'n_sameg_r', 'n_sameg_n', 'n_prevg_r', 'n_prevg_1540', 'n_prevg_r_nomompop', 'n_prevg_1540_nomompop', 
             'momonly', 'poponly', 'mompoponly', 'othersonly', 'extended', 
             'rh_child', 'rh_biochild', 'rh_othchild', 'rh_grndchild', 'rh_head',
             'rh_spouse', 'rh_sibl', 'rh_othrel', 'rh_nonrel',
             'a0_birth', 'amom_birth', 'apop_birth',
             'urban', 'ind0', 'occ0',
             'district',
             'dur')
for (v in varlist) {
  print(sprintf('%s: %s', v, NROW(dt)-sum(table(dt[,get(v)]))))
}

##############################################################################
# factorizing
##############################################################################

varf <- c('mg', 'a0_birth', 'amom_birth', 'apop_birth', 'urban', 'ind0', 'occ0')
for (v in varf) {
  dt[, (v) := as.factor(get(v))] 
}

##############################################################################
# creating a birth-decade dummy to interact with country to make country-decade
# dummies
##############################################################################

dt[, byr:= year-age]
for (i in seq(1870, 2010, by = 10)) {
  dt[byr >= i & byr <= i+9, bd := i]
}
dt[, cbd:= as.integer(as.factor(paste0(iso, '_', as.character(bd))))]

###############################################################
###############################################################


# ##############################################################
# # dependent variables
# dt[ec0==0, up := as.integer(eckid > 0)]
# dt[ec0>0, down := as.integer(eckid == 0)]
# table(dt[, up])
# table(dt[, down])
# sum(table(dt[, up])) + sum(table(dt[, down])) - NROW(dt)
# ##############################################################




nvar <- 0
# nhhmemb
nvar <- nvar+1
# mg
nvar <- nvar + length(unique(dt[,mg]))
# n_name_r, n_name_n, n_prevg_r, n_prevg_1540, n_prevg_r_nomompop, n_prevg_1540_nomompop
nvar <- nvar + 6
# momonly, poponly, mompoponly, othersonly, extended
nvar <- nvar + 5
# rh_child, rh_biochild, rh_othchild, rh_grndchild, rh_head, rh_spouse, rh_sibl, rh_othrel, rh_nonrel
nvar <- nvar + 9
# a0_birth, amom_birth, apop_birth
nvar <- nvar + length(unique(dt[,a0_birth]))
nvar <- nvar + length(unique(dt[,amom_birth]))
nvar <- nvar + length(unique(dt[,apop_birth]))
# urban
nvar <- nvar + length(unique(dt[,urban]))
# ind0
nvar <- nvar + length(unique(dt[,ind0]))
# ind0
nvar <- nvar + length(unique(dt[,occ0]))

#############################################################
#############################################################

#################################################
out <- data.table(matrix(NA, 2*3*nvar, 6))

names(out) <- c('sample', 'fe', 'var', 'level', 'b', 'se')

out[, sample:=as.character(sample)]
out[, fe:=as.character(fe)]
out[, var:=as.character(var)]
out[, level:=as.character(level)]
out[, b:=as.numeric(b)]
out[, se:=as.numeric(se)]

sampvec <- c('all', 'boys', 'girls')
out[, sample := rep(sampvec, 2*nvar)]
out <- out[order(sample)]

fevec <- c('cbd', 'cbd+dur')
out[, fe := rep(fevec, 3*nvar)]
out <- out[order(sample, fe)]

varvec <- c('nhhmemb',
            rep('mg', length(levels(dt[,mg]))),
            'n_sameg_r',
            'n_sameg_n',
            'n_prevg_r',
            'n_prevg_1540',
            'n_prevg_r_nomompop',
            'n_prevg_1540_nomompop', 
            'momonly',
            'poponly',
            'mompoponly',
            'othersonly',
            'extended', 
            'rh_child', 
            'rh_biochild', 
            'rh_othchild', 
            'rh_grndchild', 
            'rh_head',
            'rh_spouse', 
            'rh_sibl', 
            'rh_othrel', 
            'rh_nonrel',
            rep('a0_birth', length(levels(dt[,a0_birth]))),
            rep('amom_birth', length(levels(dt[,amom_birth]))),
            rep('apop_birth', length(levels(dt[,apop_birth]))),
            rep('urban', length(levels(dt[,urban]))),
            rep('ind0', length(levels(dt[,ind0]))),
            rep('occ0', length(levels(dt[,occ0]))))

out[, var := rep(varvec, 2*3)]

levvec <- c('-',
            levels(dt[,mg]),
            '-',
            '-',
            '-',
            '-',
            '-',
            '-', 
            '-',
            '-',
            '-',
            '-',
            '-', 
            '-', 
            '-', 
            '-', 
            '-', 
            '-',
            '-', 
            '-', 
            '-', 
            '-',
            levels(dt[,a0_birth]),
            levels(dt[,amom_birth]),
            levels(dt[,apop_birth]),
            levels(dt[,urban]),
            levels(dt[,ind0]),
            levels(dt[,occ0]))

out[, level := rep(levvec, 2*3)]

out[, N:=0]
outm <- copy(out)
outt <- copy(out)
rm(out)

#################################################
#################################################
#################################################

for (i in 1:NROW(outm)) {
  
  samp <- outm[i,1]$sample
  if (samp == 'all') {
    dtest <- dt
  } else if (samp == 'boys') {
    dtest <- dt[male==1]
  } else {
    dtest <- dt[male==0]
  }
  
  fe <- outm[i,2]$fe
  
  var <- outm[i,3]$var
  lev <- outm[i,4]$level
  if (lev == '-') {
    dtest[, dv := get(var)]
  } else {
    dtest[, dv := as.integer(get(var) == lev)]
  }
    
  print(sprintf('%s, %s, %s, %s', samp, fe, var, lev))
  
  if (fe == 'cbd') {
    f <- 'dv ~ muslim + traditional | cbd | 0 | cbd'
  } else if (var == 'urban') {
    f <- 'dv ~ muslim + traditional | cbd + district | 0 | cbd'
  } else {
    f <- 'dv ~ muslim + traditional | cbd + dur | 0 | cbd'
  }
  
  m <- felm(as.formula(f), data=dtest, weights=dtest[,wt])
  
  outm[i,5] <- m$coefficients[1]
  outm[i,6] <- sqrt(m$clustervcv[1,1])
  outm[i,7] <- m$N
  
  outt[i,5] <- m$coefficients[2]
  outt[i,6] <- sqrt(m$clustervcv[2,2])
  outt[i,7] <- m$N

}

outm[, religion := 'muslim']
outt[, religion := 'traditional']
out <- rbind(outm, outt)
setcolorder(out, c('sample', 'fe', 'var', 'level', 'religion'))

fwrite(out, '_indlevel_balancedness_results.csv')










