rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
# library(fixest)
library(lfe)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
outdir <- file.path(rootdir, "_2_intermediate", "data")
codedir <- file.path(rootdir, "_2_intermediate", "code")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE) 

setwd(codedir)
source("_3.8_indlevel_gapsregs_helpers_noshrlitold.R")

setwd(outdir)
dt <- data.table(readRDS('_ilevel_data_for_famchars_regs.rds'))
dt <- dt[major_religion != 'No Religion' & major_religion != 'Other']
dt[, muslim:= as.integer(major_religion=='Muslim')]
dt[, traditional:= as.integer(major_religion=='Traditional')]
# dt[, noreligion:= as.integer(major_religion=='No Religion')]
# dt[, other:= as.integer(major_religion=='Other')]
dt[, c('major_religion'):=NULL]
table(dt[, muslim])


ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA",
         "SEN", "SLE", "TGO", "UGA", "ZAF", 
         "ZMB")

pop <- fread('pop_world.csv')

pop <- pop[iso %in% ISO & year == 1980]
pop[, shrpop := pop / sum(pop)]
pop <- pop[, c('iso', 'shrpop')]

dt[, nobsc := .N, by = iso]
dt[, shrobs := nobsc / .N]
dt[, nobsc := NULL]
dt <- merge(dt, pop, by='iso')
dt[, wt := shrpop / shrobs]

similar_dists <- fread('_distribution_similarity_y0_mct_bd.csv')
similar_dists <- similar_dists[, .(iso, district, bd, 
                                   r_all_m_byc, n_all_m_byc, r_1418_m_byc, n_1418_m_byc,
                                   r_all_t_byc, n_all_t_byc, r_1418_t_byc, n_1418_t_byc
)]

similar_m <- similar_dists[!is.na(r_1418_m_byc), .(iso, district, bd,
                                                   r_1418_m_byc, n_1418_m_byc)]
similar_t <- similar_dists[!is.na(r_1418_t_byc), .(iso, district, bd,
                                                   r_1418_t_byc, n_1418_t_byc)]
similar_m[r_1418_m_byc <= 0.5 * n_1418_m_byc, similar_m:=1]
similar_m[r_1418_m_byc > 0.5 * n_1418_m_byc, similar_m:=0]
similar_t[r_1418_t_byc <= 0.5 * n_1418_t_byc, similar_t:=1]
similar_t[r_1418_t_byc > 0.5 * n_1418_t_byc, similar_t:=0]
similar_m <- similar_m[, .(iso, district, bd, similar_m)]
similar_t <- similar_t[, .(iso, district, bd, similar_t)]


dt[, bd := 10*floor((year-age)/10)]
dt <- merge(dt, similar_m, by=c('iso', 'district', 'bd'), all.x=TRUE, all.y=FALSE)
dt <- merge(dt, similar_t, by=c('iso', 'district', 'bd'), all.x=TRUE, all.y=FALSE)

rm(similar_dists, similar_m, similar_t)


weights <- dt[, c('iso', 'shrpop', 'shrobs', 'wt')]
weights <- unique(weights, by='iso')


#########################################################
# construct binned versions of variables
#########################################################
# block 1
# household size - enter as is
dt[, nhhmemb := nhhmemb]
# number of household members same, previous, previous no mom pop 
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

# # block5
# # migrant - is already binary
# dt[, mig:= mig]
# dt[is.na(mig), mig:= -9999]

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
# varf <- c('mg', 'a0_birth', 'amom_birth', 'apop_birth', 'urban', 'ind0', 'occ0', 'mig')

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


##############################################################
# dependent variables
dt[ec0==0, up := as.integer(eckid > 0)]
dt[ec0>0, down := as.integer(eckid == 0)]
table(dt[, up])
table(dt[, down])
sum(table(dt[, up])) + sum(table(dt[, down])) - NROW(dt)
##############################################################


#############################################################
#############################################################

#################################################
out <- data.table(matrix(NA, 2*3*7, 5))
names(out) <- c('sample', 'direction', 'controls', 'b', 'se')
out[, sample:=as.character(sample)]
out[, direction:=as.character(direction)]
out[, controls:=as.character(controls)]
out[, b:=as.numeric(b)]
out[, se:=as.numeric(se)]

sampvec <- c('all', 'boys', 'girls')
out[, sample := rep(sampvec, 2*7)]
out <- out[order(sample)]
dirvec <- c('up', 'down')
out[, direction := rep(dirvec, 3*7)]
out <- out[order(sample, direction)]
contvec <- c('-',
             'country-birth-decade FE + child age FEs',
             '+ hh sz + I(mg hh) + #hh mbrs same gen, prev. gen, prev. gen w/o mom/pop + fam struct dummies + rel head dummies + pg aab dummies',
             '+ urban + prev. gen. industry + prev. gen. occupation',
             '+ district-urban FEs',
             'in above median closest parental education distribution districts: C v M',
             'in above median closest parental education distribution districts: C v T')
out[, controls := rep(contvec, 2*3)]
out[, N:=0]
#################################################

outm <- copy(out)
outt <- copy(out)
rm(out)

setwd(outdir)
if (!('muslims.csv' %in% dir(outdir))) {
  fwrite(outm, 'muslims.csv')
}
if (!('traditionals.csv' %in% dir(outdir))) {
  fwrite(outt, 'traditionals.csv')
}


block1a <- ' + nhhmemb + mg +  n_sameg_r + n_sameg_n + n_prevg_r + n_prevg_1540 + n_prevg_r_nomompop + n_prevg_1540_nomompop'
block1b <- ' + momonly + poponly + mompoponly + othersonly + rh_child + rh_biochild + rh_othchild + rh_grndchild + rh_head + rh_spouse + rh_sibl + rh_nonrel' 
# note 'extended' is o.c. in momonly, poponly
# 'rh_othrel' is o.c. in relationship to head
block1c <- ' + a0_birth + amom_birth + apop_birth'
block1 <- paste0(block1a, block1b, block1c)
block2 <- ' + urban + ind0 + occ0'

closevec <- c(rep('all', 5), 'mc', 'tc')

for (spl in sampvec) {
  
  for (updn in dirvec) {
    
    for (specification in 1:4) {
      
      print(sprintf('%s, %s, %s', spl, updn, contvec[specification]))
      
      # # print('FIXEST')
      # ffixest <- prepare_formula(specification = specification, updn = updn, package = 'fixest')
      # mfixest <- run_estimation(formula=ffixest, spl=spl, updn=updn, close=closevec[specification], package='fixest')
      # collect_results(model=mfixest, specification=specification, spl=spl, updn=updn, package='fixest', outdtm=outm_fixest, outdtt=outt_fixest)
      
      # this code sometimes crashes due to memory issues - to avoid re-running
      # the entire loop, write the results to disk each time and re-read the
      # the results
      outm <- fread('muslims.csv')
      outt <- fread('traditionals.csv')
      outm[, `:=`(b=as.numeric(b), se=as.numeric(se))]
      outt[, `:=`(b=as.numeric(b), se=as.numeric(se))]
      
      if ((is.na(outm[sample==spl & direction==updn & controls==contvec[specification], b])) & 
          (is.na(outt[sample==spl & direction==updn & controls==contvec[specification], b]))) {
        
        ffelm <- prepare_formula(specification = specification, updn = updn, package = 'felm')
        mfelm <- run_estimation(formula=ffelm, spl=spl, updn=updn, close=closevec[specification], package='felm')
        collect_results(model=mfelm, specification=specification, spl=spl, updn=updn, package='felm', outdtm=outm, outdtt=outt)
        
        fwrite(outm, 'muslims.csv')
        fwrite(outt, 'traditionals.csv')
        
      }
      
    }
    
  }
  
}

rm(dt, outm, outt)

outm <- fread('muslims.csv')
outt <- fread('traditionals.csv')

outm[, religion := 'muslim']
outt[, religion := 'traditional']
out <- rbind(outm, outt)
setcolorder(out, c('sample', 'direction', 'controls', 'religion'))
fwrite(out, '_indlevel_gapregs_results_noshrlitold.csv')


