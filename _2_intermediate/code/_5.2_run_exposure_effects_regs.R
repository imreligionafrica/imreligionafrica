rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(lfe)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
datpath <- file.path(rootdir, "_2_intermediate", "data")

dir.create(datpath, showWarnings = FALSE, recursive = TRUE)

setwd(datpath)

df <- data.table(readRDS('_mig_for_exposure_newecold.rds'))

alo <- 14
ahi <- 25

cols <- c('major_religion', 'male', 'migdistr', 'migdistr_bplmatch', 'mobo', 'mobpar',
          'age', 'ageatmig', 'bdy', 'serial_new', 'mg01425', 'mgpar1425',
          sprintf('yo_%s%s_bp', alo, ahi), sprintf('d_yo_%s%s', alo, ahi),
          sprintf('yor_%s%s_bp', alo, ahi), sprintf('d_yor_%s%s', alo, ahi))
df <- df[,cols, with=FALSE]
df <- df[major_religion!='Other' & major_religion!='No Religion']

df[, `:=`(Dod_mob = get(sprintf('d_yo_%s%s', alo, ahi)),
          Dod_mobr = get(sprintf('d_yor_%s%s', alo, ahi)))]

df <- df[ageatmig <= 18 & ageatmig >= 1 & age >= alo & age <= ahi]

minbd <- min(df[, bdy])
maxbd <- max(df[, bdy])

for (bd in minbd:maxbd) {
  df[, (sprintf('bd_%s', bd)) := as.integer(bdy == bd)]
}
# # birth-decade effects interacted with origin-characteristics
# for (bd in minbd:maxbd) {
#   df[, (sprintf('bd_%s_mob_o', bd)) := get(sprintf('bd_%s', bd)) * get(sprintf('yo_%s%s_bp', alo, ahi)) ]
#   df[, (sprintf('bd_%s_mobr_o', bd)) := get(sprintf('bd_%s', bd)) * get(sprintf('yor_%s%s_bp', alo, ahi)) ]
# }
# birth-decade effects interacted with origin-destination differences
for (bd in minbd:maxbd) {
  df[, (sprintf('bd_%s_Dod_mob', bd)) := get(sprintf('bd_%s', bd)) * Dod_mob ]
  df[, (sprintf('bd_%s_Dod_mobr', bd)) := get(sprintf('bd_%s', bd)) * Dod_mobr ]
}
# interaction between age-at-mig and origin-destination differences
for (amig in 1:18) {
  df[, (sprintf('Dod_mob_%s', amig)) := (ageatmig==amig) * Dod_mob ]
  df[, (sprintf('Dod_mobr_%s', amig)) := (ageatmig==amig) * Dod_mobr ]
}


#numeric version of origin-cohort (for fixed effect)
df[, bp_bch := as.integer(as.factor(paste0(as.character(migdistr_bplmatch),
                                           '_',
                                           as.character(bdy))))]


####################################################################################
####################################################################################
####################################################################################

LHS <- c('old', 'parents')
MG <- c('all', 'nomg')

for (lhsi in LHS) {
  
  for (mgi in MG) {
    
    print(sprintf('################# %s %s #################', lhsi, mgi))

    dfest <- copy(df)
    
    if (lhsi == 'old') {
      dfest[, mob := mobo]
    } else {
      dfest[, mob := mobpar]
    }
    
    if (mgi == 'nomg') {
      if (lhsi == 'old') {
        dfest <- dfest[mg01425 == 0]
      } else {
        dfest <- dfest[mgpar1425 == 0]
      } 
    }
    
    #################################################
    out <- data.table(matrix(NA, 15*18, 5))
    names(out) <- c('estimation', 'group', 'age_at_move', 'b', 'se')
    out[, estimation:=as.character(estimation)]
    out[, group:=as.character(group)]
    out[, age_at_move:=as.integer(age_at_move)]
    out[, b:=as.numeric(b)]
    out[, se:=as.numeric(se)]
    
    for (i in 1:(5*18)) {
      out[(i-1)*3+1,2] <- 'Christian'
      out[(i-1)*3+2,2] <- 'Muslim'
      out[(i-1)*3+3,2] <- 'Traditional'
    }
    out <- out[order(group)]
    out[, age_at_move:= rep(1:18, 15)]
    
    estvec <- c('baseline', 'household FE', 'boys', 'girls', 'overall D_odb')
    out[, estimation := rep(estvec, 3*18)]
    out <- out[order(estimation, group, age_at_move)]
    out[, N:=0]
    #################################################
    
    ##############################################################################
    ##############################################################################
    # baseline, no HHFE, own religious group D_odb
    ##############################################################################
    ##############################################################################
    
    #################################
    # Christian
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mobr_%s', amig), ' + '), sep= '')
    }
    for (bd in minbd:(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Christian'])
    
    for (aam in 1:18) {
      out[estimation=='baseline' & group=='Christian' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='baseline' & group=='Christian' & age_at_move==aam, 
          N:=est$N]
    }
    
    #################################
    # Muslim
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mobr_%s', amig), ' + '), sep= '')
    }
    for (bd in minbd:(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Muslim'])
    
    for (aam in 1:18) {
      out[estimation=='baseline' & group=='Muslim' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='baseline' & group=='Muslim' & age_at_move==aam, 
          N:=est$N]
    }
    
    
    #################################
    # Traditional
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mobr_%s', amig), ' + '), sep= '')
    }
    for (bd in (minbd+1):(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Traditional'])
    
    for (aam in 1:18) {
      out[estimation=='baseline' & group=='Traditional' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='baseline' & group=='Traditional' & age_at_move==aam, 
          N:=est$N]
    }
    
    
    
    
    ##############################################################################
    ##############################################################################
    # boys, no HHFE, own religious group D_odb
    ##############################################################################
    ##############################################################################
    
    #################################
    # Christian
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mobr_%s', amig), ' + '), sep= '')
    }
    for (bd in minbd:(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Christian'&male==1])
    
    for (aam in 1:18) {
      out[estimation=='boys' & group=='Christian' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='boys' & group=='Christian' & age_at_move==aam, 
          N:=est$N]
    }
    
    #################################
    # Muslim
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mobr_%s', amig), ' + '), sep= '')
    }
    for (bd in minbd:(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Muslim'&male==1])
    
    for (aam in 1:18) {
      out[estimation=='boys' & group=='Muslim' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='boys' & group=='Muslim' & age_at_move==aam, 
          N:=est$N]
    }
    
    
    #################################
    # Traditional
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mobr_%s', amig), ' + '), sep= '')
    }
    for (bd in (minbd+1):(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Traditional'&male==1])
    
    for (aam in 1:18) {
      out[estimation=='boys' & group=='Traditional' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='boys' & group=='Traditional' & age_at_move==aam, 
          N:=est$N]
    }
    
    
    
    ##############################################################################
    ##############################################################################
    # girls, no HHFE, own religious group D_odb
    ##############################################################################
    ##############################################################################
    
    #################################
    # Christian
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mobr_%s', amig), ' + '), sep= '')
    }
    for (bd in minbd:(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Christian'&male==0])
    
    for (aam in 1:18) {
      out[estimation=='girls' & group=='Christian' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='girls' & group=='Christian' & age_at_move==aam, 
          N:=est$N]
    }
    
    #################################
    # Muslim
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mobr_%s', amig), ' + '), sep= '')
    }
    for (bd in minbd:(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Muslim'&male==0])
    
    for (aam in 1:18) {
      out[estimation=='girls' & group=='Muslim' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='girls' & group=='Muslim' & age_at_move==aam, 
          N:=est$N]
    }
    
    
    #################################
    # Traditional
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mobr_%s', amig), ' + '), sep= '')
    }
    for (bd in (minbd+1):(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Traditional'&male==0])
    
    for (aam in 1:18) {
      out[estimation=='girls' & group=='Traditional' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='girls' & group=='Traditional' & age_at_move==aam, 
          N:=est$N]
    }
    
    
    
    ##############################################################################
    ##############################################################################
    # overall D_odb, no HHFE
    ##############################################################################
    ##############################################################################
    
    #################################
    # Christian
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mob_%s', amig), ' + '), sep= '')
    }
    for (bd in minbd:(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mob', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mob', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Christian'])
    
    for (aam in 1:18) {
      out[estimation=='overall D_odb' & group=='Christian' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='overall D_odb' & group=='Christian' & age_at_move==aam, 
          N:=est$N]
    }
    
    #################################
    # Muslim
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mob_%s', amig), ' + '), sep= '')
    }
    for (bd in minbd:(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mob', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mob', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Muslim'])
    
    for (aam in 1:18) {
      out[estimation=='overall D_odb' & group=='Muslim' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='overall D_odb' & group=='Muslim' & age_at_move==aam, 
          N:=est$N]
    }
    
    
    #################################
    # Traditional
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mob_%s', amig), ' + '), sep= '')
    }
    for (bd in (minbd+1):(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mob', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mob', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Traditional'])
    
    for (aam in 1:18) {
      out[estimation=='overall D_odb' & group=='Traditional' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='overall D_odb' & group=='Traditional' & age_at_move==aam, 
          N:=est$N]
    }
    
    ##############################################################################
    ##############################################################################
    # HHFE, own religious group D_odb
    ##############################################################################
    ##############################################################################
    
    #################################
    # Christian
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mobr_%s', amig), ' + '), sep= '')
    }
    for (bd in minbd:(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'serial_new + bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Christian'])
    
    for (aam in 1:18) {
      out[estimation=='household FE' & group=='Christian' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='household FE' & group=='Christian' & age_at_move==aam, 
          N:=est$N]
    }
    
    #################################
    # Muslim
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mobr_%s', amig), ' + '), sep= '')
    }
    for (bd in minbd:(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'serial_new + bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Muslim'])
    
    for (aam in 1:18) {
      out[estimation=='household FE' & group=='Muslim' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='household FE' & group=='Muslim' & age_at_move==aam, 
          N:=est$N]
    }
    
    
    #################################
    # Traditional
    #################################
    
    string_formula <- ''
    string_formula <- paste(string_formula, 'mob ~ ', sep = '')
    # 1) own group or all mobility
    for (amig in 1:18) {
      string_formula <- paste(string_formula, paste0(sprintf('Dod_mobr_%s', amig), ' + '), sep= '')
    }
    for (bd in (minbd+1):(maxbd-1)) {
      if (bd != (maxbd-1)) {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' + '), sep= '')
      } else {
        string_formula <- paste(string_formula, paste0(sprintf('bd_%s_Dod_mobr', bd), ' | '), sep= '')
      }
    }
    # 2) fixed effects
    string_formula <- paste0(string_formula, 'serial_new + bp_bch + ageatmig | 0 |')
    # 3) cluster variables
    string_formula <- paste0(string_formula, 'migdistr + ', 'migdistr_bplmatch')
    
    est <- felm(as.formula(string_formula), data=dfest[major_religion=='Traditional'])
    
    for (aam in 1:18) {
      out[estimation=='household FE' & group=='Traditional' & age_at_move==aam, 
          `:=`(b=est$coefficients[aam],
               se=sqrt(est$clustervcv[aam,aam])
          )
          ]
      out[estimation=='household FE' & group=='Traditional' & age_at_move==aam, 
          N:=est$N]
    }
    
    out[, lhs := lhsi]
    out[, mg := mgi]
    setcolorder(out, c('lhs', 'mg'))
    
    if (lhsi == 'old' & mgi == 'all') {
      OUT <- out
    } else{
      OUT <- rbind(OUT, out)
    }
  }
}

fwrite(OUT, '_chetty_hendren_results.csv')