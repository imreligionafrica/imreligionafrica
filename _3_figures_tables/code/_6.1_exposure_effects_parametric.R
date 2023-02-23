rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(fixest)
library(stringr)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

datpath <- file.path(rootdir, "_2_intermediate", "data")
outpath <- file.path(rootdir, "_3_figures_tables", "data")

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
# birth-decade effects interacted with origin-characteristics
for (bd in minbd:maxbd) {
  df[, (sprintf('bd_%s_mob_o', bd)) := get(sprintf('bd_%s', bd)) * get(sprintf('yo_%s%s_bp', alo, ahi)) ]
  df[, (sprintf('bd_%s_mobr_o', bd)) := get(sprintf('bd_%s', bd)) * get(sprintf('yor_%s%s_bp', alo, ahi)) ]
}
# birth-decade effects interacted with origin-destination differences
for (bd in minbd:maxbd) {
  df[, (sprintf('bd_%s_Dod_mob', bd)) := get(sprintf('bd_%s', bd)) * Dod_mob ]
  df[, (sprintf('bd_%s_Dod_mobr', bd)) := get(sprintf('bd_%s', bd)) * Dod_mobr ]
}
# piecewise linear terms
df[, `:=`(amig_1_11_Dob_mob = as.integer(ageatmig < 12) * Dod_mob,
          amig_1_11_Dob_mobr = as.integer(ageatmig < 12) * Dod_mobr,
          amig_12_18_Dob_mob = as.integer(ageatmig >= 12) * Dod_mob,
          amig_12_18_Dob_mobr = as.integer(ageatmig >= 12) * Dod_mobr,
          amig_1_11_Dob_mob_ext = as.integer(ageatmig < 12) * Dod_mob * (18-ageatmig),
          amig_1_11_Dob_mobr_ext = as.integer(ageatmig < 12) * Dod_mobr * (18-ageatmig),
          amig_12_18_Dob_mob_ext = as.integer(ageatmig >= 12) * Dod_mob * (18-ageatmig),
          amig_12_18_Dob_mobr_ext = as.integer(ageatmig >= 12) * Dod_mobr * (18-ageatmig)
          )
   ]


####################################################################################
####################################################################################
####################################################################################
####################################################################################

setwd(outpath)


LHS <- c('mobo', 'mobpar')
MG <- c('including', 'excluding')
DODB <- c('own', 'overall')
SAMP <- c('all', 'male', 'female')


for (lhs in LHS) {
  for (mg in MG) {
    for (dodb in DODB){
     for (samp in SAMP){
      
       dt <- copy(df)
       
       if (mg == 'excluding' & lhs == 'mobo') {
         dt <- dt[mg01425==0]
       } else if (mg == 'excluding' & lhs == 'mobpar') {
         dt <- dt[mgpar1425==0]
       }
         
       if (samp == 'female') {
         dt <- dt[male==0]
       } else if (samp == 'male') {
         dt <- dt[male==1]
       }
       
       caption <- 'Parametric exposure effect estimates, previous generation = '
       if (lhs == 'mobo') {
         caption <- paste0(caption, 'all old, ', samp, ' individuals, ', dodb, ' $\\Delta_{odb}$, ', mg, ' multigenerational households')
       } else {
         caption <- paste0(caption, 'parents only, ', samp, ' individuals, ', dodb, ' $\\Delta_{odb}$, ', mg, ' multigenerational households')
       }
       
       
       filename <-paste(lhs, paste0(mg, 'mg'), dodb, samp, sep='_')
       label <- paste0('tab:chettyhendren_parametric_', filename)
       
       print(filename)
       
       
       ####################################################################################
       ####################################################################################
       
       
       models <- list()
       
       dtc <- dt[major_religion=='Christian']
       dtm <- dt[major_religion=='Muslim']
       dtt <- dt[major_religion=='Traditional']
       
       #############################################################
       # CHRISTIAN
       #############################################################
       
       # RHS
       if (dodb == 'own') {
         dtc[, `:=`(eeff_1_11 = amig_1_11_Dob_mobr_ext, 
                    eeff_12_18 = amig_12_18_Dob_mobr_ext)]
         rhs1 <- paste(paste0('bd_', seq(minbd, maxbd), '_mobr_o'), collapse = ' + ')
         rhs2 <- paste(paste0('bd_', seq(minbd, maxbd), '_Dod_mobr'), collapse = ' + ')
         rhs3 <- 'amig_1_11_Dob_mobr + amig_12_18_Dob_mobr'
         
       } else {
         dtc[, `:=`(eeff_1_11 = amig_1_11_Dob_mob_ext, 
                    eeff_12_18 = amig_12_18_Dob_mob_ext)]  
         rhs1 <- paste(paste0('bd_', seq(minbd, maxbd), '_mob_o'), collapse = ' + ')
         rhs2 <- paste(paste0('bd_', seq(minbd, maxbd), '_Dod_mob'), collapse = ' + ')
         rhs3 <- 'amig_1_11_Dob_mob + amig_12_18_Dob_mob'
       }
       rhs4 <- 'eeff_1_11 + eeff_12_18'
       
       f <- paste0(lhs, ' ~ ', rhs1, ' + ', rhs2, ' + ', rhs3, ' + ', rhs4)
       f <- paste0(f, ' | bdy + ageatmig')
       f <- as.formula(f) 
       
       m <- feols(f, dtc, nthreads=8)
       
       models[['c_obs']] <- m
       
       #############################################################
       
       hhfe <- 'yes'
       
       # RHS
       if (dodb == 'own') {
         dtc[, `:=`(eeff_1_11 = amig_1_11_Dob_mobr_ext, 
                    eeff_12_18 = amig_12_18_Dob_mobr_ext)]
         rhs1 <- paste(paste0('bd_', seq(minbd, maxbd), '_mobr_o'), collapse = ' + ')
         rhs2 <- paste(paste0('bd_', seq(minbd, maxbd), '_Dod_mobr'), collapse = ' + ')
         rhs3 <- 'amig_1_11_Dob_mobr + amig_12_18_Dob_mobr'
         
       } else {
         dtc[, `:=`(eeff_1_11 = amig_1_11_Dob_mob_ext, 
                    eeff_12_18 = amig_12_18_Dob_mob_ext)]  
         rhs1 <- paste(paste0('bd_', seq(minbd, maxbd), '_mob_o'), collapse = ' + ')
         rhs2 <- paste(paste0('bd_', seq(minbd, maxbd), '_Dod_mob'), collapse = ' + ')
         rhs3 <- 'amig_1_11_Dob_mob + amig_12_18_Dob_mob'
       }
       rhs4 <- 'eeff_1_11 + eeff_12_18'
       
       f <- paste0(lhs, ' ~ ', rhs1, ' + ', rhs2, ' + ', rhs3, ' + ', rhs4)
       f <- paste0(f, ' | bdy + ageatmig + serial_new')
       f <- as.formula(f)
       
       m <- feols(f, dtc, nthreads=8)
       
       models[['c_hhfe']] <- m
       
       #############################################################
       # MUSLIM
       #############################################################
       
       # RHS
       if (dodb == 'own') {
         dtm[, `:=`(eeff_1_11 = amig_1_11_Dob_mobr_ext, 
                    eeff_12_18 = amig_12_18_Dob_mobr_ext)]
         rhs1 <- paste(paste0('bd_', seq(minbd, maxbd), '_mobr_o'), collapse = ' + ')
         rhs2 <- paste(paste0('bd_', seq(minbd, maxbd), '_Dod_mobr'), collapse = ' + ')
         rhs3 <- 'amig_1_11_Dob_mobr + amig_12_18_Dob_mobr'
         
       } else {
         dtm[, `:=`(eeff_1_11 = amig_1_11_Dob_mob_ext, 
                    eeff_12_18 = amig_12_18_Dob_mob_ext)]  
         rhs1 <- paste(paste0('bd_', seq(minbd, maxbd), '_mob_o'), collapse = ' + ')
         rhs2 <- paste(paste0('bd_', seq(minbd, maxbd), '_Dod_mob'), collapse = ' + ')
         rhs3 <- 'amig_1_11_Dob_mob + amig_12_18_Dob_mob'
       }
       rhs4 <- 'eeff_1_11 + eeff_12_18'
       
       f <- paste0(lhs, ' ~ ', rhs1, ' + ', rhs2, ' + ', rhs3, ' + ', rhs4)
       f <- paste0(f, ' | bdy + ageatmig')
       f <- as.formula(f)
       
       m <- feols(f, dtm, nthreads=8)
       
       models[['m_obs']] <- m
       
       #############################################################
       
       f <- paste0(lhs, ' ~ ', rhs1, ' + ', rhs2, ' + ', rhs3, ' + ', rhs4)
       f <- paste0(f, ' | bdy + ageatmig + serial_new')
       f <- as.formula(f)
       
       m <- feols(f, dtm, nthreads=8)
       
       models[['m_hhfe']] <- m
       
       #############################################################
       # TRADITIONAL
       #############################################################
       
       # RHS
       if (dodb == 'own') {
         dtt[, `:=`(eeff_1_11 = amig_1_11_Dob_mobr_ext, 
                    eeff_12_18 = amig_12_18_Dob_mobr_ext)]
         rhs1 <- paste(paste0('bd_', seq(minbd, maxbd), '_mobr_o'), collapse = ' + ')
         rhs2 <- paste(paste0('bd_', seq(minbd, maxbd), '_Dod_mobr'), collapse = ' + ')
         rhs3 <- 'amig_1_11_Dob_mobr + amig_12_18_Dob_mobr'
         
       } else {
         dtt[, `:=`(eeff_1_11 = amig_1_11_Dob_mob_ext, 
                    eeff_12_18 = amig_12_18_Dob_mob_ext)]  
         rhs1 <- paste(paste0('bd_', seq(minbd, maxbd), '_mob_o'), collapse = ' + ')
         rhs2 <- paste(paste0('bd_', seq(minbd, maxbd), '_Dod_mob'), collapse = ' + ')
         rhs3 <- 'amig_1_11_Dob_mob + amig_12_18_Dob_mob'
       }
       rhs4 <- 'eeff_1_11 + eeff_12_18'
       
       f <- paste0(lhs, ' ~ ', rhs1, ' + ', rhs2, ' + ', rhs3, ' + ', rhs4)
       f <- paste0(f, ' | bdy + ageatmig')
       f <- as.formula(f)
       
       m <- feols(f, dtt, nthreads=8)
       
       models[['t_obs']] <- m
       
       #############################################################
       
       f <- paste0(lhs, ' ~ ', rhs1, ' + ', rhs2, ' + ', rhs3, ' + ', rhs4)
       f <- paste0(f, ' | bdy + ageatmig + serial_new')
       f <- as.formula(f)
       
       m <- feols(f, dtt, nthreads=8)
       
       models[['t_hhfe']] <- m
       
       ####################################
       ####################################
       
       setFixest_dict(c(eeff_1_11='exposure ages 1-11', eeff_12_18='exposure ages 12-18'))
       
       out <- etable(models, cluster=~migdistr+migdistr_bplmatch,
                     tex=TRUE,
                     keep=c('exposure ages 1-11', 'exposure ages 12-18'), 
                     title=caption, style.tex = style.tex(yesNo=c('Yes', 'No')))
       
       out <- str_replace(out, '\\\\tabularnewline.+?\n', '\\\\hline')
       out <- str_replace(out, '\\\\begin\\{tabular\\}.+?\n', '\\\\resizebox{\\\\textwidth}{!}{\n\\\\begin\\{tabular\\}\\{lcccccc\\}\n')
       out <- str_replace(out, 'mobo.*', 'upward IM}\\\\\\\\\n\\\\hline')
       out <- str_replace(out, 'mobpar.*', 'upward IM}\\\\\\\\\n\\\\hline')
       out <- str_replace(out, '\\\\midrule', '\\\\hline')
       out <- str_replace(out, '\\\\bottomrule', '\\\\hline')
       out <- str_replace(out, '\\\\bottomrule', '\\\\hline')
       out <- str_replace(out, '\\\\multicolumn\\{7\\}.+\n', '')
       out <- str_replace(out, '\\\\multicolumn\\{7\\}.+\n', '')
       out <- str_replace(out, '\\\\end\\{tabular\\}\n', '\\\\end\\{tabular\\}\n}\n')
       out <- str_replace(out, 'bdy', 'birth-decade')
       out <- str_replace(out, 'ageatmig', 'age-at-migration')
       out <- str_replace(out, 'serial\\\\_new', 'household')
       out <- str_replace(out, 'htbp', 'ht!') 
       
       nl1 <- '&\\\\multicolumn{2}{c}{Christian}' 
       nl2 <- '&\\\\multicolumn{2}{c}{Muslim}' 
       nl3 <- '&\\\\multicolumn{2}{c}{Traditional}\\\\\\\\\n'
       nl4 <- '\\\\cline{2-7}\n'
       
       out <- str_replace(out, 'Model:', paste0(nl1, nl2, nl3, nl4, '\nModel:'))
       
       nl1a <- '\\\\captionsetup{size=scriptsize, justification=justified, width=\\\\columnwidth}\n'
       nl2a <- '\\\\caption\\*{TABLE NOTES.}\n'
       out <- str_replace(out, '\\\\end\\{table\\}', paste0(nl1a, nl2a, '\\\\end{table}'))
       
       nl <- paste0('\\\\label{', label, '}\n')
       
       out <- str_replace(out, '\\\\resizebox', paste0(nl, '\\\\resizebox'))
       
       
       filename <- paste0('_6_chettyhendren_parametric_', filename, '.tex')
       
       fh <- file(filename)
       writeLines(out, fh)
       close(fh)
       
     } 
    }
  }
}












