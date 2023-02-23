rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "KEN", "LBR",
         "LSO", "MAR", "MLI", "MOZ", "MUS",
         "MWI", "NGA", "RWA", "SDN", "SEN", 
         "SLE", "SSD", "TGO", "TZA", "UGA", 
         "ZAF", "ZMB", "ZWE")

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

inpath <- file.path(rootdir, "_1_preprocessing", "data", "raw", "edu")
outpath <- file.path(rootdir, "_1_preprocessing", "data", "standardized","edu")

dir.create(outpath, showWarnings = FALSE, recursive = TRUE)

treat_data <- function(dt, isoc) {
  
  dt[age > 150, age := NA]
  
  ##################################################################
  ##################################################################
  
  dt[, eckid := NA]
  dt[, eckid := as.integer(eckid)]
  dt[, ysc := NA]
  dt[, ysc := as.integer(ysc)]
  
  # less than primary completed
  dt[edattain == 1, eckid:= 0]
  # primary completed
  dt[edattain == 2, eckid:= 1]
  # secondary completed
  dt[edattain == 3, eckid:= 2]
  # university completed
  dt[edattain == 4, eckid:= 3]
  
  # less than primary completed
  dt[is.na(eckid) & edattaind == 100, eckid := 0]
  # no schooling
  dt[is.na(eckid) & edattaind == 110, eckid := 0]
  # some primary
  dt[is.na(eckid) & edattaind == 120, eckid := 1]
  # primary (4 years)
  dt[is.na(eckid) & edattaind == 130, eckid := 1]
  # primary (5 years)
  dt[is.na(eckid) & edattaind == 211, eckid := 1]
  # primary (6 years)
  dt[is.na(eckid) & edattaind == 212, eckid := 1]
  # lower secondary general track
  dt[is.na(eckid) & edattaind == 221, eckid := 1]
  # lower secondary technical track
  dt[is.na(eckid) & edattaind == 222, eckid := 1]
  # secondary general track
  dt[is.na(eckid) & edattaind == 311, eckid := 2]
  # some college
  dt[is.na(eckid) & edattaind == 312, eckid := 2]
  # secondary technical track
  dt[is.na(eckid) & edattaind == 320, eckid := 2]
  # secondary technical degree
  dt[is.na(eckid) & edattaind == 321, eckid := 2]
  # post-secondary technical eduation
  dt[is.na(eckid) & edattaind == 322, eckid := 2]
  # university completed
  dt[is.na(eckid) & edattaind == 400, eckid := 3]
  
  if (isoc != "EGY" & isoc != "MOZ" & isoc != "SDN" & isoc != "SSD") {
    
    dt[, ysc := yrschool]
    # not specified
    dt[yrschool == 90, ysc := NA]
    # some primary
    dt[yrschool == 91, ysc := 3]
    # some technical after primary
    dt[yrschool == 92, ysc := 9]
    # some secondary
    dt[yrschool == 93, ysc := 9]
    # some tertiary
    dt[yrschool == 94, ysc := 14]
    # adult literacy
    dt[yrschool == 95, ysc := 6]
    # special education
    dt[yrschool == 96, ysc := NA]
    # unknown/missing
    dt[yrschool == 98, ysc := NA]
    # NIU
    dt[yrschool == 99, ysc := NA]    
    
  } else {
    
    dt[, yrschool := NA]
    
  }
  
  ##########################
  # special case: Rwanda
  ##########################
  
  if (isoc == "RWA") {
    
    # No education
    dt[is.na(eckid) & rw1991a_edattan == 1, eckid := 0]
    dt[rw1991a_edattan == 1, ysc:= 0]
    # Primary, year 1
    dt[is.na(eckid) & rw1991a_edattan == 11, eckid := 0]
    dt[rw1991a_edattan == 11, ysc:= 1]
    # Primary, year 2
    dt[is.na(eckid) & rw1991a_edattan == 12, eckid := 0]
    dt[rw1991a_edattan == 12, ysc:= 2]
    # Primary, year 3
    dt[is.na(eckid) & rw1991a_edattan == 13, eckid := 0]
    dt[rw1991a_edattan == 13, ysc:= 3]
    # Primary, year 4
    dt[is.na(eckid) & rw1991a_edattan == 14, eckid := 0]
    dt[rw1991a_edattan == 14, ysc:= 4]
    # Primary, year 5
    dt[is.na(eckid) & rw1991a_edattan == 15, eckid := 0]
    dt[rw1991a_edattan == 15, ysc:= 5]
    # Primary, year 6
    dt[is.na(eckid) & rw1991a_edattan == 16, eckid := 1]
    dt[rw1991a_edattan == 16, ysc:= 6]
    # Primary, year 7
    dt[is.na(eckid) & rw1991a_edattan == 17, eckid := 1]
    dt[rw1991a_edattan == 17, ysc:= 7]
    # Primary, year 8
    dt[is.na(eckid) & rw1991a_edattan == 18, eckid := 1]
    dt[rw1991a_edattan == 18, ysc:= 8]
    # Post primary, year 1
    dt[is.na(eckid) & rw1991a_edattan == 21, eckid := 1]
    dt[rw1991a_edattan == 21, ysc:= 9]
    # Post primary, year 2
    dt[is.na(eckid) & rw1991a_edattan == 22, eckid := 1]
    dt[rw1991a_edattan == 22, ysc:= 10]
    # Post primary, year 3
    dt[is.na(eckid) & rw1991a_edattan == 23, eckid := 1]
    dt[rw1991a_edattan == 23, ysc:= 11]
    
    dt[, yrschool := NA]
    
  }
  
  t_eck <- dt[is.na(eckid) & (!is.na(edattain) | is.na(edattaind))]
  t_ysc <- dt[is.na(ysc) & !is.na(yrschool)]
  
  print(isoc)
  print(table(t_eck$edattain))
  print(table(t_eck$edattaind))
  print(table(t_ysc$yrschool))
  cat("\n")
  
  
  rm(t_eck, t_ysc)  
  ######################################################
  
  ##############################
  # filling in eckid with ysc
  ##############################
  
  dt[is.na(eckid) & !is.na(ysc) & ysc <= 5, eckid := 0]
  dt[is.na(eckid) & !is.na(ysc) & ysc >= 6 & ysc <= 11, eckid := 1]
  dt[is.na(eckid) & !is.na(ysc) & ysc >= 12 & ysc <= 15, eckid := 2]
  dt[is.na(eckid) & !is.na(ysc) & ysc >= 16, eckid := 3]
  
  ############################################
  # filling in ysc with edattaind and edattain
  ############################################
  
  # less than primary completed
  dt[is.na(ysc) & edattaind == 100, ysc := 0]
  # no schooling
  dt[is.na(ysc) & edattaind == 110, ysc := 0]
  # some primary
  dt[is.na(ysc) & edattaind == 120, ysc := 3]
  # primary (4 years)
  dt[is.na(ysc) & edattaind == 130, ysc := 4]
  # primary (5 years)
  dt[is.na(ysc) & edattaind == 211, ysc := 5]
  # primary (6 years)
  dt[is.na(ysc) & edattaind == 212, ysc := 6]
  # lower secondary general track
  dt[is.na(ysc) & edattaind == 221, ysc := 9]
  # lower secondary technical track
  dt[is.na(ysc) & edattaind == 222, ysc := 9]
  # secondary general track
  dt[is.na(ysc) & edattaind == 311, ysc := 12]
  # some college
  dt[is.na(ysc) & edattaind == 312, ysc := 14]
  # secondary technical track
  dt[is.na(ysc) & edattaind == 320, ysc := 12]
  # secondary technical degree
  dt[is.na(ysc) & edattaind == 321, ysc := 12]
  # post-secondary technical eduation
  dt[is.na(ysc) & edattaind == 322, ysc := 14]
  # university completed
  dt[is.na(ysc) & edattaind == 400, ysc := 16]
  
  # less than primary completed
  dt[is.na(ysc) & edattain == 1, ysc:= 0]
  # primary completed
  dt[is.na(ysc) & edattain == 2, ysc:= 6]
  # secondary completed
  dt[is.na(ysc) & edattain == 3, ysc:= 12]
  # university completed
  dt[is.na(ysc) & edattain == 4, ysc:= 16]
  
  dt[, c('edattain', 'edattaind', 'yrschool'):=NULL]
  
  #######################################################
  
  nms <- names(dt)
  dt <- merge(dt, codes[, c('related', 'gencode', 'famcode')], by='related', all.x = TRUE)
  setcolorder(dt, nms)
  
  #######
  # fix for ghana, 1984
  dt[is.na(momloc), momloc := 0]
  dt[is.na(poploc), poploc := 0]
  #######
  
  #########################################################################################
  # constructing old, mom, pop, uncle ages and educational attainment, and schooling
  #########################################################################################
  
  a <- dt[, c('year', 'serial', 'pernum', 'age', 'eckid', 'ysc', 'gencode', 'famcode', 'related', 'momloc', 'poploc')]
  # b <- unique(a, by = c('year', 'serial', 'age', 'famcode', 'related'))
  b <- copy(a)
  # b[, pernum:=NULL]
  # b <- b[famcode==1]
  b[, c('famcode'):=NULL]
  # b <- unique(a, by = c('year', 'serial', 'related'))
  b[, rcid := seq_len(.N), by=.(year,serial)]
  
  maxid <- max(b[, rcid])
  
  # b[, age:=NULL]
  # b <- dcast(b, year + serial ~ rcid, value.var = "related")
  # names(b) <- c(names(b)[1:2], paste('related_', 1:(ncol(b)-2), sep=''))
  
  b <- dcast(b, year + serial ~ rcid, value.var = c("related", "age", "eckid", "ysc", "pernum"))
  
  a <- merge(a, b, by = c('year', 'serial'), all.x=TRUE)
  
  a[, minao := age+minao_plus]
  a[, maxao := age+maxao_plus]
  
  for (gc1 in c(2, 1, 0, -1, -2, -9999)) {
    
    #########
    # gc1 <- -9999
    #########
    
    print(sprintf('Assigning old age + education, generation %s', gc1))
    
    if (gc1 > -9999) {
      x <- a[gencode == gc1]
    } else {
      x <- a[is.na(gencode)]
    }
    
    nv1 <- c('a0', 'amom', 'apop',
             'ec0', 'ecmom', 'ecpop',
             'ysc0', 'yscmom', 'yscpop')
    
    nv2 <- c('n_a0', 'n_amom', 'n_apop',
             'n_ec0', 'n_ecmom', 'n_ecpop',
             'n_ysc0', 'n_yscmom', 'n_yscpop')
    
    for (nv in nv1) {
      x[, (nv) := NA]
      x[, (nv) := as.numeric(get(nv))]
    }
    for (nv in nv2) {
      x[, (nv) := NA]
      x[, (nv) := as.numeric(get(nv))]
    }  
    
    
    c0 <- codes[gencode == (gc1-1)]$related
    cnot0 <- codes[(gencode >= gc1 | is.na(gencode)) & famcode==1]$related
    
    for (irc  in 1:maxid) {
      x[get(sprintf('related_%s', irc)) %in% cnot0, (sprintf('related_%s', irc)) := 4000]
    }
    
    ###########################################
    #x <- x[year == 2008 & serial == 6000]
    ###########################################
    
    for (irc  in 1:maxid) {
      
      print(sprintf('household member %s of %s', irc, maxid))
      
      #############################################################          
      #############################################################
      # relationship-code based assignment
      #############################################################
      #############################################################          
      
      ##################################
      # first old - vars still NA
      ##################################
      
      x[get(sprintf('related_%s', irc)) %in% c0 & is.na(a0) & !is.na(get(sprintf('age_%s', irc))),
        `:=`(n_a0 = 1,
             a0 = as.numeric(get(sprintf('age_%s', irc))),
             cpn_a = as.numeric(get(sprintf('pernum_%s', irc)))
        )
      ]
      
      x[get(sprintf('related_%s', irc)) %in% c0 & is.na(ec0) & !is.na(get(sprintf('eckid_%s', irc))),
        `:=`(n_ec0 = 1,
             ec0 = as.numeric(get(sprintf('eckid_%s', irc))),
             cpn_e = as.numeric(get(sprintf('pernum_%s', irc)))
        )
      ]       
      x[get(sprintf('related_%s', irc)) %in% c0 & is.na(ysc0) & !is.na(get(sprintf('ysc_%s', irc))),
        `:=`(n_ysc0 = 1,
             ysc0 = as.numeric(get(sprintf('ysc_%s', irc))),
             cpn_y = as.numeric(get(sprintf('pernum_%s', irc)))
        )
      ]            
      
      ##################################
      # 2nd+ old - vars no longer NA
      ##################################          
      
      x[get(sprintf('related_%s', irc)) %in% c0 & !is.na(a0) & !is.na(get(sprintf('age_%s', irc)))
        & (get(sprintf('pernum_%s', irc)) != cpn_a | is.na(cpn_a)),
        `:=`(n_a0 = n_a0 + 1,
             a0 = as.numeric(get(sprintf('age_%s', irc))) + a0
        )
      ]
      x[get(sprintf('related_%s', irc)) %in% c0 & !is.na(ec0) & !is.na(get(sprintf('eckid_%s', irc)))
        & (get(sprintf('pernum_%s', irc)) != cpn_e | is.na(cpn_e)),
        `:=`(n_ec0 = n_ec0 + 1,
             ec0 = as.numeric(get(sprintf('eckid_%s', irc))) + ec0
        )
      ]       
      x[get(sprintf('related_%s', irc)) %in% c0 & !is.na(ysc0) & !is.na(get(sprintf('ysc_%s', irc)))
        & (get(sprintf('pernum_%s', irc)) != cpn_y | is.na(cpn_y)),
        `:=`(n_ysc0 = n_ysc0 + 1,
             ysc0 = as.numeric(get(sprintf('ysc_%s', irc))) + ysc0
        )
      ]  
      
      x[, c('cpn_a', 'cpn_e', 'cpn_y'):=NULL]
      
      #############################################################    
      #############################################################
      # old via 15-40 years older rule - 
      # individual itself needs to be family member
      #############################################################
      #############################################################    
      
      
      ##################################
      # first old - vars still NA
      ##################################  
      
      x[get(sprintf('related_%s', irc)) %in% c(4000, 4900) & is.na(a0) & !is.na(get(sprintf('age_%s', irc)))
        & famcode == 1 & get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao,
        `:=`(n_a0 = 1,
             a0 = as.numeric(get(sprintf('age_%s', irc))),
             cpn_a = as.numeric(get(sprintf('pernum_%s', irc)))
        )
      ]
      x[get(sprintf('related_%s', irc)) %in% c(4000, 4900) & is.na(ec0) & !is.na(get(sprintf('eckid_%s', irc)))
        & famcode == 1 & get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao,
        `:=`(n_ec0 = 1,
             ec0 = as.numeric(get(sprintf('eckid_%s', irc))),
             cpn_e = as.numeric(get(sprintf('pernum_%s', irc)))
        )
      ]
      x[get(sprintf('related_%s', irc)) %in% c(4000, 4900) & is.na(ysc0) & !is.na(get(sprintf('ysc_%s', irc)))
        & famcode == 1 & get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao,
        `:=`(n_ysc0 = 1,
             ysc0 = as.numeric(get(sprintf('ysc_%s', irc))),
             cpn_y = as.numeric(get(sprintf('pernum_%s', irc)))
        )
      ]
      
      ##################################
      # 2nd+ old - vars no longer NA
      ##################################                  
      
      x[get(sprintf('related_%s', irc)) %in% c(4000, 4900) & !is.na(a0) & !is.na(get(sprintf('age_%s', irc)))
        & famcode == 1 & get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao
        & (get(sprintf('pernum_%s', irc)) != cpn_a | is.na(cpn_a)),
        `:=`(n_a0 = n_a0 + 1,
             a0 = as.numeric(get(sprintf('age_%s', irc))) + a0
        )
      ]
      x[get(sprintf('related_%s', irc)) %in% c(4000, 4900) & !is.na(ec0) & !is.na(get(sprintf('eckid_%s', irc)))
        & famcode == 1 & get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao
        & (get(sprintf('pernum_%s', irc)) != cpn_e | is.na(cpn_e)),
        `:=`(n_ec0 = n_ec0 + 1,
             ec0 = as.numeric(get(sprintf('eckid_%s', irc))) + ec0
        )
      ]
      x[get(sprintf('related_%s', irc)) %in% c(4000, 4900) & !is.na(ysc0) & !is.na(get(sprintf('ysc_%s', irc)))
        & famcode == 1 & get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao
        & (get(sprintf('pernum_%s', irc)) != cpn_y | is.na(cpn_y)),
        `:=`(n_ysc0 = n_ysc0 + 1,
             ysc0 = as.numeric(get(sprintf('ysc_%s', irc))) + ysc0
        )
      ]      
      
      x[, c('cpn_a', 'cpn_e', 'cpn_y'):=NULL]
      
      #############################################################          
      #############################################################
      # moms and pops - for old
      #############################################################
      #############################################################
      
      # here we add to education of the old education of moms and 
      # pops that we havent picked up yet
      
      # since we don't want to add to education of the old based
      # on momloc and poploc from individuals that we have already 
      # inlcuded above based on their (prev. gen) relationship code
      # or based on their age differential, we need to add
      # those conditions as qualifiers
      
      ##################################
      # first old - vars still NA
      ##################################            
      
      x[get(sprintf('pernum_%s', irc)) == momloc & is.na(a0) & !is.na(get(sprintf('age_%s', irc))) 
        & !(get(sprintf('related_%s', irc)) %in% c0)
        & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
            & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900)),
        `:=`(n_a0 = 1,
             a0 = as.numeric(get(sprintf('age_%s', irc))),
             cpn_am = as.numeric(get(sprintf('pernum_%s', irc)))
        )
      ]
      x[get(sprintf('pernum_%s', irc)) == poploc & is.na(a0) & !is.na(get(sprintf('age_%s', irc)))
        & !(get(sprintf('related_%s', irc)) %in% c0)
        & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
            & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900)),
        `:=`(n_a0 = 1,
             a0 = as.numeric(get(sprintf('age_%s', irc))),
             cpn_ap = as.numeric(get(sprintf('pernum_%s', irc)))
        )
      ]    
      
      x[get(sprintf('pernum_%s', irc)) == momloc & is.na(ec0) & !is.na(get(sprintf('eckid_%s', irc))) 
        & !(get(sprintf('related_%s', irc)) %in% c0)
        & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
            & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900)),
        `:=`(n_ec0 = 1,
             ec0 = as.numeric(get(sprintf('eckid_%s', irc))),
             cpn_em = as.numeric(get(sprintf('pernum_%s', irc)))
        )
      ]
      x[get(sprintf('pernum_%s', irc)) == poploc & is.na(ec0) & !is.na(get(sprintf('eckid_%s', irc)))
        & !(get(sprintf('related_%s', irc)) %in% c0)
        & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
            & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900)),
        `:=`(n_ec0 = 1,
             ec0 = as.numeric(get(sprintf('eckid_%s', irc))),
             cpn_ep = as.numeric(get(sprintf('pernum_%s', irc)))
        )
      ]   
      
      x[get(sprintf('pernum_%s', irc)) == momloc & is.na(ysc0) & !is.na(get(sprintf('ysc_%s', irc))) 
        & !(get(sprintf('related_%s', irc)) %in% c0)
        & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
            & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900)),
        `:=`(n_ysc0 = 1,
             ysc0 = as.numeric(get(sprintf('ysc_%s', irc))),
             cpn_ym = as.numeric(get(sprintf('pernum_%s', irc)))
        )
      ]
      x[get(sprintf('pernum_%s', irc)) == poploc & is.na(ysc0) & !is.na(get(sprintf('ysc_%s', irc)))
        & !(get(sprintf('related_%s', irc)) %in% c0)
        & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
            & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900)),
        `:=`(n_ysc0 = 1,
             ysc0 = as.numeric(get(sprintf('ysc_%s', irc))),
             cpn_yp = as.numeric(get(sprintf('pernum_%s', irc)))
        )
      ]           
      
      ##################################
      # 2nd+ old - vars no longer NA
      ##################################   
      
      x[get(sprintf('pernum_%s', irc)) == momloc & !is.na(a0) & !is.na(get(sprintf('age_%s', irc)))
        & !(get(sprintf('related_%s', irc)) %in% c0)
        & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
            & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900))
        & (get(sprintf('pernum_%s', irc)) != cpn_am | is.na(cpn_am)),
        `:=`(n_a0 = n_a0 + 1,
             a0 = as.numeric(get(sprintf('age_%s', irc))) + a0
        )
      ]
      x[get(sprintf('pernum_%s', irc)) == poploc & !is.na(a0) & !is.na(get(sprintf('age_%s', irc)))
        & !(get(sprintf('related_%s', irc)) %in% c0)
        & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
            & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900))
        & (get(sprintf('pernum_%s', irc)) != cpn_ap | is.na(cpn_ap)),
        `:=`(n_a0 = n_a0 + 1,
             a0 = as.numeric(get(sprintf('age_%s', irc))) + a0
        )
      ]  
      
      x[get(sprintf('pernum_%s', irc)) == momloc & !is.na(ec0) & !is.na(get(sprintf('eckid_%s', irc))) 
        & !(get(sprintf('related_%s', irc)) %in% c0)
        & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
            & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900))
        & (get(sprintf('pernum_%s', irc)) != cpn_em | is.na(cpn_em)),
        `:=`(n_ec0 = n_ec0 + 1,
             ec0 = as.numeric(get(sprintf('eckid_%s', irc))) + ec0
        )
      ]
      x[get(sprintf('pernum_%s', irc)) == poploc & !is.na(ec0) & !is.na(get(sprintf('eckid_%s', irc)))
        & !(get(sprintf('related_%s', irc)) %in% c0)
        & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
            & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900))
        & (get(sprintf('pernum_%s', irc)) != cpn_ep | is.na(cpn_ep)),
        `:=`(n_ec0 = n_ec0 + 1,
             ec0 = as.numeric(get(sprintf('eckid_%s', irc))) + ec0
        )
      ]   
      
      x[get(sprintf('pernum_%s', irc)) == momloc & !is.na(ysc0) & !is.na(get(sprintf('ysc_%s', irc))) 
        & !(get(sprintf('related_%s', irc)) %in% c0)
        & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
            & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900))
        & (get(sprintf('pernum_%s', irc)) != cpn_ym | is.na(cpn_ym)),
        `:=`(n_ysc0 = n_ysc0 + 1,
             ysc0 = as.numeric(get(sprintf('ysc_%s', irc))) + ysc0
        )
      ]
      x[get(sprintf('pernum_%s', irc)) == poploc & !is.na(ysc0) & !is.na(get(sprintf('ysc_%s', irc)))
        & !(get(sprintf('related_%s', irc)) %in% c0)
        & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
            & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900))
        & (get(sprintf('pernum_%s', irc)) != cpn_yp | is.na(cpn_yp)),
        `:=`(n_ysc0 = n_ysc0 + 1,
             ysc0 = as.numeric(get(sprintf('ysc_%s', irc))) + ysc0
        )
      ]     
      
      x[, c('cpn_am', 'cpn_em', 'cpn_ym', 'cpn_ap', 'cpn_ep', 'cpn_yp'):=NULL]
      
      #############################################################          
      #############################################################
      # moms and pops - for mom and pop education
      #############################################################
      #############################################################
      
      # here we don't have to worry about duplicating any old
      # education since we are purely basing assignment on momloc
      # and poploc
      
      # also, we are never adding things to existing mom and pop
      # variables - there will only be one mom or pop
      
      x[get(sprintf('pernum_%s', irc)) == momloc & is.na(amom) & !is.na(get(sprintf('age_%s', irc))),
        `:=`(n_amom = 1,
             amom = as.numeric(get(sprintf('age_%s', irc)))
        )
      ]
      x[get(sprintf('pernum_%s', irc)) == poploc & is.na(apop) & !is.na(get(sprintf('age_%s', irc))),
        `:=`(n_apop = 1,
             apop = as.numeric(get(sprintf('age_%s', irc)))
        )
      ]    
      
      x[get(sprintf('pernum_%s', irc)) == momloc & is.na(ecmom) & !is.na(get(sprintf('eckid_%s', irc))),
        `:=`(n_ecmom = 1,
             ecmom = as.numeric(get(sprintf('eckid_%s', irc)))
        )
      ]
      x[get(sprintf('pernum_%s', irc)) == poploc & is.na(ecpop) & !is.na(get(sprintf('eckid_%s', irc))),
        `:=`(n_ecpop = 1,
             ecpop = as.numeric(get(sprintf('eckid_%s', irc)))
        )
      ]   
      
      x[get(sprintf('pernum_%s', irc)) == momloc & is.na(yscmom) & !is.na(get(sprintf('ysc_%s', irc))),
        `:=`(n_yscmom = 1,
             yscmom = as.numeric(get(sprintf('ysc_%s', irc)))
        )
      ]
      x[get(sprintf('pernum_%s', irc)) == poploc & is.na(yscpop) & !is.na(get(sprintf('ysc_%s', irc))),
        `:=`(n_yscpop = 1,
             yscpop = as.numeric(get(sprintf('ysc_%s', irc)))
        )
      ]                 
    }
    
    x[, paste("related_", 1:maxid, sep=""):=NULL]
    x[, paste("age_", 1:maxid, sep=""):=NULL]
    x[, paste("eckid_", 1:maxid, sep=""):=NULL]
    x[, paste("ysc_", 1:maxid, sep=""):=NULL]
    x[, paste("pernum_", 1:maxid, sep=""):=NULL]
    
    # uncle edu (can be defined as residual)
    
    x[, aunc := a0]
    x[, ecunc := ec0]
    x[, yscunc := ysc0]
    x[, n_aunc := n_a0]
    x[, n_ecunc := n_ec0]
    x[, n_yscunc := n_ysc0]
    
    x[!is.na(amom), aunc := aunc - amom]
    x[!is.na(amom), n_aunc := n_aunc - 1]
    x[!is.na(apop), aunc := aunc - apop]
    x[!is.na(apop), n_aunc := n_aunc - 1]   
    
    x[!is.na(ecmom), ecunc := ecunc - ecmom]
    x[!is.na(ecmom), n_ecunc := n_ecunc - 1]
    x[!is.na(ecpop), ecunc := ecunc - ecpop]
    x[!is.na(ecpop), n_ecunc := n_ecunc - 1]   
    
    x[!is.na(yscmom), yscunc := yscunc - yscmom]
    x[!is.na(yscmom), n_yscunc := n_yscunc - 1]
    x[!is.na(yscpop), yscunc := yscunc - yscpop]
    x[!is.na(yscpop), n_yscunc := n_yscunc - 1]       
    
    
    # taking the averages
    
    # x[, a0 := floor((a0 / n_a0 ) + 0.5)]
    # # x[, amom := floor((amom / n_amom ) + 0.5)]
    # # x[, apop := floor((apop / n_apop ) + 0.5)]
    # x[, aunc := floor((aunc / n_aunc ) + 0.5)]
    # 
    # x[, ec0 := floor((ec0 / n_ec0 ) + 0.5)]
    # # x[, ecmom := floor((ecmom / n_ecmom ) + 0.5)]
    # # x[, ecpop := floor((ecpop / n_ecpop ) + 0.5)]
    # x[, ecunc := floor((ecunc / n_ecunc ) + 0.5)]
    # 
    # x[, ysc0 :=floor(( ysc0 / n_ysc0 ) + 0.5)]
    # # x[, yscmom := floor((yscmom / n_yscmom ) + 0.5)]
    # # x[, yscpop := floor((yscpop / n_yscpop ) + 0.5)]
    # x[, yscunc := floor((yscunc / n_yscunc ) + 0.5)]
    
    
    x[, a0 := round(a0 / n_a0 )]
    x[, aunc := round(aunc / n_aunc)]
    
    x[, ec0 := round(ec0 / n_ec0)]
    x[, ecunc := round(ecunc / n_ecunc)]
    
    x[, ysc0 :=round(ysc0 / n_ysc0)]
    x[, yscunc := round(yscunc / n_yscunc)]
    
    
    x <- x[, c('year', 'serial', 'pernum',
               'a0', 'amom', 'apop', 'aunc',
               'ec0', 'ecmom', 'ecpop', 'ecunc',
               'ysc0', 'yscmom', 'yscpop', 'yscunc')]
    
    if (gc1 == 2) {
      X <- x
    } else {
      X <- rbind(X, x)
    }
    
  }
  
  dt <- merge(dt, X, by=c('year', 'serial', 'pernum'), all.x = TRUE)
  
  for (i in names(dt)) {
    dt[is.nan(get(i)), (i):=NA]
    dt[is.infinite(get(i)), (i):=NA]
  }
  
  rm(a, b, x, X)
  
  return(dt)
  
}

##############################################


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
             "Unknown", "Missing Data")


relstrsabb <- c('Head', 'Sps/Prtnr', 'Spouse', 'Child',
                'BioCh', 'AdopCh', 'StepCh', 'Othrel',
                'Gdchild', 'Gch/Ggch', 'Prnt/PIL', 'Parent', 'PinL', 
                'CinL', 'Sibl/SIL', 'Sibl', 'SinL', 'Grandp', 'P/Gp/Asc', 
                'Aunt/Unc', 'Nep/Nie', 'Cousin', 'OR nec', 'Nonrel', 'Fnd/Gue/Vis',
                'Visit', 'Empl', 'Domemp', 'FosCh', 'qrpq', 'NR nec', 'OR/NR', 'unknwn', 'miss')

relcodes <- c(1000, 2000, 2100, 3000, 3100, 3200, 3300, 4000,
              4100, 4110, 4200, 4210, 4220, 4300, 4400,
              4410, 4430, 4500, 4600, 4700, 4810, 4820,
              4900, 5000, 5100, 5120, 5200, 5210, 5330,
              5600, 5900, 6000, 9999, NA)

gencode <- c(0, 0, 0, 1, 1, 1, 1, NA, 2, 2, -1, -1, -1, 1, 0, 0 , 0, -2, -1, -1 , 1, 0 , NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, NA)

famcode <- c(1, 1, 1, 1,
             1, 1, 1, 
             1, 1, 1,
             1, 1, 1, 
             1, 1, 
             1, 1, 1,
             1, 1,
             1, 1, 1,
             0, 0, 0,
             0, 0, 1,
             0, 0, 0,
             0, 0)

codes <- data.table(cbind(relstrs, relstrsabb, relcodes, gencode, famcode))
codes[, relcodes := as.integer(relcodes)]
codes[, gencode := as.integer(gencode)]
codes[, famcode := as.integer(famcode)]

names(codes) <- c('relstr', 'relstrabb', 'related', 'gencode', 'famcode')


minao_plus <- 15
maxao_plus <- 40

# depending on available RAM, add / remove countries from this list
# "large" countries are treated in chunks, year-by-year
large_isos <- c('EGY', 'ETH', 'ZAF', 'GHA', 'KEN', 'MAR', 'MWI', 'TZA', 'UGA')
large_isos_yrs <- list()
for (isoc in large_isos) {
  large_isos_yrs[[isoc]] <- c(-9999)
}

iiso <- 1

while (iiso < 29) {
  
  # iiso <- 5
  
  setwd(inpath)
  isoc <- ISO[iiso]
  
  dt <- data.table(haven::read_dta(sprintf("%s_edu.dta", isoc)))
  dt[] <- lapply(dt, function(x) { attributes(x) <- NULL; x })
  if (isoc == "RWA") {
    keepcols <- c("year", "serial", "pernum", "age", "edattain", "edattaind", "yrschool", "rw1991a_edattan", "related", "momloc", "poploc")
  } else if (isoc == "EGY" | isoc == "MOZ" | isoc == "SDN" | isoc == "SSD") {
    keepcols <- c("year", "serial", "pernum", "age", "edattain", "edattaind", "related", "momloc", "poploc")
  } else {
    keepcols <- c("year", "serial", "pernum", "age", "edattain", "edattaind", "yrschool", "related", "momloc", "poploc")
  }
  
  dt <- dt[,keepcols, with=FALSE]
  
  untreated_data <- FALSE
  
  if (isoc %in% large_isos) {
    
    census_yrs <- unique(dt$year)
    
    for (census_yr in census_yrs) {
      if ((census_yr %in% large_isos_yrs[[isoc]]) == FALSE) {
        large_isos_yrs[[isoc]]  <- c(large_isos_yrs[[isoc]], census_yr)
        isoc_out <- paste0(isoc, census_yr)
        dt <- dt[year == census_yr]
        untreated_data <- TRUE
        break
      }
    }
    
  } else {
    isoc_out <- isoc
    untreated_data <- TRUE
  }
  
  if (untreated_data) {
    dt <- treat_data(dt, isoc)
    setwd(outpath)
    saveRDS(dt, sprintf('aey_old_mompop_unc_%s.rds', isoc_out))
  }
  
  iiso <- iiso + 1

  if (untreated_data & (isoc != isoc_out)) {
    iiso <- iiso - 1
  }
  
}
  
  
  
  


