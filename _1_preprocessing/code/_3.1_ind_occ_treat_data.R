treat_data <- function(dt) {

  dt[, empind := NA]
  dt[, empind := as.integer(empind)]
  
  dt[, empocc := NA]
  dt[, empocc := as.integer(empocc)]
  
  dt[age > 150, age := NA]
  
  
  dt[indgen == 10, empind := 1]
  dt[indgen == 20, empind := 2]
  dt[indgen == 30, empind := 3]
  dt[indgen == 40, empind := 4]
  dt[indgen == 50, empind := 5]
  dt[indgen == 60, empind := 6]
  dt[indgen == 70 |
       indgen == 80 |
       indgen == 90 |
       indgen == 100 |
       indgen == 110 |
       indgen == 111 |
       indgen == 112 |
       indgen == 113 |
       indgen == 114 |
       indgen == 120 , empind := 6]
  
  dt[occisco < 11, empocc := occisco]
  
  print("indgen")
  print(table(dt[is.na(empind), indgen]))
  
  print("occisco")
  print(table(dt[is.na(empocc), occisco]))
  
  dt[, c('indgen', 'occisco') := NULL]
  
  
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
  
  a <- dt[, c('year', 'serial', 'pernum', 'age', 'empind', 'empocc', 'gencode', 'famcode', 'related', 'momloc', 'poploc')]
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
  
  b <- dcast(b, year + serial ~ rcid, value.var = c("age", "related", "empind", "empocc", "pernum"))
  
  a <- merge(a, b, by = c('year', 'serial'), all.x=TRUE)
  
  a[, minao := age+minao_plus]
  a[, maxao := age+maxao_plus]
  
  
  #  for (iocc in 1:10) {
  
  #    a[, (sprintf('occ0_%s', iocc)) := NA]
  #    a[, (sprintf('occ0_%s', iocc)) := as.integer(get(sprintf('occ0_%s', iocc)))]
  
  #  }
  
  #  for (iind in 1:6) {
  
  # x[, (sprintf('ind0_%s', iind)) := NA]
  # x[, (sprintf('ind0_%s', iind)) := as.integer(get(sprintf('ind0_%s', iind)))]
  
  #  }
  
  
  for (gc1 in c(2, 1, 0, -1, -2, -9999)) {
    
    ########
    # gc1 <- 1
    ########
    
    print(sprintf('Assigning old industry, generation %s', gc1))
    
    if (gc1 > -9999) {
      x <- a[gencode == gc1]
    } else {
      x <- a[is.na(gencode)]
    }
    
    c0 <- codes[gencode == (gc1-1)]$related
    cnot0 <- codes[(gencode >= gc1 | is.na(gencode)) & famcode==1]$related
    
    for (irc  in 1:maxid) {
      x[get(sprintf('related_%s', irc)) %in% cnot0, (sprintf('related_%s', irc)) := 4000]
    }
    
    ###########################################
    #x <- x[year == 2008 & serial == 6000]
    ###########################################
    
    
    ##############################################################################
    ##############################################################################
    ##############################################################################
    ##############################################################################
    # INDUSTRY
    ##############################################################################
    ##############################################################################
    ##############################################################################
    ##############################################################################
    
    for (iind in 1:6) {
      
      x[, x0 := NA]
      x[, x0 := as.integer(x0)]
      
      for (irc  in 1:maxid) {
        
        print(sprintf('industry code %s of 6, household member %s of %s', iind, irc, maxid))
        
        #############################################################          
        #############################################################
        # relationship-code based assignment
        #############################################################
        #############################################################          
        
        ##################################
        # first old - vars still NA
        ##################################
        
        x[get(sprintf('related_%s', irc)) %in% c0 & is.na(x0) & get(sprintf('empind_%s', irc)) == iind,
          `:=`(x0 = 1,
               cpn = as.numeric(get(sprintf('pernum_%s', irc)))
          )
        ]   
        
        ##################################
        # 2nd+ old - vars no longer NA
        ##################################          
        
        x[get(sprintf('related_%s', irc)) %in% c0 & !is.na(x0) & get(sprintf('empind_%s', irc)) == iind
          & (get(sprintf('pernum_%s', irc)) != cpn | is.na(cpn)),
          `:=`(x0 = as.integer(x0 + 1)
          )
        ]     
        
        x[, c('cpn'):=NULL]
        
        #############################################################    
        #############################################################
        # old via 15-40 years older rule - 
        # individual itself needs to be family member
        #############################################################
        #############################################################    
        
        ##################################
        # first old - vars still NA
        ##################################  
        
        x[get(sprintf('related_%s', irc)) %in% c(4000, 4900) & is.na(x0) & get(sprintf('empind_%s', irc)) == iind
          & famcode == 1 & get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao,
          `:=`(x0 = 1,
               cpn = as.numeric(get(sprintf('pernum_%s', irc)))
          )
        ]
        
        ##################################
        # 2nd+ old - vars no longer NA
        ##################################                  
        
        x[get(sprintf('related_%s', irc)) %in% c(4000, 4900) & !is.na(x0) & get(sprintf('empind_%s', irc)) == iind
          & famcode == 1 & get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao
          & (get(sprintf('pernum_%s', irc)) != cpn | is.na(cpn)),
          `:=`(x0 = as.integer(x0 + 1)
          )
        ]
        
        x[, c('cpn'):=NULL]
        
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
        
        x[get(sprintf('pernum_%s', irc)) == momloc & is.na(x0) & get(sprintf('empind_%s', irc)) == iind
          & !(get(sprintf('related_%s', irc)) %in% c0)
          & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
              & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900)),
          `:=`(x0 = 1,
               cpn_m = as.numeric(get(sprintf('pernum_%s', irc)))
          )
        ]
        x[get(sprintf('pernum_%s', irc)) == poploc & is.na(x0) & get(sprintf('empind_%s', irc)) == iind
          & !(get(sprintf('related_%s', irc)) %in% c0)
          & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
              & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900)),
          `:=`(x0 = 1,
               cpn_p = as.numeric(get(sprintf('pernum_%s', irc)))
          )
        ]    
        
        ##################################
        # 2nd+ old - vars no longer NA
        ##################################   
        
        x[get(sprintf('pernum_%s', irc)) == momloc & !is.na(x0) & get(sprintf('empind_%s', irc)) == iind
          & !(get(sprintf('related_%s', irc)) %in% c0)
          & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
              & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900))
          & (get(sprintf('pernum_%s', irc)) != cpn_m | is.na(cpn_m)),
          `:=`(x0 = as.integer(x0 + 1)
          )
        ]
        x[get(sprintf('pernum_%s', irc)) == poploc & !is.na(x0) & get(sprintf('empind_%s', irc)) == iind
          & !(get(sprintf('related_%s', irc)) %in% c0)
          & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
              & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900))
          & (get(sprintf('pernum_%s', irc)) != cpn_p | is.na(cpn_p)),
          `:=`(x0 = as.integer(x0 + 1)
          )
        ]  
        
        x[, c('cpn_m', 'cpn_p'):=NULL]
        
      }
      
      x[, (sprintf('ind0_%s', iind)) := as.integer(x0)]
      x[, x0:=NULL]
      
    }
    
    
    ##############################################################################
    ##############################################################################
    ##############################################################################
    ##############################################################################
    # OCCUPATION
    ##############################################################################
    ##############################################################################
    ##############################################################################
    ##############################################################################
    
    
    for (iocc in 1:10) {
      
      x[, x0 := NA]
      x[, x0 := as.integer(x0)]
      
      for (irc  in 1:maxid) {
        
        print(sprintf('occupation code %s of 10, household member %s of %s', iocc, irc, maxid))
        
        #############################################################
        #############################################################
        # relationship-code based assignment
        #############################################################
        #############################################################
        
        ##################################
        # first old - vars still NA
        ##################################
        
        x[get(sprintf('related_%s', irc)) %in% c0 & is.na(x0) & get(sprintf('empocc_%s', irc)) == iocc,
          `:=`(x0 = 1,
               cpn = as.numeric(get(sprintf('pernum_%s', irc)))
          )
        ]
        
        ##################################
        # 2nd+ old - vars no longer NA
        ##################################
        
        x[get(sprintf('related_%s', irc)) %in% c0 & !is.na(x0) & get(sprintf('empocc_%s', irc)) == iocc
          & (get(sprintf('pernum_%s', irc)) != cpn | is.na(cpn)),
          `:=`(x0 = as.integer(x0 + 1)
          )
        ]
        
        x[, c('cpn'):=NULL]
        
        #############################################################
        #############################################################
        # old via 15-40 years older rule -
        # individual itself needs to be family member
        #############################################################
        #############################################################
        
        ##################################
        # first old - vars still NA
        ##################################
        
        x[get(sprintf('related_%s', irc)) %in% c(4000, 4900) & is.na(x0) & get(sprintf('empocc_%s', irc)) == iocc
          & famcode == 1 & get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao,
          `:=`(x0 = 1,
               cpn = as.numeric(get(sprintf('pernum_%s', irc)))
          )
        ]
        
        ##################################
        # 2nd+ old - vars no longer NA
        ##################################
        
        x[get(sprintf('related_%s', irc)) %in% c(4000, 4900) & !is.na(x0) & get(sprintf('empocc_%s', irc)) == iocc
          & famcode == 1 & get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao
          & (get(sprintf('pernum_%s', irc)) != cpn | is.na(cpn)),
          `:=`(x0 = as.integer(x0 + 1)
          )
        ]
        
        x[, c('cpn'):=NULL]
        
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
        
        x[get(sprintf('pernum_%s', irc)) == momloc & is.na(x0) & get(sprintf('empocc_%s', irc)) == iocc
          & !(get(sprintf('related_%s', irc)) %in% c0)
          & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao
              & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900)),
          `:=`(x0 = 1,
               cpn_m = as.numeric(get(sprintf('pernum_%s', irc)))
          )
        ]
        x[get(sprintf('pernum_%s', irc)) == poploc & is.na(x0) & get(sprintf('empocc_%s', irc)) == iocc
          & !(get(sprintf('related_%s', irc)) %in% c0)
          & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao
              & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900)),
          `:=`(x0 = 1,
               cpn_p = as.numeric(get(sprintf('pernum_%s', irc)))
          )
        ]
        
        ##################################
        # 2nd+ old - vars no longer NA
        ##################################
        
        x[get(sprintf('pernum_%s', irc)) == momloc & !is.na(x0) & get(sprintf('empocc_%s', irc)) == iocc
          & !(get(sprintf('related_%s', irc)) %in% c0)
          & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao
              & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900))
          & (get(sprintf('pernum_%s', irc)) != cpn_m | is.na(cpn_m)),
          `:=`(x0 = as.integer(x0 + 1)
          )
        ]
        x[get(sprintf('pernum_%s', irc)) == poploc & !is.na(x0) & get(sprintf('empocc_%s', irc)) == iocc
          & !(get(sprintf('related_%s', irc)) %in% c0)
          & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao
              & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900))
          & (get(sprintf('pernum_%s', irc)) != cpn_p | is.na(cpn_p)),
          `:=`(x0 = as.integer(x0 + 1)
          )
        ]
        
        x[, c('cpn_m', 'cpn_p'):=NULL]
        
      }
      
      x[, (sprintf('occ0_%s', iocc)) := as.integer(x0)]
      x[, x0:=NULL]
      
    }
    
    x[, paste("related_", 1:maxid, sep=""):=NULL]
    x[, paste("age_", 1:maxid, sep=""):=NULL]
    x[, paste("empind_", 1:maxid, sep=""):=NULL]
    x[, paste("empocc_", 1:maxid, sep=""):=NULL]
    x[, paste("pernum_", 1:maxid, sep=""):=NULL]
    
    x <- x[, c('year', 'serial', 'pernum', 'empind', 'empocc',
               paste("ind0_", 1:6, sep=""),
               paste("occ0_", 1:10, sep=""))]
    
    if (gc1 == 2) {
      X <- x
    } else {
      X <- rbind(X, x)
    }
    
  }
  
  X[, ind0 := NA]
  X[, occ0 := NA]
  X[, ind0 := as.integer(ind0)]
  X[, occ0 := as.integer(occ0)]
  
  X[, max_ind0 := NA]
  X[, max_occ0 := NA]
  X[, max_ind0 := as.integer(max_ind0)]
  X[, max_occ0 := as.integer(max_occ0)]
  
  for (iind in 1:6) {
    X[is.na(ind0) & !is.na(get(sprintf('ind0_%s', iind))), `:=`(ind0 = iind, max_ind0 = get(sprintf('ind0_%s', iind)))]
    X[!is.na(ind0) & get(sprintf('ind0_%s', iind)) >= max_ind0, `:=`(ind0 = iind, max_ind0 = get(sprintf('ind0_%s', iind)))]
  }
  
  for (iocc in 1:10) {
    X[is.na(occ0) & !is.na(get(sprintf('occ0_%s', iocc))), `:=`(occ0 = iocc, max_occ0 = get(sprintf('occ0_%s', iocc)))]
    X[!is.na(occ0) & get(sprintf('occ0_%s', iocc)) > max_occ0, `:=`(occ0 = iocc, max_occ0 = get(sprintf('occ0_%s', iocc)))]
    if (iocc <= 6) {
      X[!is.na(occ0) & get(sprintf('occ0_%s', iocc)) >= max_occ0, `:=`(occ0 = iocc, max_occ0 = get(sprintf('occ0_%s', iocc)))]
    }
  }
  
  X[, paste("ind0_", 1:6, sep=""):=NULL]
  X[, paste("occ0_", 1:10, sep=""):=NULL]
  X[, c('max_ind0', 'max_occ0'):=NULL]
  
  return(X)
  
}