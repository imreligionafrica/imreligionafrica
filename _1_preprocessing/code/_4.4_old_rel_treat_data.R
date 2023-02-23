treat_data <- function(dt) {
  
  dt[, relcode := NA]
  dt[, relcode := as.integer(relcode)]
  
  dt[age > 150, age := NA]
  
  
  dt[major_religion == "Christian", relcode := 1]
  dt[major_religion == "Muslim", relcode := 2]
  dt[major_religion == "Traditional", relcode := 3]
  dt[major_religion == "Other", relcode := 4]
  dt[major_religion == "No Religion", relcode := 5]
  
  print("major_religion")
  print(table(dt[is.na(relcode), major_religion]))
  
  dt[, c('major_religion') := NULL]
  
  
  nms <- names(dt)
  dt <- merge(dt, codes[, c('related', 'gencode', 'famcode')], by='related', all.x = TRUE)
  setcolorder(dt, nms)
  
  #######
  # fix for ghana, 1984
  dt[is.na(momloc), momloc := 0]
  dt[is.na(poploc), poploc := 0]
  #######
  
  
  #########################################################################################
  # constructing old, mom, pop, religion
  #########################################################################################
  
  a <- dt[, c('year', 'serial', 'pernum', 'age', 'relcode', 'gencode', 'famcode', 'related', 'momloc', 'poploc')]
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
  
  b <- dcast(b, year + serial ~ rcid, value.var = c("age", "related", "relcode", "pernum"))
  
  a <- merge(a, b, by = c('year', 'serial'), all.x=TRUE)
  
  a[, minao := age+minao_plus]
  a[, maxao := age+maxao_plus]
  
  #  for (irel in 1:5) {
  
  # x[, (sprintf('rel0_%s', irel)) := NA]
  # x[, (sprintf('rel0_%s', irel)) := as.integer(get(sprintf('rel0_%s', irel)))]
  
  #  }
  
  
  for (gc1 in c(2, 1, 0, -1, -2, -9999)) {
    
    ########
    # gc1 <- 1
    ########
    
    print(sprintf('Assigning old religion, generation %s', gc1))
    
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
    
    x[, `:=`(relmom=NA, relpop=NA)]
    x[, `:=`(relmom = as.integer(relmom), relpop=as.integer(relpop))]
    
    for (irel in 1:5) {
      
      x[, x0 := NA]
      x[, x0 := as.integer(x0)]
      
      for (irc  in 1:maxid) {
        
        print(sprintf('religion code %s of 5, household member %s of %s', irel, irc, maxid))
        
        #############################################################          
        #############################################################
        # relationship-code based assignment
        #############################################################
        #############################################################          
        
        ##################################
        # first old - vars still NA
        ##################################
        
        x[get(sprintf('related_%s', irc)) %in% c0 & is.na(x0) & get(sprintf('relcode_%s', irc)) == irel,
          `:=`(x0 = 1,
               cpn = as.numeric(get(sprintf('pernum_%s', irc)))
          )
        ]   
        
        ##################################
        # 2nd+ old - vars no longer NA
        ##################################          
        
        x[get(sprintf('related_%s', irc)) %in% c0 & !is.na(x0) & get(sprintf('relcode_%s', irc)) == irel
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
        
        x[get(sprintf('related_%s', irc)) %in% c(4000, 4900) & is.na(x0) & get(sprintf('relcode_%s', irc)) == irel
          & famcode == 1 & get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao,
          `:=`(x0 = 1,
               cpn = as.numeric(get(sprintf('pernum_%s', irc)))
          )
        ]
        
        ##################################
        # 2nd+ old - vars no longer NA
        ##################################                  
        
        x[get(sprintf('related_%s', irc)) %in% c(4000, 4900) & !is.na(x0) & get(sprintf('relcode_%s', irc)) == irel
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
        
        # here we add to old religion the religion of moms and 
        # pops that we havent picked up yet
        
        # since we don't want to add the religion of the old based
        # on momloc and poploc from individuals that we have already 
        # inlcuded above based on their (prev. gen) relationship code
        # or based on their age differential, we need to add
        # those conditions as qualifiers
        
        ##################################
        # first old - vars still NA
        ##################################            
        
        x[get(sprintf('pernum_%s', irc)) == momloc & is.na(x0) & get(sprintf('relcode_%s', irc)) == irel
          & !(get(sprintf('related_%s', irc)) %in% c0)
          & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
              & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900)),
          `:=`(x0 = 1,
               cpn_m = as.numeric(get(sprintf('pernum_%s', irc)))
          )
        ]
        x[get(sprintf('pernum_%s', irc)) == poploc & is.na(x0) & get(sprintf('relcode_%s', irc)) == irel
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
        
        x[get(sprintf('pernum_%s', irc)) == momloc & !is.na(x0) & get(sprintf('relcode_%s', irc)) == irel
          & !(get(sprintf('related_%s', irc)) %in% c0)
          & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
              & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900))
          & (get(sprintf('pernum_%s', irc)) != cpn_m | is.na(cpn_m)),
          `:=`(x0 = as.integer(x0 + 1)
          )
        ]
        x[get(sprintf('pernum_%s', irc)) == poploc & !is.na(x0) & get(sprintf('relcode_%s', irc)) == irel
          & !(get(sprintf('related_%s', irc)) %in% c0)
          & !(get(sprintf('age_%s', irc)) >= minao & get(sprintf('age_%s', irc)) <= maxao 
              & famcode == 1 & get(sprintf('related_%s', irc)) %in% c(4000, 4900))
          & (get(sprintf('pernum_%s', irc)) != cpn_p | is.na(cpn_p)),
          `:=`(x0 = as.integer(x0 + 1)
          )
        ]  
        
        x[, c('cpn_m', 'cpn_p'):=NULL]
        
        
        #############################################################          
        #############################################################
        # moms and pops - for relmom and relpop
        #############################################################
        #############################################################
        
        x[get(sprintf('pernum_%s', irc)) == momloc & is.na(relmom) & !is.na(get(sprintf('relcode_%s', irc))),
          `:=`(relmom = as.numeric(get(sprintf('relcode_%s', irc))))
        ]
        x[get(sprintf('pernum_%s', irc)) == poploc & is.na(relpop) & !is.na(get(sprintf('relcode_%s', irc))),
          `:=`(relpop = as.numeric(get(sprintf('relcode_%s', irc))))
        ]
        
      }
      
      x[, (sprintf('rel0_%s', irel)) := as.integer(x0)]
      x[, x0:=NULL]
      
    }
    
    
    x[, paste("related_", 1:maxid, sep=""):=NULL]
    x[, paste("age_", 1:maxid, sep=""):=NULL]
    x[, paste("relcode_", 1:maxid, sep=""):=NULL]
    x[, paste("pernum_", 1:maxid, sep=""):=NULL]
    
    x <- x[, c('year', 'serial', 'pernum', 'relcode',
               paste("rel0_", 1:5, sep=""), 'relmom', 'relpop')]
    
    if (gc1 == 2) {
      X <- x
    } else {
      X <- rbind(X, x)
    }
    
  }
  
  X[, rel0 := NA]
  X[, rel0 := as.integer(rel0)]
  
  X[, max_rel0 := NA]
  X[, max_rel0 := as.integer(max_rel0)]
  
  for (irel in 1:5) {
    X[is.na(rel0) & !is.na(get(sprintf('rel0_%s', irel))), `:=`(rel0 = irel, max_rel0 = get(sprintf('rel0_%s', irel)))]
    X[!is.na(rel0) & get(sprintf('rel0_%s', irel)) >= max_rel0, `:=`(rel0 = irel, max_rel0 = get(sprintf('rel0_%s', irel)))]
  }
  
  X[, paste("rel0_", 1:5, sep=""):=NULL]
  X[, c('max_rel0'):=NULL]
  
  #############################################################################
  
  X[, major_religion := NA]
  X[, major_religion := as.character(major_religion)]
  
  X[relcode == 1, major_religion := "Christian",]
  X[relcode == 2, major_religion := "Muslim"]
  X[relcode == 3, major_religion := "Traditional"]
  X[relcode == 4, major_religion := "Other"]
  X[relcode == 5, major_religion := "No Religion"]
  
  X[, relcode:=NULL]
  
  #############################################################################
  
  X[, major_religion0 := NA]
  X[, major_religion0 := as.character(major_religion0)]
  
  X[rel0 == 1, major_religion0 := "Christian",]
  X[rel0 == 2, major_religion0 := "Muslim"]
  X[rel0 == 3, major_religion0 := "Traditional"]
  X[rel0 == 4, major_religion0 := "Other"]
  X[rel0 == 5, major_religion0 := "No Religion"]
  
  X[, rel0:=NULL]
  
  #############################################################################
  
  X[, major_religion_mom := NA]
  X[, major_religion_pop := NA]
  X[, major_religion_mom  := as.character(major_religion_mom)]
  X[, major_religion_pop  := as.character(major_religion_pop)]
  
  X[relmom == 1, major_religion_mom := "Christian"]
  X[relmom == 2, major_religion_mom := "Muslim"]
  X[relmom == 3, major_religion_mom := "Traditional"]
  X[relmom == 4, major_religion_mom := "Other"]
  X[relmom == 5, major_religion_mom := "No Religion"]
  
  X[relpop == 1, major_religion_pop := "Christian"]
  X[relpop == 2, major_religion_pop := "Muslim"]
  X[relpop == 3, major_religion_pop := "Traditional"]
  X[relpop == 4, major_religion_pop := "Other"]
  X[relpop == 5, major_religion_pop := "No Religion"]
  
  X[, relmom:=NULL]
  X[, relpop:=NULL]
  
  #############################################################################
  
  X <- X[order(year, serial, pernum)]
  # dt <- dt[order(year,serial,pernum)][, c('relcode', 'gencode', 'famcode'):=NULL]
  
  # BWA example
  # Xi <- X[year==2001&serial==27207000]
  # dti <- dt[year==2001&serial==27207000]
  # Yi <- merge(dti, Xi, by=c('year', 'serial', 'pernum'))
  
  return(X)
  
}