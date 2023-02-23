treat_data <- function(dt, relstrs, relcodes, gencodes, famcodes, codes, minao_plus, maxao_plus) {
  
  dt[, gencode := NA]
  dt[, gencode := as.integer(gencode)]
  dt[, famcode := NA]
  dt[, famcode := as.integer(famcode)]  
  
  for (k in 1:length(relstrs)) {
    dt[related == relcodes[k], gencode := as.integer(gencodes[k])]
    dt[related == relcodes[k], famcode := as.integer(famcodes[k])]
  }
  # dt[,related:=NULL]
  
  dt[, nfam := sum(famcode), by=c('year', 'serial')]
  dt[nfam >= 2 & famcode == 1, withrel := 1]
  dt[is.na(withrel), withrel:=0]
  
  #######
  # fix for ghana, 1984
  dt[is.na(momloc), momloc := 0]
  dt[is.na(poploc), poploc := 0]
  #######
  
  dt[momloc > 0 | poploc > 0, withrel := 1]
  
  dt[, gcold := gencode-1]
  dt[gcold < -2, gcold := NA]
  
  
  #########################################################################################
  
  a <- dt[, c('year', 'serial', 'pernum', 'age', 'gencode', 'famcode', 'related')]
  b <- unique(a, by = c('year', 'serial', 'age', 'famcode', 'related'))
  b[, pernum:=NULL]
  b <- b[famcode==1]
  b[, c('famcode'):=NULL]
  # b <- unique(a, by = c('year', 'serial', 'related'))
  b[, rcid := seq_len(.N), by=.(year,serial)]
  
  maxid <- max(b[, rcid])
  
  # b[, age:=NULL]
  # b <- dcast(b, year + serial ~ rcid, value.var = "related")
  # names(b) <- c(names(b)[1:2], paste('related_', 1:(ncol(b)-2), sep=''))
  
  b <- dcast(b, year + serial ~ rcid, value.var = c("related", "age"))
  
  a <- merge(a, b, by = c('year', 'serial'), all.x=TRUE)
  
  a[, minao := age+minao_plus]
  a[, maxao := age+maxao_plus]
  
  
  ilist <- 1
  codeslist <- list()
  for (gc1 in c(2, 1, 0, -1, -2, -9999)) {
    
    print(sprintf('Assigning dummies, generation %s', gc1))
    
    if (gc1 > -9999) {
      x <- a[gencode == gc1]
    } else {
      x <- a[is.na(gencode) & famcode == 1]
    }
    # cnot0 <- codes[(gencodes!= (gc1-1) | is.na(gencodes)) & famcodes==1]$relcodes
    cnot0 <- codes[(gencodes >= gc1 | is.na(gencodes)) & famcodes==1]$relcodes
    
    for (ci in cnot0) {
      for (irc  in 1:maxid) {
        x[get(sprintf('related_%s', irc)) == ci, (sprintf('related_%s', irc)) := 4000]
      }
    }
    
    if (gc1 > -2) {
      
      c0 <- codes[gencodes == (gc1-1)]$relcodes
      
      for (ci in c0) {
        
        x[, (sprintf('c%s', ci)) := 0]
        x[, (sprintf('c%s', ci)) := as.integer(get(sprintf('c%s', ci)))]
        
      }
      
      for (ic0 in c0) {
        for (irc  in 1:maxid) {
          x[get(sprintf('related_%s', irc)) == ic0, (sprintf('c%s', ic0)) := as.integer(get(sprintf('c%s', ic0)) + 1)]
        }
      }      
    }
    
    
    for (ic0 in c(4000, 4900)) {
      
      x[, (sprintf('c%s', ic0)) := 0]
      x[, (sprintf('c%s', ic0)) := as.integer(get(sprintf('c%s', ic0)))]
      
      for (irc in 1:maxid) {
        x[get(sprintf('related_%s', irc)) == ic0
          & get(sprintf('age_%s', irc)) >= minao
          & get(sprintf('age_%s', irc)) <= maxao,
          (sprintf('c%s', ic0)) := as.integer(get(sprintf('c%s', ic0)) + 1)]
      }
    }
    
    x[, paste("related_", 1:maxid, sep=""):=NULL]  
    x[, paste("age_", 1:maxid, sep=""):=NULL] 
    x[, c('gencode', 'age', 'related', 'famcode', 'minao', 'maxao'):=NULL]
    codeslist[[ilist]] <- x
    ilist <- ilist+1
  }
  
  
  ######################################################
  # dealing with generation 2: grandkids
  ######################################################
  x <- codeslist[[1]]
  m <- merge(dt, x, by = c('year', 'serial', 'pernum'), all.x=TRUE)
  m <- m[gencode == 2]
  
  m <- m[, c('year', 'serial', 'pernum', 'momloc', 'poploc', 'related', 'c4000', 'c4900',
             'c3000', 'c3100', 'c3200', 'c3300', 'c4300', 'c4810', 'c5330')]
  
  m[, aux := c4000 + c4900 + c3000 + c3100 + c3200 + c3300 + c4300 + c4810 + c5330]  
  
  pgc <- codes[gencodes==1]  
  
  # a) Mom only:
  # - momloc observed
  # - poploc not observed
  # - sum of all possible previous generation variables == 1
  #   we do less than or equal one because of pathological cases where momloc is observed
  #   but momloc refers to an individual that has a label like 4000 or 4900 and which falls out of the
  #   the 15-40 years older age range
  m[, momonly := 0]
  #m[momloc != 0 & poploc == 0 & aux == 1, momonly := 1]
  m[momloc != 0 & poploc == 0 & aux <= 1, momonly := 1]
  
  # b) Pop only:
  # - momloc not observed
  # - poploc observed
  # - sum of all possible previous generation variables == 1
  #   we do less than or equal one because of pathological cases
  m[, poponly := 0]
  #m[momloc == 0 & poploc != 0 & aux == 1, poponly := 1]
  m[momloc == 0 & poploc != 0 & aux <= 1, poponly := 1]
  
  # c) Mom and Pop only:
  # - momloc observed
  # - poploc observed
  # - sum of all possible previous generation variables == 2
  #   we do less than or equal to two because of pathological cases
  m[, mompoponly := 0]
  #m[momloc != 0 & poploc != 0 & aux == 2, mompoponly := 1]
  m[momloc != 0 & poploc != 0 & aux <= 2, mompoponly := 1]
  
  # d) Mom + others
  # - momloc observed
  # - poploc not observed
  # - sum of all possible previous generation variables > 1
  m[, momothers := 0]
  m[momloc != 0 & poploc == 0 & aux > 1, momothers := 1]  
  
  # e) Pop + others
  # - momloc not observed
  # - poploc observed
  # - sum of all possible previous generation variables > 1
  m[, popothers := 0]
  m[momloc == 0 & poploc != 0 & aux > 1, popothers := 1]   
  
  # f) Mom + Pop + others 
  # - momloc observed
  # - poploc observed
  # - sum of all possible previous generation variables > 2
  m[, mompopothers := 0]
  m[momloc != 0 & poploc != 0 & aux > 2, mompopothers := 1]   
  
  # g) others only
  # - momloc not observed
  # - poploc not observed
  # - sum of all possible previous generation variables > 0 
  m[, othersonly := 0]
  m[momloc == 0 & poploc == 0 & aux > 0, othersonly := 1]     
  
  m <- m[, c('year', 'serial', 'pernum', 'related', 'momonly', 'poponly', 'mompoponly',
             'momothers', 'popothers', 'mompopothers', 'othersonly')]
  m[, unrecorded := 0]
  
  m[, check := momonly+poponly+mompoponly+momothers+popothers+mompopothers+othersonly+unrecorded]
  table(m[, check])
  M <- m
  
  ######################################################
  # dealing with generation 1: kids
  ######################################################
  x <- codeslist[[2]]
  m <- merge(dt, x, by = c('year', 'serial', 'pernum'), all.x=TRUE)
  m <- m[gencode == 1]
  
  m <- m[, c('year', 'serial', 'pernum', 'momloc', 'poploc', 'related', 'c4000', 'c4900',
             "c1000", "c2000", "c2100", "c4400", "c4410", "c4430", "c4820")]
  
  m[, aux := c4000 + c4900 + c1000 + c2000 + c2100 + c4400 + c4410 + c4430 + c4820]  
  
  pgc <- codes[gencodes==0]  
  
  # a) Mom only:
  # - momloc observed
  # - poploc not observed
  # - sum of all possible previous generation variables == 1
  #   we do less than or equal one because of pathological cases where momloc is observed
  #   but momloc refers to an individual that has a label like 4000 or 4900 and which falls out of the
  #   the 15-40 years older age range
  m[, momonly := 0]
  #m[momloc != 0 & poploc == 0 & aux == 1, momonly := 1]
  m[momloc != 0 & poploc == 0 & aux <= 1, momonly := 1]
  
  # b) Pop only:
  # - momloc not observed
  # - poploc observed
  # - sum of all possible previous generation variables == 1
  #   we do less than or equal one because of pathological cases
  m[, poponly := 0]
  #m[momloc == 0 & poploc != 0 & aux == 1, poponly := 1]
  m[momloc == 0 & poploc != 0 & aux <= 1, poponly := 1]
  
  # c) Mom and Pop only:
  # - momloc observed
  # - poploc observed
  # - sum of all possible previous generation variables == 2
  #   we do less than or equal to two because of pathological cases
  m[, mompoponly := 0]
  #m[momloc != 0 & poploc != 0 & aux == 2, mompoponly := 1]
  m[momloc != 0 & poploc != 0 & aux <= 2, mompoponly := 1]
  
  # d) Mom + others
  # - momloc observed
  # - poploc not observed
  # - sum of all possible previous generation variables > 1
  m[, momothers := 0]
  m[momloc != 0 & poploc == 0 & aux > 1, momothers := 1]  
  
  # e) Pop + others
  # - momloc not observed
  # - poploc observed
  # - sum of all possible previous generation variables > 1
  m[, popothers := 0]
  m[momloc == 0 & poploc != 0 & aux > 1, popothers := 1]   
  
  # f) Mom + Pop + others 
  # - momloc observed
  # - poploc observed
  # - sum of all possible previous generation variables > 2
  m[, mompopothers := 0]
  m[momloc != 0 & poploc != 0 & aux > 2, mompopothers := 1]   
  
  # g) others only
  # - momloc not observed
  # - poploc not observed
  # - sum of all possible previous generation variables > 0 
  m[, othersonly := 0]
  m[momloc == 0 & poploc == 0 & aux > 0, othersonly := 1]  
  
  m <- m[, c('year', 'related', 'serial', 'pernum', 'momonly', 'poponly', 'mompoponly',
             'momothers', 'popothers', 'mompopothers', 'othersonly')]
  m[, unrecorded := 0]
  m[momonly == 0 &
      poponly == 0 &
      mompoponly == 0 &
      momothers == 0 &
      popothers == 0 & 
      mompopothers == 0 &
      othersonly == 0,
    unrecorded := 1]
  
  m[, check := momonly+poponly+mompoponly+momothers+popothers+mompopothers+othersonly+unrecorded]
  table(m[, check])
  
  M <- rbind(M, m)
  
  ######################################################
  # dealing with generation 0: head's generation
  ######################################################
  x <- codeslist[[3]]
  m <- merge(dt, x, by = c('year', 'serial', 'pernum'), all.x=TRUE)
  m <- m[gencode == 0]
  
  m <- m[, c('year', 'serial', 'pernum', 'momloc', 'poploc', 'related', 'c4000', 'c4900',
             "c4200", "c4210", "c4220", "c4600", "c4700")]
  
  m[, aux := c4000 + c4900 + c4200 + c4210 + c4220 + c4600 + c4700]  
  
  pgc <- codes[gencodes==-1]  
  
  # a) Mom only:
  # - momloc observed
  # - poploc not observed
  # - sum of all possible previous generation variables == 1
  #   we do less than or equal one because of pathological cases where momloc is observed
  #   but momloc refers to an individual that has a label like 4000 or 4900 and which falls out of the
  #   the 15-40 years older age range
  m[, momonly := 0]
  #m[momloc != 0 & poploc == 0 & aux == 1, momonly := 1]
  m[momloc != 0 & poploc == 0 & aux <= 1, momonly := 1]
  
  # b) Pop only:
  # - momloc not observed
  # - poploc observed
  # - sum of all possible previous generation variables == 1
  #   we do less than or equal one because of pathological cases
  m[, poponly := 0]
  #m[momloc == 0 & poploc != 0 & aux == 1, poponly := 1]
  m[momloc == 0 & poploc != 0 & aux <= 1, poponly := 1]
  
  # c) Mom and Pop only:
  # - momloc observed
  # - poploc observed
  # - sum of all possible previous generation variables == 2
  #   we do less than or equal to two because of pathological cases
  m[, mompoponly := 0]
  #m[momloc != 0 & poploc != 0 & aux == 2, mompoponly := 1]
  m[momloc != 0 & poploc != 0 & aux <= 2, mompoponly := 1]
  
  # d) Mom + others
  # - momloc observed
  # - poploc not observed
  # - sum of all possible previous generation variables > 1
  m[, momothers := 0]
  m[momloc != 0 & poploc == 0 & aux > 1, momothers := 1]  
  
  # e) Pop + others
  # - momloc not observed
  # - poploc observed
  # - sum of all possible previous generation variables > 1
  m[, popothers := 0]
  m[momloc == 0 & poploc != 0 & aux > 1, popothers := 1]   
  
  # f) Mom + Pop + others 
  # - momloc observed
  # - poploc observed
  # - sum of all possible previous generation variables > 2
  m[, mompopothers := 0]
  m[momloc != 0 & poploc != 0 & aux > 2, mompopothers := 1]   
  
  # g) others only
  # - momloc not observed
  # - poploc not observed
  # - sum of all possible previous generation variables > 0 
  m[, othersonly := 0]
  m[momloc == 0 & poploc == 0 & aux > 0, othersonly := 1]  
  
  m <- m[, c('year', 'serial', 'pernum', 'related', 'momonly', 'poponly', 'mompoponly',
             'momothers', 'popothers', 'mompopothers', 'othersonly')]
  m[, unrecorded := 0]
  
  m[, check := momonly+poponly+mompoponly+momothers+popothers+mompopothers+othersonly+unrecorded]
  table(m[, check])
  
  M <- rbind(M, m)
  
  ######################################################
  # dealing with generation -1: head's parents generation
  ######################################################
  x <- codeslist[[4]]
  m <- merge(dt, x, by = c('year', 'serial', 'pernum'), all.x=TRUE)
  m <- m[gencode == -1]
  
  m <- m[, c('year', 'serial', 'pernum', 'momloc', 'poploc', 'related', 'c4000', 'c4900',
             "c4500")]
  
  m[, aux := c4000 + c4900 + c4500]  
  
  pgc <- codes[gencodes==-2]  
  
  # a) Mom only:
  # - momloc observed
  # - poploc not observed
  # - sum of all possible previous generation variables == 1
  #   we do less than or equal one because of pathological cases where momloc is observed
  #   but momloc refers to an individual that has a label like 4000 or 4900 and which falls out of the
  #   the 15-40 years older age range
  m[, momonly := 0]
  #m[momloc != 0 & poploc == 0 & aux == 1, momonly := 1]
  m[momloc != 0 & poploc == 0 & aux <= 1, momonly := 1]
  
  # b) Pop only:
  # - momloc not observed
  # - poploc observed
  # - sum of all possible previous generation variables == 1
  #   we do less than or equal one because of pathological cases
  m[, poponly := 0]
  #m[momloc == 0 & poploc != 0 & aux == 1, poponly := 1]
  m[momloc == 0 & poploc != 0 & aux <= 1, poponly := 1]
  
  # c) Mom and Pop only:
  # - momloc observed
  # - poploc observed
  # - sum of all possible previous generation variables == 2
  #   we do less than or equal to two because of pathological cases
  m[, mompoponly := 0]
  #m[momloc != 0 & poploc != 0 & aux == 2, mompoponly := 1]
  m[momloc != 0 & poploc != 0 & aux <= 2, mompoponly := 1]
  
  # d) Mom + others
  # - momloc observed
  # - poploc not observed
  # - sum of all possible previous generation variables > 1
  m[, momothers := 0]
  m[momloc != 0 & poploc == 0 & aux > 1, momothers := 1]  
  
  # e) Pop + others
  # - momloc not observed
  # - poploc observed
  # - sum of all possible previous generation variables > 1
  m[, popothers := 0]
  m[momloc == 0 & poploc != 0 & aux > 1, popothers := 1]   
  
  # f) Mom + Pop + others 
  # - momloc observed
  # - poploc observed
  # - sum of all possible previous generation variables > 2
  m[, mompopothers := 0]
  m[momloc != 0 & poploc != 0 & aux > 2, mompopothers := 1]   
  
  # g) others only
  # - momloc not observed
  # - poploc not observed
  # - sum of all possible previous generation variables > 0 
  m[, othersonly := 0]
  m[momloc == 0 & poploc == 0 & aux > 0, othersonly := 1]  
  
  m <- m[, c('year', 'serial', 'pernum', 'related', 'momonly', 'poponly', 'mompoponly',
             'momothers', 'popothers', 'mompopothers', 'othersonly')]
  m[, unrecorded := 0]
  
  m[, check := momonly+poponly+mompoponly+momothers+popothers+mompopothers+othersonly+unrecorded]
  table(m[, check])
  
  M <- rbind(M, m)
  
  ######################################################
  # dealing with generation -2: head's grandparents generation
  ######################################################
  x <- codeslist[[5]]
  m <- merge(dt, x, by = c('year', 'serial', 'pernum'), all.x=TRUE)
  m <- m[gencode == -2]
  
  m <- m[, c('year', 'serial', 'pernum', 'momloc', 'poploc', 'related', 'c4000', 'c4900')]
  
  m[, aux := c4000 + c4900]  
  
  # a) Mom only:
  # - momloc observed
  # - poploc not observed
  # - sum of all possible previous generation variables == 1
  #   we do less than or equal one because of pathological cases where momloc is observed
  #   but momloc refers to an individual that has a label like 4000 or 4900 and which falls out of the
  #   the 15-40 years older age range
  m[, momonly := 0]
  #m[momloc != 0 & poploc == 0 & aux == 1, momonly := 1]
  m[momloc != 0 & poploc == 0 & aux <= 1, momonly := 1]
  
  # b) Pop only:
  # - momloc not observed
  # - poploc observed
  # - sum of all possible previous generation variables == 1
  #   we do less than or equal one because of pathological cases
  m[, poponly := 0]
  #m[momloc == 0 & poploc != 0 & aux == 1, poponly := 1]
  m[momloc == 0 & poploc != 0 & aux <= 1, poponly := 1]
  
  # c) Mom and Pop only:
  # - momloc observed
  # - poploc observed
  # - sum of all possible previous generation variables == 2
  #   we do less than or equal to two because of pathological cases
  m[, mompoponly := 0]
  #m[momloc != 0 & poploc != 0 & aux == 2, mompoponly := 1]
  m[momloc != 0 & poploc != 0 & aux <= 2, mompoponly := 1]
  
  # d) Mom + others
  # - momloc observed
  # - poploc not observed
  # - sum of all possible previous generation variables > 1
  m[, momothers := 0]
  m[momloc != 0 & poploc == 0 & aux > 1, momothers := 1]  
  
  # e) Pop + others
  # - momloc not observed
  # - poploc observed
  # - sum of all possible previous generation variables > 1
  m[, popothers := 0]
  m[momloc == 0 & poploc != 0 & aux > 1, popothers := 1]   
  
  # f) Mom + Pop + others 
  # - momloc observed
  # - poploc observed
  # - sum of all possible previous generation variables > 2
  m[, mompopothers := 0]
  m[momloc != 0 & poploc != 0 & aux > 2, mompopothers := 1]   
  
  # g) others only
  # - momloc not observed
  # - poploc not observed
  # - sum of all possible previous generation variables > 0 
  m[, othersonly := 0]
  m[momloc == 0 & poploc == 0 & aux > 0, othersonly := 1]  
  
  m <- m[, c('year', 'serial', 'pernum', 'related', 'momonly', 'poponly', 'mompoponly',
             'momothers', 'popothers', 'mompopothers', 'othersonly')]
  m[, unrecorded := 0]
  
  m[, check := momonly+poponly+mompoponly+momothers+popothers+mompopothers+othersonly+unrecorded]
  table(m[, check])
  
  M <- rbind(M, m)
  
  ######################################################
  # dealing with the None generation
  ######################################################
  
  x <- codeslist[[6]]
  m <- merge(dt, x, by = c('year', 'serial', 'pernum'), all.x=TRUE)
  #m <- m[age >= 14 & age <= 18 & is.na(gencode) & famcode == 1]
  m <- m[is.na(gencode)]
  
  # m <- m[serial==4129000]
  
  m <- m[, c('year', 'serial', 'pernum', 'momloc', 'poploc', 'related', 'c4000', 'c4900')]
  
  m[, aux := c4000 + c4900]  
  
  # a) Mom only:
  # - momloc observed
  # - poploc not observed
  # - sum of all possible previous generation variables == 1
  #   we do less than or equal one because of pathological cases where momloc is observed
  #   but momloc refers to an individual that has a label like 4000 or 4900 and which falls out of the
  #   the 15-40 years older age range
  m[, momonly := 0]
  #m[momloc != 0 & poploc == 0 & aux == 1, momonly := 1]
  m[momloc != 0 & poploc == 0 & aux <= 1, momonly := 1]
  # special case
  m[momloc != 0 & poploc == 0 & is.na(aux), momonly := 1]
  
  # b) Pop only:
  # - momloc not observed
  # - poploc observed
  # - sum of all possible previous generation variables == 1
  #   we do less than or equal one because of pathological cases
  m[, poponly := 0]
  #m[momloc == 0 & poploc != 0 & aux == 1, poponly := 1]
  m[momloc == 0 & poploc != 0 & aux <= 1, poponly := 1]
  # special case
  m[momloc == 0 & poploc != 0 & is.na(aux), poponly := 1]
  
  # c) Mom and Pop only:
  # - momloc observed
  # - poploc observed
  # - sum of all possible previous generation variables == 2
  #   we do less than or equal to two because of pathological cases
  m[, mompoponly := 0]
  #m[momloc != 0 & poploc != 0 & aux == 2, mompoponly := 1]
  m[momloc != 0 & poploc != 0 & aux <= 2, mompoponly := 1]
  # special case
  m[momloc != 0 & poploc != 0 & is.na(aux), mompoponly := 1]
  
  # d) Mom + others
  # - momloc observed
  # - poploc not observed
  # - sum of all possible previous generation variables > 1
  m[, momothers := 0]
  m[momloc != 0 & poploc == 0 & aux > 1, momothers := 1]  
  
  # e) Pop + others
  # - momloc not observed
  # - poploc observed
  # - sum of all possible previous generation variables > 1
  m[, popothers := 0]
  m[momloc == 0 & poploc != 0 & aux > 1, popothers := 1]   
  
  # f) Mom + Pop + others 
  # - momloc observed
  # - poploc observed
  # - sum of all possible previous generation variables > 2
  m[, mompopothers := 0]
  m[momloc != 0 & poploc != 0 & aux > 2, mompopothers := 1]   
  
  # g) others only
  # - momloc not observed
  # - poploc not observed
  # - sum of all possible previous generation variables > 0 
  m[, othersonly := 0]
  m[momloc == 0 & poploc == 0 & aux > 0, othersonly := 1]  
  
  m <- m[, c('year', 'serial', 'pernum', 'related', 'momonly', 'poponly', 'mompoponly',
             'momothers', 'popothers', 'mompopothers', 'othersonly')]
  m[, unrecorded := 0]
  
  m[, check := momonly+poponly+mompoponly+momothers+popothers+mompopothers+othersonly+unrecorded]
  table(m[, check])
  
  M <- rbind(M, m)
  
  ###################################################################
  # outputting
  ###################################################################
  
  M[, check:=NULL]
  
  return(M)
  
}