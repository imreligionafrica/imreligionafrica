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

codepath <- file.path(rootdir, "_1_preprocessing", "code")

inpath <- file.path(rootdir, "_1_preprocessing", "data","raw","other")

edupath <- file.path(rootdir, "_1_preprocessing", "data","raw","edu")

outpath <- file.path(rootdir, "_1_preprocessing", "data","standardized","ind_occ")

dir.create(outpath, showWarnings = FALSE, recursive = TRUE)

setwd(codepath)

source("_3.1_ind_occ_treat_data.R")

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
  
  # iiso <- 3
  
  isoc <- ISO[iiso]
  
  print(sprintf("%s", isoc))
  
  setwd(inpath)
  dt <- data.table(haven::read_dta(sprintf('%s_other.dta', isoc)))
  if (('indgen' %in% names(dt)) == FALSE) {
    dt[, indgen := -9999]
  } 
  if (('occisco' %in% names(dt)) == FALSE) {
    dt[, occisco := -9999]
  }
  dt <- dt[, .(year, serial, pernum, occisco, indgen)]
  
  setwd(edupath)
  ed <- data.table(haven::read_dta(sprintf('%s_edu.dta', isoc)))
  ed <- ed[, .(year, serial, pernum, age, related, momloc, poploc)]
  
  dt[] <- lapply(dt, function(x) { attributes(x) <- NULL; x })
  ed[] <- lapply(ed, function(x) { attributes(x) <- NULL; x })
  dt <- merge(dt, ed, by=c('year', 'serial', 'pernum'))
  rm(ed)
  
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
    X <- treat_data(dt)
    setwd(outpath)
    saveRDS(X, sprintf('new_ind_occ_%s.rds', isoc_out))
  }
  
  iiso <- iiso + 1
  
  if (untreated_data & (isoc != isoc_out)) {
    iiso <- iiso - 1
  }
  

}