rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
path <- file.path(rootdir, "_1_preprocessing", "code")

new <- fread("related_codebook_new.csv")
new[, iso:=NULL]
new <- unique(new, by=c('related', 'related_str'))
new <- new[order(related)]


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

old <- data.table(cbind(relstrs, relstrsabb, relcodes, gencode, famcode))
old[, relcodes := as.integer(relcodes)]
old[, gencode := as.integer(gencode)]
old[, famcode := as.integer(famcode)]

names(old) <- c('relstr', 'relstrabb', 'related', 'gencode', 'famcode')

mrg <- merge(new, old, by='related')

for (i in 1:34) {
  
  if (mrg[i, 2] != tolower(mrg[i,3])) {
    print(i)
  }

}

View(mrg[c(1, 32)])