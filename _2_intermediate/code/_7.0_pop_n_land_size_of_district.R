rm(list=ls()) # this clears all variables from the work-space
gc()
cat("\014")   # this clears the console

library(data.table)
library(countrycode)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
indir_pwt <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "perwt")
indir_dis <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts")
outdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE) 

cpaths <- c("Benin","Botswana", "Burkina Faso","Cameroon","Egypt","Ethiopia",
            "Ghana","Guinea","Liberia","Malawi","Mali","Mauritius","Mozambique",
            "Nigeria","Rwanda","Senegal","Sierra Leone","Togo","Uganda","South Africa","Zambia")

ISO_n <- c(204, 72, 854, 120, 818,231, 288, 324,
           430, 454,466, 480, 508,566, 646, 686, 
           694,768, 800, 710, 894)

ISO <- c("BEN", "BWA", "BFA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MWI", 
         "MLI", "MUS", "MOZ","NGA", "RWA", 
         "SEN", "SLE", "TGO", "UGA",
         "ZAF", "ZMB")

master<-data.table()

#Loop over countries
print('Loop over countries')

for (iiso in 1:length(ISO)) {
#for (iiso in 1:2) { #run example only for benin (i.e. iiso=1)
  
  isoc <- ISO[iiso]
  cntry <- cpaths[iiso]
  print(paste0("Working on ",cntry))
  
  #Open the ISO country of the IPUMS dataset 
  print('Open the ISO country of the IPUMS dataset')
  
  setwd(indir_edu)
  edu <- data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', isoc)))[,c("year","serial","pernum","age")]

  setwd(indir_pwt)
  perwt <- data.table(readRDS(sprintf('perwt_%s.rds', isoc)))[,c("year","serial","pernum","perwt")]
  
  setwd(indir_dis)
  dis <- data.table(readRDS(sprintf('districts_%s.rds', isoc)))[,c("year","serial","pernum","district")]
  
  dt <- merge(edu, perwt, by = c("year","serial","pernum"))
  
  dt <- merge(dt, dis, by = c("year","serial","pernum"))
  
  dt[,country:=isoc]
  
  dt<-dt[,c("country","year","serial", "pernum", "age","perwt","district")]
  
  #Create a column named decade of birth
  dt[,dob:=floor((year-age)/10)*10]

  #Calculate the number of districts for each country
  print("Calculate number of districts")
  dta <- unique(dt, by = c('country', 'district'))
  ndist <- dta[, .(ndist = .N), by = .(country)]
  pop<-dt[dob==1980, .(nin = .N,pop=sum(perwt)), by=.(country,district,dob,year)]
  sum_pop<-pop[,.(sum_pop=sum(pop)), by=.(country,district)]
  mean_pop<-sum_pop[,.(mean_pop=mean(sum_pop)), by=.(country)]
  med_pop<-sum_pop[,.(med_pop=median(sum_pop)), by=.(country)]
  std_pop<-sum_pop[,.(std_pop=sd(sum_pop)), by=.(country)]
    
  #Create output
  print('Create output')
  out <- merge(ndist, mean_pop, by = c('country'))
  out <- merge(out,med_pop, by = c('country'))
  out <- merge(out,std_pop, by = c('country'))
  
  #Append the collapsed data for this specific country-year to a master table with all country years
  print('Append all dts into master')
  
  if (isoc == 1) {
    master <- out
  } else {
    master <- rbind(master, out,fill=TRUE)   
  }
}  

#Turn character iso3 into full country name, i.e., BEN to Benin
master$country<-countrycode(master$country, "iso3c", "country.name")

#Create a final row called 'Total' that sums the values of certain columns
master<-rbind(master, data.frame(country = "Total", t(colSums(master[,c("ndist")]))), fill=TRUE)

#Add new column with row index
master$row_index <- 1:nrow(master)

#Set column order
print('Set column order')
setcolorder(master, c('row_index','country','ndist','mean_pop','med_pop','std_pop'))

setwd(outdir)

#Save master
print('Save master as a .csv')
fwrite(master, "pop_n_land_size_of_districts.csv")
