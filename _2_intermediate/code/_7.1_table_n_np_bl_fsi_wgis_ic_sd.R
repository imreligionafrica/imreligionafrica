library(data.table)
library(haven)
library(dplyr)
library(openxlsx)
library(rstudioapi) 

rm(list=ls()) # this clears all variables from the work-space
gc()
cat("\014")   # this clears the console

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir <- file.path(rootdir, "_1_preprocessing", "data", "external")
outdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

country <- c("Benin","Botswana", "Burkina Faso","Cameroon","Egypt","Ethiopia",
            "Ghana","Guinea","Liberia","Malawi","Mali","Mauritius","Mozambique",
            "Nigeria","Rwanda","Senegal","Sierra Leone","Togo","Uganda","South Africa","Zambia")

dt<-data.table(country)

ISO <- c("BEN", "BWA", "BFA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MWI", 
         "MLI", "MUS", "MOZ","NGA", "RWA", 
         "SEN", "SLE", "TGO", "UGA",
         "ZAF", "ZMB")

dt1<-data.table(ISO)

dt<-cbind(dt,dt1)

setwd(indir)

#Read Nunn slavery data
dt_nunn_slave_trade<-data.table(read_dta("Nunn_Slave_Trade_QJE.dta"))

#Rename column 
dt_nunn_slave_trade<-dplyr::rename(dt_nunn_slave_trade, ISO=isocode)

#Rename row value of certain column
dt_nunn_slave_trade$country<-gsub("Cape Verde Islands","Cape Verde",dt_nunn_slave_trade$country)

#Keep certain columns
dt_nunn_slave_trade<-dt_nunn_slave_trade[,c("ISO","country","ln_maddison_pcgdp2000","ln_export_area",
                                            "ln_export_pop","ln_coastline_area",
                                            "ln_avg_gold_pop","ln_avg_oil_pop",
                                            "ln_avg_all_diamonds_pop","ln_pop_dens_1400",
                                            "atlantic_distance_minimum","indian_distance_minimum",
                                            "saharan_distance_minimum","red_sea_distance_minimum",
                                            "ethnic_fractionalization","state_dev")]
#Read Nunn-Puga rugged data
dt_nunn_puga_ruggedness<-data.table(read_dta("rugged_data.dta"))

#Rename column 
dt_nunn_puga_ruggedness<-dplyr::rename(dt_nunn_puga_ruggedness, ISO=isocode)

#Rename row values of certain column
dt_nunn_puga_ruggedness$country<-gsub("Côte d'Ivoire", "Ivory Coast",dt_nunn_puga_ruggedness$country)
dt_nunn_puga_ruggedness$country<-gsub("Democratic Republic of the Congo", "Democratic Republic of Congo",dt_nunn_puga_ruggedness$country)
dt_nunn_puga_ruggedness$country<-gsub("Libyan Arab Jamahiriya", "Libya",dt_nunn_puga_ruggedness$country)
dt_nunn_puga_ruggedness$country<-gsub("United Republic of Tanzania", "Tanzania",dt_nunn_puga_ruggedness$country)

#Keep certain columns
dt_nunn_puga_ruggedness<-dt_nunn_puga_ruggedness[,c("ISO","country","rugged","rugged_popw", "rugged_slope",          
                                                    "rugged_lsd","rugged_pc", "soil","desert","tropical","dist_coast",
                                                    "near_coast","gemstones","rgdppc_2000","rgdppc_1950_m",
                                                    "rgdppc_1975_m","rgdppc_2000_m","rgdppc_1950_2000_m","q_rule_law","cont_africa",
                                                    "legor_gbr","legor_fra","legor_soc","legor_deu","legor_sca","colony_esp","colony_gbr",           
                                                    "colony_fra","colony_prt", "colony_oeu","africa_region_n","africa_region_s","africa_region_w",       
                                                    "africa_region_e","africa_region_c","slave_exports","dist_slavemkt_atlantic","dist_slavemkt_indian",   
                                                    "dist_slavemkt_saharan","dist_slavemkt_redsea","pop_1400","european_descent")]

#Merge Nunn slavery data to Nunn-Puga rugged data based on the sample ISO codes and country names of the former 
dt_n_np<-merge(dt_nunn_slave_trade,dt_nunn_puga_ruggedness,by=c("ISO","country"),all.x=TRUE)

#Keep rows of countries in Africa
#dt_n_np<-subset(dt_n_np,cont_africa==1)

#Create an identifier column for the countries in the Nature sample
dt_n_np<-dt_n_np %>% 
  group_by(country) %>% 
  mutate(nature_sample_identifier= ifelse(country=="Benin" | country=="Botswana" | country=="Burkina Faso" | country=="Cameroon" |
                                            country=="Egypt" | country=="Ethiopia" | country=="Ghana" | country=="Guinea" | country=="Liberia" | country=="Malawi" |
                                            country=="Mali" | country=="Mauritius" | country=="Mozambique" | country=="Nigeria" | country=="Rwanda" | country=="Senegal"|
                                            country=="Sierra Leone" | country=="Togo" | country=="Uganda" | country=="South Africa" | country=="Zambia", 1, 0))

#Read Barro-Lee education data
dt_barro_lee_education <- data.table(read_dta("Barro_Lee_education.dta"))

#Rename column
dt_barro_lee_education<-dplyr::rename(dt_barro_lee_education, ISO=WBcode)

#Rename row values of certain column
dt_barro_lee_education$country<-gsub("Cote dIvoire", "Ivory Coast",dt_barro_lee_education$country)
dt_barro_lee_education$country<-gsub("Democratic Republic of the Congo", "Democratic Republic of Congo",dt_barro_lee_education$country)
dt_barro_lee_education$country<-gsub("Libyan Arab Jamahiriya", "Libya",dt_barro_lee_education$country)
dt_barro_lee_education$country<-gsub("United Republic of Tanzania", "Tanzania",dt_barro_lee_education$country)

#Keep certain columns
dt_barro_lee_education<-dt_barro_lee_education[,c("country","year","lpc","yr_sch","ISO")]

#dt_nnp<-merge(dt_nnp,dt_barro_lee_education,by=c("ISO","country"),all.x=TRUE)

#Merge the already merged Nunn/Nunn-Puga dataset to the Barro-Lee education data based on the sample ISO codes and country names of the former 
dt_barro_lee_education<-merge(dt_n_np[,c("ISO","country","cont_africa")],dt_barro_lee_education,by=c("ISO","country"),all.x=TRUE)

#Keep rows for decades: 1950s, 1960s, 1970s and 1980s
dt_barro_lee_education<-subset(dt_barro_lee_education,year==1950 | year==1955 | year==1960 | year==1965 | 
                                 year==1970 | year==1975 | year==1980 | year==1985 | year==1990 | year==1995)

#Get unique values of the country names in the Barro-Lee education dataset
cntrs<-unique(c(dt_barro_lee_education$country))

#Generate tables for each decade
master_barro_lee_education_1950s<-data.table()
master_barro_lee_education_1960s<-data.table()
master_barro_lee_education_1970s<-data.table()
master_barro_lee_education_1980s<-data.table()
master_barro_lee_education_1990s<-data.table()

#Loop over the country names in the Barro-Lee education dataset
for (c in 1:length(cntrs)) {
  
  #Print name of country in the current iteration
  cntry <- cntrs[c]
  
  #Keep rows in the Barro-Lee education dataset where for country in the current iteration as well as for the years 1950 and 1955
  dt_barro_lee_education_1950s<-subset(dt_barro_lee_education,country==cntry & (year==1950 | year==1955))
  
  #Generate a table with the mean of the percentage of the population with completed primary education in the 1950s
  dt_barro_lee_education_1950s_mean_lpc<-as.data.table(mean(dt_barro_lee_education_1950s$lpc))
  
  #Generate a table with the mean of the years of schooling of the population in the 1950s
  dt_barro_lee_education_1950s_mean_yr_sch<-as.data.table(mean(dt_barro_lee_education_1950s$yr_sch))
  dt_barro_lee_education_1950s_means<-cbind(dt_barro_lee_education_1950s_mean_lpc,dt_barro_lee_education_1950s_mean_yr_sch)
  
  #Repeat the aforementioned process for the:
  
  #1960s
  dt_barro_lee_education_1960s<-subset(dt_barro_lee_education,country==cntry & (year==1960 | year==1965))
  dt_barro_lee_education_1960s_mean_lpc<-as.data.table(mean(dt_barro_lee_education_1960s$lpc))
  dt_barro_lee_education_1960s_mean_yr_sch<-as.data.table(mean(dt_barro_lee_education_1960s$yr_sch))
  dt_barro_lee_education_1960s_means<-cbind(dt_barro_lee_education_1960s_mean_lpc,dt_barro_lee_education_1960s_mean_yr_sch)
  
  #1970s
  dt_barro_lee_education_1970s<-subset(dt_barro_lee_education,country==cntry & (year==1970 | year==1975))
  dt_barro_lee_education_1970s_mean_lpc<-as.data.table(mean(dt_barro_lee_education_1970s$lpc))
  dt_barro_lee_education_1970s_mean_yr_sch<-as.data.table(mean(dt_barro_lee_education_1970s$yr_sch))
  dt_barro_lee_education_1970s_means<-cbind(dt_barro_lee_education_1970s_mean_lpc,dt_barro_lee_education_1970s_mean_yr_sch)
  
  #1980s
  dt_barro_lee_education_1980s<-subset(dt_barro_lee_education,country==cntry & (year==1980 | year==1985))
  dt_barro_lee_education_1980s_mean_lpc<-as.data.table(mean(dt_barro_lee_education_1980s$lpc))
  dt_barro_lee_education_1980s_mean_yr_sch<-as.data.table(mean(dt_barro_lee_education_1980s$yr_sch))
  dt_barro_lee_education_1980s_means<-cbind(dt_barro_lee_education_1980s_mean_lpc,dt_barro_lee_education_1980s_mean_yr_sch)
  
  #1990s
  dt_barro_lee_education_1990s<-subset(dt_barro_lee_education,country==cntry & (year==1990 | year==1995))
  dt_barro_lee_education_1990s_mean_lpc<-as.data.table(mean(dt_barro_lee_education_1990s$lpc))
  dt_barro_lee_education_1990s_mean_yr_sch<-as.data.table(mean(dt_barro_lee_education_1990s$yr_sch))
  dt_barro_lee_education_1990s_means<-cbind(dt_barro_lee_education_1990s_mean_lpc,dt_barro_lee_education_1990s_mean_yr_sch)
  
  #Append the rows for all the variables and for each country to a master table
  master_barro_lee_education_1950s<-rbind(master_barro_lee_education_1950s,dt_barro_lee_education_1950s_means)
  master_barro_lee_education_1960s<-rbind(master_barro_lee_education_1960s,dt_barro_lee_education_1960s_means)
  master_barro_lee_education_1970s<-rbind(master_barro_lee_education_1970s,dt_barro_lee_education_1970s_means)
  master_barro_lee_education_1980s<-rbind(master_barro_lee_education_1980s,dt_barro_lee_education_1980s_means)
  master_barro_lee_education_1990s<-rbind(master_barro_lee_education_1990s,dt_barro_lee_education_1990s_means)
}

#Generate table with the country names in Barro-Lee education dataset
country_names<-as.data.table(unique(dt_barro_lee_education$country))

#Append the tables created above
dt_barro_lee_education_means<-cbind(country_names,master_barro_lee_education_1950s,master_barro_lee_education_1960s,
                                    master_barro_lee_education_1970s,master_barro_lee_education_1980s,
                                    master_barro_lee_education_1990s)

#Rename the columns of the table created above
colnames(dt_barro_lee_education_means)<-c("country","mean_lpc_1950s","mean_yr_sch_1950s",
                                          "mean_lpc_1960s","mean_yr_sch_1960s",
                                          "mean_lpc_1970s","mean_yr_sch_1970s",
                                          "mean_lpc_1980s","mean_yr_sch_1980s",
                                          "mean_lpc_1990s","mean_yr_sch_1990s")

#Merge the already merged Nunn/Nunn-Puga dataset to generated table with the means (for each decade) 
#the Barro-Lee education dataset based on the sample ISO codes and country names of the former 
dt_n_np_bl<-merge(dt_n_np,dt_barro_lee_education_means,by=c("country"),all.x=TRUE)

#Read the Fragility of State Index datasets of the target years 
dt_fsi_2006<-data.table(read.xlsx("fsi_2006.xlsx"))
dt_fsi_2007<-data.table(read.xlsx("fsi_2007.xlsx"))
dt_fsi_2008<-data.table(read.xlsx("fsi_2008.xlsx"))
dt_fsi_2009<-data.table(read.xlsx("fsi_2009.xlsx"))

#Merge the FSI datasets to the merged Nunn/Nunn-Puga dataset based on the sample country names of the former 
dt_fsi<-merge(dt_n_np[,c("country")],dt_fsi_2006,by=c("country"),all.x=TRUE)
dt_fsi<-merge(dt_fsi,dt_fsi_2007,by=c("country"),all.x=TRUE)
dt_fsi<-merge(dt_fsi,dt_fsi_2008,by=c("country"),all.x=TRUE)
dt_fsi<-merge(dt_fsi,dt_fsi_2009,by=c("country"),all.x=TRUE)

#Create a column with the means of the FSI dataset for the target years
dt_fsi$mean_fsi <-apply(dt_fsi[,2:5],1,mean)

#Merge the merged Nunn/Nunn-Puga/Barro-Lee dataset to the generated FSI dataset based on the sample country names of the former 
dt_n_np_bl_fsi<-merge(dt_n_np_bl,dt_fsi[,c("country","mean_fsi")],by=c("country"))

#Read the World Bank World Governance Indices dataset
dt_wgis<-data.table(read.xlsx("WB_WGIs.xlsx",colNames = TRUE))

#Merge the merged Nunn/Nunn-Puga/Barro-Lee/FSI dataset to the WGIS dataset based on the sample country names of the former 
dt_n_np_bl_fsi_wgis<-merge(dt_n_np_bl_fsi,dt_wgis,by=c("country"),all.x=TRUE)

#Read the UCDP intrastate conflict dataset
dt_ic<-data.table(read.xlsx("UCDP_intrastate_conflict.xlsx",sheet=2,colNames = T))

#Merge the merged Nunn/Nunn-Puga/Barro-Lee/FSI/WGIS dataset to the IC dataset based on the sample country names of the former 
dt_n_np_bl_fsi_wgis_ic<-merge(dt_n_np_bl_fsi_wgis,dt_ic,by=c("country"),all.x=TRUE)

#Read Besley-Persson state capacity data
dt_sd<-data.table(read_dta("Besley_Persson_state_capacity.dta"))

#Rename column
dt_sd<-dplyr::rename(dt_sd, country=countryname)

#Merge the merged Nunn/Nunn-Puga/Barro-Lee/FSI/WGIS/IC dataset to the SC dataset based on the sample country names of the former 
dt_n_np_bl_fsi_wgis_ic_sd<-merge(dt_n_np_bl_fsi_wgis_ic,dt_sd[,c("country","incometaxrevenuegdp","mtaxrevenue","mincometaxrevenuegdp","mgadp")],by=c("country"),all.x=TRUE)

#Drop small countries in the sample such as Seychelles and Sao Tome & Principe
dt_n_np_bl_fsi_wgis_ic_sd<-subset(dt_n_np_bl_fsi_wgis_ic_sd,!(country=="Sao Tome & Principe" | country=="Seychelles"))

#Load Maddison data
dt_mad<-data.table(read.xlsx("mpd2020.xlsx",sheet="Full data"))

#Keep certain columns
dt_mad<-dt_mad[,c("ISO","country","year","gdppc")]

#Keep rows for year: 1995
dt_mad<-subset(dt_mad,year==1995)

#Rename column
dt_mad<-dplyr::rename(dt_mad, rgdppc_1995=gdppc)

#Merge the full data with the Maddison data
dt_n_np_bl_fsi_wgis_ic_sd<-merge(dt_n_np_bl_fsi_wgis_ic_sd,dt_mad[,c("ISO","rgdppc_1995")],by=c("ISO"),all.x=TRUE)

#Create columns with the natural logarithms of the 5 GDP variables
dt_n_np_bl_fsi_wgis_ic_sd$ln_rgdppc_2000<-log(dt_n_np_bl_fsi_wgis_ic_sd$rgdppc_2000)
dt_n_np_bl_fsi_wgis_ic_sd$ln_rgdppc_1950_m<-log(dt_n_np_bl_fsi_wgis_ic_sd$rgdppc_1950_m)
dt_n_np_bl_fsi_wgis_ic_sd$ln_rgdppc_1975_m<-log(dt_n_np_bl_fsi_wgis_ic_sd$rgdppc_1975_m)
dt_n_np_bl_fsi_wgis_ic_sd$ln_rgdppc_2000_m<-log(dt_n_np_bl_fsi_wgis_ic_sd$rgdppc_2000_m)
dt_n_np_bl_fsi_wgis_ic_sd$ln_rgdppc_1950_2000_m<-log(dt_n_np_bl_fsi_wgis_ic_sd$rgdppc_1950_2000_m)
dt_n_np_bl_fsi_wgis_ic_sd$ln_rgdppc_1995_m<-log(dt_n_np_bl_fsi_wgis_ic_sd$rgdppc_1995)

setwd(outdir)

#Save the produced dataset as a .csv
fwrite(dt_n_np_bl_fsi_wgis_ic_sd,"table_n_np_bl_fsi_wgis_ic_sd.csv")

