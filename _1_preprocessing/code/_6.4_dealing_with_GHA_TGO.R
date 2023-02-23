rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)

library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

indir <- file.path(rootdir, "_1_preprocessing", "data", "raw", "georaw")

geocorrdir <- file.path(rootdir, "_1_preprocessing", "data", "raw", "geo_correspondences")

outdir_dists <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts")

dir.create(outdir_dists, showWarnings = FALSE, recursive = TRUE)

setwd(indir)

###############################################################
# GHANA
###############################################################

isoc <- "GHA"

georaw_old <- data.table(readRDS(sprintf("georaw_%s.rds", tolower(isoc))))
georaw_old <- georaw_old[, c('year', 'serial', 'pernum', 'localgh_x', 'geo2_gh2000_x', 'geo2_gh2010_x')]
georaw_old[, year := as.integer(as.character(year))]
georaw_old1984 <- georaw_old[year==1984, c('year', 'serial', 'pernum', 'localgh_x')]
georaw_old2000 <- georaw_old[year==2000, c('year', 'serial', 'pernum', 'geo2_gh2000_x')]
georaw_old2010 <- georaw_old[year==2010, c('year', 'serial', 'pernum', 'geo2_gh2010_x')]

georaw_new <- data.table(haven::read_dta('GHA_georaw.dta'))
georaw_new[] <- lapply(georaw_new, function(x) { attributes(x) <- NULL; x })
georaw_new <- georaw_new[, c('year', 'serial', 'pernum', 'localgh', 'geo2_gh2000', 'geo2_gh2010')]
georaw_new1984 <- georaw_new[year==1984, c('year', 'serial', 'pernum', 'localgh')]
georaw_new2000 <- georaw_new[year==2000, c('year', 'serial', 'pernum', 'geo2_gh2000')]
georaw_new2010 <- georaw_new[year==2010, c('year', 'serial', 'pernum', 'geo2_gh2010')]

georaw1984 <- merge(georaw_old1984, georaw_new1984, by=c('year', 'serial', 'pernum'))
georaw1984_mis <- georaw1984[localgh_x != localgh]
georaw2000 <- merge(georaw_old2000, georaw_new2000, by=c('year', 'serial', 'pernum'))
georaw2000_mis <- georaw2000[geo2_gh2000_x != geo2_gh2000]
georaw2010 <- merge(georaw_old2010, georaw_new2010, by=c('year', 'serial', 'pernum'))
georaw2010_mis <- georaw2010[geo2_gh2010_x != geo2_gh2010]

print(NROW(georaw1984_mis))
print(NROW(georaw2000_mis))
print(NROW(georaw2010_mis))

print(NROW(georaw1984))
print(NROW(georaw2000))
print(NROW(georaw2010))

rm(list=setdiff(ls(), c("indir", "georaw_new", "geocorrdir", "rootdir")))

conc_8400 <- data.table(haven::read_dta('conc_gha_84_00.dta'))
conc_8400[] <- lapply(conc_8400, function(x) { attributes(x) <- NULL; x })
conc_8400[, name:=NULL]
names(conc_8400) <- c('localgh', 'geo2_gh2000_x')

conc_1000 <- data.table(haven::read_dta('conc_gha_10_00.dta'))
conc_1000[] <- lapply(conc_1000, function(x) { attributes(x) <- NULL; x })
names(conc_1000) <- c('geo2_gh2000_y', 'geo2_gh2010')


georaw_new2 <- merge(georaw_new, conc_8400, by='localgh', all=FALSE)
print(NROW(georaw_new2))
georaw_new <- merge(georaw_new, conc_8400, by='localgh', all=TRUE)

georaw_new2 <- merge(georaw_new, conc_1000, by='geo2_gh2010', all=FALSE)
print(NROW(georaw_new2))
georaw_new <- merge(georaw_new, conc_1000, by='geo2_gh2010', all=TRUE)

rm(list=setdiff(ls(), c("indir", "georaw_new", "geocorrdir", "rootdir")))
georaw_new[year==1984, geo2_gh2000:=geo2_gh2000_x]
georaw_new[year==2010, geo2_gh2000:=geo2_gh2000_y]
georaw_new <- georaw_new[order(year, serial, pernum),
                         c('year', 'serial', 'pernum', 
                           'localgh', 'geo2_gh2000', 'geo2_gh2010')]

print(NROW(unique(georaw_new, by='geo2_gh2000')))
# this is exactly the number geo_low units of 'corr_GHA2000.dta'
setwd(geocorrdir)
dt <- data.table(haven::read_dta("corr_GHA2000.dta"))
dt[] <- lapply(dt, function(x) { attributes(x) <- NULL; x })
dt <- dt[, .(serial, pernum, geo_low)]
dt <- merge(dt, georaw_new[year==2000, .(serial, pernum, geo2_gh2000)])
dt <- unique(dt[, .(geo_low, geo2_gh2000)], by=c('geo_low', 'geo2_gh2000'))
names(dt) <- c('geo_low_2000', 'geo2_gh2000')

out <- merge(georaw_new, dt, by='geo2_gh2000')
out <- out[order(year, serial, pernum),
           c('year', 'serial', 'pernum', 
           'localgh', 'geo2_gh2000', 'geo2_gh2010',
           'geo_low_2000')]

setwd(indir)
saveRDS(out, "correspondence_GHA_allyears_geo_low2000.rds")

outpath <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts", "_gha_geo_low2000_to_geo2_gh2000.csv")

fwrite(dt, outpath)

###############################################################
# TOGO
###############################################################

rm(list=setdiff(ls(), c("indir", "geocorrdir", "rootdir")))
setwd(indir)
dists <- data.table(readRDS('districts_TGO.rds'))
georaw_new <- data.table(haven::read_dta('TGO_georaw.dta'))
georaw_new[] <- lapply(georaw_new, function(x) { attributes(x) <- NULL; x })
georaw_new <- georaw_new[, c("year", "serial", "pernum", "geo1_tg1960", 
                             "geo2_tg1970", "geo2_tg2010")]
dt <- merge(dists, georaw_new, by=c('year', 'serial', 'pernum'))
print(NROW(dt))
rm(dists, georaw_new)
conc60 <- unique(dt[year==1960, c('district', 'geo1_tg1960')], 
                 by=c('district', 'geo1_tg1960'))
conc70 <- unique(dt[year==1970, c('district', 'geo2_tg1970')], 
                 by=c('district', 'geo2_tg1970'))
conc10 <- unique(dt[year==2010, c('district', 'geo2_tg2010')], 
                 by=c('district', 'geo2_tg2010'))
conc <- merge(conc60, conc70, by='district', all=TRUE)
conc <- merge(conc, conc10, by='district', all=TRUE)
names(conc) <- c('district', 'geo1_tg1960_x', 'geo2_tg1970_x', 'geo2_tg2010_x')
dt <- merge(dt, conc, by='district', all=FALSE)
print(NROW(dt))
dt60 <- dt[year==1960, .(year, serial, pernum, geo1_tg1960, geo1_tg1960_x, geo2_tg2010, geo2_tg2010_x)]
dt70 <- dt[year==1970, .(year, serial, pernum, geo2_tg1970, geo2_tg1970_x, geo2_tg2010, geo2_tg2010_x)]
dt10 <- dt[year==2010, .(year, serial, pernum, geo2_tg2010, geo2_tg2010_x)]

NROW(dt60[geo1_tg1960!=geo1_tg1960_x])
NROW(dt70[geo2_tg1970!=geo2_tg1970_x])
NROW(dt10[geo2_tg2010!=geo2_tg2010_x])
dt60[is.na(geo2_tg2010), geo2_tg2010:=geo2_tg2010_x]
dt70[is.na(geo2_tg2010), geo2_tg2010:=geo2_tg2010_x]
dt60[, c('geo1_tg1960_x', 'geo2_tg2010_x'):=NULL]
dt70[, c('geo2_tg1970_x', 'geo2_tg2010_x'):=NULL]
dt10[, 'geo2_tg2010_x':=NULL]
dt60[, geo2_tg1970:=NA]
dt70[, geo1_tg1960:=NA]
dt10[, `:=`(geo1_tg1960=NA, geo2_tg1970=NA)]
dt60[, geo2_tg1970:=as.numeric(geo2_tg1970)]
dt70[, geo1_tg1960:=as.numeric(geo1_tg1960)]
dt10[, `:=`(geo1_tg1960=as.numeric(geo1_tg1960),
            geo2_tg1970=as.numeric(geo2_tg1970))]
dt60 <- dt60[, .(year, serial, pernum, geo1_tg1960, geo2_tg1970, geo2_tg2010)]
dt70 <- dt70[, .(year, serial, pernum, geo1_tg1960, geo2_tg1970, geo2_tg2010)]
dt10 <- dt10[, .(year, serial, pernum, geo1_tg1960, geo2_tg1970, geo2_tg2010)]
dt <- rbind(dt60, dt70, dt10)

setwd(geocorrdir)
new10 <- data.table(haven::read_dta("corr_TGO2010.dta"))
new10[] <- lapply(new10, function(x) { attributes(x) <- NULL; x })
new10[, pr_low_p:=NULL]

new10 <- merge(new10, dt[year==2010, .(serial, pernum, geo2_tg2010)], 
               by=c('serial', 'pernum'))
conc <- unique(new10[, .(geo_low, geo2_tg2010)], by=c('geo_low', 'geo2_tg2010'))
out <- merge(dt, conc, by='geo2_tg2010', all=FALSE)
names(out) <- c(names(out)[1:NCOL(out)-1], 'geo_low_2010')
out <- out[order(year, serial, pernum),
           c('year', 'serial', 'pernum', 
             'geo1_tg1960', 'geo2_tg1970', 'geo2_tg2010',
             'geo_low_2010')]


setwd(indir)
saveRDS(out, "correspondence_TGO_allyears_geo_low2010.rds")

dt <- unique(out[, .(geo_low_2010, geo2_tg2010)], by=c('geo_low_2010', 'geo2_tg2010'))

outpath <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts", "_tgo_geo_low2010_to_geo2_tg2010.csv")

fwrite(dt, outpath)


