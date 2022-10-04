#!/usr/bin/env Rscript
# /* Copyright (C) 2018 Athanasios Natsis <natsisthanasis@gmail.com> */

####_ Set environment _####
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = c("data_preperation.R")

data_dir = "/home/athan/DATA/Broad_Band/"


library(data.table)

chp1files <- list.files(data_dir,
                        pattern = "LAP_CHP1_L1_[0-9]{4}.*.rds",
                        ignore.case = T,
                        full.names = T)

cm21files <- list.files(data_dir,
                        pattern = "LAP_CM21_H_L1_[0-9]{4}.*.rds",
                        ignore.case = T,
                        full.names = T)
## load all data
CM21_a <- data.table()
for (af in cm21files) {
    CM21_a <- rbind(CM21_a, readRDS(af))
}
setorder(CM21_a,Date30)

CHP1_a <- data.table()
for (af in chp1files) {
    CHP1_a <- rbind(CHP1_a, readRDS(af))
}
setorder(CHP1_a,Date30)

## filter out some data
CHP1_a <- CHP1_a[ Elevat  > 0  ]
CM21_a <- CM21_a[ Elevat  > 0  ]

CHP1_a <- CHP1_a[ wattHOR > 15 ]
CM21_a <- CM21_a[ wattGLB > 15 ]


## agregate by quarter
CHP1 <- CHP1_a[, .(Date30,wattHOR)]
CM21 <- CM21_a[, .(Date30,wattGLB)]

CHP1 <- CHP1[, .(wattHOR = mean(wattHOR, na.rm = T)),
             by = (Date = as.POSIXct((as.numeric(Date30) %/% (60 * 15)) * 60 * 15 , origin = "1970-01-01"))]

CM21 <- CM21[, .(wattGLB = mean(wattGLB, na.rm = T)),
             by = (Date = as.POSIXct((as.numeric(Date30) %/% (60 * 15)) * 60 * 15 , origin = "1970-01-01"))]


## agregate by hour
CHP1 <- CHP1[, .(wattHOR = mean(wattHOR)),
             by = (Date = as.POSIXct((as.numeric(Date) %/% (3600)) * 3600 , origin = "1970-01-01"))]

CM21 <- CM21[, .(wattGLB = mean(wattGLB)),
             by = (Date = as.POSIXct((as.numeric(Date) %/% (3600)) * 3600 , origin = "1970-01-01"))]

## agregate by hour
CHP1 <- CHP1[, .(wattHOR = mean(wattHOR, na.rm = T)),
             by = (Date = as.POSIXct((as.numeric(Date) %/% (24*3600)) * 24*3600 , origin = "1970-01-01"))]

CM21 <- CM21[, .(wattGLB = mean(wattGLB, na.rm = T)),
             by = (Date = as.POSIXct((as.numeric(Date) %/% (24*3600)) * 24*3600 , origin = "1970-01-01"))]




xrange <- c( max(min(CM21$Date), min(CHP1$Date)),
             max(max(CM21$Date), max(CHP1$Date)))



CM21 <- CM21[ Date >= xrange[1] & Date <= xrange[2]]
CHP1 <- CHP1[ Date >= xrange[1] & Date <= xrange[2]]

min(CM21$wattGLB)
min(CHP1$wattHOR,na.rm = T)



pdf("/home/athan/Aerosols/Presentation20191119/files/timeser.pdf",width = 10)
{
    par(mfrow = c(2,1))
    par(mar = c(2,4,1,1))

    par(cex = 1.5)
    plot( CM21$Date, CM21$wattGLB, "l", col= 'green', xlim = xrange, lwd = 2,
          ylab = "GHI  [Watt/m^2]", xlab = "" )
    title(main = "Daily mean of Global Iradiance on horizontal plane (L1)")

    plot( CHP1$Date, CHP1$wattHOR, "l", col= 'blue', xlim = xrange, lwd = 2,
          ylab = "BHI  [Watt/m^2]", xlab = "")
    title(main = "Daily mean of Direct Iradiance on horizontal plane (L1)")
}
dev.off()




tac = Sys.time(); difftime(tac,tic,units="mins")
cat(paste("\n  --  ",  Script.Name, " DONE  --  \n\n"))
cat(sprintf("%s H:%s U:%s S:%s T:%f\n\n",Sys.time(),Sys.info()["nodename"],Sys.info()["login"],Script.Name,difftime(tac,tic,units="mins")))
