#!/usr/bin/env Rscript
#' Copyright (C) 2016 Athanasios Natsis <natsisathanasios@gmail.com>
#'

# https://stackoverflow.com/questions/48921217/r-center-red-to-blue-color-palette-at-0-in-levelplot

####  Clear environment  -------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = funr::sys.script()
if(!interactive())pdf(file=sub("\\.R$",".pdf",Script.Name))
# sink(file=sub("\\.R$",".out",Script.Name,),split=TRUE)

library(data.table)
library(reshape2)
library(lattice)
library(RColorBrewer)

sza_bin  <- 10
azi_bin  <- 10
min_elev <- 15
max_nor  <- 100
since    <- 2016
p_lim    <- 0.05   ## statistical sign
rel_lim  <- 0.3    ## limit percent difference in N

#### LOAD DATA #################################################################

## Using Clear Sky data
filelist <- list.files(path = "/home/athan/DATA/Broad_Band",
                     pattern = "Clear_Sky_20[1-2][0-9]+.Rds",
                     full.names = T)

## Since 2016
data <- data.table()
for (af in filelist) {
    temp <- readRDS(af)
    if (any(year(temp$Date30) >= since)) {
        data <- rbind(data,temp)
    }
}

## Keep only flagged as clear
data <- data[ CSflag == 0 ]
## Declination from North
data[, North := Azimuth - 180 ]

## Exclude extreme angles
data <- data[ Elevat    > min_elev ]
data <- data[ abs(North)< max_nor  ]


## Bin sza and azimuth
data[, SZA_BIN := sza_bin * (SZA     %/% sza_bin) ]
data[, AZI_BIN := sza_bin * (Azimuth %/% azi_bin) ]

## symmetric binning around zero
data[, NOR_BIN := sign(North) * azi_bin * ( abs(North) %/% azi_bin + 1) ]

## Break in direct and global data
datGLB <- data[ QCF_GLB == "good" & !is.na(wattGLB) ]
data   <- datGLB


####  ANALYSIS  ################################################################


# hist(data$wattDIR)
# hist(data$wattHOR)
# hist(data$North)
#
# summary(data$wattDIR)
# summary(data$wattHOR)


pwelch <- function( Ma, Mb, Sa, Sb, Na, Nb ) {
    ## get p value
    abs(
        ## compute t welch value
        ( Ma - Mb ) / ( sqrt(( Sa^2 / Na ) + ( Sb^2 / Nb)) )
    )
}


## represent data by some metric

VAR <- "wattGLB"
data[ , Month := month(Day) ]
sky <- data[, .( Vmax = max(get(VAR),    na.rm = T ),
                 Vmin = min(get(VAR),    na.rm = T ),
                 Vavg = mean(get(VAR),   na.rm = T ),
                 Vmed = median(get(VAR), na.rm = T ),
                 VN   = sum(!is.na(get(VAR))),
                 Vsd  = sd(get(VAR), na.rm = T )   ),
            by = c("NOR_BIN", "SZA_BIN" )]

sky_m <- data[, .( Vmax = max(get(VAR),    na.rm = T ),
                 Vmin = min(get(VAR),    na.rm = T ),
                 Vavg = mean(get(VAR),   na.rm = T ),
                 Vmed = median(get(VAR), na.rm = T ),
                 VN   = sum(!is.na(get(VAR))),
                 Vsd  = sd(get(VAR), na.rm = T )   ),
            by = .(NOR_BIN, SZA_BIN, MONTH = month(Day) )]


sky[, AZI_BIN := NOR_BIN + 180 ]


# plot(sky$NOR_BIN, sky$SZA_BIN, pch=15)
#
# mtx <- acast(sky, NOR_BIN~-SZA_BIN, value.var="Vmax")
# mtx[is.na(mtx)] <- 0




#### whole period create symmetric diffs ####
WE <- data.table( sky[ NOR_BIN > 0 ] )
ES <- data.table( sky[ NOR_BIN < 0 ] )

wecare <- grep("^V", names(sky), value = T)



sky_temp <- unique( sky[, NOR_BIN, SZA_BIN] )
sky_temp <- sky_temp[ NOR_BIN > 0 ]

sky_diff <- data.table()
sky_rati <- data.table()
for (ii in 1:nrow(sky_temp)) {
    li <- sky_temp[ii,]

    tempWE <- WE[ SZA_BIN == li$SZA_BIN & abs(NOR_BIN) == abs(li$NOR_BIN), ..wecare  ]
    tempES <- ES[ SZA_BIN == li$SZA_BIN & abs(NOR_BIN) == abs(li$NOR_BIN), ..wecare  ]

    if (all(dim(tempWE) == c(1,length(wecare))) & all(dim(tempES) == c(1,length(wecare)))) {

        ## valid only for avg
        tw_avg <- pwelch( tempWE$Vavg, tempES$Vavg,
                          tempWE$Vsd,  tempES$Vsd,
                          tempWE$VN,   tempES$VN   )

        ##  two-tailed p-value
        pw_avg  <- 2*pt(q=tw_avg, df= (tempWE$VN + tempES$VN - 2 ), lower.tail=FALSE)

        ## Right-tailed p-value
        pw_avgR <-   pt(q=tw_avg, df= (tempWE$VN + tempES$VN - 2 ), lower.tail=FALSE)

        ## left-tailed p-value
        pw_avgL <-   pt(q=tw_avg, df= (tempWE$VN + tempES$VN - 2 ), lower.tail=TRUE )



        sky_diff <- rbind(sky_diff,
                          cbind(li,
                                tempES - tempWE,
                                tw_avg, pw_avg,
                                relN = (tempES$VN - tempWE$VN ) / tempWE$VN,
                                pw_avgR, pw_avgL) )
        sky_rati <- rbind(sky_rati,
                          cbind(li, tempES / tempWE) )
    }
}

####  monthly create symmetric diffs  ####
WE <- data.table( sky_m[ NOR_BIN > 0 ] )
ES <- data.table( sky_m[ NOR_BIN < 0 ] )

sky_temp   <- unique( sky_m[, .(NOR_BIN, SZA_BIN) ] )
sky_temp   <- sky_temp[ NOR_BIN > 0 ]
sky_diff_m <- data.table()

for (im in 1:12) {
    for (ii in 1:nrow(sky_temp)) {
        li <- sky_temp[ii,]

        tempWE <- WE[ SZA_BIN == li$SZA_BIN & abs(NOR_BIN) == abs(li$NOR_BIN) & MONTH == im , ..wecare  ]
        tempES <- ES[ SZA_BIN == li$SZA_BIN & abs(NOR_BIN) == abs(li$NOR_BIN) & MONTH == im, ..wecare  ]


        if (all(dim(tempWE) == c(1,length(wecare))) & all(dim(tempES) == c(1,length(wecare)))) {

            sky_diff_m <- rbind(sky_diff_m,
                                cbind(li,
                                      MONTH = im,
                                      tempES - tempWE,
                                      tw_avg, pw_avg,
                                      relN = (tempES$VN - tempWE$VN ) / tempWE$VN,
                                      pw_avgR, pw_avgL) )
        }
    }
}


## add all the missing pixels
all_pixels <- expand.grid(
    NOR_BIN = seq(range(sky_diff$NOR_BIN)[1], range(sky_diff$NOR_BIN)[2], azi_bin ),
    SZA_BIN = seq(range(sky_diff$SZA_BIN)[1], range(sky_diff$SZA_BIN)[2], sza_bin )
)
sky_diff <- merge(sky_diff, all_pixels, all = T)

all_pixels <- expand.grid(
    NOR_BIN = seq(range(sky_diff$NOR_BIN)[1], range(sky_diff$NOR_BIN)[2], azi_bin ),
    SZA_BIN = seq(range(sky_diff$SZA_BIN)[1], range(sky_diff$SZA_BIN)[2], sza_bin ),
    MONTH   = 1:12
)
sky_diff_m <- merge(sky_diff_m, all_pixels, all = T)





## select colors
breaks <- 13
Ocols   <- colorRampPalette(brewer.pal(breaks, "RdBu"))(breaks)


## plot all variables
for (av in c(wecare, "relN")) {
    mtx <- acast(sky_diff, NOR_BIN~90-SZA_BIN, value.var = av )
    ## symmetric scale
    max_abs <- max(abs(mtx), na.rm = T)
    brk     <- do.breaks(c(-max_abs, max_abs), breaks)
    print(
        levelplot(mtx,
                  ylab = "Elevation",
                  xlab = "Declination from South",
                  ylab.right = "West                                 East",
                  main = paste("Global Irradiance asymetry:", av),
                  col.regions = Ocols,
                  at = brk,
                  panel=function(...) {
                      arg <- list(...)
                      panel.levelplot(...)
                      panel.text(arg$x,
                                 arg$y,
                                 ifelse(!is.na(arg$z),round(arg$z,2),""),
                                 cex=0.7)
                  },
                  colorkey = list(col = Ocols,
                                  at = brk),
                  par.settings = list(
                      layout.widths = list(
                          axis.key.padding = 0.5,
                          ylab.right = 2))
                   )
    )

#     ## asymmetric scale
#     max_abs <- max(abs(mtx), na.rm = T)
#     brk     <- do.breaks(c(-max_abs, max_abs), breaks)
#     first_true <- which.max(brk > min(mtx,na.rm = T))
#     brk  <- brk[(first_true -1):length(brk)]
#     cols <- Ocols[(first_true -1):length(Ocols)]
#     print(
#     levelplot(mtx,
#               col.regions = cols,
#               at = brk,
#               colorkey = list(col = cols,
#                               at = brk),
#               main = av
#     )
# )
    ## For testing
    # print( levelplot(mtx, main=av) )
}


#### Plot meta stats ####

# dcare <- c("tw_avg", "pw_avg", "pw_avgR", "pw_avgL" )
dcare <- c( "pw_avg" )
for (av in dcare) {
    mtx <- acast(sky_diff, NOR_BIN~90-SZA_BIN, value.var = av )

    ## symmetric scale
    max_abs <- max(abs(mtx), na.rm = T)
    brk     <- do.breaks(c(0, max_abs), breaks)
    print(
        levelplot(mtx,
                  ylab = "Elevation",
                  xlab = "Declination from South",
                  ylab.right = "More Sig.                        Less Sig.",
                  main = paste("Global Irradiance asymetry:", av),
                  # col.regions = Ocols,
                  at = brk,
                  panel=function(...) {
                      arg <- list(...)
                      panel.levelplot(...)
                      panel.text(arg$x,
                                 arg$y,
                                 ifelse(!is.na(arg$z),round(arg$z,3),""),
                                 cex=0.7)
                  },
                  # colorkey = list(col = Ocols,
                  #                 at = brk),
                  par.settings = list(
                      layout.widths = list(
                          axis.key.padding = 0.5,
                          ylab.right = 2))
        )
    )
}


####  Selective plot  ####

## exclude values using p value and data available
av  <- "Vavg"
mtx <- acast(sky_diff, NOR_BIN~90-SZA_BIN, value.var =  av )
mtp <- acast(sky_diff, NOR_BIN~90-SZA_BIN, value.var = "pw_avg" )
mtr <- acast(sky_diff, NOR_BIN~90-SZA_BIN, value.var = "relN" )
mtx[ mtp > p_lim ] <- NA
mtx[ abs(mtr) > rel_lim ] <- NA

## symmetric scale
max_abs <- max(abs(mtx), na.rm = T)
brk     <- do.breaks(c(-max_abs, max_abs), breaks)
print(
    levelplot(mtx,
              ylab = "Elevation",
              xlab = "Declination from South",
              ylab.right = "West                                 East",
              main = paste("Global Irradiance asymetry:", av, "p <", p_lim, "relN <", rel_lim ),
              col.regions = Ocols,
              at = brk,
              panel=function(...) {
                  arg <- list(...)
                  panel.levelplot(...)
                  panel.text(arg$x,
                             arg$y,
                             ifelse(!is.na(arg$z),round(arg$z,2),""),
                             cex=0.7)
              },
              colorkey = list(col = Ocols,
                              at = brk),
              par.settings = list(
                  layout.widths = list(
                      axis.key.padding = 0.5,
                      ylab.right = 2))
    )
)









#### plot monthly mean difference
av  <- "Vavg"
for (im in 1:12) {
    temp <- sky_diff_m[MONTH == im]

    mtx <- acast(temp, NOR_BIN~90-SZA_BIN, value.var = av )
    ## symmetric scale
    max_abs <- max(abs(mtx), na.rm = T)
    brk     <- do.breaks(c(-max_abs, max_abs), breaks)
    print(
        levelplot(mtx,
                  ylab = "Elevation",
                  xlab = "Declination from South",
                  ylab.right = "West                                 East",
                  main = paste("Global Irradiance asymetry:", av, "Month:",im),
                  col.regions = Ocols,
                  at = brk,
                  panel=function(...) {
                      arg <- list(...)
                      panel.levelplot(...)
                      panel.text(arg$x,
                                 arg$y,
                                 ifelse(!is.na(arg$z),round(arg$z,2),""),
                                 cex=0.7)
                  },
                  colorkey = list(col = Ocols,
                                  at = brk),
                  par.settings = list(
                      layout.widths = list(
                          axis.key.padding = 0.5,
                          ylab.right = 2))
        )
    )
}




# ## plot stats on rations
# for (av in wecare) {
#
#     mtx <- acast(sky_rati, NOR_BIN~90-SZA_BIN, value.var = av )
#
#     ## symmetric scale
#     max_abs <- max(abs(1-mtx), na.rm = T)
#     brk     <- do.breaks(c(1-max_abs, 1+max_abs), breaks)
#     print(
#         levelplot(mtx,
#                   ylab = "Elevation",
#                   xlab = "Declination from South",
#                   ylab.right = "West                                 East",
#                   main = paste("Global Irradiance Ratio:", av),
#                   col.regions = Ocols,
#                   at = brk,
#                   panel=function(...) {
#                       arg <- list(...)
#                       panel.levelplot(...)
#                       panel.text(arg$x,
#                                  arg$y,
#                                  ifelse(!is.na(arg$z),round(arg$z,2),""),
#                                  cex=0.7)
#                   },
#                   colorkey = list(col = Ocols,
#                                   at = brk),
#                   par.settings = list(
#                       layout.widths = list(
#                           axis.key.padding = 0.5,
#                           ylab.right = 2))
#                    )
#     )

    #     ## asymmetric scale
    #     max_abs <- max(abs(mtx), na.rm = T)
    #     brk     <- do.breaks(c(-max_abs, max_abs), breaks)
    #     first_true <- which.max(brk > min(mtx,na.rm = T))
    #     brk  <- brk[(first_true -1):length(brk)]
    #     cols <- Ocols[(first_true -1):length(Ocols)]
    #
    #     print(
    #     levelplot(mtx,
    #               col.regions = cols,
    #               at = brk,
    #               colorkey = list(col = cols,
    #                               at = brk),
    #               main = av
    #     )
    # )

    ## For testing
    # print( levelplot(mtx, main=av) )

# }




##
# http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r





# R function to compute unpaired two-samples t-test
# t.test(x, y, alternative = "two.sided", var.equal = FALSE)
# R function to compute paired two-samples t-test
# t.test(x, y, paired = TRUE, alternative = "two.sided")

## stats and asymmetry by month


## stats and asymmetry by bin of dates








tac = Sys.time();
cat(paste("\n  --  ",  Script.Name, " DONE  --  \n\n"))
cat(sprintf("%s %-10s %-10s %-20s  %f mins\n\n",Sys.time(),Sys.info()["nodename"],Sys.info()["login"],Script.Name,difftime(tac,tic,units="mins")))



#'
#' | CS Flag | Test |
#' |:-------:|:--------------------------------------------------------------|
#' |   NA    | Undefined, untested                                           |
#' |    0    | Passed as clear sky                                           |
#' |    1    | Mean value of irradiance during the time period (MeanVIP)     |
#' |    2    | Max Value of Irradiance during the time Period (MaxVIP)       |
#' |    3    | Variability in irradiance by the length (VIL)                 |
#' |    4    | Variance of Changes in the Time series (VCT)                  |
#' |    5    | Variability in the Shape of the irradiance Measurements (VSM) |
#' |    6    | Low Direct Irradiance limit (LDI)                             |
#' |    7    | Low Global Irradiance limit (LGI)                             |
#' |    8    | Too Few CS point for the day (FCS)                            |
#' |    9    | Too Few data points for the day                               |
#' |   10    | Missing Data                                                  |
#'

