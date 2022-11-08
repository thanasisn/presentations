# /* Copyright (C) 2022 Athanasios Natsis <natsisthanasis@gmail.com> */
#' ---
#' title: "Study of Global radiation enhancement over Thessaloniki"
#' author:
#'   - Natsis Athanasios^[Laboratory of Atmospheric Physics, AUTH, natsisthanasis@gmail.com]
#'   - Alkiviadis Bais^[Laboratory of Atmospheric Physics, AUTH.]
#'   - Charikleia Meleti^[Laboratory of Atmospheric Physics, AUTH.]
#' date: "`r format(Sys.time(), '%F')`"
#'
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      11pt
#' geometry:      "left=1in,right=1in,top=1in,bottom=1in"
#'
#' bibliography:  [references.bib]
#' biblio-style:  apalike
#' csl:           anthropocene.csl
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#' - \usepackage{multicol}
#' - \setlength{\columnsep}{1cm}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     latex_engine:     xelatex
#'     toc:              no
#'   word_document: default
#'
#'
#' ---


#+ include=FALSE, echo=FALSE

####_  Document options _####

knitr::opts_chunk$set(echo       = FALSE   )
knitr::opts_chunk$set(cache      = FALSE   )
knitr::opts_chunk$set(include    = TRUE    )
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "pdf"   )
# knitr::opts_chunk$set(dev        = "png"   )

knitr::opts_chunk$set(fig.width  = 8       )
knitr::opts_chunk$set(fig.height = 6       )

knitr::opts_chunk$set(out.width  = "60%"    )
knitr::opts_chunk$set(fig.align  = "center" )
# knitr::opts_chunk$set(fig.pos    = '!h'     )

#'
#' ## Abstract
#'
#' Here we identify and investigate the occurrence of enhancement events of GHI in relation to the visibility of the Sun, as derived by an algorithm that use of the DNI measurements, the clearness index
#' ($K_t = {GHI}_{MEASURED}/GHI_{MODEL}$),
#'  the solar zenith angle. Moreover, we investigate the long-term behavior of these events in relation to the above factors. The time series of GHI and DNI for the period 2016-2021 is analyzed by an iterative optimization method, in order to tune the clear-sky detection algorithm of
#'  Reno et al.(2016)
#'  to the local conditions and to test a few simple global radiation models for obtaining a better match with the measurements conducted under cloud-free conditions. Based on these results the detection of enhancement events can be extended back to the start of the GHI record of Thessaloniki in the early 1990s. This backward extension will allow investigation of the long-term behavior of the enhancement events.
#'




#+ include=F, echo=F
####  Set environment  ####
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("Enhancement_") })
if(!interactive()) {
    pdf(  file = paste0("~/MANUSCRIPTS/presentations/IRS2020_enhancement/runtime/",   basename(sub("\\.R$",".pdf",  Script.Name))))
    sink( file = paste0("~/MANUSCRIPTS/presentations/IRS2020_enhancement/runtime/",   basename(sub("\\.R$",".out",  Script.Name))), split = TRUE)
    filelock::lock(paste0("~/MANUSCRIPTS/presentations/IRS2020_enhancement/runtime/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}


library(data.table)
# library(RAerosols)
# library(R.utils)

source("~/FUNCTIONS/R/data.R")
source("~/FUNCTIONS/R/trig_deg.R")


## load models
# load("/home/athan/Aerosols/source_R/Global_models.Rda")
load("./data/Combinations_results_2022-06-14_153313.Rds")


GLB_ench_THRES     <- 0    ## enchantment % relative to HAU
GLB_diff_THRES     <- 10   ## enchantment absolute diff to HAU
Clearness_Kt_THRES <- 0.8  ## enchantment threshold
wattGLB_THRES      <- 20   ## minimum value to consider
wattDIR_THRES      <- 20   ## minimum value to consider
min_elevation      <- 10   ## minimum sun elevation to use
ampl               <- 1.05 ## adjusted HAU amplified threshold
SZA_BIN            <- 1

#' ## Introduction
#'
#' For many applications, and especially for those related to renewable energy, there is an increasing interest on accurate prediction of the Global Horizontal Irradiance (GHI), on a fine temporal resolution. A major concern is the attenuation of radiation by the clouds, which can have a positive or negative feedback on radiation, depending on the actual conditions. The effects depend on multiple factors, such as the structure and consistency of clouds and the geometry between the observer, the clouds and the Sun. Accurate knowledge of the enhancement events characteristics is useful for discerning long term changes of the principal factors, and for providing valuable information for the design of energy production and distribution systems. Here we provide a statistical analysis that identifies cases of enhancement of GHI from measurements at the city of Thessaloniki.
#'
#'
#'
#' ## Data and Methodology
#'
#' Measurements of solar shortwave global horizontal irradiance (GHI) and direct normal irradiance (DNI) are performed simultaneously since 2016 in Thessaloniki, Greece, respectively with a CM-21 pyranometer and a CHP-1 pyrheliometer both by Kipp & Zonen. A data quality assurance procedure was applied on these data based on methods proposed by
#' Long and Shi\ [-@long_automated_2008; -@Long2006],
#' which were adjusted for the specific site. Only data characterized with acceptable quality was used. We are using, for the global radiation reference, the Haurwitz’s model (reference). The selection was done with data of GHI and DNI for the period 2016 – 2021 using the iterative method of optimizing the ‘Clear sky’ identification method, as proposed by
#' @Long2000 and @Reno2016.
#' We have calibrated the above method for our site. Among the eight simple models (Daneshyar–Paltridge–Proctor, Kasten–Czeplak, Haurwitz, Berger–Duffie, Adnot–Bourges–Campana–Gicquel, Robledo-Soler, Kasten and Ineichen-Perez), as described in
#' @Reno2012 and tested by
#' @Reno2016,
#' we found the best result with an adjusted Haurwitz model (A-HAU)
#' (Eq. \@ref(eq:ahau)),
#' using as the main selection criterion the root mean squared error (RMSE).
#'
#' The enhancement cases for the 1-minute measurements of GHI were identified when the following conditions were met:
#' a) Sun elevation angle above $`r min_elevation`^\circ$,
#' b) GHI values above $`r ampl` \times \text{A-HAU} + `r GLB_diff_THRES`$ ($\text{GHI}_\text{Threshold}$), and
#' c) Clearness index $k_t > `r Clearness_Kt_THRES`$.
#' These criteria have been used in previous studies
#' (e.g., @Vamvakas2020).
#' An example of this procedure is given for the 2017&#8209;04&#8209;08 in Fig.\ (\@ref(fig:dayexample)),
#' where the enhancement cases and the role of the other physical quantities are visualized.
#'
#' \begin{equation}
#' \text{GHI}_\text{Clear Sky} = `r signif(gather_results$alpha[gather_results$CS_models=="HAU"],digits = 3 )` \times 1098 \times \cos( \text{SZA} ) \times \exp \left( \frac{ - 0.057}{\cos(\text{SZA})} \right)  (\#eq:ahau)
#' \end{equation}
#'


##TODO HAU selection plot



HAU <- function(sza,
                a = 1098,
                b = 0.057 ) {
    GHI <- a * cosde( sza ) * exp( - b / cosde(sza) )
    GHI <- gather_results$alpha[gather_results$CS_models == "HAU"] * GHI
    return(GHI)
}


CSdt <- readRDS("./data/Clear_Sky.Rds")
suppressMessages( rm.cols.DT(CSdt, "CSflag_*") )

## keep only whole years
CSdt <- CSdt[ year(Date) >= 1994 & year(Date) <= 2021 ]

## only when sun is up
CSdt <- CSdt[ Elevat > min_elevation ]

## Quality Control data only
## FIXME this will change in the future
CSdt[QCF_DIR != "good", wattDIR := NA]
CSdt[QCF_GLB != "good", wattGLB := NA]
CSdt$QCF_DIR <- NULL
CSdt$QCF_GLB <- NULL


## Use cs model
## i) the GHI_MEASURED - MODELED higher than 5% (GHI_MEASURED-MODELED >= 5%)
CSdt[ , HAU    := HAU(SZA) * ampl ]
CSdt[ , defHAU := HAU(SZA) ]



## some metrics
CSdt[ , GLB_diff :=   wattGLB - HAU ]         ## enhancement
CSdt[ , GLB_ench := ( wattGLB - HAU ) / HAU ] ## relative enhancement
CSdt[ , GLB_rati :=   wattGLB / HAU   ]

## select some days for display
enh_days <- CSdt[GLB_ench     > GLB_ench_THRES      &
                 Clearness_Kt > Clearness_Kt_THRES  &
                 wattGLB      > wattGLB_THRES       &
                 GLB_diff     > GLB_diff_THRES,
                 .(Enh_sum      = sum(GLB_ench, na.rm = TRUE),
                   Enh_max      = max(GLB_ench, na.rm = TRUE),
                   Enh_diff_sum = sum(GLB_diff, na.rm = TRUE),
                   Enh_diff_max = sum(GLB_diff, na.rm = TRUE)) , Day]


## interesting days first
setorder(enh_days, -Enh_sum )
setorder(enh_days, -Enh_max )
setorder(enh_days, -Enh_diff_sum )

## plot some interesting days
daylist <- enh_days$Day
daylist <- daylist[1:30]

## plot one selected day ####
#+ dayexample, include=T, echo=F, fig.cap="Diurnal variability of GHI (green) and DNI (blue) for 08-4-2017. Red cycles denote the enhancement cases that were identified during the day. The red line represents the the GHI threshold ($\\text{GHI}_\\text{Threshold}$) we use, and the black line is the TSI at the TOA for reference."
daylist <- as.Date(c("2017-04-08"))
for (aday in daylist) {
    temp <- CSdt[ Day == aday ]
    par(mar = c(4,4,1,1))
    ylim = range(0, temp$TSIextEARTH_comb * cosde(temp$SZA), temp$wattGLB)

    plot(temp$Date, temp$wattGLB, "l", col = "green",
         ylim = ylim,
         ylab = expression(Watt/m^2), xlab = "UTC")

    lines(temp$Date, temp$wattHOR, col = "blue")

    lines(temp$Date, temp$TSIextEARTH_comb * cosde(temp$SZA))

    # lines(temp$Date, temp$defHAU, col = "red", lty = 2 )
    lines(temp$Date, temp$HAU + GLB_diff_THRES, col = "red" )

    # lines(temp$Date, temp$HAU + wattGLB_THRES , col = "red" )
    # lines(temp$Date, temp$CS_ref, col = "red" ,lty=3)
    # points(temp[ GLB_ench > GLB_ench_THRES, Date ], temp[ GLB_ench > GLB_ench_THRES, wattGLB ], col = "cyan")
    # points(temp[ Clearness_Kt > Clearness_Kt_THRES, Date ], temp[ Clearness_Kt > Clearness_Kt_THRES , wattGLB ], col = "yellow")

    points(temp[GLB_ench     > GLB_ench_THRES     &
                Clearness_Kt > Clearness_Kt_THRES &
                wattGLB      > wattGLB_THRES      &
                GLB_diff     > GLB_diff_THRES, Date ],
           temp[GLB_ench     > GLB_ench_THRES     &
                Clearness_Kt > Clearness_Kt_THRES &
                wattGLB      > wattGLB_THRES      &
                GLB_diff     > GLB_diff_THRES, wattGLB ], col = "red")

    title(main = as.Date(aday, origin = "1970-01-01"))
    # legend("topleft", c("GHI","DNI",  "A-HAU", "TSI on horizontal level","GHI Enhancement event"),
    #        col = c("green",   "blue", "red", "black", "red"),
    #        pch = c(     NA,       NA,    NA,      NA,    1 ),
    #        lty = c(      1,        1,     1,       1,   NA ),
    #        bty = "n"
    # )

    legend("topleft", c("GHI","DNI",  "GHI threshold", "TSI on horizontal level","GHI Enhancement event"),
           col = c("green",   "blue", "red", "black",  "red"),
           pch = c(     NA,       NA,    NA,      NA,     1 ),
           lty = c(      1,        1,     1,       1,    NA ),
           bty = "n"
    )

    # plot(temp$Date, temp$Clearness_Kt)
    # abline(h=.8,col="red")
    # plot(temp$Date, temp$DiffuseFraction_Kd)
    # plot(temp$Date, temp$GLB_ench)
    # plot(temp$Date, temp$GLB_diff)
}
#'



## keep only enhanced cases
Enh <- CSdt[GLB_ench     > GLB_ench_THRES     &
            Clearness_Kt > Clearness_Kt_THRES &
            wattGLB      > wattGLB_THRES      &
            GLB_diff     > GLB_diff_THRES    ]


DATA_Enh <- CSdt[ , Enhancement := GLB_ench     > GLB_ench_THRES     &
                                   Clearness_Kt > Clearness_Kt_THRES &
                                   wattGLB      > wattGLB_THRES      &
                                   GLB_diff     > GLB_diff_THRES    ]





##TODO get indexes of continues cases
## ## get time diffs
## coo <- diff(Enh$Date)
## ## get indexes of successive cases
## suc <- which(coo == 1)
## ## get the range of each sequence
## iv  <- seqToIntervals(suc)
## ## stats on each event
## Events <- data.frame( Start_date = apply(iv,1, function(x) { min( Enh$Date[ c( x[1]:(x[2]+1) ) ] ) } ),
##                       End_date   = apply(iv,1, function(x) { max( Enh$Date[ c( x[1]:(x[2]+1) ) ] ) } ),
##                       Duration   = apply(iv,1, function(x) { length( Enh$Date[ c( x[1]:(x[2]+1) ) ] ) } )
## )
## hist(Events$Duration, breaks = 100)
## ivv <- seqToIntervals(coo)
## coo[ivv[1,1]:ivv[1,2]]
## # gives the indices of the 'jumps'.
## which(diff(coo) != 1)


Enh_daily <- Enh[, .( N        = sum(!is.na(GLB_ench)),
                      N_ex     = sum( wattGLB > TSIextEARTH_comb * cosde(SZA)),
                      sum_Ench = sum( GLB_diff),
                      avg_Ench = mean(GLB_ench),
                      sd_Ench  = sd( GLB_ench),
                      sum_Diff = sum( GLB_diff)),
                 by = "Day"  ]

Enh_yearly <- Enh[, .( N        = sum(!is.na(GLB_ench)),
                       N_ex     = sum( wattGLB > TSIextEARTH_comb * cosde(SZA)),
                       sum_Ench = sum( GLB_diff),
                       avg_Ench = mean(GLB_ench),
                       sd_Ench  = sd(  GLB_ench),
                       sum_Diff = sum( GLB_diff)),
                  by = year(Date)  ]

Enh_total <- Enh[, .( N        = sum(!is.na(GLB_ench)),
                       N_ex     = sum( wattGLB > TSIextEARTH_comb * cosde(SZA)),
                       sum_Ench = sum( GLB_diff),
                       avg_Ench = mean(GLB_ench),
                       sd_Ench  = sd(  GLB_ench),
                       sum_Diff = sum( GLB_diff))   ]



Enh_sza    <- Enh[, .(N        = sum(!is.na(GLB_ench)),
                      N_ex     = sum( wattGLB > TSIextEARTH_comb * cosde(SZA)),
                      sum_Ench = sum( GLB_diff),
                      avg_Ench = mean(GLB_ench),
                      sd_Ench  = sd(  GLB_ench),
                      sum_Diff = sum( GLB_diff)),
                  by = .(SZA = (SZA - SZA_BIN / 2 ) %/% SZA_BIN) ]


Data_sza    <- DATA_Enh[, .(N_enha  = sum(Enhancement, na.rm = TRUE),
                            N_total = sum(!is.na(wattGLB))),
                        by = .(SZA = (SZA - SZA_BIN / 2 ) %/% SZA_BIN) ]





CONF_INTERV <- .95
conf_param  <- 1 - (1 - CONF_INTERV) / 2
suppressWarnings({
Enh_sza[,   Ench_EM := qt(conf_param, df = N - 1) * sd_Ench / sqrt(N)]
Enh_daily[, Ench_EM := qt(conf_param, df = N - 1) * sd_Ench / sqrt(N)]
Enh_yearly[,Ench_EM := qt(conf_param, df = N - 1) * sd_Ench / sqrt(N)]
Enh_total[, Ench_EM := qt(conf_param, df = N - 1) * sd_Ench / sqrt(N)]
})


## Make values relative ####
Enh_yearly[ , N_att        := 100*(N - mean(N))/mean(N)]
Enh_yearly[ , sum_Ench_att := 100*(sum_Ench - mean(sum_Ench))/mean(sum_Ench)]
Enh_yearly[ , Ench_intesit := sum_Ench / N ]

# plot(Enh_daily$Day, Enh_daily$N)
# plot(Enh_daily$Day, Enh_daily$N_ex)
# plot(Enh_daily$Day, Enh_daily$sum_Ench)
# plot(Enh_daily$Day, Enh_daily$avg_Ench)





fit1 <- lm( Enh_yearly$N_att ~ Enh_yearly$year )[[1]]
fit2 <- lm( Enh_yearly$Ench_intesit ~ Enh_yearly$year )[[1]]

## results ####

#'
#' ## Results
#'
#' The enhancement events occur in
#' $`r signif( 100*(sum(!is.na(Enh$GLB_ench)) / sum(!is.na(CSdt$wattGLB))), 3 )`\%$
#' of the total GHI measurements, and for
#' $`r signif( 100* length(unique(Enh$Day)) / length(unique(CSdt$Day)), 3 )`\%$
#' of the days in the data set.
#' The total number of cases we identified, is increasing steadily the last decades,
#' with a rate of $`r signif(abs(fit1[2]*1),3)`\%$ per year (Figure \@ref(fig:enchtrend)).
#' However, the yearly mean excess radiation (radiation above the threshold) per enhancement event seems to be almost
#' constant with a mean value of $`r round(mean(Enh_yearly$Ench_intesit),1)`\,Wm^{-2}$, with a marginal trend of $`r signif((fit2[2]*1),2)`Wm^{-2}$ per year.
#'



#+ enchtrend, include=T, echo=F, fig.cap="Trend of the total of enhancement cases per year."
plot( Enh_yearly$year, Enh_yearly$N_att ,
      xlab = "",
      ylab = bquote("Difference from mean [%]" )
      )
# title("Number of enchantments incidences", cex = 0.7)
lm1        <- lm( Enh_yearly$N_att ~ Enh_yearly$year )
abline(lm1)
fit <- lm1[[1]]
legend('topleft', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*1),3),'* year'))
#'



#+ enchtrendN, include=F, echo=F, fig.cap="Trend of yearly number of enhancement cases."
plot( Enh_yearly$year, Enh_yearly$N ,
      xlab = "",
      ylab = bquote("Number of yearly cases" )
)
# title("Number of enchantments incidences", cex = 0.7)
lm1        <- lm( Enh_yearly$N ~ Enh_yearly$year )
abline(lm1)
fit <- lm1[[1]]
legend('topleft', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-', signif(abs(fit[2]*1),3),'* year'))
#'





#+ excessenergy, include=F, echo=F, fig.cap="The sum of the energy (in 1 minute resolution), above the reference model."
plot( Enh_yearly$year, Enh_yearly$sum_Ench_att,
      xlab = "Year",
      ylab = bquote("Difference from mean [%]")
     )
title("Sum of radiation above enhancement threshold", cex = 0.7)
lm1        <- lm( Enh_yearly$sum_Ench_att ~ Enh_yearly$year )
abline(lm1)
fit <- lm1[[1]]
legend('topleft', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*1),3),'* year'))
#'


#+ excess, include=F,echo=F, fig.cap="Trend and mean radiation enhancement radiation, above threshold, per case."
plot( Enh_yearly$year, Enh_yearly$Ench_intesit,
      xlab = "Year",
      ylab = bquote("Mean enhancement intensity ["~ Watt~m^-2~N^-1~"]")
)
abline( h = mean(Enh_yearly$Ench_intesit, na.rm = TRUE), lty = 2 )
lm1        <- lm( Enh_yearly$Ench_intesit ~ Enh_yearly$year )
abline(lm1)
fit <- lm1[[1]]
legend('topleft', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*1),3),'* year'))
#'


# plot( Enh_yearly$year, Enh_yearly$avg_Ench,
#       xlab = "Year",
#       ylab = bquote("Average enchantment intensity ["~ Watt~m^-2~"]")
# )





# #' Although the mean difference in radiation per event seems to be almost
# #' constant about $`r signif(Enh_total$avg_Ench,3)`\pm`r format(Enh_total$Ench_EM,scientific = T,digits = 2)`%$.



# plot(Enh_daily$Day, Enh_daily$sum_Diff)


#'
#' The number of case characterization is skewed by the SZA angle of the case, although this connection is indicative to the complexity of factor we have to take into account.
#'

#+ include=F, echo=F, fig.cap="Number of cases by SZA."
plot(Enh_sza$SZA, Enh_sza$N)
#'

## ignore extreme cases for now
# plot(Enh_sza$SZA, Enh_sza$N_ex)


# #' Interestingly, when we examine the total energy contribution by SZA we found a maximum at
# #' $`r Enh_sza[ which.max(sum_Ench), SZA]`^\circ$.
# plot(Enh_sza$SZA, Enh_sza$sum_Ench)
# # Enh_sza[ which.max(sum_Ench), SZA ]

#'
#' there is a dependency of the magnitude of the enhancement with the
#' SZA.
#'

#+ include=F, echo=F, fig.cap="Mean enhancement intensity relative to reference (A-HAU) by SZA."
ylim <- range(Enh_sza$avg_Ench - Enh_sza$Ench_EM, Enh_sza$avg_Ench + Enh_sza$Ench_EM, na.rm = T)
plot(  Enh_sza$SZA, Enh_sza$avg_Ench, pch = 19, cex = 0.7, ylim = ylim)
arrows(Enh_sza$SZA, Enh_sza$avg_Ench - Enh_sza$Ench_EM, Enh_sza$SZA, Enh_sza$avg_Ench + Enh_sza$Ench_EM, length=0.03, angle=90, code=3)
#'


#+ include=T, echo=F, fig.cap="Enhancement cases percentage in total data per SZA."
plot( Data_sza$SZA, Data_sza[, 100 * N_enha / N_total ],
      xlab = "Solar zenith angle",
      ylab = bquote("Enhancement cases [%] of total data")
      )
#'



#'
#' Some aspects we have to study is the seasonality of the above finds
#' remove the SZA dependence in order to see some local phenomena of the site
#'
#'
#' \vfill
#'


#'
#+ include=F, echo=F
tac <- Sys.time()
cat(sprintf("\n%s %s@%s %s %f mins\n\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
