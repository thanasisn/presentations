# /*  #!/usr/bin/env Rscript */
# /* Copyright (C) 2016 Athanasios Natsis <natsisthanasis@gmail.com> */
#' ---
#' title: "Study of Global radiation enhancement over Thessaloniki"
#' author: "Natsis Athanasios"
#' institute: "AUTH"
#' affiliation: "Laboratory of Atmospheric Physics"
#' date: "`r format(Sys.time(), '%F')`"
#' abstract: "Based on Evaluation of enhancement events of global horizontal
#' irradiance due to clouds at Patras, South-West Greece. Vamvakas Ioannis"
#'
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      11pt
#' geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
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
#'     toc:              yes
#'   html_document:
#'     keep_md:          yes
#'   odt_document:  default
#'   word_document: default
#'
#' ---


#+ include=FALSE, echo=FALSE

####_  Document options _####

knitr::opts_chunk$set(echo       = FALSE   )
knitr::opts_chunk$set(cache      = TRUE    )
knitr::opts_chunk$set(include    = TRUE    )
knitr::opts_chunk$set(comment    = ""      )
# knitr::opts_chunk$set(dev        = "pdf"   )
knitr::opts_chunk$set(dev        = "png"   )

knitr::opts_chunk$set(fig.width  = 8       )
knitr::opts_chunk$set(fig.height = 6       )

knitr::opts_chunk$set(out.width  = "60%"    )
knitr::opts_chunk$set(fig.align  = "center" )
# knitr::opts_chunk$set(fig.pos    = '!h'     )

####_ Notes _####

#
# Just preliminary tests.
#

#+ include=F, echo=F
####  Set environment  ####
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("Climatological_") })
if(!interactive()) {
    pdf(  file = paste0("~/MANUSCRIPTS/2022_sdr_trends/runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink( file = paste0("~/MANUSCRIPTS/2022_sdr_trends/runtime/",  basename(sub("\\.R$",".out", Script.Name))), split=TRUE)
    filelock::lock(paste0("~/MANUSCRIPTS/2022_sdr_trends/runtime/",basename(sub("\\.R$",".lock",Script.Name))),timeout = 0)
}


library(data.table)
library(RAerosols)
library(R.utils)

source("~/FUNCTIONS/R/data.R")
source("~/FUNCTIONS/R/trig_deg.R")


## load models
# load("/home/athan/Aerosols/source_R/Global_models.Rda")
load("./data/Combinations_results_2022-06-14_153313.Rds")



#'
#' ### Adjusted Haurwitz model (HAU) (1945)  ###
#'
#' $$ \text{GHI} = \alpha * 1098 \times \cos( z ) \times \exp \left( \frac{ - 0.057}{\cos(z)} \right) $$
#'
#' Where $\alpha$ is the adjustment factor $\alpha = `r gather_results$alpha[gather_results$CS_models=="HAU"]`$
#'

HAU <- function( sza,
                 a = 1098,
                 b = 0.057 ) {
    GHI <- a * cosde( sza ) * exp( - b / cosde(sza) )
    GHI <- gather_results$alpha[gather_results$CS_models=="HAU"] * GHI
    return(GHI)
}


CSdt <- readRDS("./data/Clear_Sky.Rds")
rm.cols.DT(CSdt, "TSIextEARTH_comb.y")

## only when sun is up
CSdt <- CSdt[ Elevat > 10 ]

## exclude some low lever measurements
CSdt[ wattGLB < 15 , wattGLB := NA ]
CSdt[ wattHOR < 15 , wattHOR := NA ]



## use cs model
##  i) the GHIMEASURED - MODELED to be higher than 5% (GHI_MEASURED-MODELED >= 5%)
ampl = 1.15
CSdt[ , HAU := HAU(SZA) * ampl ]

##  i) clearness index to be lower or higher than 1 for the detection of non-extreme (Kt <= 1 ) and extreme enhancements cases (Kt > 1).


## some metrics
CSdt[ , GLB_diff :=   wattGLB - HAU ]
CSdt[ , GLB_ench := ( wattGLB - HAU ) / HAU ]
CSdt[ , GLB_rati :=   wattGLB / HAU   ]

enh_days <- CSdt[ GLB_diff > 15 & wattHOR > 15 & GLB_diff > 0,
                  .( Enh_sum = sum(GLB_ench, na.rm = T),
                     Enh_max = max(GLB_ench, na.rm = T),
                     Enh_diff_sum = sum(GLB_diff, na.rm = T),
                     Enh_diff_max = sum(GLB_diff, na.rm = T)) , Day ]

setorder( enh_days, -Enh_sum )
setorder( enh_days, -Enh_max )
# setorder( enh_days, -Enh_diff_max )

# setorder( enh_days, -Enh_diff_sum )

daylist <- enh_days$Day

library(RColorBrewer)
kcols <- brewer.pal(7, "Dark2")

for ( aday in daylist[1:20] ) {
    temp <- CSdt[ Day == aday ]

    par(mar=c(1,2,1,1))

    layout(matrix(c(1,1,2,3), 4, 1, byrow = TRUE))
    ylim = range(0, temp$TSIextEARTH_comb * cosde(temp$SZA) )

    plot(temp$Date, temp$wattGLB, "l", col = "green", ylim = ylim)

    lines(temp$Date, temp$wattHOR, col = "blue")
    lines(temp$Date, temp$TSIextEARTH_comb * cosde(temp$SZA))
    lines(temp$Date, temp$HAU,    col = "red" )
    lines(temp$Date, temp$CS_ref, col = "red" ,lty=3)


    points(temp$Date[temp$CSflag!=0], temp$wattGLB[temp$CSflag!=0], col = kcols[ temp$CSflag[temp$CSflag!=0] ], pch =19,cex=0.4)

    title(main = as.Date(aday, origin = "1970-01-01"))

    unique( temp$CSflag )


    # plot(temp$Date, temp$GLB_ench, col = kcols[ temp$CSflag ])
    # plot(temp$Date, temp$GLB_diff, col = kcols[ temp$CSflag ])

    plot(temp$Date, temp$Cleaness_Kt)
    abline(h=.8,col="red")

    # plot(temp$Date, temp$DiffuseFraction_Kd)

    # plot(temp$Date, temp$GLB_ench)
    plot(temp$Date, temp$GLB_diff)

}



# HAU value is the threshold value we use
## keep enchanced cases


Enh <- CSdt[ GLB_diff > 0 ]

length(unique(Enh$Day))

#'
#' We select a simple clear sky model for Thessaloniki, and use it to determine
#' cases of GHI enhancement based on a threshold of `r ampl` the modelled value.
#'
#' There are `r length(unique(CSdt$Day))` days in the timeseries of GHI
#' concurent with DNI. Of which `r length(unique(Enh$Day))` have at list of one
#' minute of enhanced GHI.
#'


## get indexes of continues cases

## get time diffs
coo <- diff(Enh$Date)
## get indexes of sucesive cases
suc <- which(coo == 1)
## get the range of each sequence
iv  <- seqToIntervals(suc)

## stats on each event
Events <- data.frame( Start_date = apply(iv,1, function(x) { min( Enh$Date[ c( x[1]:(x[2]+1) ) ] ) } ),
                      End_date = apply(iv,1, function(x) { max( Enh$Date[ c( x[1]:(x[2]+1) ) ] ) } ),
                      Duration = apply(iv,1, function(x) { length( Enh$Date[ c( x[1]:(x[2]+1) ) ] ) } )
)

hist(Events$Duration, breaks = 100)


ivv <- seqToIntervals(coo)

coo[ivv[1,1]:ivv[1,2]]

# gives the indices of the 'jumps'.
which(diff(coo) != 1)





#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
