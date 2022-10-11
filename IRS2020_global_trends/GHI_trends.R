# /*  #!/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisthanasis@gmail.com> */
#' ---
#' title: "Study of Global radiation enhancement over Thessaloniki"
#' author:
#'   - Natsis Athanasios^[Laboratory of Atmospheric Physics, AUTH, natsisthanasis@gmail.com]
#'   - Alkiviadis Bais^[Laboratory of Atmospheric Physics, AUTH.]
#'   - Charikleia Meleti^[Laboratory of Atmospheric Physics, AUTH.]
#'   - Kleareti Tourpali^[Laboratory of Atmospheric Physics, AUTH.]
#' date: "`r format(Sys.time(), '%F')`"
#'
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      11pt
#' geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#'
#' bibliography:    [references.bib]
#' biblio-style:    apalike
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

#'
#' ## Abstract
#'
#' Measurements of downward shortwave solar irradiance are performed at Thessaloniki since 1993 with a Kipp & Zonen CM-21 pyranometer. Data are recorded as 1 min means and are quality controlled by adopting the procedure proposed by Long et al. (2008). The 26-year long time series is analysed in order to investigate long- and short-term changes caused by changes in atmospheric composition over the site. The overall increasing trend in daily solar irradiation under all-sky conditions of 0.33% per year reported for the period 1993–2012 (Bais et al., 2013) is reconfirmed, although a stronger trend is calculated for the last decade of the data series.
#'
#' An iterative optimization method has been employed to tune the clear-sky detection algorithm of Reno el al. (2016) to the local conditions, in order to identify the cloud-free cases. A few simple global radiation models have been tested and adjustment factors for each model have been determined which match best the measured values.
#'
#' Linear trends have been calculated for the entire period and different seasons for both clear- and all-sky conditions, in an attempt to attribute the long-term variability to variations in atmospheric water vapour and aerosols. Ancillary information from a collocated Cimel sun-photometer (since 2004), and a pyrheliometer (since 2016) are used to verify the effects of water vapor and aerosols on clear-sky irradiance. Finally, data under cloudy conditions are analysed to uncover potential effects from changes in cloudiness that have occurred over the period or record.

#' References
#'
#'  Bais, A. F., T. Drosoglou, C. Meleti, K. Tourpali, and N. Kouremeti (2013), Changes in surface shortwave solar irradiance from 1993 to 2011 at Thessaloniki (Greece), International Journal of Climatology, 33(13), 2871–2876.
#'
#' Long, C. N., Y. Shi (2008), An Automated Quality Assessment and Control Algorithm for Surface Radiation Measurements, The Open Atmospheric Science Journal, 2, 23–37.
#'
#' Reno, M. J., C. W. Hansen (2016), Identification of periods of clear sky irradiance in time series of GHI measurements, Renewable Energy, 90, 520–531.
#'
#'
#'




#+ include=F, echo=F
####  Set environment  ####
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("Climatological_") })
if(!interactive()) {
    pdf(  file = paste0("~/MANUSCRIPTS/presentations/IRS2020_global_trends/runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink( file = paste0("~/MANUSCRIPTS/presentations/IRS2020_global_trends/runtime/",  basename(sub("\\.R$",".out", Script.Name))), split=TRUE)
    filelock::lock(paste0("~/MANUSCRIPTS/presentations/IRS2020_global_trends/runtime/",basename(sub("\\.R$",".lock",Script.Name))),timeout = 0)
}


library(data.table)

source("~/FUNCTIONS/R/data.R")
source("~/FUNCTIONS/R/trig_deg.R")



#' ## Introduction
#'
#' Radiation clouds trends
#'
#' ## Data description
#'
#'





#'
#' Some aspects we have to study is the seasonality of the above finds
#' remove the sza dependence in order to see some local phenomena of the site
#'
#'

