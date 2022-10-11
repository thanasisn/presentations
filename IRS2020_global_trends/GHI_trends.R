# /*  #!/usr/bin/env Rscript */
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
    pdf(  file = paste0("~/MANUSCRIPTS/presentations/IRS2020_enhancement/runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink( file = paste0("~/MANUSCRIPTS/presentations/IRS2020_enhancement/runtime/",  basename(sub("\\.R$",".out", Script.Name))), split=TRUE)
    filelock::lock(paste0("~/MANUSCRIPTS/presentations/IRS2020_enhancement/runtime/",basename(sub("\\.R$",".lock",Script.Name))),timeout = 0)
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



CSdt <- readRDS("./data/Clear_Sky.Rds")
suppressMessages( rm.cols.DT(CSdt, "CSflag_*") )

## keep only whole years
CSdt <- CSdt[ year(Date) >= 1994 & year(Date) <= 2021 ]

## only when sun is up
CSdt <- CSdt[ Elevat > min_elevation ]

## Quality Control data only
CSdt[ QCF_DIR != "good", wattDIR := NA ]
CSdt[ QCF_GLB != "good", wattGLB := NA ]
CSdt$QCF_DIR <- NULL
CSdt$QCF_GLB <- NULL



#'
#' Some aspects we have to study is the seasonality of the above finds
#' remove the sza dependence in order to see some local phenomena of the site
#'
#'


#'
#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("\n%s %s@%s %s %f mins\n\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
