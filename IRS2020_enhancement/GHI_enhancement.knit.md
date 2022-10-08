---
title: "Study of Global radiation enhancement over Thessaloniki"
author:
  - Natsis Athanasios^[Laboratory of Atmospheric Physics, AUTH, natsisthanasis@gmail.com]
  - Alkiviadis Bais^[Laboratory of Atmospheric Physics, AUTH.]
  - Charikleia Meleti^[Laboratory of Atmospheric Physics, AUTH.]
date: "2022-10-08"

documentclass: article
classoption:   a4paper,oneside
fontsize:      11pt
geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"

bibliography:    [references.bib]
biblio-style:    apalike

header-includes:
- \usepackage{caption}
- \usepackage{placeins}
- \captionsetup{font=small}
- \usepackage{multicol}
- \setlength{\columnsep}{1cm}

output:
  bookdown::pdf_document2:
    number_sections:  no
    fig_caption:      no
    keep_tex:         no
    latex_engine:     xelatex
    toc:              yes
  html_document:
    keep_md:          yes
  odt_document:  default
  word_document: default

---




## Abstract

Measurements of solar shortwave global horizontal irradiance (GHI) and direct normal irradiance (DNI) are performed simultaneously since 2016 in Thessaloniki, Greece, respectively with a CM-21 pyranometer and a CHP-1 pyrheliometer both by Kipp & Zonen. Here we identify and investigate the occurrence of enhancement events of GHI in relation to the visibility of the Sun as derived by the DNI measurements, the clearness index ($K_t = {GHI}_{MEASURED}/GHI_{MODEL}$), the solar zenith angle and the magnitude of the enhancement events. Simulations of GHI and DNI are derived by LibRadtran based on aerosol and water vapor measurements of a collocated Cimel sun-photometer. Moreover, we investigate the seasonal and long-term behavior of these events in relation to the above factors.

In addition, the time series of GHI and DNI for the period 2016-2019 is analyzed by an iterative optimization method, in order to tune the clear-sky detection algorithm of Reno et al. (2016) to the local conditions and to test a few simple global radiation models for obtaining a better match with the measurements conducted under cloud-free conditions. Based on these results the detection of enhancement events can be extended back to the start of the GHI record of Thessaloniki in the early 1990s. This backward extension will allow investigation of the long-term behavior of the enhancement events which may have been affected by changes in climate.





## Introduction

Radiation clouds trends
## Data description

A radiation data quality assurance procedure was applied adjusted for the site,
based on methods of Long and Shi\ [-@long_automated_2008; -@Long2006].
Only data characterized with acceptable quality was used.


By the GHI and DNI for the period 2016 – 2021 and using the iterative
method of optimizing the 'Clear sky' identification method, as proposed
by @Long2000 and @Reno2016. We were able to calibrate the above method
for our site. Among the eight simple models tested by @Reno2016, we found
the best result with the Haurwitz model, using as the main criteria the
RMSE.

We u
To establish some criteria for the GHI enhancement cases we use similar
criteria to ...........[@Vamvakas2020]


### Adjusted Haurwitz model (HAU) (1945)  ###

$$ \text{GHI} = \alpha * 1098 \times \cos( z ) \times \exp \left( \frac{ - 0.057}{\cos(z)} \right) $$

Where $\alpha$ is the adjustment factor $\alpha = 0.9653023$



```
Removing columns:  
```



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-1} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-2} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-3} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-4} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-5} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-6} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-7} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-8} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-9} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-10} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-11} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-12} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-13} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-14} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-15} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-16} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-17} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-18} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-19} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-20} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-21} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-22} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-23} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-24} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-25} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-26} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-27} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-28} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-29} \end{center}



\begin{center}\includegraphics[width=0.6\linewidth]{GHI_enhancement_files/figure-latex/unnamed-chunk-3-30} \end{center}

```
[1] 4784
```


We select a simple clear sky model for Thessaloniki, and use it to determine
cases of GHI enhancement based on a threshold of 1.05 the modeled value.

There are 10047 days in the timeseries of GHI
concurrent with DNI. Of which 4784 have at list of one
minute of enhanced GHI.




**END**


```
2022-10-07 17:34:43.5 athan@tyler Climatological_ 1.674593 mins
```

