setwd("C:/Users/gra09318/Documents/Universite Jean Monnet/FATGAIT/Actigraph/R")

library(read.gt3x)
library(GGIR)
source("C:/Users/gra09318/Documents/Universite Jean Monnet/FATGAIT/Actigraph/R/myscript.R")
test <- GGIR::GGIR(mode = 1:5,
             datadir = "C:/Users/gra09318/Documents/Universite Jean Monnet/FATGAIT/Actigraph/R/Actigraph_FAT_GAIT",
             outputdir = "C:/Users/gra09318/Documents/Universite Jean Monnet/FATGAIT/Actigraph/R/outnyt",
             studyname = "Fat-Gait_test",
             overwrite = TRUE,
             includedaycrit = 12,
             do.enmo = TRUE,
             acc.metric = "ENMO",
             mvpathreshold = c(100.6),
             threshold.lig = c(44.8),
             threshold.mod = c(100.6),
             threshold.vig = c(428.8),
             boutcriter = 0.8,      
             boutcriter.in = 0.9,     
             boutcriter.lig = 0.8,
             boutcriter.mvpa = 0.8, 
             boutdur.in = c(1,10,30), 
             boutdur.lig = c(1,10),
             boutdur.mvpa = c(1),
             visualreport = TRUE,
             do.report = c(2,4,5),
             myfun = myfun
             )

