setwd("C:/Users/gra09318/Documents/Universite Jean Monnet/FATGAIT/Questionnaires/FACIT-F")

# devtools::install_github("raybaser/FACTscorer")
library(readxl)

FACIT_F_March <- read_excel("FACIT-F_july.xlsx")

FACIT_F_scored <- FACTscorer::scoreFACIT_F(FACIT_F_March)
writexl::write_xlsx(FACIT_F_scored,"C:/Users/gra09318/Documents/Universite Jean Monnet/FATGAIT/Questionnaires/FACIT-F/FACIT_F_scored-july.xlsx")
