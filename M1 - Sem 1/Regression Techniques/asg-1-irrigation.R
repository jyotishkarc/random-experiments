
library(readxl)

LAD_irrigation <- read_excel("D:/All Downloads/Regression Tables2.xlsx")
View(LAD_irrigation)

df.irr <- data.frame()

n <- nrow(LAD_irrigation)
d <- ncol(LAD_irrigation)

for(k in 1:18){
   
   df.irr[k,1] <- LAD_irrigation[(2*k - 1),1]
   df.irr[k,c(2:4)] <- LAD_irrigation[2*k,1:3]
}

writexl::write_xlsx(df.irr, path = "")