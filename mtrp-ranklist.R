
library(dplyr)
library(tidyr)
library(rio)

df.marks <- import_list("D:/All Downloads/MTRP Score Sheet.xlsx")

# df.marks %>% lapply(function(val) return(c(which(colnames(val) == "reg_no"),
#                                            which(colnames(val) == "name"),
#                                            which(colnames(val) == "category"),
#                                            which(colnames(val) == "classroom"),
#                                            which(colnames(val) == "Score (Objective)"),
#                                            which(colnames(val) == "Score (Subjective)"),
#                                            which(colnames(val) == "Total"))))

junior <- TRUE
senior <- TRUE

range.junior <- 1:10
range.senior <- 11:14

imp.cols <- c("reg_no","name","email","contact","alt_contact",
              "category","classroom","medium",
              "Score (Objective)","Score (Subjective)")

#### Junior

if(junior == TRUE){
   
   df.marks.junior <- as.data.frame(matrix(NA, ncol = 10))
   names(df.marks.junior) <- imp.cols
   
   for(h in range.junior){
      
      temp <- df.marks[[h]] %>% select(imp.cols)
      df.marks.junior <- df.marks.junior %>% rbind(temp)
   }
   
   df.marks.junior.temp <- df.marks.junior
   
   df.marks.junior.temp <- df.marks.junior.temp %>% 
      replace_na(list(`Score (Objective)` = 0,
                      `Score (Subjective)` = 0))# %>%
   # select(`Score (Objective)`,`Score (Subjective)`) %>%
   # as.numeric()
   
   
   # df.marks.junior.temp <- df.marks.junior.temp %>% select(`Score (Subjective)`) %>% 
   #                                     replace(is.na(.),0) %>% as.numeric()
   
   df.marks.junior.temp <- df.marks.junior.temp %>% 
      cbind("Total" = df.marks.junior.temp$`Score (Objective)` +
               df.marks.junior.temp$`Score (Subjective)`)
   
   df.marks.junior.final <- df.marks.junior.temp %>% arrange(desc(Total))
}



#### Senior

if(senior == TRUE){
   
   df.marks.senior <- as.data.frame(matrix(NA, ncol = 10))
   names(df.marks.senior) <- imp.cols
   
   for(h in range.senior){
      
      temp <- df.marks[[h]] %>% select(imp.cols)
      df.marks.senior <- df.marks.senior %>% rbind(temp)
   }
   
   df.marks.senior.temp <- df.marks.senior
   
   df.marks.senior.temp <- df.marks.senior.temp %>% 
      replace_na(list(`Score (Objective)` = 0,
                      `Score (Subjective)` = 0))
   
   df.marks.senior.temp <- df.marks.senior.temp %>% 
      cbind("Total" = df.marks.senior.temp$`Score (Objective)` +
               df.marks.senior.temp$`Score (Subjective)`)
   
   df.marks.senior.final <- df.marks.senior.temp %>% arrange(desc(Total))
}


writexl::write_xlsx(df.marks.junior.final,
                    path = "D:\\My Documents\\R\\R Codes\\Experiments with R\\mtrp-2023-online-junior-all.xlsx")

writexl::write_xlsx(df.marks.senior.final,
                    path = "D:\\My Documents\\R\\R Codes\\Experiments with R\\mtrp-2023-online-senior-all.xlsx")

