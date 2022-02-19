

X <- import_list("~/R/R Codes/Classification of HDLSS Data (Summer, 2021)/HDLSS-Summer-2021/TwoClass-PopularClassifiers-Simulated-JRC/Updated-SVMRBF/Results-Final/Pop-Ex-5.xlsx")

Z <- import_list("~/R/R Codes/Classification of HDLSS Data (Summer, 2021)/HDLSS-Summer-2021/Simulated/Ex-5.xlsx")

Y <- matrix(0,10,12)
d <- c(50,100,250,500,1000)

for(k in 1:5){
   
   Y[(2*k-1):(2*k),1] <- "Ex-5"
   Y[(2*k-1):(2*k),2] <- d[k]
   
   Y[(2*k-1):(2*k),3] <- c(mean(Z[[k]]$e0.sin), se(Z[[k]]$e0.sin)) * 100
   Y[(2*k-1):(2*k),4] <- c(mean(Z[[k]]$e0.sin.comp), se(Z[[k]]$e0.sin.comp)) * 100
   Y[(2*k-1):(2*k),5] <- c(mean(Z[[k]]$e2.sin.comp), se(Z[[k]]$e2.sin.comp)) * 100
   
   Y[(2*k-1):(2*k),6] <- X[[k]]$BYS[102:103] * 100
   Y[(2*k-1):(2*k),7] <- X[[k]]$GLMNET[102:103] * 100
   Y[(2*k-1):(2*k),8] <- X[[k]]$ONN[102:103] * 100
   Y[(2*k-1):(2*k),9] <- X[[k]]$NNRAND[102:103] * 100
   
   temp <- c(X[[k]]$`NN-R-1`[102],
             X[[k]]$`NN-R-3`[102],
             X[[k]]$`NN-R-5`[102],
             X[[k]]$`NN-R-10`[102],
             X[[k]]$`NN-lg-1`[102],
             X[[k]]$`NN-lg-3`[102],
             X[[k]]$`NN-lg-5`[102],
             X[[k]]$`NN-lg-10`[102])
   
   temp2 <- c(X[[k]]$`NN-R-1`[103],
              X[[k]]$`NN-R-3`[103],
              X[[k]]$`NN-R-5`[103],
              X[[k]]$`NN-R-10`[103],
              X[[k]]$`NN-lg-1`[103],
              X[[k]]$`NN-lg-3`[103],
              X[[k]]$`NN-lg-5`[103],
              X[[k]]$`NN-lg-10`[103])
   
   pos <- which.min(temp)
   
   Y[(2*k-1),10] <- temp[pos] * 100
   Y[(2*k),10] <- temp2[pos] * 100
   Y[(2*k-1):(2*k),11] <- X[[k]]$SVMLIN[102:103] * 100
   Y[(2*k-1):(2*k),12] <- X[[k]]$SVMRBF[102:103] * 100
}

Y <- as.data.frame(Y)
writexl::write_xlsx(Y, "C:\\Users\\JYOTISHKA\\Desktop\\Ex-5.xlsx")


if(FALSE){
   for(i in seq(1,49, by = 2)){
      for(j in 3:12){
         B[i,j] <- round(as.numeric(B[i,j]),2)
      }
   }
   
   for(i in seq(2,50, by = 2)){
      for(j in 3:12){
         B[i,j] <- paste0("(",round(as.numeric(B[i,j]),2),")")
      }
   }
}



