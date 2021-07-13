vm <- read.csv("~/Excel Sheets/vmmarks-csv.csv", header=FALSE)
vmc1 <- as.numeric(vm[!is.na(as.numeric(vm$V5)) & !is.na(as.numeric(vm$V8)),]$V5)
vmc2 <- as.numeric(vm[!is.na(as.numeric(vm$V5)) & !is.na(as.numeric(vm$V8)),]$V6)*2.5
vmname <- vm[!is.na(as.numeric(vm$V5)) & !is.na(as.numeric(vm$V8)),]$V2

fin <- data.frame(vmname,vmc1,vmc2)
order.score <- order(fin$vmc1)
fin1 <- fin[order.score,]
fin1$rank <- rank(fin1$vmc1)

A <- fin1$vmc1
B <- fin1$vmc2

d <- data.frame(x=1:25,y=A)
d2 <- data.frame(x=1:25,y=B)
dd <- rbind(cbind(d, case = "1st Semester \t\t"), cbind(d2, case = "2nd Semester midsem (Scaled)"))
ddsmooth <- plyr::ddply(dd, .(case), function(k) as.data.frame(spline(k)))

library(ggplot2)

comp <- ggplot(dd, aes(x, y)) +
  geom_line(aes(colour = case, size = case), data = ddsmooth,lwd=1.1) +
  geom_point(colour = "black") +
  scale_colour_manual(values = c("blue", "red"), name = " ") +
  scale_size_manual(values = c(1, 1), name = " ") +
  scale_x_continuous(breaks = 1:25) +
#  scale_y_continuous(breaks = 0:100) +
  theme(legend.position = "bottom") +
  xlab("\n Rank (Reversed) of student in 1st Semester") +
  ylab("Marks Obtained")

print(comp)