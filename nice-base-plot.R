accu1 <- c(75,80,92,83,67)/100
accu2 <- c(83,62.5,83,58,100)/100
accu3 <- c(83,100,75,89,83)/100

plot(accu1,type = "o",col = "red",
     main="Line Chart for Accuracy Rates of the Participants",
     xaxt = "none", yaxt = "none", lwd = 2, ylim = c(0,1), xlab = " ", 
     ylab = " ")

lines(accu2, type = "o", col = "blue", lwd = 2)
lines(accu3, type = "o", col = "black", lwd = 2)

axis(1, at=1:5, labels=c("Similarities","Anagrams","Syllogistic Reasoning",
                         "Data Sufficiency","Coding"), font = 2)
axis(2, at=seq(0,1,0.2), las = 2, font = 2)

abline(h=seq(0,1,0.1), v=seq(1,5, 1), lty=3, col="gray")
mtext(side=2, line=2, "Accuracy Rate\n", col="black", 
      font=2,cex=0.8)

legend(4.2, 0.24, legend=c("Participant 1", "Participant 2", "Participant 3"),
       col=c("red", "blue","black"), lty=1, cex=1, lwd = 2)