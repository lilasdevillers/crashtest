#PART 1
#suspicion of collinearity
base <- read.csv("programme final .csv", sep=";")
View(base)
x <- base[,-c(24:33)]
x <- x[,-c(14:22)]
x <- x[,-c(9:12)]
x <- x[,-c(4:7)]
x <- x[,-c(1:2)]
colnames(x)[2] <- "Rye"
colnames(x)[1] <- "Wheat"
colnames(x)[3] <- "Buckwheat"
colnames(x)[4] <- "Potato"
x
mcor <- cor(x)
mcor
library(corrplot)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=50)
#We can see a strong correlation of the different cereals with each other.

#Comparaison of the prices of Tulle and Uzerche 
rm(x, mcor)
x = strptime(base$dates, "%d/%m/%Y")
limitations = c(as.POSIXct(strptime("01/05/1874","%d/%m/%Y")),as.POSIXct(strptime("01/05/1883","%d/%m/%Y")))
  # WHEAT
y = base$wheat.tl
z = base$wheat.uz
par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,34), axes=F, cex=1, type="o",col="black", main="comparison for the Wheat")
axis(2, ylim=c(0,34),col="black")
mtext("Wheat price in Tulle",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=16,  xlab="", ylab="",ylim=c(0,34), axes=F, type="p", col="red")
mtext("Wheat price in Uzerche",side=4,col="red",line=2.5)
axis(4, ylim=c(0,34), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)
r <- lm(wheat.uz~wheat.tl, base)
resume <- summary(r)
w<-unlist(resume[8])
legend("topright", legend =paste("R²=",round(w, digits=3)))
  #RYE
y = base$rye.tl
z = base$rye.uz
par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,28), axes=F, cex=1, type="o",col="black", main="comparison for the Rye")
axis(2, ylim=c(0,28),col="black")
mtext("rye price in Tulle",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=16,  xlab="", ylab="",ylim=c(0,28), axes=F, type="p", col="red")
mtext("rye price in Uzerche",side=4,col="red",line=2.5)
axis(4, ylim=c(0,28), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)
r <- lm(rye.uz~rye.tl, base)
resume <- summary(r)
w<-unlist(resume[8])
legend("topright", legend =paste("R²=",round(w, digits=3)))
  #Buckwheat
y = base$bwheat.tl
z = base$bwheat.uz
par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,22), axes=F, cex=1, type="o",col="black", main="comparison for the Buckwheat")
axis(2, ylim=c(0,22),col="black")
mtext("buckwheat price in Tulle",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=16,  xlab="", ylab="",ylim=c(0,22), axes=F, type="p", col="red")
mtext("buckwheat price in Uzerche",side=4,col="red",line=2.5)
axis(4, ylim=c(0,22), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)
r <- lm(bwheat.uz~bwheat.tl, base)
resume <- summary(r)
w<-unlist(resume[8])
legend("topright", legend =paste("R²=",round(w, digits=3)))
  #Chesnut
y = base$ches.tl
z = base$ches.uz
par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,10), axes=F, cex=1, type="o",col="black", main="comparison for the Chesnut")
axis(2, ylim=c(0,10),col="black")
mtext("chesnut price in Tulle",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=16,  xlab="", ylab="",ylim=c(0,10), axes=F, type="p", col="red")
mtext("chesnut price in Uzerche",side=4,col="red",line=2.5)
axis(4, ylim=c(0,10), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)
r <- lm(ches.uz~ches.tl, base)
resume <- summary(r)
w<-unlist(resume[8])
legend("topright", legend =paste("R²=",round(w, digits=3)))
  #Potato
y = base$pota.tl
z = base$pota.uz
par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,10), axes=F, cex=1, type="o",col="black", main="comparison for the Potato")
axis(2, ylim=c(0,10),col="black")
mtext("potato price in Tulle",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=16,  xlab="", ylab="",ylim=c(0,10), axes=F, type="p", col="red")
mtext("potato price in Uzerche",side=4,col="red",line=2.5)
axis(4, ylim=c(0,10), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)
r <- lm(pota.uz~pota.tl, base)
resume <- summary(r)
w<-unlist(resume[8])
legend("topright", legend =paste("R²=",round(w, digits=3)))
  #Cereals (wheighted)
y = base$cer.tl
z = base$cer.uz
par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,28), axes=F, cex=1, type="o",col="black", main="comparison for Weighted average cereals")
axis(2, ylim=c(0,28),col="black")
mtext("cereals price in Tulle",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=16,  xlab="", ylab="",ylim=c(0,28), axes=F, type="p", col="red")
mtext("cereals price in Uzerche",side=4,col="red",line=2.5)
axis(4, ylim=c(0,28), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)
r <- lm(cer.uz~cer.tl, base)
resume <- summary(r)
w<-unlist(resume[8])
legend("topright", legend =paste("R²=",round(w, digits=3)))


#Correlation between may and the whole year (to justify the relevance of may) 
rm(list=ls())
base <- base <- programme.final. <- read.csv("programme final .csv", sep=";")

#Wheat
plot.new()
par(mar=c(5,4,3,4))
position = barplot(base$wheat.y,col="grey",names.arg=base$date,axes=F,ylab="",xlab="",main="Comparison for the Wheat",ylim=c(0,32), las=2,space=0,cex.main=0.8)
axis(2,col="black",at=seq(0,32, by=2))
par(new=TRUE,mar=c(4,4,3,4))
plot(position, base$wheat.tl,col="red",type="o",lwd=2,pch=16,axes=F,ylab="",xlab="",ylim=c(0,32), xlim=c(0,length(base$wheat1)),yaxs ="i")
axis(4,col.axis="red",col="red",at=seq(0,32, by=2))
box();grid()
mtext("Annual average price",side=2,line=2,cex=1.1)
mtext("May price",side=4,col="red",line=2,cex=1.1)
r <- lm(wheat.y~wheat.tl, base)
resume <- summary(r)
w<-unlist(resume[8])
plot(wheat.y~wheat.tl, base, ylab="Average annual price",xlab="Price in May", main="Wheat", ylim=c(15,32),xlim=c(15,30))
abline(r, col="red", lwd=3)
legend("topright", legend =paste("R²=",round(w, digits=3)))
#Rye
plot.new()
par(mar=c(5,4,3,4))
position = barplot(base$rye.y,col="grey",names.arg=base$date,axes=F,ylab="",xlab="",main="Comparison for the Rye",ylim=c(0,24), las=2,space=0,cex.main=0.8)
axis(2,col="black",at=seq(0,24, by=2))
par(new=TRUE,mar=c(4,4,3,4))
plot(position, base$rye.tl,col="red",type="o",lwd=2,pch=16,axes=F,ylab="",xlab="",ylim=c(0,24), xlim=c(0,length(base$rye.tl)),yaxs ="i")
axis(4,col.axis="red",col="red",at=seq(0,24, by=2))
box();grid()
mtext("Annual average price",side=2,line=2,cex=1.1)
mtext("May price",side=4,col="red",line=2,cex=1.1)
r <- lm(rye.y~rye.tl, base)
resume <- summary(r)
w<-unlist(resume[8])
plot(rye.y~rye.tl, base, ylab="Average annual price",xlab="Price in May", main="Rye", ylim=c(13,24),xlim=c(13,25))
abline(r, col="red", lwd=3)
legend("topright", legend =paste("R²=",round(w, digits=3)))
#Buckwheat
plot.new()
par(mar=c(5,4,3,4))
position = barplot(base$bwheat.y,col="grey",names.arg=base$date,axes=F,ylab="",xlab="",main="Comparison for the Buckwheat",ylim=c(0,24), las=2,space=0,cex.main=0.8)
axis(2,col="black",at=seq(0,24, by=2))
par(new=TRUE,mar=c(4,4,3,4))
plot(position, base$bwheat.tl,col="red",type="o",lwd=2,pch=16,axes=F,ylab="",xlab="",ylim=c(0,24), xlim=c(0,length(base$bwheat.tl)),yaxs ="i")
axis(4,col.axis="red",col="red",at=seq(0,24, by=2))
box();grid()
mtext("Annual average price",side=2,line=2,cex=1.1)
mtext("May price",side=4,col="red",line=2,cex=1.1)
r <- lm(bwheat.y~bwheat.tl, base)
resume <- summary(r)
w<-unlist(resume[8])
plot(bwheat.y~bwheat.tl, base, ylab="Average annual price",xlab="Price in May", main="Buckwheat", ylim=c(8,20),xlim=c(8,20))
abline(r, col="red", lwd=3)
legend("topright", legend =paste("R²=",round(w, digits=3)))
#Chesnut
plot.new()
par(mar=c(5,4,3,4))
position = barplot(base$ches.y,col="grey",names.arg=base$date,axes=F,ylab="",xlab="",main="Comparison for the Chesnut",ylim=c(0,10), las=2,space=0,cex.main=0.8)
axis(2,col="black",at=seq(0,10, by=2))
par(new=TRUE,mar=c(4,4,3,4))
plot(position, base$ches.tl,col="red",type="o",lwd=2,pch=16,axes=F,ylab="",xlab="",ylim=c(0,10), xlim=c(0,length(base$ches.tl)),yaxs ="i")
axis(4,col.axis="red",col="red",at=seq(0,10, by=2))
box();grid()
mtext("Annual average price",side=2,line=2,cex=1.1)
mtext("May price",side=4,col="red",line=2,cex=1.1)
r <- lm(ches.y~ches.tl, base)
resume <- summary(r)
w<-unlist(resume[8])
plot(ches.y~ches.tl, base, ylab="Average annual price",xlab="Price in May", main="Chesnut", ylim=c(0,10),xlim=c(0,10))
abline(r, col="red", lwd=3)
legend("topright", legend =paste("R²=",round(w, digits=3)))
#Potato
plot.new()
par(mar=c(5,4,3,4))
position = barplot(base$pota.y,col="grey",names.arg=base$date,axes=F,ylab="",xlab="",main="Comparison for the Potato",ylim=c(0,10), las=2,space=0,cex.main=0.8)
axis(2,col="black",at=seq(0,10, by=2))
par(new=TRUE,mar=c(4,4,3,4))
plot(position, base$pota.tl,col="red",type="o",lwd=2,pch=16,axes=F,ylab="",xlab="",ylim=c(0,10), xlim=c(0,length(base$pota.tl)),yaxs ="i")
axis(4,col.axis="red",col="red",at=seq(0,10, by=2))
box();grid()
mtext("Annual average price",side=2,line=2,cex=1.1)
mtext("May price",side=4,col="red",line=2,cex=1.1)
r <- lm(pota.y~pota.tl, base)
resume <- summary(r)
w<-unlist(resume[8])
plot(pota.y~pota.tl, base, ylab="Average annual price",xlab="Price in May", main="Potato", ylim=c(0,10),xlim=c(0,10))
abline(r, col="red", lwd=3)
legend("topright", legend =paste("R²=",round(w, digits=3)))

# Child mortality rate 
base <- base <- programme.final. <- read.csv("programme final .csv", sep=";")
x = strptime(base$dates, "%d/%m/%Y")
limitations = c(as.POSIXct(strptime("01/05/1874","%d/%m/%Y")),as.POSIXct(strptime("01/05/1883","%d/%m/%Y")))
y = base$tm
z = base$MR.inf
par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,50), axes=F, cex=1, type="o",col="black", main="Comparison of different mortality rates")
axis(2, ylim=c(0,50),col="black")
mtext("Population mortality rate",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=16,  xlab="", ylab="",ylim=c(0,260), axes=F, type="o", col="red")
mtext("Infant mortality rate",side=4,col="red",line=2.5)
axis(4, ylim=c(0,260), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)

#Linear regression of different mortality rates 
r <- lm(MR.inf~tm, base)
plot(MR.inf~tm, base, xlab='Population mortality rate', ylab='Infant mortality rate')
abline(r, col="red")
summary(r)

#PART 2 
#mortality rate and the price of the same year. 
#cereals
pabsolu <- base <- programme.final. <- read.csv("programme final .csv", sep=";")
base <- pabsolu
x = strptime(base$dates, "%d/%m/%Y")
limitations = c(as.POSIXct(strptime("01/05/1874","%d/%m/%Y")),as.POSIXct(strptime("01/05/1883","%d/%m/%Y")))
y = base$tm
z = base$cer.tl
par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,40), axes=F, cex=1, type="o",col="black", main="Link between the average price of cereals and death rate")
axis(2, ylim=c(0,35),col="black")
mtext("Death rate",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=15,  xlab="", ylab="",ylim=c(0,26), axes=F, type="o", col="red")
mtext("Average price of cereals",side=4,col="red",line=2.5)
axis(4, ylim=c(0,26), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)

#chesnuts
x = strptime(base$dates, "%d/%m/%Y")
limitations = c(as.POSIXct(strptime("01/05/1874","%d/%m/%Y")),as.POSIXct(strptime("01/05/1883","%d/%m/%Y")))
y = base$tm
z = base$ches.tl
par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,40), axes=F, cex=1, type="o",col="black", main="price of chesnuts and death rate")
axis(2, ylim=c(0,40),col="black")
mtext("Death rate",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=15,  xlab="", ylab="",ylim=c(0,6.7), axes=F, type="o", col="red")
mtext("Price of chesnuts",side=4,col="red",line=2.5)
axis(4, ylim=c(0,6.7), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)

#potatoes
x = strptime(base$dates, "%d/%m/%Y")
limitations = c(as.POSIXct(strptime("01/05/1874","%d/%m/%Y")),as.POSIXct(strptime("01/05/1883","%d/%m/%Y")))
y = base$tm
z = base$pota.tl
par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(10,46), axes=F, cex=1, type="o",col="black", main="price of potatoes and death rate")
axis(2, ylim=c(0,0.052),col="black")
mtext("Death rate",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=15,  xlab="", ylab="",ylim=c(0,8), axes=F, type="o", col="red")
mtext("Price of potatoes",side=4,col="red",line=2.5)
axis(4, ylim=c(0,8), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)

# lm cereals
r <- lm(tm ~ cer.tl, base)
resume <- summary(r)
w<-unlist(resume[8])
plot(tm ~ cer.tl, base, ylab="Death rate",xlab="Price in May", main="Cereals", ylim=c(0,45),xlim=c(12,25))
abline(r,col="red", lwd=3)
legend("topright", legend =paste("R²=",round(w, digits=3)))

#lm chesnut 
r <- lm(tm ~ ches.tl, base)
resume <- summary(r)
w<-unlist(resume[8])
plot(tm ~ ches.tl, base, ylab="Death rate",xlab="Price in May", main="Chesnut", ylim=c(0,45),xlim=c(0,10))
abline(r,col="red", lwd=3)
legend("topright", legend =paste("R²=",round(w, digits=3)))

#lm potato
r <- lm(tm ~ pota.tl, base)
resume <- summary(r)
w<-unlist(resume[8])
plot(tm ~ pota.tl, base, ylab="Death rate",xlab="Price in May", main="Potato", ylim=c(0,45),xlim=c(0,10))
abline(r,col="red", lwd=3)
legend("topright", legend =paste("R²=",round(w, digits=3)))

# All CCF
# CCF Cereals
rm(list=ls())
base <- base <- programme.final. <- read.csv("programme final .csv", sep=";")
ccf(base$tm, base$cer.tl,lag.max = NULL,type = c("correlation", "covariance"),plot = TRUE,na.action = na.contiguous, main="Cross corelation between Cereals and mortality rate", ylab="")

# Lag Cereals
x = strptime(base$dates, "%d/%m/%Y")
limitations = c(as.POSIXct(strptime("01/05/1874","%d/%m/%Y")),as.POSIXct(strptime("01/05/1883","%d/%m/%Y")))
y = base$tm
z = base$cer.tl
t = base$cer1

par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,40), axes=F, cex=1, type="o",col="black", main="Link between Cereals price and mortality rate")
axis(2, ylim=c(0,35),col="black")
mtext("Mortality rate",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=15,  xlab="", ylab="",ylim=c(0,26), axes=F, type="o", col="red")
par(new=T)
plot(x, t, pch=15,  xlab="", ylab="",ylim=c(0,26), axes=F, type="o", col="blue")
mtext("Cereals price in t and t+1",side=4,col="black",line=2.5)
axis(4, ylim=c(0,26), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)

# CCF Wheat
ccf(base$tm, base$wheat.tl,lag.max = NULL,type = c("correlation", "covariance"),plot = TRUE,na.action = na.contiguous, main="Cross corelation between Wheat and mortality rate", ylab="")

# Lag Wheat
x = strptime(base$dates, "%d/%m/%Y")
limitations = c(as.POSIXct(strptime("01/05/1874","%d/%m/%Y")),as.POSIXct(strptime("01/05/1883","%d/%m/%Y")))
y = base$tm
z = base$wheat.tl
t = base$wheat1

par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,40), axes=F, cex=1, type="o",col="black", main="Link between Wheat price and mortality rate")
axis(2, ylim=c(0,35),col="black")
mtext("Mortality rate",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=15,  xlab="", ylab="",ylim=c(0,32), axes=F, type="o", col="red")
par(new=T)
plot(x, t, pch=15,  xlab="", ylab="",ylim=c(0,32), axes=F, type="o", col="blue")
mtext("Wheat price in t and t+1",side=4,col="black",line=2.5)
axis(4, ylim=c(0,32), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)

# CCF Rye
ccf(base$tm, base$rye.tl,lag.max = NULL,type = c("correlation", "covariance"),plot = TRUE,na.action = na.contiguous, main="Cross corelation between Rye and mortality rate", ylab="")

# Lag Rye
x = strptime(base$dates, "%d/%m/%Y")
limitations = c(as.POSIXct(strptime("01/05/1874","%d/%m/%Y")),as.POSIXct(strptime("01/05/1883","%d/%m/%Y")))
y = base$tm
z = base$rye.tl
t = base$rye1

par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,40), axes=F, cex=1, type="o",col="black", main="Link between Rye price and mortality rate")
axis(2, ylim=c(0,35),col="black")
mtext("Mortality rate",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=15,  xlab="", ylab="",ylim=c(0,25), axes=F, type="o", col="red")
par(new=T)
plot(x, t, pch=15,  xlab="", ylab="",ylim=c(0,25), axes=F, type="o", col="blue")
mtext("Rye price in t and t+1",side=4,col="black",line=2.5)
axis(4, ylim=c(0,25), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)


# CCF Buckwheat
ccf(base$tm, base$bwheat.tl,lag.max = NULL,type = c("correlation", "covariance"),plot = TRUE,na.action = na.contiguous, main="Cross corelation between Buckwheat and mortality rate", ylab="")

# Lag Buckwheat
x = strptime(base$dates, "%d/%m/%Y")
limitations = c(as.POSIXct(strptime("01/05/1874","%d/%m/%Y")),as.POSIXct(strptime("01/05/1883","%d/%m/%Y")))
y = base$tm
z = base$bwheat.tl
t = base$bwheat1

par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,40), axes=F, cex=1, type="o",col="black", main="Link between Buckwheat price and mortality rate")
axis(2, ylim=c(0,35),col="black")
mtext("Mortality rate",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=15,  xlab="", ylab="",ylim=c(0,22), axes=F, type="o", col="red")
par(new=T)
plot(x, t, pch=15,  xlab="", ylab="",ylim=c(0,22), axes=F, type="o", col="blue")
mtext("Buckwheat price in t and t+1",side=4,col="black",line=2.5)
axis(4, ylim=c(0,22), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)

# CCF Chesnut
ccf(base$tm, base$ches.tl,lag.max = NULL,type = c("correlation", "covariance"),plot = TRUE,na.action = na.contiguous, main="Cross corelation between Chesnut and mortality rate", ylab="")

# Lag Chesnut
x = strptime(base$dates, "%d/%m/%Y")
limitations = c(as.POSIXct(strptime("01/05/1874","%d/%m/%Y")),as.POSIXct(strptime("01/05/1883","%d/%m/%Y")))
y = base$tm
z = base$ches.tl
t = base$ches2

par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,40), axes=F, cex=1, type="o",col="black", main="Link between Chesnut price and mortality rate")
axis(2, ylim=c(0,35),col="black")
mtext("Mortality rate",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=15,  xlab="", ylab="",ylim=c(0,7), axes=F, type="o", col="red")
par(new=T)
plot(x, t, pch=15,  xlab="", ylab="",ylim=c(0,7), axes=F, type="o", col="blue")
mtext("Chesnut price in t and t+1",side=4,col="black",line=2.5)
axis(4, ylim=c(0,7), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)

# CCF Potato
ccf(base$tm, base$pota.tl,lag.max = NULL,type = c("correlation", "covariance"),plot = TRUE,na.action = na.contiguous, main="Cross corelation between Potato and mortality rate", ylab="")

# Lag Potato
x = strptime(base$dates, "%d/%m/%Y")
limitations = c(as.POSIXct(strptime("01/05/1874","%d/%m/%Y")),as.POSIXct(strptime("01/05/1883","%d/%m/%Y")))
y = base$tm
z = base$pota.tl
t = base$pota2

par(mar=c(5.1,4.1,4.1,4))
plot(x, y, pch=1, xlab="", ylab="",xlim=limitations, ylim=c(0,40), axes=F, cex=1, type="o",col="black", main="Link between Potato price and mortality rate")
axis(2, ylim=c(0,35),col="black")
mtext("Mortality rate",side=2,line=2.5)
box()
par(new=T)
plot(x, z, pch=15,  xlab="", ylab="",ylim=c(0,9), axes=F, type="o", col="red")
par(new=T)
plot(x, t, pch=15,  xlab="", ylab="",ylim=c(0,9), axes=F, type="o", col="blue")
mtext("Potato price in t and t+1",side=4,col="black",line=2.5)
axis(4, ylim=c(0,9), col="red",col.axis="red")
axis.POSIXct(1, at=seq(from=strptime("01/05/1874","%d/%m/%Y"), to=strptime("01/05/1883","%d/%m/%Y" ), by="year"), format="%m/%Y", las=1)

# Multiple regressions 
r <- lm(tm ~ cer.tl + ches.tl + pota.tl, base)
resume <- summary(r)
summary(r)

s <- lm(tm ~ cer1 + ches1 + pota1, base)
resume <- summary(s)
summary(s)

t <- lm(tm ~ cer2 + ches2 + pota2, base)
resume <- summary(t)
summary(t)

# Multiple regressions with different lag  

s <- lm(tm ~ cer1 + ches2 + pota2, base)
resume <- summary(s)
summary(s)

# Log-regression without lag for all population 
  #Wheat
z <- lm(log(tm) ~ log(wheat.tl), base)
summary(z)
  #Rye
z <- lm(log(tm) ~ log(rye.tl), base)
summary(z)
  #Buckwheat
z <- lm(log(tm) ~ log(bwheat.tl), base)
summary(z)
  #Chesnut
z <- lm(log(tm) ~ log(ches.tl), base)
summary(z)
  #Potato
z <- lm(log(tm) ~ log(pota.tl), base)
summary(z)
  #Cereals
z <- lm(log(tm) ~ log(cer.tl), base)
summary(z)

# Log-regression with a lag  
  #Wheat
z <- lm(log(tm) ~ log(wheat1), base)
summary(z)
  #Rye
z <- lm(log(tm) ~ log(rye1), base)
summary(z)
  #Buckwheat
z <- lm(log(tm) ~ log(bwheat1), base)
summary(z)
  #Chesnut
z <- lm(log(tm) ~ log(ches2), base)
summary(z)
  #Potato
z <- lm(log(tm) ~ log(pota2), base)
summary(z)
  #Cereals
z <- lm(log(tm) ~ log(cer1), base)
summary(z)

# Log-regression without lag for child mortality rate 
#Wheat
z <- lm(log(MR.inf) ~ log(wheat.tl), base)
summary(z)
#Rye
z <- lm(log(MR.inf) ~ log(rye.tl), base)
summary(z)
#Buckwheat
z <- lm(log(MR.inf) ~ log(bwheat.tl), base)
summary(z)
#Chesnut
z <- lm(log(MR.inf) ~ log(ches.tl), base)
summary(z)
#Potato
z <- lm(log(MR.inf) ~ log(pota.tl), base)
summary(z)
#Cereals
z <- lm(log(MR.inf) ~ log(cer.tl), base)
summary(z)

# Log-regression with lag for child mortality rate 
#Wheat
z <- lm(log(MR.inf) ~ log(wheat1), base)
summary(z)
#Rye
z <- lm(log(MR.inf) ~ log(rye1), base)
summary(z)
#Buckwheat
z <- lm(log(MR.inf) ~ log(bwheat1), base)
summary(z)
#Chesnut
z <- lm(log(MR.inf) ~ log(ches1), base)
summary(z)
#Potato
z <- lm(log(MR.inf) ~ log(pota1), base)
summary(z)
#Cereals
z <- lm(log(MR.inf) ~ log(cer1), base)
summary(z)
rm(list = ls())



#PART3
#individual analysis
#import data named "finalbase"
finalbase <- read.csv("finalbase.csv", sep=";")
#in the data,pdt1 = the price of potato in t 
#in the data, pdtc1= the price of potato in t for cultivator
#"",pdtc2 = the price of potato in t-1 for cultivator

#multiple regression with one good and several years

#for chestnut
r <- lm(age~cha1+cha2+cha3,finalbase)
coef(r)
summary(r)

#for potato
r <- lm(age~pdt1+pdt2+pdt3,finalbase)
coef(r)
summary(r)

#for cereal
r <- lm(age~cer1+cer2+cer3,finalbase)
coef(r)
summary(r)
#regression simple
r <- lm(age~cha3,finalbase)
coef(r)
summary(r)
plot(age~cha3,finalbase, ylab="age of death",xlab="prices of chestnut in t-2", main="chestnut prices impact on death", ylim=c(0,100),xlim=c(1,10))
abline(r, col="red", lwd=3)

#multiple regression with several goods and one year
#h=0
r <- lm(age~cha1+pdt1+cer1,finalbase)
coef(r)
summary(r)
#h=1
r <- lm(age~cha2+pdt2+cer2,finalbase)
coef(r)
summary(r)
#h=2
r <- lm(age~cha3+pdt3+cer3,finalbase)
coef(r)
summary(r)

#population group
#cultivator
#h=0

r <- lm(cultivator.age~chac1+pdtc1+cerc1,finalbase)
coef(r)
summary(r)

#h=1

r <- lm(cultivator.age~chac2+pdtc2+cerc2,finalbase)
coef(r)
summary(r)
#h=2
r <- lm(cultivator.age~chac3+pdtc3+cerc3,finalbase)
coef(r)
summary(r)

#owner

#h=0
r <- lm(owner.age~chao1+pdto1+cero1,finalbase)
coef(r)
summary(r)

#h=1
r <- lm(owner.age~chao2+pdto2+cero2,finalbase)
coef(r)
summary(r)

#h=2
r <- lm(owner.age~chao3+pdto3+cero3,finalbase)
coef(r)
summary(r)

#gender
#men :
#h=0
r <- lm(men.age~cham1+pdtm1+cerm1,finalbase)
coef(r)
summary(r)

#h=1
r <- lm(men.age~cham2+pdtm2+cerm2,finalbase)
coef(r)
summary(r)

#h=2
r <- lm(men.age~cham3+pdtm3+cerm3,finalbase)
coef(r)
summary(r)


#women
#h=0
r <- lm(women.age~chaw1+pdtw1+cerw1,finalbase)
coef(r)
summary(r)

#h=1
r <- lm(women.age~chaw2+pdtw2+cerw2,finalbase)
coef(r)
summary(r)

#h=2
r <- lm(women.age~chaw3+pdtw3+cerw3,finalbase)
coef(r)
summary(r)

