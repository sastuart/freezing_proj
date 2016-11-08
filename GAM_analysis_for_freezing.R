
library(nlme)
library(mgcv)
library(scales)
library(ggplot2)
source("smoothplot.R")

setwd("~/Dropbox/F/Ch3 - Freezing/Freezing Project Data/freezing_proj")

ft0.data <- read.csv("/Users/sassy/Dropbox/F/Ch3 - Freezing/Freezing Project Data/Compiled FT Data - All Data 0s.csv", header=TRUE)

str(ft0.data)


gfit1 <- gam(Fv.Fm ~ s(Temp, k=5), data=ft0.data)
summary(gfit1)

smoothplot(Temp, Fv.Fm, Site, data=ft0.data, kgam=5, linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"))

FvFmLoss.name <- expression("Relative Loss in F"[v]*"/F"[m]*"")
pdf(file = "GAM freezing by site.pdf", width = 9.02, height = 4.96)
par(oma=c(1,1,0,1), mar=c(4,4,1,1) + 0.1)
par(cex=1.25)
smoothplot(Temp, Fv.Fm, Site, data=ft0.data, kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"), xlab="Temperature",
           ylab=FvFmLoss.name, ylim=c(0,0.9))
dev.off()

ft0.mfcc <- subset(ft0.data, Site != "FM")
ft0.mffm <- subset(ft0.data, Site != "CC")
ft0.fmcc <- subset(ft0.data, Site != "MF")

ft0.mf <- subset(ft0.data, (Site != "FM" & Site != "CC"))
ft0.fm <- subset(ft0.data, (Site != "MF" & Site != "CC"))
ft0.cc <- subset(ft0.data, (Site != "FM" & Site != "MF"))

smoothplot(Temp, Fv.Fm, Site, data=ft0.mfcc, kgam=5, linecols=c("DarkGreen","LightBlue"),
           pointcols=c("DarkGreen","LightBlue"))

smoothplot(Temp, Fv.Fm, Site, data=ft0.mffm, kgam=5, linecols=c("Orange","LightBlue"),
           pointcols=c("Orange","LightBlue"))

smoothplot(Temp, Fv.Fm, Site, data=ft0.fmcc, kgam=5, linecols=c("DarkGreen","Orange"),
           pointcols=c("DarkGreen","Orange"))

pdf(file = "GAM freezing wetcold.pdf", width = 9.02, height = 4.96)
par(oma=c(1,1,0,1), mar=c(4,4,1,1) + 0.1)
par(cex=1.25)
smoothplot(Temp, Fv.Fm, Site, data=ft0.mfcc, kgam=5, linecols=c("DarkGreen","LightBlue"),
           pointcols=c("DarkGreen","LightBlue"),
           xlab="Temperature", ylab=FvFmLoss.name, ylim=c(0,0.9))
dev.off()

pdf(file = "GAM freezing drycold.pdf", width = 9.02, height = 4.96)
par(oma=c(1,1,0,1), mar=c(4,4,1,1) + 0.1)
par(cex=1.25)
smoothplot(Temp, Fv.Fm, Site, data=ft0.mffm, kgam=5, linecols=c("Orange","LightBlue"),
           pointcols=c("Orange","LightBlue"),
           xlab="Temperature", ylab=FvFmLoss.name, ylim=c(0,0.9))
dev.off()

pdf(file = "GAM freezing drywet.pdf", width = 9.02, height = 4.96)
par(oma=c(1,1,0,1), mar=c(4,4,1,1) + 0.1)
par(cex=1.25)
smoothplot(Temp, Fv.Fm, Site, data=ft0.fmcc, kgam=5, linecols=c("DarkGreen","Orange"),
           pointcols=c("DarkGreen","Orange"),
           xlab="Temperature", ylab=FvFmLoss.name, ylim=c(0,0.9))
dev.off()


smoothplot(Temp, Fv.Fm, Site, data=ft0.mf, kgam=5, linecols=c("LightBlue"),
           pointcols=c("LightBlue"))

smoothplot(Temp, Fv.Fm, Site, data=ft0.fm, kgam=5, linecols=c("Orange"),
           pointcols=c("Orange"))

smoothplot(Temp, Fv.Fm, Site, data=ft0.cc, kgam=5, linecols=c("DarkGreen"),
           pointcols=c("DarkGreen"))

smoothplot(Temp, Fv.Fm, Family, data=ft0.data, kgam=5)
interaction(ft0.data$Family,ft0.data$Site)
smoothplot(Temp, Fv.Fm, Family, data=ft0.data, kgam=5)
smoothplot(Temp, Fv.Fm, interaction(ft0.data$Family,ft0.data$Site),data=ft0.data, kgam=5)

smoothplot(Temp, Fv.Fm, Family, data=ft0.data, kgam=12)

smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Fabaceae"), kgam=12, 
           linecols=c("DarkGreen","Orange","LightBlue"), 
           pointcols=c("DarkGreen","Orange","LightBlue"),
           ylim=c(0,0.9))
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Myrtaceae"), kgam=12, 
           linecols=c("DarkGreen","Orange","LightBlue"), 
           pointcols=c("DarkGreen","Orange","LightBlue"),
           ylim=c(0,0.9))
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Oleaceae"), kgam=12, 
           linecols=c("DarkGreen","Orange","LightBlue"), 
           pointcols=c("DarkGreen","Orange","LightBlue"),
           ylim=c(0,0.9))
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Pittosporaceae"), kgam=12, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           ylim=c(0,0.9))
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Rhamnaceae"), kgam=12, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           ylim=c(0,0.9))
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Rubiaceae"), kgam=12, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           ylim=c(0,0.9))
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Rutaceae"), kgam=12, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           ylim=c(0,0.9))

# Rubiaceae and Oleaceae have different sampling sites/lower sample sizes

# Most resisitant
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Fabaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="Temperature", ylab=FvFmLoss.name,
           ylim=c(0,0.9))
text(0,0.01, adj=c(0,0), "Fabaceae", cex=1.3)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Pittosporaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="Temperature", ylab=FvFmLoss.name,
           ylim=c(0,0.9))
text(-1,0.01, adj=c(0,0), "Pittosporaceae", cex=1.3)
 plot.new()
# Moderately Resistant
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Rhamnaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="Temperature", ylab=FvFmLoss.name,
           ylim=c(0,0.9))
text(0,0.01, adj=c(0,0), "Rhamnaceae", cex=1.3)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Rutaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="Temperature", ylab=FvFmLoss.name,
           ylim=c(0,0.9))
text(0,0.01, adj=c(0,0), "Rutaceae", cex=1.3)
plot.new()
# Sensitive Families 
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Oleaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="Temperature", ylab=FvFmLoss.name,
           ylim=c(0,0.9))
text(0,0.01, adj=c(0,0), "Oleaceae", cex=1.3)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Rubiaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="Temperature", ylab=FvFmLoss.name,
           ylim=c(0,0.9))
text(0,0.01, adj=c(0,0), "Rubiaceae", cex=1.3)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Myrtaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="Temperature", ylab=FvFmLoss.name,
           ylim=c(0,0.9))
text(0,0.01, adj=c(0,0), "Myrtaceae", cex=1.3)


levels(ft0.data$Site)
levels(ft0.data$Family)

par(mfrow=c(1,1))


####### Figure for talk, all families #####
pdf(file = "GAM all Families.pdf", width = 9.02, height = 4.96)
# par(oma=c(1,1,0,1), mar=c(4,4,1,1) + 0.1)
par(oma=c(0.1,0.1,0.1,0.1), mar=c(3,3,0.5,0.5) + 0.1)
par(cex=1.25)
par(mfrow=c(3,3))
# Most resisitant
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Fabaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9))
title(ylab=FvFmLoss.name, line=2)
text(5,0.05, pos=2, "Fabaceae", cex=1.3)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Pittosporaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9))
text(5,0.05, pos=2, "Pittosporaceae", cex=1.3)
plot.new()
# Moderately Resistant
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Rhamnaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9))
title(ylab=FvFmLoss.name, line=2)
text(5,0.05, pos=2, "Rhamnaceae", cex=1.3)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Rutaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9))
text(5,0.05, pos=2, "Rutaceae", cex=1.3)
plot.new()
# Sensitive Families 
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Oleaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9))
title(xlab = "Temperature", line = 2)
title(ylab=FvFmLoss.name, line=2)
text(5,0.05, pos=2, "Oleaceae", cex=1.3)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Rubiaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9))
title(xlab = "Temperature", line = 2)
text(5,0.05, pos=2, "Rubiaceae", cex=1.3)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Myrtaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9))
title(xlab = "Temperature", line = 2)
text(5,0.05, pos=2, "Myrtaceae", cex=1.3)
dev.off()


ft.subset <- subset(ft0.data, Family != "Rhamnaceae" & Family != "Oleaceae")

gfit.sub <- gam(Fv.Fm ~ s(Temp, k=5), data=ft.subset)


smoothplot(Temp, Fv.Fm, Site, data=ft.subset, kgam=5, linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"))

####### Figure for paper, five families with complete data ##### ------
pdf(file = "GAM for just five Families.pdf", width = 12, height = 9)
# par(oma=c(1,1,0,1), mar=c(4,4,1,1) + 0.1)
par(oma=c(0.1,0.1,0.1,0.1))
par(cex.axis=1.5, cex.lab=1.8)
par(mfrow=c(3,3))
# Most resisitant
plot(c(1:10),c(1:10),xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
text(5,5,"Most Resistant", cex=2)
par(mar=c(3,4,0.5,0.5) + 0.1)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Fabaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9), cex=1.5)
title(ylab=FvFmLoss.name, line=2.5, cex=3)
text(5,0.05, pos=2, "Fabaceae", cex=2)
par(mar=c(3,3,0.5,1.5) + 0.1)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Pittosporaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9), cex=1.5)
text(5,0.05, pos=2, "Pittosporaceae", cex=2)
# Moderately Resistant
plot(c(1:10),c(1:10),xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
text(5,5,"Moderately Resistant", cex=2)
par(mar=c(3,4,0.5,0.5) + 0.1)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Rutaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9), cex=1.5)
text(5,0.05, pos=2, "Rutaceae", cex=2)
title(ylab=FvFmLoss.name, line=2.5, cex=3)
plot.new()
# Sensitive Families 
par(mar=c(3,4,0.5,0.5) + 0.1)
plot(c(1:10),c(1:10),xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
text(5,5,"Sensitive", cex=2)
par(mar=c(4,4,0.5,0.5) + 0.1)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Rubiaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9), cex=1.5)
title(xlab = "Temperature", line=2.5)
title(ylab=FvFmLoss.name, line=2.5, cex=3)
text(5,0.05, pos=2, "Rubiaceae", cex=2)
par(mar=c(4,3,0.5,1.5) + 0.1)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Myrtaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9), cex=1.5)
title(xlab = "Temperature", line=2.5)
text(5,0.05, pos=2, "Myrtaceae", cex=2)
dev.off()

### Test centering
pdf(file = "Center_test.pdf", width = 12, height = 9)
# par(oma=c(1,1,0,1), mar=c(4,4,1,1) + 0.1)
par(oma=c(0.1,0.1,0.1,0.1))
par(cex.axis=1.5, cex.lab=1.8)
par(mfrow=c(3,3))
# Most resisitant
plot(c(1:10),c(1:10),xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
text(5,5,"|", cex=2)
par(mar=c(3,4,0.5,0.5) + 0.1)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Fabaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9), cex=1.5)
title(ylab=FvFmLoss.name, line=2.5, cex=3)
text(5,0.05, pos=2, "Fabaceae", cex=2)
par(mar=c(3,3,0.5,1.5) + 0.1)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Pittosporaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9), cex=1.5)
text(5,0.05, pos=2, "Pittosporaceae", cex=2)
# Moderately Resistant
plot(c(1:10),c(1:10),xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
text(5,5,"|", cex=2)
par(mar=c(3,4,0.5,0.5) + 0.1)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Rutaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9), cex=1.5)
text(5,0.05, pos=2, "Rutaceae", cex=2)
title(ylab=FvFmLoss.name, line=2.5, cex=3)
plot.new()
# Sensitive Families 
par(mar=c(3,4,0.5,0.5) + 0.1)
plot(c(1:10),c(1:10),xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
text(5,5,"|", cex=2)
par(mar=c(4,4,0.5,0.5) + 0.1)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Rubiaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9), cex=1.5)
title(xlab = "Temperature", line=2.5)
title(ylab=FvFmLoss.name, line=2.5, cex=3)
text(5,0.05, pos=2, "Rubiaceae", cex=2)
par(mar=c(4,3,0.5,1.5) + 0.1)
smoothplot(Temp, Fv.Fm, Site, data=subset(ft0.data, Family == "Myrtaceae"), kgam=5, 
           linecols=c("DarkGreen","Orange","LightBlue"),
           pointcols=c("DarkGreen","Orange","LightBlue"),
           xlab="", ylab="",
           ylim=c(0,0.9), cex=1.5)
title(xlab = "Temperature", line=2.5)
text(5,0.05, pos=2, "Myrtaceae", cex=2)
dev.off()

