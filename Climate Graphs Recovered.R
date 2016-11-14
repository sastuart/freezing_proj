par(mar=c(4,4.5,1,2))

# Special Plot (Daily Range)

plot(fm.mean ~ names(fm.mean), type = "o",
     col="orange", ylim = c(-10,42),
     ylab="",xlab="", axes=F, par(bty="n"),
     yaxs="i", pch=20)
       
axis(1, col = "lightgray", at=1:12,
     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"),
     tck=0)
axis(2, col = "lightgray", las=2, tck=0.01)
       
lines(c(1,12),c(0,0), lty=2, col="lightgray")
polygon(c(1:12,12:1),c(fm.max.q90, rev(fm.q10)),
        col=rgb(255,165,0,40,maxColorValue=255),
        border=NA)
lines(fm.max.q90 ~ names(fm.max.q90), type = "l", col="orange", lty=3)
lines(fm.q10 ~ names(fm.q10), type = "l",col="orange", lty=3)
       
lines(cc.min.mean ~ names(cc.min.mean), type = "o",
      col="darkgreen", lty=1, pch=18)
polygon(c(1:12,12:1),c(cc.max.q90, rev(cc.min.q10)),
        col=rgb(0,100,0,20,maxColorValue=255), border=NA)
lines(cc.min.q10 ~ names(cc.min.q10), type = "l",
      col="darkgreen", lty=3)
lines(cc.max.q90 ~ names(cc.max.q90), type = "l",
      col="darkgreen", lty=3)

lines(mf.min.mean ~ names(mf.min.mean), type = "o",
      col="lightblue", lty=1, pch=15, cex=.7)
polygon(c(1:12,12:1),c(mf.max.q90, rev(mf.min.q10)),
        col=rgb(173,216,230,80,maxColorValue=255),
        border=NA)
lines(mf.max.q90 ~ names(mf.max.q90), type = "l",
      col="lightblue", lty=3)
lines(mf.min.q10 ~ names(mf.min.q10), type = "l",
      col="lightblue", lty=3)
legend(1,0, c("Warm and Wet","Warm and Dry","Cool and Wet"), cex=1,
       bty="n", col=c("darkgreen","orange","lightblue"),
       pch=c(18,20,15), lty=1)

y.name <-expression(paste("Temperature ( "*degree,"C)"))
title(ylab=y.name)
       
## the plot thickens: Rainfall

plot(cc.rain.mean ~ names(cc.rain.mean), type = "o",
       col="darkgreen", ylim = c(0,810),
       ylab="",xlab="", axes=F, par(bty="n"),
       #     xaxs="i",yaxs="i")
       yaxs="i", pch=18)
       
axis(1, col = "lightgray", at=1:12,
     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"),
     tck=0)
axis(2, col = "lightgray", las=2, tck=0.01)
       
polygon(c(1:12,12:1),c(cc.rain.q90, rev(cc.rain.q10)),
        col=rgb(0,100,0,20,maxColorValue=255),border=NA)
lines(cc.rain.q90 ~ names(cc.rain.q90), type = "l",
      col="darkgreen", lty=3)
lines(cc.rain.q10 ~ names(cc.rain.q10), type = "l",
      col="darkgreen", lty=3)
       
lines(fm.rain.mean ~ names(fm.rain.mean), type = "o",
      col="orange", lty=1, pch=20)
polygon(c(1:12,12:1),c(fm.rain.q90, rev(fm.rain.q10)),
        col=rgb(255,165,0,40,maxColorValue=255), border=NA)
lines(fm.rain.q90 ~ names(fm.rain.q90), type = "l",
      col="orange", lty=3)
lines(fm.rain.q10 ~ names(fm.rain.q10), type = "l",
      col="orange", lty=3)
       
lines(mf.rain.mean ~ names(mf.rain.mean), type = "o",
      col="lightblue", lty=1, pch=15, cex=0.7)
polygon(c(1:12,12:1),c(mf.rain.q90, rev(mf.rain.q10)),
        col=rgb(173,216,230,80,maxColorValue=255),border=NA)
lines(mf.rain.q90 ~ names(mf.rain.q90), type = "l",
      col="lightblue", lty=3)
lines(mf.rain.q10 ~ names(mf.rain.q10), type = "l",
      col="lightblue", lty=3)
       
legend(12,810, c("Warm and Wet","Warm and Dry","Cool and Wet"),
       cex=1,bty="n", col=c("darkgreen","orange","lightblue"),
       pch=c(18,20,15), lty=1, xjust=1)
title(ylab="Precipitation (mm)")

       
#### Mins Only
plot(fm.mean ~ names(fm.mean), type = "o", 
     col="orange", ylim = c(-10,42),
     ylab="",xlab="", axes=F, par(bty="n"),
     yaxs="i", pch=20)
       
axis(1, col = "lightgray",at=1:12,
     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"),
     tck=0)
axis(2, col = "lightgray", las=2, tck=0.01)
lines(c(1,12),c(0,0), lty=2, col="lightgray")

polygon(c(1:12,12:1),c(fm.q90, rev(fm.q10)),
        col=rgb(255,165,0,40,maxColorValue=255),
        border=NA)
lines(fm.q90 ~ names(cc.min.mean), type = "l",
      col="orange", lty=3)
lines(fm.q10 ~ names(cc.min.mean), type = "l",
      col="orange", lty=3)

lines(cc.min.mean ~ names(cc.min.mean), type = "o",
      col="darkgreen", lty=1, pch=18)
polygon(c(1:12,12:1),c(cc.min.q90, rev(cc.min.q10)),
        col=rgb(0,100,0,20,maxColorValue=255),
        border=NA)
lines(cc.min.q10 ~ names(cc.min.q10), type = "l",
      col="darkgreen", lty=3)
lines(cc.min.q90 ~ names(cc.min.q90), type = "l",
      col="darkgreen", lty=3)

lines(mf.min.mean ~ names(mf.min.mean), type = "o",
      col="lightblue", lty=1, pch=15, cex=0.7)
polygon(c(1:12,12:1),c(mf.min.q90, rev(mf.min.q10)),
        col=rgb(173,216,230,80,maxColorValue=255),
        border=NA)
lines(mf.min.q90 ~ names(mf.min.q90), type = "l",
      col="lightblue", lty=3)
lines(mf.min.q10 ~ names(mf.min.q10), type = "l",
      col="lightblue", lty=3)
legend(1,0, c("Warm and Wet","Warm and Dry","Cool and Wet"),
       cex=1, bty="n", col=c("darkgreen","orange","lightblue"),
       pch=c(18,20,15), lty=1)

y.name <-expression(paste("Minimum Temperature ( "*degree,"C)"))
title(ylab=y.name)

  