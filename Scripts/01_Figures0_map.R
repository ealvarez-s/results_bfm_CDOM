if (Sys.info()['sysname']=="Windows") {OS<-"C:/"} else {OS<-paste("/Users/",Sys.info()['user'],"/", sep="")}
source(paste(o_dir,"Scripts/00_Color palette.R", sep=""))

##############
## Figure 2 ##
##############

png(file=paste(path_figures,"Figure2_Map.png",sep=""), width = 1000, height = 900, pointsize=24) 
    par(mfrow=c(1,1))
    par(mar=c(5.1,6,4,2))
    par(oma=c(0,1,1,0))
    
    load(paste(o_dir,"SPINUP/batimetria_world_degree6.Rdata", sep=""))
    contour(x=lon, y=lat, z=prof,levels=c(0,-200,-2000,-2500), lwd=2,
            ylab="", xlab=expression(paste(degree,"E", sep="")), las=1,
            drawlabels=T, labcex = 1.0,
            las=1, xaxs="i", yaxs="i", col="grey50", bty="o",add=F, xlim=c(6.25,9.5), ylim=c(42.5,44.75))
    
    mtext(2, at= 43.4, line=5.0, text=expression(paste(degree,"N",sep="")), las=1)
    
    abline(v=seq(6,10,by=1),  col="grey50", lwd=1)
    abline(h=seq(40,46,by=1), col="grey50", lwd=1)
    
      contour(x=lon, y=lat, z=prof,levels=seq(1000,10000,by=10), lwd=40, ylab="", xlab="", drawlabels=F,
              las=1, xaxs="i", yaxs="i", col="grey80", bty="n",add=T)
      contour(x=lon, y=lat, z=prof,levels=seq(550,2000,  by=5), lwd=20, ylab="", xlab="", drawlabels=F,
              las=1, xaxs="i", yaxs="i", col="grey80", bty="n",add=T)
      contour(x=lon, y=lat, z=prof,levels=seq(300,2000,  by=2), lwd=10, ylab="", xlab="", drawlabels=F,
              las=1, xaxs="i", yaxs="i", col="grey80", bty="n",add=T)
      contour(x=lon, y=lat, z=prof,levels=seq(58,1600,   by=1), lwd=3, ylab="", xlab="", drawlabels=F,
              las=1, xaxs="i", yaxs="i", col="grey80", bty="n",add=T)
      contour(x=lon, y=lat, z=prof,levels=seq(200,600,   by=1), lwd=1, ylab="", xlab="", drawlabels=F,
              las=1, xaxs="i", yaxs="i", col="grey80", bty="n",add=T)
      contour(x=lon, y=lat, z=prof,levels=seq(1,60,      by=1), lwd=1, ylab="", xlab="", drawlabels=F,
              las=1, xaxs="i", yaxs="i", col="grey80", bty="n",add=T)
    
    polygon(x=c(6.3,6.6,6.4), y=c(43.1,43.28,44.9), col="grey80", border = NA)
    polygon(x=c(9,9.45,9.45), y=c(42.55,42.55,42.8), col="grey80", border = NA)
    polygon(x=c(8.2,8.75,8.7), y=c(44.3,44.45,44.7), col="grey80", border = NA)
    
    # Coast
    contour(x=lon, y=lat, z=prof,levels=c(0), lwd=3, ylab="", xlab="", drawlabels=T,
            las=1, xaxs="i", yaxs="i", col="black", bty="o",add=T)
    box(which = "plot", lty = "solid", col="black", lwd=3)
    
    # Box Sat
    polygon(x=c(7.77,8.03,8.03,7.77), y=c(43.266667,43.266667,43.466667,43.466667), lwd=4)
    # Boussole cruises
            p_dir <- paste(o_dir,"BOUSSOLE_DATA/", sep="")
            tabla_total<-read.csv(paste(p_dir,"Boussole_HPLC.csv", sep=""))
            longi<-tabla_total$longitude
            lati<-tabla_total$latitude
            longi[longi<7.7|longi>8.03]<-NA
            lati[lati<43.266667|lati>43.466667]<-NA
            #points(longi, lati, col="black", pch=19, cex=0.2)
    # Mooring        
    points(x=7.9, y=43.366667, pch=4, cex=1.2, lwd=3, col="red2")
    # Dyfamed
    points(x=7+(52/60), y=43+(25/60), pch=4, cex=1.2, lwd=3, col="black")
    
    
    # Little Map
    par(new=T)
    par(mar=c(18.9,2,2,16))
    contour(x=lon, y=lat, z=prof,levels=c(0), lwd=1, ylab="", xlab="", drawlabels=F,
            las=1, xaxs="i", yaxs="i", col="grey50", bty="o",add=F,
            xlim=c(-7,19), ylim=c(35,45.5), xaxt="n", yaxt="n")
    polygon(x=c(-7,19,19,-7),y=c(35,35,45.5,45.5), col="white")
    contour(x=lon, y=lat, z=prof,levels=c(0,-2000, -2500), lwd=1, ylab="", xlab="", drawlabels=F,
            las=1, xaxs="i", yaxs="i", col="grey50", bty="o",add=T)
    contour(x=lon, y=lat, z=prof,levels=c(0), lwd=2, ylab="", xlab="", drawlabels=F,
            las=1, xaxs="i", yaxs="i", col="black", bty="o",add=T)
    
    # Fill land
    cL <- contourLines(lon, lat, prof,  nlevels = c(0))
    lapply(cL,function(x)polygon(x$x,x$y,col="grey80"))
    points(lon, lat,pch = datay+1)
    
    box(which = "plot", lty = "solid", col="black", lwd=3)
    axis(2, at=seq(30,46, by=2), las=1)
    axis(3, at=seq(-10,20, by=4), las=1)
    
    polygon(x=c(6.25,9.5,9.5,6.25), y=c(42.5,42.5,44.75,44.75), lwd=4)

dev.off()


        



############################
## Figure for NECCTON WP3 ##
############################

png(file=paste(path_figures,"Figure2_Map_NECCTON.png",sep=""), width = 1000, height = 900, pointsize=24) 
par(mfrow=c(1,1))
par(mar=c(5.1,6,4,2))
par(oma=c(0,1,1,0))

load(paste(o_dir,"SPINUP/batimetria_world_degree6.Rdata", sep=""))
contour(x=lon, y=lat, z=prof,levels=c(0,-200,-2000,-2500), lwd=2,
        ylab="", xlab=expression(paste(degree,"E", sep="")), las=1,
        drawlabels=T, labcex = 1.0,
        las=1, xaxs="i", yaxs="i", col="grey50", bty="o",add=F, xlim=c(6.25,9.5), ylim=c(42.5,44.75))

mtext(2, at= 43.4, line=3.5, text=expression(paste(degree,"N",sep="")), las=1)

abline(v=seq(6,10,by=1),  col="grey50", lwd=1)
abline(h=seq(40,46,by=1), col="grey50", lwd=1)

contour(x=lon, y=lat, z=prof,levels=seq(1000,10000,by=10), lwd=40, ylab="", xlab="", drawlabels=F,
        las=1, xaxs="i", yaxs="i", col="grey80", bty="n",add=T)
contour(x=lon, y=lat, z=prof,levels=seq(550,2000,  by=5), lwd=20, ylab="", xlab="", drawlabels=F,
        las=1, xaxs="i", yaxs="i", col="grey80", bty="n",add=T)
contour(x=lon, y=lat, z=prof,levels=seq(300,2000,  by=2), lwd=10, ylab="", xlab="", drawlabels=F,
        las=1, xaxs="i", yaxs="i", col="grey80", bty="n",add=T)
contour(x=lon, y=lat, z=prof,levels=seq(58,1600,   by=1), lwd=3, ylab="", xlab="", drawlabels=F,
        las=1, xaxs="i", yaxs="i", col="grey80", bty="n",add=T)
contour(x=lon, y=lat, z=prof,levels=seq(200,600,   by=1), lwd=1, ylab="", xlab="", drawlabels=F,
        las=1, xaxs="i", yaxs="i", col="grey80", bty="n",add=T)
contour(x=lon, y=lat, z=prof,levels=seq(1,60,      by=1), lwd=1, ylab="", xlab="", drawlabels=F,
        las=1, xaxs="i", yaxs="i", col="grey80", bty="n",add=T)

polygon(x=c(6.3,6.6,6.4), y=c(43.1,43.28,44.9), col="grey80", border = NA)
polygon(x=c(9,9.45,9.45), y=c(42.55,42.55,42.8), col="grey80", border = NA)
polygon(x=c(8.2,8.75,8.7), y=c(44.3,44.45,44.7), col="grey80", border = NA)

# Coast
contour(x=lon, y=lat, z=prof,levels=c(0), lwd=3, ylab="", xlab="", drawlabels=T,
        las=1, xaxs="i", yaxs="i", col="black", bty="o",add=T)
box(which = "plot", lty = "solid", col="black", lwd=3)

# Box Sat
polygon(x=c(7.77,8.03,8.03,7.77), y=c(43.266667,43.266667,43.466667,43.466667), lwd=4)
# Boussole cruises
p_dir <- paste(o_dir,"BOUSSOLE_DATA/", sep="")
tabla_total<-read.csv(paste(p_dir,"Boussole_HPLC.csv", sep=""))
longi<-tabla_total$longitude
lati<-tabla_total$latitude
longi[longi<7.7|longi>8.03]<-NA
lati[lati<43.266667|lati>43.466667]<-NA
#points(longi, lati, col="black", pch=19, cex=0.2)
# Mooring        
points(x=7.9, y=43.366667, pch=4, cex=1.2, lwd=3, col="red2")
# Dyfamed
points(x=7+(52/60), y=43+(25/60), pch=4, cex=1.2, lwd=3, col="black")


# # Little Map
# par(new=T)
# par(mar=c(18.9,2,2,16))
# contour(x=lon, y=lat, z=prof,levels=c(0), lwd=1, ylab="", xlab="", drawlabels=F,
#         las=1, xaxs="i", yaxs="i", col="grey50", bty="o",add=F,
#         xlim=c(-7,19), ylim=c(35,45.5), xaxt="n", yaxt="n")
# polygon(x=c(-7,19,19,-7),y=c(35,35,45.5,45.5), col="white")
# contour(x=lon, y=lat, z=prof,levels=c(0,-2000, -2500), lwd=1, ylab="", xlab="", drawlabels=F,
#         las=1, xaxs="i", yaxs="i", col="grey50", bty="o",add=T)
# contour(x=lon, y=lat, z=prof,levels=c(0), lwd=2, ylab="", xlab="", drawlabels=F,
#         las=1, xaxs="i", yaxs="i", col="black", bty="o",add=T)
# 
# # Fill land
# cL <- contourLines(lon, lat, prof,  nlevels = c(0))
# lapply(cL,function(x)polygon(x$x,x$y,col="grey80"))
# points(lon, lat,pch = datay+1)
# 
# box(which = "plot", lty = "solid", col="black", lwd=3)
# axis(2, at=seq(30,46, by=2), las=1)
# axis(3, at=seq(-10,20, by=4), las=1)
# 
# polygon(x=c(6.25,9.5,9.5,6.25), y=c(42.5,42.5,44.75,44.75), lwd=4)

dev.off()











