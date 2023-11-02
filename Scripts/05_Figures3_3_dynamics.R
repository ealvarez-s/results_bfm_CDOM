if (Sys.info()['sysname']=="Windows") {OS<-"C:/"} else {OS<-paste("/Users/",Sys.info()['user'],"/", sep="")}

### Figures vertical profiles, absorption budget and bio-optical relationships (Results 3.3)

source(paste(o_dir,"Scripts/00_Color palette.R", sep=""))


    ## Depth layers
    datos <- read.table(paste(o_dir,"SPINUP/grid.dat", sep=""))
    nlevs=196
    profun=2438
    thickness<-(datos$V1[2:length(datos$V1)])*profun
    depth_breaks<-c(profun,rep(NA,length=nlevs-1),0)
    for (i in c(2:nlevs)){depth_breaks[i]<-sum(thickness[i:length(thickness)])}
    depth_center<-rep(NA,length=nlevs)
    for (i in c(1:nlevs)){depth_center[i]<-depth_breaks[i]-(thickness[i]/2) }
    layers_bous<-cbind(c(1:nlevs),round(cbind(depth_breaks[-length(depth_breaks)],depth_center,depth_breaks[-1]),2))
    colnames(layers_bous)<-c("index","bottom(m)","center(m)","top(m)")

    ## Model runs
    experimentos <- read.csv(paste(o_dir,"run_log_ALL3.csv", sep=""), sep=",") 
    nombres<-experimentos$file 
    carpetas<-experimentos$folder
    # OPTIM5
    elegidos<-c(7)
    nombres[elegidos]
    colores[elegidos]
    
    
################################
### FIGURE 8 depth profiles  ###
################################
    
png(file=paste(path_figures,"Figure8_Conc_IOPs_profiles_depths_2011_2014.png",sep=""), width=1000, height=600, pointsize=22, family="Helvetica") 
#pdf(file=paste(path_figures,"Figure8_Conc_IOPs_profiles_depths_2011_2014.pdf",sep=""), width=1000/100, height=600/100, pointsize=16, family="Helvetica")
    par(mfrow=c(1,4))
    #layout(matrix(c(1:12),ncol=3, byrow=T), widths = c(1,0.5,0.7))
    par(mar=c(8,4,8,0))
    par(oma=c(1,1,1,1))
    #layout.show(n=4)
    pointsize=0.5
    
    ####################### 
    i=elegidos[1]
    archivo<-paste(nombres[i],"/result.nc",sep="")
    filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
    filenc <- open.nc(filename)
    filerc <- read.nc(filenc)
    tiempo<-filerc$time/86400
    meses<-tiempo/30
    mid_meses<-seq(-2192+15, 4382-15, length=18*12)
    meses<-c(1:(18*12))
    etiquetas_meses<-c(paste("2003",c(1:12),sep="-"),paste("2004",c(1:12),sep="-"),
                       paste("2005",c(1:12),sep="-"),paste("2006",c(1:12),sep="-"),
                       paste("2007",c(1:12),sep="-"),paste("2008",c(1:12),sep="-"),
                       paste("2009",c(1:12),sep="-"),paste("2010",c(1:12),sep="-"),
                       paste("2011",c(1:12),sep="-"),paste("2012",c(1:12),sep="-"),
                       paste("2013",c(1:12),sep="-"),paste("2014",c(1:12),sep="-"),
                       paste("2015",c(1:12),sep="-"),paste("2016",c(1:12),sep="-"),
                       paste("2017",c(1:12),sep="-"),paste("2018",c(1:12),sep="-"),
                       paste("2019",c(1:12),sep="-"),paste("2020",c(1:12),sep="-"))
    ###########################
    
## WINTER
##################################   
    plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
         main="", ylab="depth (m)", xlab=expression(paste("TChl-a (mg ", m^-3,")", sep="  ")),
         xlim=c(0,1),xaxs="i", ylim=c(200,0), yaxs="i", mgp=c(2.5,1,0)) 
    #abline(h=0.01,lty=3, col="grey")
    #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
    #text(x=mid_meses[seq(1,length(mid_meses),by=6)][17:25], y = par("usr")[3] - 0.05,
    #labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][17:25], xpd = NA,srt = 40,adj = 0.965,cex = 0.6) 
    
    # Phyto chl
    ###########
    R0<-filerc$P1_Chl+filerc$P2_Chl+filerc$P3_Chl+filerc$P4_Chl
    aph_2011<-R0[,((361*2)+0):((361*2)+90)]
    aph_2012<-R0[,((361*3)+0):((361*3)+90)]
    aph_2013<-R0[,(((361*4)+1)+0):(((361*4)+1)+90)]
    aph_2014<-R0[,(((361*5)+1)+0):(((361*5)+1)+90)]
    aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
    upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
    lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(depth_center,depth_center[length(depth_center):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[4], border=NA)
                ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
                polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[3], border=NA)
                ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
                polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[2], border=NA)
                ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
                polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[1], border=NA)
     media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
     media_phyto<-media
     points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
     #media<-apply(aph,MARGIN=c(2),FUN=mean, na.rm=T)
     #points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)     
    ##########     
     
    # acdom 450nm
    #############
    R0<-filerc$lightspectral_acdom450*20
              aph_2011<-R0[,((361*2)+0):((361*2)+90)]
              aph_2012<-R0[,((361*3)+0):((361*3)+90)]
              aph_2013<-R0[,(((361*4)+1)+0):(((361*4)+1)+90)]
              aph_2014<-R0[,(((361*5)+1)+0):(((361*5)+1)+90)]
              aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
              upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
              lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
              equis<-c(depth_center,depth_center[length(depth_center):1])
              ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
              polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[4], border=NA)
              ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
              polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[3], border=NA)
              ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
              polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[2], border=NA)
              ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
              polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[1], border=NA)
    media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
    media_cdom<-media
    points(y=depth_center, x=media, type="l", col=linea_cdom, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
    #media<-apply(aph,MARGIN=c(2),FUN=mean, na.rm=T)
    #points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize) 
    axis(3,  at=seq(0,1,length=6), labels=seq(0,1,length=6)/20, las=1, cex.axis=1)
    ############
    mtext(3, at=0.5, line=2, text=expression(paste(a[CDOM],"(450) (", m^-1,")", sep="")), cex=0.75, outer=F)
    
    # Bact
    ###########
    R0<-filerc$B1_c/30
    #aph<-rowMeans(R0[,((365*2)+1):((365*2)+91)])
    #points(y=depth_center, x=aph/30, type="p", col=linea_bacteria, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize) 
            aph_2011<-R0[,((361*2)+0):((361*2)+90)]
            aph_2012<-R0[,((361*3)+0):((361*3)+90)]
            aph_2013<-R0[,(((361*4)+1)+0):(((361*4)+1)+90)]
            aph_2014<-R0[,(((361*5)+1)+0):(((361*5)+1)+90)]
            aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
            upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(depth_center,depth_center[length(depth_center):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[1], border=NA)
    media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
    media_bact<-media
    points(y=depth_center, x=media, type="l", col=linea_bact, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
    #media<-apply(aph,MARGIN=c(2),FUN=mean, na.rm=T)
    #points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)     
    axis(3,  line=4, at=seq(0,1,length=6), labels=seq(0,1,length=6)*30, las=1, cex.axis=1)
    ############
    mtext(3, at=0.5, line=6, text=expression(paste(B[C]," (mg ", m^-3,")", sep="  ")), cex=0.75, outer=F)
    
    # DOC
    #########
    par(new=T)
    plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize,
         main="", ylab="", xlab="", xaxt="n", yaxt="n",
         xlim=c(35,70),xaxs="i", ylim=c(200,0), yaxs="i", mgp=c(2.5,1,0)) 
    R0<-(filerc$R1_c+filerc$R2_c+filerc$R3_c+
      filerc$X1_c+filerc$X2_c+filerc$X3_c)/12
    #aph<-rowMeans(R0[,((365*2)+1):((365*2)+91)])
    #range(aph,na.rm=T)
    #aph<-aph/12
    #points(x=aph, y=depth_center, type="b", col=linea_grey, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)
            aph_2011<-R0[,((361*2)+0):((361*2)+90)]
            aph_2012<-R0[,((361*3)+0):((361*3)+90)]
            aph_2013<-R0[,(((361*4)+1)+0):(((361*4)+1)+90)]
            aph_2014<-R0[,(((361*5)+1)+0):(((361*5)+1)+90)]
            aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
            upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(depth_center,depth_center[length(depth_center):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[1], border=NA)
            media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
            points(y=depth_center, x=media, type="l", col=linea_grey, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
            #media<-apply(aph,MARGIN=c(2),FUN=mean, na.rm=T)
            #points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)
    axis(1,  line=4, at=seq(35,70,length=8), labels=seq(35,70,length=8), las=1, cex.axis=1)
    ###########
    mtext(1, at=55, line=6.5, text=expression(paste("DOC (",mu,"mol ", L^-1,")", sep="  ")), cex=0.75, outer=F)
    
##################################  
    ## Replot all means so they are visible
    par(new=T)
    plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize,
         main="", ylab="", xlab="", xaxt="n", yaxt="n",
         xlim=c(0,1),xaxs="i", ylim=c(200,0), yaxs="i", mgp=c(2.5,1,0))     
    points(y=depth_center, x=media_bact, type="l", col=linea_bact, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
    points(y=depth_center, x=media_cdom, type="l", col=linea_cdom, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
    points(y=depth_center, x=media_phyto, type="l", col=linea_phyto, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
    
    #legend(x="topright", legend="Winter", cex=1.4,bty="n")
    text(x=1,y=130, pos=2, labels="Winter (J-M)", cex=1.3)
    legend(x="bottomright",legend=c(expression(B[C]),(expression(paste(a[CDOM],"(450)"))),"TChl-a","DOC"),
           col=c(linea_bact,linea_cdom, linea_phyto, linea_grey), bty="n", lwd=3) 
    
    
## SPRING
##################################   
    plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize,
         main="", ylab="depth (m)", xlab=expression(paste("TChl-a (mg ", m^-3,")", sep="  ")),
         xlim=c(0,1),xaxs="i", ylim=c(200,0), yaxs="i", mgp=c(2.5,1,0)) 
    #abline(h=0.01,lty=3, col="grey")
    #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
    #text(x=mid_meses[seq(1,length(mid_meses),by=6)][17:25], y = par("usr")[3] - 0.05,
    #labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][17:25], xpd = NA,srt = 40,adj = 0.965,cex = 0.6) 
    
    # Phyto chl
    ##############
    R0<-filerc$P1_Chl+filerc$P2_Chl+filerc$P3_Chl+filerc$P4_Chl
    #aph<-rowMeans(R0[,((365*2)+92):((365*2)+182)])
    #length(aph)
    #points(x=aph, y=depth_center, type="b", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)
          aph_2011<-R0[,((361*2)+91):((361*2)+181)]
          aph_2012<-R0[,((361*3)+91):((361*3)+181)]
          aph_2013<-R0[,(((361*4)+1)+91):(((361*4)+1)+181)]
          aph_2014<-R0[,(((361*5)+1)+91):(((361*5)+1)+181)]
          aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
          upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
          lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
          equis<-c(depth_center,depth_center[length(depth_center):1])
          ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
          polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[4], border=NA)
          ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
          polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[3], border=NA)
          ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
          polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[2], border=NA)
          ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
          polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[1], border=NA)
          media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
          media_phyto<-media
          points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)     
          #media<-apply(aph,MARGIN=c(2),FUN=mean, na.rm=T)
          #points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)     
    #############   
          
    # acdom 450nm
    ############
    R0<-filerc$lightspectral_acdom450*20
    #adg<-rowMeans(R0[,((365*2)+92):((365*2)+182)])
    #points(y=depth_center, x=adg*20, type="p", col="orange2", lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)               
            aph_2011<-R0[,((361*2)+91):((361*2)+181)]
            aph_2012<-R0[,((361*3)+91):((361*3)+181)]
            aph_2013<-R0[,(((361*4)+1)+91):(((361*4)+1)+181)]
            aph_2014<-R0[,(((361*5)+1)+91):(((361*5)+1)+181)]
            aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
            upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(depth_center,depth_center[length(depth_center):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[1], border=NA)
            media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
            media_cdom<-media
            points(y=depth_center, x=media, type="l", col=linea_cdom, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)     
            #media<-apply(aph,MARGIN=c(2),FUN=mean, na.rm=T)
            #points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)     
    axis(3,  at=seq(0,1,length=6), labels=seq(0,1,length=6)/20, las=1, cex.axis=1)
    ################
    mtext(3, at=0.5, line=2, text=expression(paste(a[CDOM],"(450) (", m^-1,")", sep="")), cex=0.75, outer=F)
    
    # Bact
    ###########
    R0<-filerc$B1_c/30
    #aph<-rowMeans(R0[,((365*2)+92):((365*2)+182)])
    #points(y=depth_center, x=aph/30, type="p", col="blue3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize) 
            aph_2011<-R0[,((361*2)+91):((361*2)+181)]
            aph_2012<-R0[,((361*3)+91):((361*3)+181)]
            aph_2013<-R0[,(((361*4)+1)+91):(((361*4)+1)+181)]
            aph_2014<-R0[,(((361*5)+1)+91):(((361*5)+1)+181)]
            aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
            upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(depth_center,depth_center[length(depth_center):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[1], border=NA)
            media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
            media_bact<-media
            points(y=depth_center, x=media, type="l", col=linea_bact, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)     
            #media<-apply(aph,MARGIN=c(2),FUN=mean, na.rm=T)
            #points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize) 
    axis(3,  line=4, at=seq(0,1,length=6), labels=seq(0,1,length=6)*30, las=1, cex.axis=1)
    #############
    mtext(3, at=0.5, line=6, text=expression(paste(B[C]," (mg ", m^-3,")", sep="  ")), cex=0.75, outer=F)
    
    # DOC
    #########
    par(new=T)
    plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize,
         main="", ylab="", xlab="", xaxt="n", yaxt="n",
         xlim=c(35,70),xaxs="i", ylim=c(200,0), yaxs="i", mgp=c(2.5,1,0)) 
    R0<-(filerc$R1_c+filerc$R2_c+filerc$R3_c+
      filerc$X1_c+filerc$X2_c+filerc$X3_c)/12
    #aph<-rowMeans(R0[,((365*2)+92):((365*2)+182)])
    #range(aph,na.rm=T)
    #aph<-aph/12
    #points(x=aph, y=depth_center, type="b", col="black", lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)
          aph_2011<-R0[,((361*2)+91):((361*2)+181)]
          aph_2012<-R0[,((361*3)+91):((361*3)+181)]
          aph_2013<-R0[,(((361*4)+1)+91):(((361*4)+1)+181)]
          aph_2014<-R0[,(((361*5)+1)+91):(((361*5)+1)+181)]
          aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
          upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
          lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
          equis<-c(depth_center,depth_center[length(depth_center):1])
          ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
          polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[4], border=NA)
          ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
          polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[3], border=NA)
          ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
          polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[2], border=NA)
          ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
          polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[1], border=NA)
          media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
          points(y=depth_center, x=media, type="l", col=linea_grey, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)     
          #media<-apply(aph,MARGIN=c(2),FUN=mean, na.rm=T)
          #points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize) 
          axis(1,  line=4, at=seq(35,70,length=8), labels=seq(35,70,length=8), las=1, cex.axis=1)
    #############
    mtext(1, at=55, line=6.5, text=expression(paste("DOC (",mu,"mol ", L^-1,")", sep="  ")), cex=0.75, outer=F)
    
    
##################################
          ## Replot all means so they are visible
          par(new=T)
          plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize,
               main="", ylab="", xlab="", xaxt="n", yaxt="n",
               xlim=c(0,1),xaxs="i", ylim=c(200,0), yaxs="i", mgp=c(2.5,1,0))     
          points(y=depth_center, x=media_bact, type="l", col=linea_bact, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
          points(y=depth_center, x=media_cdom, type="l", col=linea_cdom, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
          points(y=depth_center, x=media_phyto, type="l", col=linea_phyto, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
          
    text(x=1,y=130, pos=2, labels="Spring (A-J)", cex=1.3)
    legend(x="bottomright",legend=c(expression(B[C]),(expression(paste(a[CDOM],"(450)"))),"TChl-a","DOC"),
           col=c(linea_bact,linea_cdom, linea_phyto, linea_grey), bty="n", lwd=3) 
    
    
## SUMMER
##################################   
    plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize,
         main="", ylab="depth (m)", xlab=expression(paste("TChl-a (mg ", m^-3,")", sep="  ")),
         xlim=c(0,1),xaxs="i", ylim=c(200,0), yaxs="i", mgp=c(2.5,1,0)) 
    #abline(h=0.01,lty=3, col="grey")
    #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
    #text(x=mid_meses[seq(1,length(mid_meses),by=6)][17:25], y = par("usr")[3] - 0.05,
    #labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][17:25], xpd = NA,srt = 40,adj = 0.965,cex = 0.6) 
    
    # Phyto chl
    ##########
    R0<-filerc$P1_Chl+filerc$P2_Chl+filerc$P3_Chl+filerc$P4_Chl
            #aph<-rowMeans(R0[,((365*2)+183):((365*2)+273)])
            #length(aph)
            #points(x=aph, y=depth_center, type="b", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)
            aph_2011<-R0[,((361*2)+182):((361*2)+272)]
            aph_2012<-R0[,((361*3)+182):((361*3)+272)]
            aph_2013<-R0[,(((361*4)+1)+182):(((361*4)+1)+272)]
            aph_2014<-R0[,(((361*5)+1)+182):(((361*5)+1)+272)]
            aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
            upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(depth_center,depth_center[length(depth_center):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[1], border=NA)
            media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
            media_phyto<-media
            points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
            #media<-apply(aph,MARGIN=c(2),FUN=mean, na.rm=T)
            #points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)     
    ##########
        
    # acdom 450nm
    #############        
    R0<-filerc$lightspectral_acdom450*20
    #adg<-rowMeans(R0[,((365*2)+183):((365*2)+273)])
    #points(y=depth_center, x=adg*20, type="p", col="orange2", lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)               
            aph_2011<-R0[,((361*2)+182):((361*2)+272)]
            aph_2012<-R0[,((361*3)+182):((361*3)+272)]
            aph_2013<-R0[,(((361*4)+1)+182):(((361*4)+1)+272)]
            aph_2014<-R0[,(((361*5)+1)+182):(((361*5)+1)+272)]
            aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
            upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(depth_center,depth_center[length(depth_center):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[1], border=NA)
            media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
            media_cdom<-media
            points(y=depth_center, x=media, type="l", col=linea_cdom, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)     
            #media<-apply(aph,MARGIN=c(2),FUN=mean, na.rm=T)
            #points(y=depth_center, x=media, type="l", col=linea_cdom, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize) 
    axis(3,  at=seq(0,1,length=6), labels=seq(0,1,length=6)/20, las=1, cex.axis=1)
    ###########
    mtext(3, at=0.5, line=2, text=expression(paste(a[CDOM],"(450) (", m^-1,")", sep="")), cex=0.75, outer=F)
    
    # Bact
    #########
    R0<-filerc$B1_c/30
    #aph<-rowMeans(R0[,((365*2)+183):((365*2)+273)])
    #points(y=depth_center, x=aph/30, type="p", col="blue3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize) 
            aph_2011<-R0[,((361*2)+182):((361*2)+272)]
            aph_2012<-R0[,((361*3)+182):((361*3)+272)]
            aph_2013<-R0[,(((361*4)+1)+182):(((361*4)+1)+272)]
            aph_2014<-R0[,(((361*5)+1)+182):(((361*5)+1)+272)]
            aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
            upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(depth_center,depth_center[length(depth_center):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[1], border=NA)
            media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
            media_bact<-media
            points(y=depth_center, x=media, type="l", col=linea_bact, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)     
            #media<-apply(aph,MARGIN=c(2),FUN=mean, na.rm=T)
            #points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)   
    axis(3,  line=4, at=seq(0,1,length=6), labels=seq(0,1,length=6)*30, las=1, cex.axis=1)
    ###########
    mtext(3, at=0.5, line=6, text=expression(paste(B[C]," (mg ", m^-3,")", sep="  ")), cex=0.75, outer=F)
    
    # DOC
    #########
    par(new=T)
    plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize,
         main="", ylab="", xlab="", xaxt="n", yaxt="n",
         xlim=c(35,70),xaxs="i", ylim=c(200,0), yaxs="i", mgp=c(2.5,1,0)) 
    R0<-(filerc$R1_c+filerc$R2_c+filerc$R3_c+filerc$X1_c+filerc$X2_c+filerc$X3_c)/12
    #aph<-rowMeans(R0[,((365*2)+183):((365*2)+273)])
    #range(aph,na.rm=T)
    #aph<-aph/12
    #points(x=aph, y=depth_center, type="b", col="black", lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)
          aph_2011<-R0[,((361*2)+182):((361*2)+272)]
          aph_2012<-R0[,((361*3)+182):((361*3)+272)]
          aph_2013<-R0[,(((361*4)+1)+182):(((361*4)+1)+272)]
          aph_2014<-R0[,(((361*5)+1)+182):(((361*5)+1)+272)]
          aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
          upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
          lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
          equis<-c(depth_center,depth_center[length(depth_center):1])
          ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
          polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[4], border=NA)
          ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
          polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[3], border=NA)
          ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
          polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[2], border=NA)
          ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
          polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[1], border=NA)
          media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
          points(y=depth_center, x=media, type="l", col=linea_grey, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)     
          #media<-apply(aph,MARGIN=c(2),FUN=mean, na.rm=T)
          #points(y=depth_center, x=media, type="l", col=linea_grey, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)      
          axis(1,  line=4, at=seq(35,70,length=8), labels=seq(35,70,length=8), las=1, cex.axis=1)
    ###########
    mtext(1, at=55, line=6.5, text=expression(paste("DOC (",mu,"mol ", L^-1,")", sep="  ")), cex=0.75, outer=F)
    
    
##################################
          ## Replot all means so they are visible
          par(new=T)
          plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize,
               main="", ylab="", xlab="", xaxt="n", yaxt="n",
               xlim=c(0,1),xaxs="i", ylim=c(200,0), yaxs="i", mgp=c(2.5,1,0))     
          points(y=depth_center, x=media_bact, type="l", col=linea_bact, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
          points(y=depth_center, x=media_cdom, type="l", col=linea_cdom, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
          points(y=depth_center, x=media_phyto, type="l", col=linea_phyto, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
          
    #legend(x="topright", legend="Summer", cex=1.4,bty="n")
    text(x=1,y=130, pos=2, labels="Summer (J-S)", cex=1.3)
    legend(x="bottomright",legend=c(expression(B[C]),(expression(paste(a[CDOM],"(450)"))),"TChl-a","DOC"),
           col=c(linea_bact,linea_cdom, linea_phyto, linea_grey), bty="n", lwd=3) 
    
    
## FALL
##################################   
    plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize,
         main="", ylab="depth (m)", xlab=expression(paste("TChl-a (mg ", m^-3,")", sep="  ")),
         xlim=c(0,1),xaxs="i", ylim=c(200,0), yaxs="i", mgp=c(2.5,1,0)) 
    #abline(h=0.01,lty=3, col="grey")
    #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
    #text(x=mid_meses[seq(1,length(mid_meses),by=6)][17:25], y = par("usr")[3] - 0.05,
    #labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][17:25], xpd = NA,srt = 40,adj = 0.965,cex = 0.6) 
    
    # Phyto chl
    ###########
    R0<-filerc$P1_Chl+filerc$P2_Chl+filerc$P3_Chl+filerc$P4_Chl
    #aph<-rowMeans(R0[,((365*2)+274):((365*2)+365)])
    #length(aph)
    #points(x=aph, y=depth_center, type="b", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)
              aph_2011<-R0[,((361*2)+273):((361*2)+363)]
              aph_2012<-R0[,((361*3)+273):((361*3)+363)]
              aph_2013<-R0[,(((361*4)+1)+273):(((361*4)+1)+363)]
              aph_2014<-R0[,(((361*5)+1)+273):(((361*5)+1)+363)]
              aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
              upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
              lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
              equis<-c(depth_center,depth_center[length(depth_center):1])
              ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
              polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[4], border=NA)
              ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
              polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[3], border=NA)
              ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
              polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[2], border=NA)
              ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
              polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_phyto[1], border=NA)
              media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
              media_phyto<-media
              points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
              #media<-apply(aph,MARGIN=c(2),FUN=mean, na.rm=T)
              #points(y=depth_center, x=media, type="l", col=linea_phyto, lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)   
    #############
              
    # acdom 450nm
    #############          
    R0<-filerc$lightspectral_acdom450*20
    #adg<-rowMeans(R0[,((365*2)+274):((365*2)+365)])
    #points(y=depth_center, x=adg, type="p", col="orange2", lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)               
            aph_2011<-R0[,((361*2)+273):((361*2)+363)]
            aph_2012<-R0[,((361*3)+273):((361*3)+363)]
            aph_2013<-R0[,(((361*4)+1)+273):(((361*4)+1)+363)]
            aph_2014<-R0[,(((361*5)+1)+273):(((361*5)+1)+363)]
            aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
            upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(depth_center,depth_center[length(depth_center):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_cdom[1], border=NA)
            media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
            media_cdom<-media
            points(y=depth_center, x=media, type="l", col=linea_cdom, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)      
    axis(3,  at=seq(0,1,length=6), labels=seq(0,1,length=6)/20, las=1, cex.axis=1)
    #############
    mtext(3, at=0.5, line=2, text=expression(paste(a[CDOM],"(450) (", m^-1,")", sep="")), cex=0.75, outer=F)
    
    # Bact
    ##############
    R0<-filerc$B1_c/30
    #aph<-rowMeans(R0[,((365*2)+274):((365*2)+365)])
    #points(y=depth_center, x=aph/30, type="p", col="blue3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize) 
            aph_2011<-R0[,((361*2)+273):((361*2)+363)]
            aph_2012<-R0[,((361*3)+273):((361*3)+363)]
            aph_2013<-R0[,(((361*4)+1)+273):(((361*4)+1)+363)]
            aph_2014<-R0[,(((361*5)+1)+273):(((361*5)+1)+363)]
            aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
            upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(depth_center,depth_center[length(depth_center):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_bact[1], border=NA)
            media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
            media_bact<-media
            points(y=depth_center, x=media, type="l", col=linea_bact, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)      
    axis(3,  line=4, at=seq(0,1,length=6), labels=seq(0,1,length=6)*30, las=1, cex.axis=1)
    ##############
    mtext(3, at=0.5, line=6, text=expression(paste(B[C]," (mg ", m^-3,")", sep="  ")), cex=0.75, outer=F)
    
    # DOC
    ##########
    par(new=T)
    plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize,
         main="", ylab="", xlab="", xaxt="n", yaxt="n",
         xlim=c(35,70),xaxs="i", ylim=c(200,0), yaxs="i", mgp=c(2.5,1,0)) 
    R0<-(filerc$R1_c+filerc$R2_c+filerc$R3_c+filerc$X1_c+filerc$X2_c+filerc$X3_c)/12
    #aph<-rowMeans(R0[,((365*2)+274):((365*2)+365)])
    #range(aph,na.rm=T)
    #aph<-aph/12
    #points(x=aph, y=depth_center, type="b", col="black", lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize)
            aph_2011<-R0[,((361*2)+273):((361*2)+363)]
            aph_2012<-R0[,((361*3)+273):((361*3)+363)]
            aph_2013<-R0[,(((361*4)+1)+273):(((361*4)+1)+363)]
            aph_2014<-R0[,(((361*5)+1)+273):(((361*5)+1)+363)]
            aph<-abind(aph_2011,aph_2012,aph_2013,aph_2014,along=0.5)
            upper<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-apply(aph,MARGIN=c(2),FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(depth_center,depth_center[length(depth_center):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(y=equis[!is.na(ies)], x=ies[!is.na(ies)], col=mycol_grey[1], border=NA)
            media<-apply(aph,MARGIN=c(2),FUN=quantile, probs=0.5, na.rm=T)
            points(y=depth_center, x=media, type="l", col=linea_grey, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)       
    axis(1,  line=4, at=seq(35,70,length=8), labels=seq(35,70,length=8), las=1, cex.axis=1)
    ###########
    mtext(1, at=55, line=6.5, text=expression(paste("DOC (",mu,"mol ", L^-1,")", sep="  ")), cex=0.75, outer=F)
    
################################## 
    ## Replot all means so they are visible
    par(new=T)
    plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=pointsize,
         main="", ylab="", xlab="", xaxt="n", yaxt="n",
         xlim=c(0,1),xaxs="i", ylim=c(200,0), yaxs="i", mgp=c(2.5,1,0))     
    points(y=depth_center, x=media_bact, type="l", col=linea_bact, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
    points(y=depth_center, x=media_cdom, type="l", col=linea_cdom, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
    points(y=depth_center, x=media_phyto, type="l", col=linea_phyto, lwd=3, las=1, cex.lab=1.0, pch=19, cex=pointsize)    
    
    #legend(x="topright", legend="Fall", cex=1.4,bty="n")
    text(x=1,y=130, pos=2, labels="Fall (O-D)", cex=1.3)
    legend(x="bottomright",legend=c(expression(B[C]),(expression(paste(a[CDOM],"(450)"))),"TChl-a","DOC"),
           col=c(linea_bact,linea_cdom, linea_phyto, linea_grey), bty="n", lwd=3) 
    
    
    
    dev.off()
    
    
    
    
    
    
    


#################
### FIGURE 9  ### Concentrations seasonal
#################

#png(file=paste(path_figures,"Figure9_Conc_IOPs_depths_2011_2014.png",sep=""),width=1300, height =370, pointsize=22, family="Helvetica") 
pdf(file=paste(path_figures,"Figure9_Conc_IOPs_depths_2011_2014.pdf",sep=""),
        width=1300/100, height =370/100, pointsize=16, family="Helvetica") 
    par(mfrow=c(1,2))
    par(mar=c(2,1,1,1))
    par(oma=c(1,7,0,7))
    #layout.show(n=2)
    cexlabs=0.9
    anchol=4
    cexaxis=0.9

    ########################
    i=elegidos[1]
    archivo<-paste(nombres[i],"/result.nc",sep="")
    filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
    filenc <- open.nc(filename)
    filerc <- read.nc(filenc)
    tiempo<-filerc$time/86400
    meses<-tiempo/30
    mid_meses<-seq(-2192+15, 4382-15, length=18*12)
    meses<-c(1:(18*12))
    etiquetas_meses<-c(paste("2003",c(1:12),sep="-"),paste("2004",c(1:12),sep="-"),
                       paste("2005",c(1:12),sep="-"),paste("2006",c(1:12),sep="-"),
                       paste("2007",c(1:12),sep="-"),paste("2008",c(1:12),sep="-"),
                       paste("2009",c(1:12),sep="-"),paste("2010",c(1:12),sep="-"),
                       paste("2011",c(1:12),sep="-"),paste("2012",c(1:12),sep="-"),
                       paste("2013",c(1:12),sep="-"),paste("2014",c(1:12),sep="-"),
                       paste("2015",c(1:12),sep="-"),paste("2016",c(1:12),sep="-"),
                       paste("2017",c(1:12),sep="-"),paste("2018",c(1:12),sep="-"),
                       paste("2019",c(1:12),sep="-"),paste("2020",c(1:12),sep="-")) 

    mesitos<-tiempo[(365*2):max(tiempo)]
    anitos<-rep(mesitos[1:365],length=length(mesitos))
    time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
    ##################
    
    # Find DCM indexes
    ##################              
    myfun<-function(x){ res<-which(x==max(x,na.rm=T))
    if (length(res)>1) res<-res[1]
    return(res)}
    TChla<-filerc$P1_Chl+filerc$P2_Chl+filerc$P3_Chl+filerc$P4_Chl
    dcm<-apply(TChla, MARGIN=2, FUN=myfun)      # these are indexes not depths
    dcm_depth<-depth_center[dcm]
    
## SURFACE: means in the first 10m
##################################   

  plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
       main="", ylab=expression(paste("TChl-a (mg ", m^-3,")", sep="  ")), xlab="",
       xlim=c(1,365), xaxs="i", ylim=c(0,1.4), yaxs="i", mgp=c(2.5,1,0), xaxt="n") 
    axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
    
       #abline(h=0.01,lty=3, col="grey")
       #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
        #text(x=mid_meses[seq(1,length(mid_meses),by=6)][17:25], y = par("usr")[3] - 0.05,
             #labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][17:25], xpd = NA,srt = 40,adj = 0.965,cex = 0.6) 

#### Phyto chl
################  
      R0<-filerc$P1_Chl+filerc$P2_Chl+filerc$P3_Chl+filerc$P4_Chl
      aph<-colMeans(R0[171:196,(365*2):max(tiempo)])
      #points(x=tiempo, y=aph, type="b", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2)
      #points(x=anitos, y=aph, type="p", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2)
      #range(anitos)
      time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
      groups<-split(aph, time_clases, drop = FALSE)
              #seecol(pal = "unikn_all")
              mycol<-usecol(pal_seegruen,n=4,alpha=0.2)
              upper<-sapply(groups,FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
              lower<-sapply(groups,FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
                    equis<-c(c(1:365),c(1:365)[length(c(1:365)):1])
                    ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
                    polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[4], border=NA)
                    ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
                    polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[3], border=NA)
                    ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
                    polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[2], border=NA)
                    ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
                    polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[1], border=NA)
                    media<-sapply(groups,median)
                    media_phyto<-media
                    points(x=c(1:365), y=media, type="l", col=linea_phyto, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
####################
      #axis(2, at=seq(0,1.4,length=8), labels=seq(0,1.4,length=8), las=1, cex.axis=1)
      mtext(2, at=0.7, line=2.5, text=expression(paste("TChl-a (mg ", m^-3,")", sep="  ")), cex=cexlabs, outer=F)
                    
      
#### acdom 450nm
################            
      R0<-filerc$lightspectral_acdom450
      #adg<-colMeans(R0[171:196,])
      #points(x=tiempo, y=adg*20, type="p", col="orange2", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2)
      adg<-colMeans(R0[171:196,(365*2):max(tiempo)])*20
      #aph<-colMeans(R0[171:196,(365*2):max(tiempo)])
      #points(x=tiempo, y=aph, type="b", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2)
      #points(x=anitos, y=aph, type="p", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2)
      #range(anitos)
      time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
      groups<-split(adg, time_clases, drop = FALSE)
              #seecol(pal = "unikn_all")
              mycol<-usecol(pal_peach,n=4,alpha=0.2)
              upper<-sapply(groups,FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
              lower<-sapply(groups,FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
              equis<-c(c(1:365),c(1:365)[length(c(1:365)):1])
              ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
              polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[4], border=NA)
              ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
              polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[3], border=NA)
              ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
              polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[2], border=NA)
              ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
              polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[1], border=NA)
              media<-sapply(groups,median)
              media_cdom<-media
              points(x=c(1:365), y=media, type="l", col=linea_cdom, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
              
#####################      
      axis(4, at=seq(0,1.4,length=8), labels=F, las=1, cex.axis=1)
      #mtext(4, at=0.5, line=3, text=expression(paste(m^-1, sep="")), cex=0.75, outer=F)
      
      
#### Bact
####################     
      R0<-filerc$B1_c
      #aph<-colMeans(R0[171:196,])
      #points(x=tiempo, y=aph/30, type="p", col="blue3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2) 
      aph<-colMeans(R0[171:196,(365*2):max(tiempo)])/30
      #aph<-colMeans(R0[171:196,(365*2):max(tiempo)])
      #points(x=tiempo, y=aph, type="b", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2)
      #points(x=anitos, y=aph, type="p", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2)
      #range(anitos)
      time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
      groups<-split(aph, time_clases, drop = FALSE)
            #seecol(pal = "unikn_all")
            mycol<-usecol(pal_karpfenblau,n=4,alpha=0.2)
            upper<-sapply(groups,FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-sapply(groups,FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(c(1:365),c(1:365)[length(c(1:365)):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_bact[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_bact[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_bact[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_bact[1], border=NA)
            media<-sapply(groups,median)
            media_bact<-media
            points(x=c(1:365), y=media, type="l", col=linea_bact, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
####################       
      #axis(4,  line=4.5, at=seq(0,1,length=6), labels=seq(0,1,length=6)*30, las=1, cex.axis=1)
      #mtext(4, at=0.5, line=7, text=expression(paste("mg ", m^-3, sep="  ")), cex=0.75, outer=F)
     
  
#### DOC
####################     
     R0<-filerc$R1_c+filerc$R2_c+filerc$R3_c+filerc$X1_c+filerc$X2_c+filerc$X3_c
            #aph<-colMeans(R0[171:196,])
            #points(x=tiempo, y=aph/30, type="p", col="blue3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2) 
            aph<-colMeans(R0[171:196,(365*2):max(tiempo)])/12
            aph<-aph/50
            #aph<-colMeans(R0[171:196,(365*2):max(tiempo)])
            #points(x=tiempo, y=aph, type="b", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2)
            #points(x=anitos, y=aph, type="p", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2)
            #range(anitos)
            time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
            groups<-split(aph, time_clases, drop = FALSE)
            #seecol(pal = "unikn_all")
            mycol<-usecol(pal_grau,n=4,alpha=0.2)
            upper<-sapply(groups,FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-sapply(groups,FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(c(1:365),c(1:365)[length(c(1:365)):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[1], border=NA)
            media<-sapply(groups,median)
            points(x=c(1:365), y=media, type="l", col=linea_grey, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
####################       
         axis(2,  line=4.5, at=seq(0,1.4,length=8), labels=seq(0,1.4,length=8)*50, las=1, cex.axis=1)
         mtext(2, at=0.7, line=6.5, text=expression(paste("DOC (",mu,"mol ", L^-1,")", sep="  ")), cex=cexlabs, outer=F)
            

  ################################## 
  ## Replot medians so they are visible
         points(x=c(1:365), y=media_bact, type="l", col=linea_bact, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
         points(x=c(1:365), y=media_cdom, type="l", col=linea_cdom, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
         points(x=c(1:365), y=media_phyto, type="l", col=linea_phyto, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
         
  #legend(x="topright",legend=c("phyto_chl","aCDOM(450)","bact_c"),col=c("green3", "orange2", "blue"), bty="n", lwd=2) 
  legend(x="topright", legend="(0-9m)", cex=1.2,bty="n")
     
          

## Concentrations and IOPs at the DCM
#####################################
          dcm2<-dcm[(365*2):max(tiempo)]
#dcm          # indexes
#dcm_depth    # depths

  plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
         main="", ylab="", xlab="",
         xlim=c(1,365), xaxs="i", ylim=c(0,1.4), yaxs="i", mgp=c(2.5,1,0), yaxt="n", xaxt="n")
  axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
  
         #abline(h=0.01,lty=3, col="grey")
         #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
         #text(x=mid_meses[seq(1,length(mid_meses),by=6)][17:25], y = par("usr")[3] - 0.1,
             #labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][17:25], xpd = NA,srt = 40,adj = 0.965,cex = 0.6) 

  legend(x=150,y=1.4,legend=c("DOC","TChla",(expression(paste(a[CDOM],"(450)"))),expression(B[C])),
         col=c(linea_grey, linea_phyto, linea_cdom, linea_bact), bty="n", lwd=anchol, cex=0.75)  

  
          # Phyto chl
          ############
          R0<-filerc$P1_Chl+filerc$P2_Chl+filerc$P3_Chl+filerc$P4_Chl
          R0<-R0[,(365*2):max(tiempo)]
          aph<-rep(NA,length=ncol(R0))
          for (i in c(1:ncol(R0))) aph[i]<-R0[dcm2[i],i]
              time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
              groups<-split(aph, time_clases, drop = FALSE)
              #seecol(pal = "unikn_all")
              mycol<-usecol(pal_seegruen,n=4,alpha=0.2)
              upper<-sapply(groups,FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
              lower<-sapply(groups,FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
              equis<-c(c(1:365),c(1:365)[length(c(1:365)):1])
              ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
              polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[4], border=NA)
              ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
              polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[3], border=NA)
              ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
              polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[2], border=NA)
              ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
              polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[1], border=NA)
              media<-sapply(groups,median)
              media_phyto<-media
              points(x=c(1:365), y=media, type="l", col=linea_phyto, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
          ####################           
          axis(2, at=seq(0,1.4,length=8), labels=F, las=1, cex.axis=1)
              
          # acdom 450nm
          #############    
          R0<-filerc$lightspectral_acdom450
          R0<-R0[,(365*2):max(tiempo)]
          adg<-rep(NA,length=ncol(R0))
          for (i in c(1:ncol(R0))) adg[i]<-R0[dcm2[i],i]*20
          
                  #aph<-colMeans(R0[171:196,(365*2):max(tiempo)])
                  #points(x=tiempo, y=aph, type="b", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2)
                  #points(x=anitos, y=aph, type="p", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2)
                  #range(anitos)
                  time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
                  groups<-split(adg, time_clases, drop = FALSE)
                  #seecol(pal = "unikn_all")
                  mycol<-usecol(pal_peach,n=4,alpha=0.2)
                  upper<-sapply(groups,FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
                  lower<-sapply(groups,FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
                  equis<-c(c(1:365),c(1:365)[length(c(1:365)):1])
                  ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
                  polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[4], border=NA)
                  ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
                  polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[3], border=NA)
                  ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
                  polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[2], border=NA)
                  ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
                  polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[1], border=NA)
                  media<-sapply(groups,median)
                  media_cdom<-media
                  points(x=c(1:365), y=media, type="l", col=linea_cdom, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
          #################
          axis(4, at=seq(0,1.4,length=8), labels=seq(0,1.4,length=8)/20, las=1, cex.axis=1)
          mtext(4, at=0.7, line=3, text=expression(paste(a[CDOM],"(450) (", m^-1,")", sep="")), cex=cexlabs, outer=F)
          
          # Bact
          ##############
          R0<-filerc$B1_c
          R0<-R0[,(365*2):max(tiempo)]
          aph<-rep(NA,length=ncol(R0))
          for (i in c(1:ncol(R0))) aph[i]<-R0[dcm2[i],i]/30
                  time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
                  groups<-split(aph, time_clases, drop = FALSE)
                  #seecol(pal = "unikn_all")
                  mycol<-usecol(pal_karpfenblau,n=4,alpha=0.2)
                  upper<-sapply(groups,FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
                  lower<-sapply(groups,FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
                  equis<-c(c(1:365),c(1:365)[length(c(1:365)):1])
                  ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
                  polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_bact[4], border=NA)
                  ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
                  polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_bact[3], border=NA)
                  ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
                  polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_bact[2], border=NA)
                  ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
                  polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_bact[1], border=NA)
                  media<-sapply(groups,median)
                  media_bact<-media
                  points(x=c(1:365), y=media, type="l", col=linea_bact, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
          ################ 
          axis(4,  line=4.5, at=seq(0,1.4,length=8), labels=seq(0,1.4,length=8)*30, las=1, cex.axis=1)
          mtext(4, at=0.7, line=7, text=expression(paste(B[C]," (mg ", m^-3,")", sep="  ")), cex=cexlabs, outer=F)
 
          
          #### DOC
          ####################     
          R0<-filerc$R1_c+filerc$R2_c+filerc$R3_c+filerc$X1_c+filerc$X2_c+filerc$X3_c
          R0<-R0[,(365*2):max(tiempo)]
          aph<-rep(NA,length=ncol(R0))
          for (i in c(1:ncol(R0))) aph[i]<-R0[dcm2[i],i]/12          
          aph<-aph/50
          #aph<-colMeans(R0[171:196,(365*2):max(tiempo)])
          #points(x=tiempo, y=aph, type="b", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2)
          #points(x=anitos, y=aph, type="p", col="green3", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2)
          #range(anitos)
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(aph, time_clases, drop = FALSE)
          #seecol(pal = "unikn_all")
          mycol<-usecol(pal_grau,n=4,alpha=0.2)
          upper<-sapply(groups,FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
          lower<-sapply(groups,FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
          equis<-c(c(1:365),c(1:365)[length(c(1:365)):1])
          ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
          polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[4], border=NA)
          ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
          polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[3], border=NA)
          ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
          polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[2], border=NA)
          ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
          polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[1], border=NA)
          media<-sapply(groups,median)
          points(x=c(1:365), y=media, type="l", col=linea_grey, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
          ####################       
          #axis(2,  line=4.5, at=seq(0,1.4,length=8), labels=seq(0,1.4,length=8)*40, las=1, cex.axis=1)
          #mtext(2, at=0.7, line=7, text=expression(paste(mu,"mol ", L^-1, sep="  ")), cex=0.75, outer=F)
         
 #####################################
          ## Replot medians so they are visible
          points(x=c(1:365), y=media_bact, type="l", col=linea_bact, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
          points(x=c(1:365), y=media_cdom, type="l", col=linea_cdom, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
          points(x=c(1:365), y=media_phyto, type="l", col=linea_phyto, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
          
 legend(x="topright", legend="DCM", cex=1.2,bty="n")
          
dev.off()      















#################
### FIGURE 10 ### Absorption budget
#################

#png(file=paste(path_figures,"Figure10_Absorption_budget_depths_2011_2014.png",sep=""),width=1200, height=400, pointsize=22, family="Helvetica") 
pdf(file=paste(path_figures,"Figure10_Absorption_budget_depths_2011_2014.pdf",sep=""),
    width=1200/100, height=400/100, pointsize=16, family="Helvetica")

    layout(matrix(c(1,2),ncol=2, byrow=T))
    par(mar=c(0,0,0,0))
    par(oma=c(0,1,2,1))
    #layout.show(n=2)
    anchol=3
    cexletter=1.6
    cexaxis=0.9

    ########################
    i=elegidos[1]
    archivo<-paste(nombres[i],"/result.nc",sep="")
    filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
    filenc <- open.nc(filename)
    filerc <- read.nc(filenc)
    tiempo<-filerc$time/86400
    meses<-tiempo/30
    mid_meses<-seq(-2192+15, 4382-15, length=18*12)
    meses<-c(1:(18*12))
    etiquetas_meses<-c(paste("2003",c(1:12),sep="-"),paste("2004",c(1:12),sep="-"),
                       paste("2005",c(1:12),sep="-"),paste("2006",c(1:12),sep="-"),
                       paste("2007",c(1:12),sep="-"),paste("2008",c(1:12),sep="-"),
                       paste("2009",c(1:12),sep="-"),paste("2010",c(1:12),sep="-"),
                       paste("2011",c(1:12),sep="-"),paste("2012",c(1:12),sep="-"),
                       paste("2013",c(1:12),sep="-"),paste("2014",c(1:12),sep="-"),
                       paste("2015",c(1:12),sep="-"),paste("2016",c(1:12),sep="-"),
                       paste("2017",c(1:12),sep="-"),paste("2018",c(1:12),sep="-"),
                       paste("2019",c(1:12),sep="-"),paste("2020",c(1:12),sep="-")) 
    
    mesitos<-tiempo[(365*2):max(tiempo)]
    anitos<-rep(mesitos[1:365],length=length(mesitos))  
    time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
    #######################

    # Find DCM indexes
    ##################              
    myfun<-function(x){ res<-which(x==max(x,na.rm=T))
    if (length(res)>1) res<-res[1]
    return(res)}
    TChla<-filerc$P1_Chl+filerc$P2_Chl+filerc$P3_Chl+filerc$P4_Chl
    dcm<-apply(TChla, MARGIN=2, FUN=myfun)      # these are indexes not depths
    dcm_depth<-depth_center[dcm]
    
    # ## % for the text
    # ####################
    # aCDOM<-filerc$lightspectral_acdom450[138:196,]
    # aNAP<-filerc$lightspectral_anap450[138:196,]
    # aPH<-filerc$lightspectral_aph450[138:196,]
    #     resul_atot<-abind(aNAP,aCDOM,aPH,along=0.5)
    #     myfunc<-function(x){x/sum(x,na.rm=TRUE)}
    #     resul2<-apply(resul_atot, MARGIN=c(2,3), FUN=myfunc)
    #     per_acdom<-resul2[2,,]
    #     min(per_acdom,na.rm=T)
    #     max(per_acdom,na.rm=T)
    #     mean(per_acdom,na.rm=T)

    ## Absorption budget surface
    ############################
    R0<-filerc$lightspectral_acdom450
    aCDOM<-colMeans(R0[171:196,])
    R0<-filerc$lightspectral_anap450
    aNAP<-colMeans(R0[171:196,])
    R0<-filerc$lightspectral_aph450
    aPH<-colMeans(R0[171:196,])
    
    resul_atot<-abind(aNAP,aCDOM,aPH,along=0.5)
    # Proportions
    myfunc<-function(x){x/sum(x,na.rm=TRUE)}
    resul2<-apply(resul_atot, MARGIN=c(2), FUN=myfunc)
    resul<-resul2
    resul<-t(resul)
    colnames(resul)<-c("Anap","Acdom","Aph")
    res<-resul[(365*2):max(tiempo),]  
    #################

        # Timeseries of %
        par(mar=c(3.4,3.4,0,1)) 
        plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
             main="", ylab="", xlab="",
             xlim=c(1,365),xaxs="i", ylim=c(0,1), yaxs="i", mgp=c(2.2,1,0), xaxt="n", yaxt="n")
        mtext(2,at=0.5,line=2, text=expression(paste("%", sep="  ")), las=1)
        axis(2, at=seq(0.2,1,by=0.2), labels=100*seq(0.2,1,by=0.2), las=1)
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        mtext(2, at=1.1, line=-1, text="(a)", cex=cexletter, font=2, las=1, outer=F)
        
            # anap
            ################# 
            aph<-res[,1]
            groups<-split(aph, time_clases, drop = FALSE)
            upper<-sapply(groups,FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-sapply(groups,FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(c(1:365),c(1:365)[length(c(1:365)):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[1], border=NA)
            media<-sapply(groups,median)
            points(x=c(1:365), y=media, type="l", col=linea_grey, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
            ################          
            
            # acdom
            ################ 
            aph<-res[,2]
            groups<-split(aph, time_clases, drop = FALSE)
            upper<-sapply(groups,FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-sapply(groups,FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(c(1:365),c(1:365)[length(c(1:365)):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[1], border=NA)
            media<-sapply(groups,median)
            points(x=c(1:365), y=media, type="l", col=linea_cdom, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
            #############        
            
            # aph
            ############ 
            aph<-res[,3]
            groups<-split(aph, time_clases, drop = FALSE)
            upper<-sapply(groups,FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
            lower<-sapply(groups,FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
            equis<-c(c(1:365),c(1:365)[length(c(1:365)):1])
            ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[4], border=NA)
            ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[3], border=NA)
            ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[2], border=NA)
            ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
            polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[1], border=NA)
            media<-sapply(groups,median)
            points(x=c(1:365), y=media, type="l", col=linea_phyto, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
            ###############          
        
        #################
        legend(x="topright", legend="(0-9m)", cex=1.4, bty="n", text.font=1)
        #legend(x="topright",legend=c("aCDOM(450)", "aPH(450)","aNAP(450)"),col=c("orange3", "green3", "red2"), bty="n", lwd=2) 
        




      ## Absorption budget DCM
      ####################
      R0<-filerc$lightspectral_acdom450
      aCDOM<-rep(NA,length=ncol(R0))
      for (i in c(1:ncol(R0))) aCDOM[i]<-R0[dcm[i],i]
      R0<-filerc$lightspectral_anap450
      aNAP<-rep(NA,length=ncol(R0))
      for (i in c(1:ncol(R0))) aNAP[i]<-R0[dcm[i],i]
      R0<-filerc$lightspectral_aph450
      aPH<-rep(NA,length=ncol(R0))
      for (i in c(1:ncol(R0))) aPH[i]<-R0[dcm[i],i]
      resul_atot<-abind(aNAP,aCDOM,aPH,along=0.5)
      # Proportions
      myfunc<-function(x){x/sum(x,na.rm=TRUE)}
      resul2<-apply(resul_atot, MARGIN=c(2), FUN=myfunc)
      resul<-resul2
      resul<-t(resul)
      colnames(resul)<-c("Anap","Acdom","Aph")
      res<-resul[(365*2):max(tiempo),]
      #################

          # Timeseries of %
          par(mar=c(3.4,3.4,0,1)) 
          plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
               main="", ylab="", xlab="",
               xlim=c(1,365),xaxs="i", ylim=c(0,1), yaxs="i", mgp=c(2.2,1,0), xaxt="n", yaxt="n") 
          mtext(2,at=0.5,line=2, text=expression(paste("%", sep="  ")), las=1)
          axis(2, at=seq(0.2,1,by=0.2), labels=100*seq(0.2,1,by=0.2), las=1)
          axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
          mtext(2, at=1.1, line=-1, text="(b)", cex=cexletter, font=2, las=1, outer=F)
          
                # anap
                #################
                aph<-res[,1]
                groups<-split(aph, time_clases, drop = FALSE)
                upper<-sapply(groups,FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
                lower<-sapply(groups,FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
                equis<-c(c(1:365),c(1:365)[length(c(1:365)):1])
                ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
                polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[4], border=NA)
                ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
                polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[3], border=NA)
                ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
                polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[2], border=NA)
                ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
                polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[1], border=NA)
                media<-sapply(groups,median)
                points(x=c(1:365), y=media, type="l", col=linea_grey, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
                ################          
                
                # acdom
                ################
                aph<-res[,2]
                groups<-split(aph, time_clases, drop = FALSE)
                upper<-sapply(groups,FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
                lower<-sapply(groups,FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
                equis<-c(c(1:365),c(1:365)[length(c(1:365)):1])
                ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
                polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[4], border=NA)
                ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
                polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[3], border=NA)
                ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
                polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[2], border=NA)
                ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
                polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_cdom[1], border=NA)
                media<-sapply(groups,median)
                points(x=c(1:365), y=media, type="l", col=linea_cdom, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
                #############        
                
                # aph
                ############
                aph<-res[,3]
                groups<-split(aph, time_clases, drop = FALSE)
                upper<-sapply(groups,FUN=quantile, probs=c(0.6,0.7,0.8,0.9), na.rm=T)
                lower<-sapply(groups,FUN=quantile, probs=c(0.4,0.3,0.2,0.1), na.rm=T)
                equis<-c(c(1:365),c(1:365)[length(c(1:365)):1])
                ies<-c(upper[4,],lower[4,][length(lower[4,]):1])
                polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[4], border=NA)
                ies<-c(upper[3,],lower[3,][length(lower[3,]):1])
                polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[3], border=NA)
                ies<-c(upper[2,],lower[2,][length(lower[2,]):1])
                polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[2], border=NA)
                ies<-c(upper[1,],lower[1,][length(lower[1,]):1])
                polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_phyto[1], border=NA)
                media<-sapply(groups,median)
                points(x=c(1:365), y=media, type="l", col=linea_phyto, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
                ###############            
          
          #################
          legend(x="topright", legend="DCM", cex=1.4, bty="n", text.font=1)
          legend(x=110,y=1,legend=c(expression(a[CDOM] (450)), expression(a[PH] (450)),expression(a[NAP] (450))), col=c(linea_cdom, linea_phyto, linea_grey), bty="n", lwd=anchol) 


dev.off()      







#################
### FIGURE 11 ### Bio-optical relationships
#################

#png(file=paste(path_figures,"Figure11_Bio_optical_relationships_2011_2014.png",sep=""),width=1200, height=580, pointsize=20, family="Helvetica") 
pdf(file=paste(path_figures,"Figure11_Bio_optical_relationships_2011_2014.pdf",sep=""),width=1200/100, height=580/100, pointsize=15, family="Helvetica") 
            par(mfrow=c(1,2))
            par(mar=c(4,4,2,1))
            par(oma=c(0,1,0,0))
            cexletter=1.6
            
            ########################
            i=elegidos[1]
            archivo<-paste(nombres[i],"/result.nc",sep="")
            filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
            filenc <- open.nc(filename)
            filerc <- read.nc(filenc)
            tiempo<-filerc$time/86400
 
            ## SURFACE: means in the first 10m
            ################################## 
            
      ### aCDOM(440) vs. TChla      
            plot(x=1, y=1, type="n", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
                 main="",ylab=expression(paste(a[CDOM], "(450) ", m^-1, sep="  ")),
                 xlab=expression(paste("TChl-a (mg ", m^-3,")", sep="  ")),
                 xaxs="i",xaxt="n", yaxs="i", mgp=c(2.6,1,0),
                 ylim=c(0.00,0.07),
                 #ylim=c(-4,1),
                 xlim=c(-3,1)) 
            axis(1, at=seq(-3,1, length=5), labels=10^seq(-3,1, length=5))
            mtext(2, at=0.07, line=3, text="(a)", cex=cexletter, font=2, las=1, outer=F)
            
            
  ## Observations Boussole          
  #########################     
            ### TChla
            p_dir <- paste(o_dir,"BOUSSOLE_DATA/",sep="")
            reflectancia<-read.csv(paste(p_dir,"Boussole_HPLC.csv", sep=""), sep=",", header=T)
            fechita<-paste(reflectancia$year,reflectancia$month,reflectancia$day, sep="-")
            horita<-paste((reflectancia$time)-(reflectancia$time%%1),round((reflectancia$time%%1)*60,0),"00", sep=":")
            horita<-paste("00","00","00", sep=":")
            fechas <- chron(dates=fechita,times=horita,format=c("y-m-d","h:m:s"), out.format=c("y-m-d","h:m:s")) 
            julianos<- julian(x=month(fechas), d=day(fechas), y=year(fechas), origin.=c(month = 1, day = 1, year = 2009))
            etiquetas_julianas<- julian(x=1, d=1, y=seq(2009,2015,by=1), origin.=c(month = 1, day = 1, year = 2009))
            
            fechas2<-fechas   #[33:length(fechas)]
            datos2<-reflectancia$Tchla
            depth2<-reflectancia$Depth
            #length(datos2)
            
            ### Acdom 450
            p_dir <- paste(o_dir,"BOUSSOLE_DATA/",sep="")
            reflectancia<-read.csv(paste(p_dir,"BOUSSOLE_ACDOM.csv", sep=""), sep=",", header=T)
            #names(reflectancia)
            fechita<-paste(reflectancia$year,reflectancia$month,reflectancia$day, sep="-")
            horita<-paste(reflectancia$Time_CTD_start..UTC.,"00", sep=":")
            horita<-paste("00","00","00", sep=":")
            fechas <- chron(dates=fechita,times=horita,format=c("y-m-d","h:m:s"), out.format=c("y-m-d","h:m:s")) 
            julianos<- julian(x=month(fechas), d=day(fechas), y=year(fechas), origin.=c(month = 1, day = 1, year = 2009))
            etiquetas_julianas<- julian(x=1, d=1, y=seq(2009,2015,by=1), origin.=c(month = 1, day = 1, year = 2009))
            
            fechas3<-fechas#[33:length(fechas)]
            datos3<-rowMeans(reflectancia[,258:282])
            depth3<-reflectancia$depth..m.
            #length(datos3)
           
            m <- match(interaction(fechas2, depth2), interaction(fechas3, depth3))
            chla_value<-datos2[!is.na(m)]
            chla_fecha<-fechas2[!is.na(m)]
            chla_depth<-depth2[!is.na(m)]
            #length(chla_value)
            m<-m[!is.na(m)]
            #length(m)
            aph_value<-datos3[m]
            aph_fecha<-fechas3[m]
            aph_depth<-depth3[m]          
            #length(aph_value)           
  #########################
            
            #data.frame(chla_fecha,aph_fecha,chla_depth,aph_depth,chla_value,aph_value) 
            points(log10(chla_value),aph_value, pch=19, col="grey80", cex=0.7)
            #range(aph_fecha)
            
            ## Phyto
            R0<-filerc$P1_Chl+filerc$P2_Chl+filerc$P3_Chl+filerc$P4_Chl
            cloro<-colMeans(R0[171:196,])[(365*2):max(tiempo)]
            ## acdom 450nm
            R0<-filerc$lightspectral_acdom450
            adg<-colMeans(R0[171:196,])[(365*2):max(tiempo)]
            #points(x=tiempo, y=adg, type="p", col=colores[2], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2)               
            points(log10(cloro),adg, las=1, pch=19, col=colores_estaciones, cex=0.8)
            
            newx <- seq(2e-3, 20, length=10000)
            # Morel & Gentili 2009
            newc <- 0.0316*(newx^0.63)
            points(log10(newx), newc, type="l", col="grey30", lwd=2, lty=1)            
            # Bricaud 2010
            newc <- 0.034*(newx^0.619)
            points(log10(newx), newc, type="l", col="grey30", lwd=2, lty=2)
            
     legend(x=-3,y=0.06,legend=c("Winter (J-M)","Spring (A-J)","Summer (J-S)","Fall (O-D)"),
            col=unique(colores_estaciones), bty="n", pch=19)  
     
     legend(x="topleft", legend=c(expression(paste("Morel & Gentili (2009b)",sep="")),
                                  expression(paste("Bricaud ", italic("et al"),". (2010)",sep=""))),
            col=c("grey30", "grey30"), bty="n", lwd=2, lty=c(1,2)) 
     
     legend(x="bottomright",legend=expression(paste(italic("In situ")," ",BOUSSOLE, sep="")),
            col="grey80", bty="n", pch=19)            
     
          
      ### aCDOM(440) vs. DOC      
      plot(x=1, y=1, type="n", lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
                 main="",ylab=expression(paste("DOC (",mu, "mol ", L^-1,")", sep="  ")),
                 xlab=expression(paste(a[CDOM], "(250) ", m^-1, sep="  ")),
                 xaxs="i", yaxs="i", mgp=c(2.6,1,0),
                 ylim=c(25,85), xlim=c(0.5,1.8))
                 mtext(2, at=85, line=2.5, text="(b)", cex=cexletter, font=2, las=1, outer=F)

                 
          ## Areas DOC Literature                 
          dir<-paste(o_dir,"figures_DOC_literature/", sep="")
          data_Cat_surf<-read.csv(file=paste(dir,"Catala2018_surf.csv",sep=""))
          polygon(x=data_Cat_surf$x,y=data_Cat_surf$y,  col="grey88", border=NA)
          data_Cat_depth<-read.csv(file=paste(dir,"Catala2018_depth.csv",sep=""))
          polygon(x=data_Cat_depth$x,y=data_Cat_depth$y, col="grey86", border=NA)
          data_Gat_surf<-read.csv(file=paste(dir,"Galletti2019_surf.csv",sep=""))
          polygon(x=data_Gat_surf$x,y=data_Gat_surf$y,  col="grey92", border=NA)
          data_Gat_depth<-read.csv(file=paste(dir,"Galletti2019_depth.csv",sep=""))
          polygon(x=data_Gat_depth$x,y=data_Gat_depth$y, col="grey90", border=NA)
   
            ## DOC  (0-9m) 
            R0<-filerc$R1_c+filerc$R2_c+filerc$R3_c
            doc<-colMeans(R0[171:196,])[(365*2):max(tiempo)]
            doc<-doc/12
            # acdom 450nm
            R0<-filerc$lightspectral_acdom250
            adg<-colMeans(R0[171:196,])[(365*2):max(tiempo)]

            # My fit
            fit5 <- lm(doc~adg)
            newx <- seq(2e-2, 2, length=50)
            newc <- fit5$coefficients[1]+newx*fit5$coefficients[2]
            #points(newx, newc, type="l", col="black", lwd=2)
            
            # Catala 2018
            newc <- 9+newx*46
            points(newx, newc, type="l", col="grey30", lwd=2, lty=c(1))            
            # Galletti 2019
            newc <- 15+newx*32
            points(newx, newc, type="l", col="grey30", lwd=2, lty=c(2))            
            
            points(adg,doc, las=1, pch=19, col=colores_estaciones, cex=0.8)
            
            name <- "Catalá"
            Encoding(name) <- "UTF-8"              
            legend(x="topleft",legend=c("",""),fill=c("grey86", "grey90"), bty="n", border=NA)  
            legend(x="bottomright",legend=c("Winter (J-M)","Spring (A-J)","Summer (J-S)","Fall (O-D)"),
                   col=unique(colores_estaciones), bty="n", pch=19)            
            legend(x="topleft",
                   legend=c(substitute(paste(a, " ", italic("et al"),". (2018)",sep=""), list(a = name)),
                            expression(paste("Galletti ", italic("et al"),". (2019)",sep=""))),
                   col=c("grey30", "grey30"), bty="n", lwd=2, lty=c(1,2))
            ##################################  
            
dev.off()      
