if (Sys.info()['sysname']=="Windows") {OS<-"C:/"} else {OS<-paste("/Users/",Sys.info()['user'],"/", sep="")}

### Figures timeseries comparison to satellite (Section 3.2.4)

source(paste(o_dir,"Scripts/00_Color palette.R", sep=""))

    ## depth layers
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
   
    ## bidirectional function Q
    load(paste(o_dir,"SPINUP/QRrs_surf.RData",sep=""))
    QRrs<-rep(NA,length=dim(mod)[3])
    for (k in c(1:dim(mod)[3])){
      station <- mod[lon_mod>7.77 & lon_mod<8.03,  lat_mod>43.266667 & lat_mod<43.466667, k]
      # dim(station)  # 20km
      if (sum(!is.na(station))>10) QRrs[k]<-mean(station, na.rm=T)
    }
    #     plot(seq(15, 365, by=30), QRrs, las=1)
    QRrs_365<-approx(x=seq(15, 365, by=30), y=QRrs, xout=c(1:365), rule=2) 
    #     names(QRrs_365)
    #     points(c(1:365), QRrs_365$y)
    #     length(QRrs_365$y)
    min(QRrs_365$y)
    max(QRrs_365$y)
    
    ## Model runs
    experimentos <- read.csv(paste(o_dir,"run_log_ALL3.csv", sep=""), sep=",")    
    nombres<-experimentos$file 
    carpetas<-experimentos$folder
    elegidos<-c(7)
    nombres[elegidos]
    colores[elegidos]


############
### FIGURE 9
############

png(file=paste(path_figures,"Figure9_timeseries_surface_satellite.png",sep=""),width=1000, height =700, pointsize=24, family="Helvetica")
    par(mfcol=c(3,1))
    par(mar=c(2,5,0,2))
    par(oma=c(2,1,1,1))
    layout.show(n=3)
    cexdates=1
    cexunits=0.8

    ########################
    i=elegidos[1]
    archivo<-paste(nombres[i],"/result.nc",sep="")
    filename <- paste(OS,"Datos/Res_C28_Boussole/",carpetas[i], "/",archivo, sep="")
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
    length(mid_meses)
    length(etiquetas_meses)
    ########################

    ## TChla (0-9m) 
    plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
             main="", ylab="", xlab="yyyy-m",
             #xlim=c(-2193,4383),      #  2003 to 2020
             #xlim=c(-2193,3276),      #  2003 to 2017
             xlim=c((365*2)+2,max(tiempo)+20),    #  2009 to 2014
             xaxs="i", ylim=c(-1.6,0.8),yaxs="i", xaxt="n", yaxt="n") 
        #abline(h=0.01,lty=3, col="grey")
        axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
        axis(2, at=log10(c(0.03,0.1,0.3,1,3)), labels=c(0.03,0.1,0.3,1,3), las=1)
        #text(x=mid_meses[seq(1,length(mid_meses),by=6)][13:20], y = par("usr")[3] - 0.1,
        #     labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][13:20], xpd = NA,srt = 40,adj = 0.965,cex = 0.6)
        mtext(2, at=-0.3, line=2.2, text=expression(paste("mg ", m^-3, sep="")), las=1, cex=cexunits)
        legend(x="topleft", legend=expression(paste("TChl (0-9m) (","mg ", m^-3,")")), cex=1.2,bty="n")
        
        
        ## OCEANCOLOUR_MED_OPTICS_L3_REP_OBSERVATIONS_009_095
        ##########################        
        site<-paste(o_dir,"SAT_DATA/", sep="")
        archive<-paste(site,"dataset-oc-med-chl-multi-l3-chl_1km_daily-rep-v02_1653985969219.nc", sep="")
        filenc <- open.nc(archive)
        tmp <- read.nc(filenc)
        rrs <- tmp$CHL
        lon <- tmp$lon
        lat <- tmp$lat
        tim <- tmp$time
        dimnames(rrs)=list(x=lon, y=lat, t=tim)
        
        datos2<-rep(NA,length=length(tim))
        datos3<-rep(NA,length=length(tim))
        for (k in c(1:length(tim))){
          station <- rrs[lon>7.77 & lon<8.03,  lat>43.266667 & lat<43.466667, k]
          # dim(station)  # 20km
          if (sum(!is.na(station))>80) {datos2[k]<-mean(station, na.rm=T)
                                       datos3[k]<-sd(station, na.rm=T)}
        }
        fechas2<-chron(c(0:(length(tim)-1)),origin=c(month=1, day=1, year=2003), out.format=c("y-m-d","h:m:s"))
        julianos<- julian(x=month(fechas2), d=day(fechas2), y=year(fechas2), origin.=c(month = 1, day = 1, year = 2009))        
        fechas2<-fechas2[!is.na(datos2)]
        julianos<-julianos[!is.na(datos2)]
        datos3<-datos3[!is.na(datos2)]
        datos2<-datos2[!is.na(datos2)]
        
        ## Running averages
        ma <- function(x, n = 5){filter(x, rep(1 / n, n), sides = 2)}
        rsum <- ma(datos2,n=30) #1month
        polygon(x=c(julianos, julianos[length(julianos):1]),
               y=c(log10(datos2-datos3),log10(datos2+datos3)[length(datos3):1]), col=col_sat, border=NA) 
        ########################## 
        
        
        #### Model
        ##########     
        R0<-filerc$P1_Chl+filerc$P2_Chl+filerc$P3_Chl+filerc$P4_Chl
        aph<-colMeans(R0[171:196,])
        points(x=tiempo, y=log10(aph), type="p", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.4)

          # remove mixing periods
          aph[c(1124:1164,1498:1570)]<-NA
          points(x=tiempo, y=log10(aph), type="p", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.6) 
          o<-match(tiempo,julianos)
          sate<-datos2[o]
          model<-aph
          
          # Metrics
          x<-cbind(model, sate)
          x[x=="-Inf"] <- NA
          x<-x[complete.cases(x)==TRUE,]
          ene_chla   <- nrow(x)
          R_chla     <- rcorr(x, type="pearson")$r[1,2]
          RI_chla    <- exp(sqrt((sum((log(x[,2]/x[,1]))^2, na.rm=TRUE))/nrow(x)))                  
          MEF_chla   <- (sum((x[,2]-mean(x[,2]))^2) - sum((x[,1]-x[,2])^2)) / sum((x[,2]-mean(x[,2]))^2)             
          # igual a: 1-(RMSE_chla^2)/var(x[,2])
          RMSE_chla  <- sqrt((sum((x[,1]-x[,2])^2))/nrow(x))
          AE_chla    <- (sum(x[,1]-x[,2]))/nrow(x)
          AAE_chla   <- (sum(abs(x[,1]-x[,2])))/nrow(x)             
          
          legend(x=1940, y=log10(6.3),
                 legend=c(paste("R=",round(R_chla,2),sep=""),
                          paste("MEF=",round(MEF_chla,2),sep=""),
                          paste("RMSE=",round(RMSE_chla,3),sep=""),
                          paste("Bias=",round(AE_chla,3),sep="")   ),bty="n")                
        ########################    
          
### Boussole surface
#########################   
          p_dir <- paste(o_dir,"BOUSSOLE_DATA/",sep="")
          reflectancia<-read.csv(paste(p_dir,"Boussole_HPLC.csv", sep=""), sep=",", header=T)
          #names(reflectancia)
          fechita<-paste(reflectancia$year,reflectancia$month,reflectancia$day, sep="-")
          horita<-paste("00","00","00", sep=":")
          # range(horita)        # rrs ya esta filtrada entre 10:00 y 14:00
          fechas <- chron(dates=fechita,times=horita,format=c("y-m-d","h:m:s"), out.format=c("y-m-d","h:m:s")) 
          julianos<- julian(x=month(fechas), d=day(fechas), y=year(fechas), origin.=c(month = 1, day = 1, year = 2009))
          etiquetas_julianas<- julian(x=1, d=1, y=seq(2009,2015,by=1), origin.=c(month = 1, day = 1, year = 2009))
          fechas2<-fechas#[33:length(fechas)]
          datos2<-reflectancia$Tchla
          datos2[reflectancia$Depth>10]<-NA
          fechas2<-fechas2[!is.na(datos2)]
          julianos<-julianos[!is.na(datos2)]
          datos2<-datos2[!is.na(datos2)]
          points(julianos, log10(datos2), type="p", las=1, pch=19, cex=0.6, ylim=c(0,0.01),
                 col=col_bou, ylab=expression(m^-1), xlab="yy-mm",xlim=c(0,max(tiempo)), xaxs="i",yaxs="i", xaxt="n")
 
     ## Chla fluor
          # p_dir <- paste(o_dir,"BOUSSOLE_DATA/",sep="")
          # reflectancia<-read.csv(paste(p_dir,"orig/buoy.DPFF.2003-09-06_2012-12-31.csv", sep=""), sep=",", header=T)
          # names(reflectancia)
          # fechita<-paste(reflectancia$YEAR,reflectancia$MONTH,reflectancia$DAY, sep="-")
          # horita<-paste(reflectancia$HH,reflectancia$MM,"00", sep=":")
          # # range(horita)        # rrs ya esta filtrada entre 10:00 y 14:00
          # fechas <- chron(dates=fechita,times=horita,format=c("y-m-d","h:m:s"), out.format=c("y-m-d","h:m:s")) 
          # julianos<- julian(x=month(fechas), d=day(fechas), y=year(fechas), origin.=c(month = 1, day = 1, year = 2009))
          # etiquetas_julianas<- julian(x=1, d=1, y=seq(2009,2015,by=1), origin.=c(month = 1, day = 1, year = 2009))
          # fechas2<-fechas#[33:length(fechas)]
          # datos2<-reflectancia$chl
          # #datos2[reflectancia$Depth>10]<-NA
          # fechas2<-fechas2[!is.na(datos2)]
          # julianos<-julianos[!is.na(datos2)]
          # datos2<-datos2[!is.na(datos2)]
          # points(julianos, log10(datos2), type="p", las=1, pch=19, cex=0.6, ylim=c(0,0.01),
          #        col="green", ylab=expression(m^-1), xlab="yy-mm",xlim=c(0,max(tiempo)), xaxs="i",yaxs="i", xaxt="n")
#########################
          
#          legend(x=1300, y=0.7,legend=c(expression("Chla-fluor")),fill=c("green"), bty="o", border=NA)                
          
    ### aPH(450)(9m)
    plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
             main="",
             ylab="", xlab="yyyy-m",
             #xlim=c(-2193,4383),      #  2003 to 2020
             #xlim=c(-2193,3276),      #  2003 to 2017
             xlim=c((365*2)+2,max(tiempo)+20),    #  2009 to 2014
             xaxs="i", ylim=c(0,0.1),yaxs="i", xaxt="n") 
        #abline(h=0.01,lty=3, col="grey")
        axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
        mtext(2, at=0.05, line=3.7, text=expression(m^-1), las=1, cex=cexunits)
        #text(x=mid_meses[seq(1,length(mid_meses),by=6)], y = par("usr")[3] - 0.007,
             #labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)], xpd = NA,srt = 40,adj = 0.965,cex = 0.6) 
        legend(x="topleft", legend=expression(paste(a[PH],"(450) (0-9m) (", m^-1,")")), cex=1.2,bty="n")
        
        ## OCEANCOLOUR_MED_OPTICS_L3_REP_OBSERVATIONS_009_095
        ##########################        
        site<-paste(o_dir,"SAT_DATA/", sep="")
        archive<-paste(site,"dataset-oc-med-opt-multi-l3-aph443_1km_daily-rep-v02_1653324264839.nc", sep="")
        filenc <- open.nc(archive)
        tmp <- read.nc(filenc)
        rrs <- tmp$APH443
        lon <- tmp$lon
        lat <- tmp$lat
        tim <- tmp$time
        dimnames(rrs)=list(x=lon, y=lat, t=tim)
        
        datos2<-rep(NA,length=length(tim))
        datos3<-rep(NA,length=length(tim))
        for (k in c(1:length(tim))){
          station <- rrs[lon>7.77 & lon<8.03,  lat>43.266667 & lat<43.466667, k]
          # dim(station)  # 20km
          if (sum(!is.na(station))>80) {datos2[k]<-mean(station, na.rm=T)
                                        datos3[k]<-sd(station, na.rm=T)}
        }
        #fechas2<-chron(c(0:(length(tim)-1)),origin=c(month=1, day=1, year=2004), out.format=c("y-m-d","h:m:s"))
        fechas2<-chron(c(0:(length(tim)-1)),origin=c(month=1, day=1, year=2003), out.format=c("y-m-d","h:m:s"))
        julianos<- julian(x=month(fechas2), d=day(fechas2), y=year(fechas2), origin.=c(month = 1, day = 1, year = 2009))        
        fechas2<-fechas2[!is.na(datos2)]
        julianos<-julianos[!is.na(datos2)]
        datos3<-datos3[!is.na(datos2)]
        datos2<-datos2[!is.na(datos2)]
        
        ## Running averages
        ma <- function(x, n = 5){filter(x, rep(1 / n, n), sides = 2)}
        rsum <- ma(datos2,n=30) #1month
        polygon(x=c(julianos, julianos[length(julianos):1]),
                y=c((datos2-datos3),(datos2+datos3)[length(datos3):1]), col=col_sat, border=NA) 
        ########################## 
       
        #### Model
        ##########     
        R0<-filerc$lightspectral_aph450
        aph<-colMeans(R0[171:196,])
        points(x=tiempo, y=aph, type="p", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.4) 
         
          # remove mixing period
          aph[c(1124:1164,1498:1570)]<-NA
          points(x=tiempo, y=aph, type="p", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.6) 
          o<-match(tiempo,julianos)
          sate<-datos2[o]
          model<-aph
          
          # Metrics
          x<-cbind(model, sate)
          x[x=="-Inf"] <- NA
          x<-x[complete.cases(x)==TRUE,]
          ene_chla   <- nrow(x)
          R_chla     <- rcorr(x, type="pearson")$r[1,2]
          RI_chla    <- exp(sqrt((sum((log(x[,2]/x[,1]))^2, na.rm=TRUE))/nrow(x)))                  
          MEF_chla   <- (sum((x[,2]-mean(x[,2]))^2) - sum((x[,1]-x[,2])^2)) / sum((x[,2]-mean(x[,2]))^2)             
          # igual a: 1-(RMSE_chla^2)/var(x[,2])
          RMSE_chla  <- sqrt((sum((x[,1]-x[,2])^2))/nrow(x))
          AE_chla    <- (sum(x[,1]-x[,2]))/nrow(x)
          AAE_chla   <- (sum(abs(x[,1]-x[,2])))/nrow(x)             
        ########################   
          
          legend(x=1940, y=0.1,
                 legend=c(paste("R=",round(R_chla,2),sep=""),
                          paste("MEF=",round(MEF_chla,2),sep=""),
                          paste("RMSE=",round(RMSE_chla,4),sep=""),
                          paste("Bias=",round(AE_chla,4),sep="")   ),bty="n")            
          
                  
### Boussole observations
#########################    
          # Aph
          p_dir <- paste(OS,"Datos/Dat_SGlobal_depth/Boussole/",sep="")
          reflectancia<-read.csv(paste(p_dir,"BOUSSOLE_APHYT_new.csv", sep=""), sep=",", header=T)
          fechita<-paste(reflectancia$year,reflectancia$month,reflectancia$day, sep="-")
          horita<-paste(reflectancia$time,"00", sep=":")
          # range(horita)        # rrs ya esta filtrada entre 10:00 y 14:00
          fechas <- chron(dates=fechita,times=horita,format=c("y-m-d","h:m:s"), out.format=c("y-m-d","h:m:s")) 
          julianos<- julian(x=month(fechas), d=day(fechas), y=year(fechas), origin.=c(month = 1, day = 1, year = 2009))
          etiquetas_julianas<- julian(x=1, d=1, y=seq(2009,2015,by=1), origin.=c(month = 1, day = 1, year = 2009))
          fechas2<-fechas#[33:length(fechas)]
          datos2<-rowMeans(reflectancia[,149:173])
          #length(datos2)
          datos2[reflectancia$prof.m.>9]<-NA
          fechas2<-fechas2[!is.na(datos2)]
          julianos<-julianos[!is.na(datos2)]
          datos2<-datos2[!is.na(datos2)]
          points(julianos, datos2, type="p", las=1, pch=19, cex=0.6, ylim=c(0,0.01),
                 col=col_bou, ylab=expression(m^-1), xlab="yy-mm",xlim=c(0,max(tiempo)), xaxs="i",yaxs="i", xaxt="n")
#########################      
          
      legend(x=1225, y=0.0950,legend=c(expression("Satellite"), expression(paste(italic("In situ")," BOUSSOLE")), expression(italic("Optimized"))),
                 fill=c(col_sat, col_bou, colores[elegidos]), bty="o", border=NA)                
          
              

### aDG(450)(9m)
        plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
             main="",
             ylab="", xlab="yyyy-m",
             #xlim=c(-2193,4383),      #  2003 to 2020
             xlim=c((365*2)+2,max(tiempo)+20),    #  2009 to 2014
             #xlim=c(-2193,3276),      #  2003 to 2017
             xaxs="i", ylim=c(0,0.1),yaxs="i", xaxt="n")
        axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
        mtext(2, at=0.05, line=3.7, text=expression(m^-1), las=1, cex=cexunits)
        text(x=mid_meses[seq(1,length(mid_meses),by=3)][33:48], y = par("usr")[3] - 0.02*0.5,
             labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=3)][33:48], xpd = NA,srt = 40,adj = 0.965,cex = cexdates) 
        legend(x="topleft", legend=expression(paste(a[DG],"(450) (0-9m) (", m^-1,")")), cex=1.2,bty="n")
        
        ## OCEANCOLOUR_MED_OPTICS_L3_REP_OBSERVATIONS_009_095
        ##########################        
        site<-paste(o_dir,"SAT_DATA/", sep="")
        archive<-paste(site,"dataset-oc-med-opt-multi-l3-adg443_1km_daily-rep-v02_1653325971125.nc", sep="")
        filenc <- open.nc(archive)
        tmp <- read.nc(filenc)
        rrs <- tmp$ADG443
        lon <- tmp$lon
        lat <- tmp$lat
        tim <- tmp$time
        dimnames(rrs)=list(x=lon, y=lat, t=tim)
        
        datos2<-rep(NA,length=length(tim))
        datos3<-rep(NA,length=length(tim))
        for (k in c(1:length(tim))){
          station <- rrs[lon>7.77 & lon<8.03,  lat>43.266667 & lat<43.466667, k]
          # dim(station)  # 20km
          if (sum(!is.na(station))>80) {datos2[k]<-mean(station, na.rm=T)
                                       datos3[k]<-sd(station, na.rm=T)}
        }
        fechas2<-chron(c(0:(length(tim)-1)),origin=c(month=1, day=1, year=2003), out.format=c("y-m-d","h:m:s"))
        julianos<- julian(x=month(fechas2), d=day(fechas2), y=year(fechas2), origin.=c(month = 1, day = 1, year = 2009))        
        fechas2<-fechas2[!is.na(datos2)]
        julianos<-julianos[!is.na(datos2)]
        datos3<-datos3[!is.na(datos2)]
        datos2<-datos2[!is.na(datos2)]
        
        ## Running averages
        ma <- function(x, n = 5){filter(x, rep(1 / n, n), sides = 2)}
        rsum <- ma(datos2,n=30) #1month
        polygon(x=c(julianos, julianos[length(julianos):1]),
                y=c((datos2-datos3),(datos2+datos3)[length(datos3):1]), col=col_sat, border=NA)         
        ##########################
       
        #### Model
        #########     
        R0<-filerc$lightspectral_acdom450+filerc$lightspectral_anap450
        adg<-colMeans(R0[171:196,])
        points(x=tiempo, y=adg, type="p", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.4) 

          # remove mixing period
          adg[c(1124:1164,1498:1570)]<-NA
          points(x=tiempo, y=adg, type="p", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.6) 
          o<-match(tiempo,julianos)
          sate<-datos2[o]
          model<-adg
          
          # Metrics
          x<-cbind(model, sate)
          x[x=="-Inf"] <- NA
          x<-x[complete.cases(x)==TRUE,]
          ene_chla   <- nrow(x)
          R_chla     <- rcorr(x, type="pearson")$r[1,2]
          RI_chla    <- exp(sqrt((sum((log(x[,2]/x[,1]))^2, na.rm=TRUE))/nrow(x)))                  
          MEF_chla   <- (sum((x[,2]-mean(x[,2]))^2) - sum((x[,1]-x[,2])^2)) / sum((x[,2]-mean(x[,2]))^2)             
          # igual a: 1-(RMSE_chla^2)/var(x[,2])
          RMSE_chla  <- sqrt((sum((x[,1]-x[,2])^2))/nrow(x))
          AE_chla    <- (sum(x[,1]-x[,2]))/nrow(x)
          AAE_chla   <- (sum(abs(x[,1]-x[,2])))/nrow(x)             
        ######################## 
          
          legend(x=1940, y=0.1,
                 legend=c(paste("R=",round(R_chla,2),sep=""),
                          paste("MEF=",round(MEF_chla,2),sep=""),
                          paste("RMSE=",round(RMSE_chla,4),sep=""),
                          paste("Bias=",round(AE_chla,4),sep="")   ),bty="n")             

          
### Boussole observations
#########################    
          # Acdom
          p_dir <- paste(OS,"Datos/Dat_SGlobal_depth/Boussole/",sep="")
          reflectancia<-read.csv(paste(p_dir,"BOUSSOLE_ACDOM.csv", sep=""), sep=",", header=T)
          #names(reflectancia)
          fechita<-paste(reflectancia$year,reflectancia$month,reflectancia$day, sep="-")
          horita<-paste(reflectancia$Time_CTD_start..UTC.,"00", sep=":")
          # range(horita)        # rrs ya esta filtrada entre 10:00 y 14:00
          fechas <- chron(dates=fechita,times=horita,format=c("y-m-d","h:m:s"), out.format=c("y-m-d","h:m:s")) 
          julianos<- julian(x=month(fechas), d=day(fechas), y=year(fechas), origin.=c(month = 1, day = 1, year = 2009))
          #range(julianos)
          etiquetas_julianas<- julian(x=1, d=1, y=seq(2009,2015,by=1), origin.=c(month = 1, day = 1, year = 2009))
          fechas2<-fechas#[33:length(fechas)]
          datos2<-rowMeans(reflectancia[,258:282])
          #length(datos2)
          datos2[reflectancia$depth..m.>9]<-NA
          fechas2<-fechas2[!is.na(datos2)]
          julianos<-julianos[!is.na(datos2)]
          datos2<-datos2[!is.na(datos2)]
          #points(julianos, datos2, type="b", las=1, pch=19, cex=0.2, ylim=c(0,0.01),
          #       col="yellow2", ylab=expression(m^-1), xlab="yy-mm",xlim=c(0,max(tiempo)), xaxs="i",yaxs="i", xaxt="n")
          julianos_cdom<-julianos
          
          # Anap
          p_dir <- paste(OS,"Datos/Dat_SGlobal_depth/Boussole/",sep="")
          reflectancia<-read.csv(paste(p_dir,"BOUSSOLE_ANAP.csv", sep=""), sep=",", header=T)
          names(reflectancia)
          fechita<-paste(reflectancia$year,reflectancia$month,reflectancia$day, sep="-")
          horita<-paste(reflectancia$time,"00", sep=":")
          # range(horita)        # rrs ya esta filtrada entre 10:00 y 14:00
          fechas <- chron(dates=fechita,times=horita,format=c("y-m-d","h:m:s"), out.format=c("y-m-d","h:m:s")) 
          julianos<- julian(x=month(fechas), d=day(fechas), y=year(fechas), origin.=c(month = 1, day = 1, year = 2009))
          #range(julianos)
          etiquetas_julianas<- julian(x=1, d=1, y=seq(2009,2015,by=1), origin.=c(month = 1, day = 1, year = 2009))
          fechas3<-fechas#[33:length(fechas)]
          datos3<-rowMeans(reflectancia[,149:173])
          #length(datos2)
          datos3[reflectancia$prof.m.>9]<-NA
          fechas3<-fechas3[!is.na(datos2)]
          julianos<-julianos[!is.na(datos3)]
          datos3<-datos3[!is.na(datos3)]
          #points(julianos, datos3, type="b", las=1, pch=19, cex=0.2, ylim=c(0,0.01),
          #       col="red2", ylab=expression(m^-1), xlab="yy-mm",xlim=c(0,max(tiempo)), xaxs="i",yaxs="i", xaxt="n")
          julianos_nap<-julianos
          
          # Adg
          o<-match(julianos_cdom, julianos_nap)    
          #length(o)    
          #cbind(julianos_nap[o],julianos_cdom)
          datos4<-rowSums(cbind(datos3[o], datos2))
          points(julianos_cdom, datos4, type="p", las=1, pch=19, cex=0.6, ylim=c(0,0.01),
                 col=col_bou, ylab=expression(m^-1), xlab="yy-mm",xlim=c(0,max(tiempo)), xaxs="i",yaxs="i", xaxt="n")
####################
          

dev.off()
