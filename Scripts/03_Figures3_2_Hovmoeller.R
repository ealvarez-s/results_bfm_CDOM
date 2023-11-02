if (Sys.info()['sysname']=="Windows") {OS<-"C:/"} else {OS<-paste("/Users/",Sys.info()['user'],"/", sep="")}

### Figures Profiles ### Physics, Chla, IOPs

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
    elegidos<-c(7)
    nombres[elegidos]
    colores[elegidos]


#################
#### FIGURE 3 ###
#################

png(file=paste(path_figures,"Figure3_profiles_nutrients.png",sep=""),width=1200, height =800, pointsize=24, family="Helvetica")
#pdf(file=paste(path_figures,"Figure3_profiles_nutrients.pdf",sep=""),width=1200/100, height =800/100, pointsize=17, family="Helvetica") 
    
  #par(mfcol=c(3,1))
  layout(matrix(c(4:6,1:3),ncol=2, byrow=F), widths = c(0.9,1))
  par(mar=c(3,4,0,4))
  par(oma=c(2,1,3,1))
  nombres[elegidos]
  #layout.show(n=6)
  cexletter=1.4
  cexaxis=1.1
  cextitle=1.1
  
  i=elegidos[1]
  ################# 
  archivo<-paste(nombres[i],"/result.nc",sep="")
  filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
  filenc <- open.nc(filename)
  filerc <- read.nc(filenc)
  tiempo<-filerc$time/86400
  meses<-tiempo/30
  mid_meses<-seq(-2192+15, 4382-15, length=18*12)
  meses<-c(1:(18*12))
  etiquetas_meses<-c(paste("2003",c(1:12),sep="-"),paste("2004",c(1:12),sep="-"),paste("2005",c(1:12),sep="-"),paste("2006",c(1:12),sep="-"),
                     paste("2007",c(1:12),sep="-"),paste("2008",c(1:12),sep="-"),paste("2009",c(1:12),sep="-"),paste("2010",c(1:12),sep="-"),
                     paste("2011",c(1:12),sep="-"),paste("2012",c(1:12),sep="-"),paste("2013",c(1:12),sep="-"),paste("2014",c(1:12),sep="-"),
                     paste("2015",c(1:12),sep="-"),paste("2016",c(1:12),sep="-"),paste("2017",c(1:12),sep="-"),paste("2018",c(1:12),sep="-"),
                     paste("2019",c(1:12),sep="-"),paste("2020",c(1:12),sep="-")) 
  
  myfun<-function(x){ res<-which(x==max(x,na.rm=T))
  if (length(res)>1) res<-res[1]
  return(res)}
  #################

    ## temperature  # zlim=c(12,30)
    ##############
    TChla<-filerc$temp
    # 2011
    TChla_2011<-TChla[,(361*2):(361*3)]
    # 2012
    TChla_2012<-TChla[,(361*3):(361*4)]    
    # 2013
    TChla_2013<-TChla[,((361*4)+1):((361*5)+1)]    
    # 2014
    TChla_2014<-TChla[,((361*5)+1):((361*6)+1)]    
    TChla<-abind(TChla_2011,TChla_2012,TChla_2013,TChla_2014,along=0.5)
    #dim(TChla)
    TChla_show<-apply(TChla, MARGIN=c(2,3), FUN=mean, na.rm=T)
    #dim(TChla_show)
    TChla_show[TChla_show>26]<-26
    image2D(y=depth_center, x=c(1:362), z=t(TChla_show), las=1, cex.lab=1.2, col=colores_tre,
            ylab="depth (m)", xlab="", xaxt="n", ylim=c(200,0), zlim=c(12,26),main="", mgp=c(2.5,1,0))
    mtext(4, at=100, line=2, text=expression(paste("temperature (",""^o,C,")",sep="")), outer=F, cex=0.7)
    axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=1.2)
    #axis(1, at=seq(0,350, by=50) ,labels=T, cex.axis=1.2)
    #text(x=seq(15,362-14,by=30), y = par("usr")[3] + 35,
         #labels = c("J","F","M","A","M","J","J","A","S","O","N","D"), xpd = NA,srt = 40,adj = 0.965,cex = 1.2) 
    #text(x=mid_meses[seq(1,length(mid_meses),by=6)][13:20], y = par("usr")[3] + 30,
    #     labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][13:20], xpd = NA,srt = 40,adj = 0.965,cex = 0.75) 
    # MLD
            # 2011
            mld_2011<-filerc$mld_surf[(361*2):(361*3)]
            # 2012
            mld_2012<-filerc$mld_surf[(361*3):(361*4)]    
            # 2013
            mld_2013<-filerc$mld_surf[((361*4)+1):((361*5)+1)]    
            # 2014
            mld_2014<-filerc$mld_surf[((361*5)+1):((361*6)+1)]
    mld<-abind(mld_2011,mld_2012,mld_2013,mld_2014,along=0.5)
    mld_show<-colMeans(mld, na.rm=T)
    points(x=c(1:362), y=mld_show, type="l", col="white", lwd=3)
    
    ################
    #legend(x=100,y=150, legend=expression(paste("COLOUR:temperature  LINE: mld",sep="")), cex=1.2, text.col="white", bty="n")
    legend(x=145,y=130, legend=c(expression(paste("COLOUR: temperature",sep="")),expression(paste("LINE: mld",sep=""))),
           cex=1.2, text.col="white",bty="n")    
    mtext(2, at=0, line=2, text="(b)", cex=cexletter, font=1, las=1)
  
  
    ## N
    ############
    TChlao<-filerc$N3_n
    # 2011
    TChla_2011<-TChlao[,(361*2):(361*3)]
    # 2012
    TChla_2012<-TChlao[,(361*3):(361*4)]    
    # 2013
    TChla_2013<-TChlao[,((361*4)+1):((361*5)+1)]    
    # 2014
    TChla_2014<-TChlao[,((361*5)+1):((361*6)+1)]    
    TChla<-abind(TChla_2011,TChla_2012,TChla_2013,TChla_2014,along=0.5)
    TChla_show<-apply(TChla, MARGIN=c(2,3), FUN=mean, na.rm=T)
    TChla_show[TChla_show>10]<-10
    image2D(y=depth_center, x=c(1:362), z=t(TChla_show), las=1, cex.lab=1.2, col=colores_tre,
            ylab="depth (m)", xlab="", xaxt="n", ylim=c(200,0), zlim=c(0,8),main="", mgp=c(2.5,1,0))
    mtext(4, at=100, line=1.8, text=expression(paste("N (mmol ",m^-3,")",sep="")), outer=F, cex=0.7)
    #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
    axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=1.2)
    #text(x=mid_meses[seq(1,length(mid_meses),by=6)][17:25], y = par("usr")[3] + 30,
         #labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][17:25], xpd = NA,srt = 40,adj = 0.965,cex = 0.75) 

    # Nutricline
            myfun_iso<-function(x, n){
              res<-which(abs(x-n)==min(abs(x-n),na.rm=T))
              if (length(res)>1) res<-res[1]
              return(res)}      
            
            # Find isoline
            isopic<-apply(TChlao, MARGIN=2, FUN=myfun_iso, n=2)    # these are indexes not depths
            #points(x=tiempo, y=depth_center[nlevs-dcm], type="l", col="white", lwd=1)
            pic_depth<-depth_center[isopic]
            # 2011
            pic_2011<-pic_depth[(361*2):(361*3)]
            # 2012
            pic_2012<-pic_depth[(361*3):(361*4)]    
            # 2013
            pic_2013<-pic_depth[((361*4)+1):((361*5)+1)]    
            # 2014
            pic_2014<-pic_depth[((361*5)+1):((361*6)+1)]
            pic<-abind(pic_2011,pic_2012,pic_2013,pic_2014,along=0.5)
            pic_show<-colMeans(pic, na.rm=T)    
    points(x=c(1:362),pic_show,col="white", type="l", lwd=3)
    ############
    #legend(x=75,y=150, legend=expression(paste("COLOUR: nitrate  LINE: nitracline")), cex=1.2, text.col="black",bty="n")
    legend(x=165,y=130, legend=c(expression(paste("COLOUR: nitrate",sep="")),expression(paste("LINE: nitracline",sep=""))),
           cex=1.2, text.col="black",bty="n")
    mtext(2, at=0, line=2, text="(d)", cex=cexletter, font=1, las=1)
  
  
    ## P
    #############
    TChla<-filerc$N1_p
    # 2011
    TChla_2011<-TChla[,(361*2):(361*3)]
    # 2012
    TChla_2012<-TChla[,(361*3):(361*4)]    
    # 2013
    TChla_2013<-TChla[,((361*4)+1):((361*5)+1)]    
    # 2014
    TChla_2014<-TChla[,((361*5)+1):((361*6)+1)]    
    TChla<-abind(TChla_2011,TChla_2012,TChla_2013,TChla_2014,along=0.5)
    TChla_show<-apply(TChla, MARGIN=c(2,3), FUN=mean, na.rm=T)
    TChla_show[TChla_show>0.5]<-0.5
    image2D(y=depth_center, x=c(1:362), z=t(TChla_show), las=1, cex.lab=1.2, col=colores_tre,
            ylab="depth (m)", xlab="", xaxt="n", ylim=c(200,0), zlim=c(0,0.5),main="", mgp=c(2.5,1,0))
    mtext(4, at=100, line=1.8, text=expression(paste("P (mmol ",m^-3,")",sep="")), outer=F, cex=0.7)
    #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
    axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=1.2)
    #text(x=mid_meses[seq(1,length(mid_meses),by=6)][17:25], y = par("usr")[3] + 35,
    #     labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][17:25], xpd = NA,srt = 40,adj = 0.965,cex = 1.2) 
    ###########
    
    legend(x=185,y=150, legend=expression(paste("phosphate")), cex=1.2, text.col="black",bty="n")
    mtext(2, at=0, line=2, text="(f)", cex=cexletter, font=1, las=1)
    

      
       
##### Climatologies Observations
#################
par(mar=c(3,4,0,2))       
       ## Tre dyfamed
       ##############
       site<-paste(o_dir,"BOUSSOLE_DATA/oceansites/DATA_GRIDDED/DYFAMED/", sep="")
       archive<-paste(site,"OS_DYFAMED_1994-2014_D_TSO.nc", sep="")
       filenc <- open.nc(archive)
       filetemp <- read.nc(filenc)
       #names(filetemp)
       profun<-filetemp$PRES
       value<-filetemp$TEMP
       x <- chron(dates = c("1994/04/08","2014/12/10"),times = c("12:07:47","12:33:20"),format=c("y/m/d","h:m:s")) 
       dias    <-x[2]-x[1]
       ranguito<-filetemp$TIME-filetemp$TIME[1]
       escala  <-ranguito/max(ranguito)
       secuencia_dias<-dias*escala
       fechas<-x[1]+as.numeric(as.character(secuencia_dias))
       julianos<- julian(x=month(fechas), d=day(fechas), y=year(fechas), origin.=c(month = 1, day = 1, year = 1994))
       # todo vectores
       fechitas<-rep(fechas, each=length(profun))
       profunditas<-rep(profun, times=length(fechas))
       fechas2<-paste(year(fechitas),month(fechitas),day(fechitas), sep="-")
       times<-yday(fechas2)
       depth=(profunditas)
       valor1<-c(value) 
       valor1[depth>400]<-NA
       times<-times[!is.na(valor1)]
       depth<-depth[!is.na(valor1)]
       valor1<-valor1[!is.na(valor1)]
       resolx=52  # 365/52
       resoly=40
       resul<-interp(x=times, y=depth, z=valor1, xo=seq(0,365,length=resolx),yo=seq(0,300,length=resoly),
                     linear = TRUE, extrap=TRUE, duplicate = "mean", dupfun = NULL, nx = resolx, ny = resoly,
                     jitter = 10^-12, jitter.iter = 6, jitter.random = FALSE)
       climat <- resul$z 
       month <-resul$x
       prof  <-resul$y          
       image2D(x=month, y=prof, z=climat, ylim=c(200,0), zlim=c(12,26), las=1,col=colores_tre,
               ylab="depth (m)", colkey=F, cex.lab=1.2, xlim=c(4,360), xaxs="i", xaxt="n")
       #axis(1, at=seq(1,350, by=20) ,labels=T)
       axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=1.2)
       #mtext(3, at=0, adj=0, text="temperature")
       mtext(2, at=0, line=2, text="(a)", cex=cexletter, font=1, las=1)
       #############

       #### MLD
       resul<-interp(x=times, y=depth, z=valor1, xo=seq(0,365,length=resolx),yo=seq(0,300,length=301),
                     linear = TRUE, extrap=TRUE, duplicate = "mean", dupfun = NULL, nx = resolx, ny = resoly,
                     jitter = 10^-12, jitter.iter = 6, jitter.random = FALSE)
       climat <- resul$z 
       month <-resul$x
       prof  <-resul$y 
       dim(climat)
 
       myfun_iso<-function(x, n=0.2){
         if (sum(!is.na(x))==0) {res<-NA
         } else {
           #x[c(1:10)]<-NA
         threshold=x[11]-n
         res<-which(abs(x-threshold)==min(abs(x-threshold),na.rm=T))
         res<-unname(res)
         if (identical(res, integer(0)))  res<-NA
         if (length(res)>1) res<-res[1]
         }
         return(res)}      
       
       # Find isoline
       isopic<-apply(climat, MARGIN=1, FUN=myfun_iso, n=2)    # these are indexes not depths
       length(isopic)
       isopic2<-unlist(isopic,use.names = F)
       length(isopic2)
       #points(x=tiempo, y=depth_center[nlevs-dcm], type="l", col="white", lwd=1)
       pic_depth<-prof[isopic]
       month<-month[!is.na(pic_depth)]
       pic_depth<-pic_depth[!is.na(pic_depth)]
       points(x=month,pic_depth,col="white", type="l", lwd=3)       
       
     ### ifremer: DeBoyer Montegut 2004         
     ##############
     #   modelname<-paste(o_dir,"BOUSSOLE_DATA/physics/",sep="")
     #   archivo<-"mld_DT02_c1m_reg2.0.nc"
     #   filename <- paste(modelname,archivo, sep="")
     #   filenc <- open.nc(filename)
     #   filerc <- read.nc(filenc)
     #         m <- filerc$mld
     #         m[m==1.0e+09] <- NA
     #         #image(m[,,1],col = rainbow(12)[1:11],breaks = c(seq(0,100, length=4),seq(150,2000, length=8)),add = FALSE, xaxs = "i", yaxs = "i", xaxt="n",yaxt="n", main="", las=1)
     #         lat  <- filerc$lat
     #         lon  <- filerc$lon
     # 
     #         for (k in 1:dim(m)[3]) {
     #           m[,,k]<-abind(m[91:180,,k],m[1:90,,k], along=1) }   #  180 126 30
     #         lon<-c(seq(-178,-2,by=2),seq(0,180,by=2))   
     #         #image2D(x=lon,y=lat,m[,,2],col = rainbow(12)[1:11],breaks = c(seq(0,100, length=4),seq(150,2000, length=8)),add = FALSE, xaxs = "i", yaxs = "i", xaxt="n",yaxt="n", main="", las=1)
     #         dimnames(m) <- list(x=lon, y=lat, t=c(1:12))
     #   
     #           # Subset the Med
     #           #dim(m[lon>=(-10) & lon<=38, lat>=30 & lat<=46, k])
     #           z<-array(NA,dim=c(25,9,12))
     #           for (k in 1:dim(m)[3]) {
     #             z[,,k]<- m[lon>=(-10) & lon<=38, lat>=30 & lat<=46, k] }
     #           lon_med <-lon[lon>=(-10) & lon<=38]
     #           lat_med <-lat[lat>=30 & lat<=46]
     #           lon<-lon_med
     #           lat<-lat_med
     #        #image2D(x=lon,y=lat,z[,,1], zlim=c(0,100))
     #           med<-z 
     #        #image2D(x=lon_med,y=lat_med,med[,,1],
     #        #col = rainbow(12)[9:1],breaks = c(seq(0,100, length=10),seq(150,2000, length=0)),add = FALSE, xaxs = "i", yaxs = "i", main="", las=1)
     #        ## Boussole     
     #        #points(x=7.9, y=43.366667, pch=4, cex=1.2, lwd=3, col="red2")
     #        ## Dyfamed
     #        #points(x=7+(52/60), y=43+(25/60), pch=4, cex=1.2, lwd=3, col="black")
     # points(filerc$time,med[10,8,], col="red",bg="white", pch=21, type="l", lwd=3)
     #############        
     
     #legend(x="bottomright", legend=expression(paste("COLOUR:temp ("^o, C,")  LINE: mld ",sep="")), cex=1.2, text.col="white",bty="n")               
     legend(x=125,y=130, legend=c(expression(paste("COLOUR: temperature",sep="")),expression(paste("LINE: mld",sep=""))),
              cex=1.2, text.col="white",bty="n")    
       
       
   ## NUTRIENTS DYFAMED
   ##########       
   p_dir<-paste(o_dir,"BOUSSOLE_DATA/bit_sea/", sep="")
   datos <- read.delim2(paste(p_dir,"nitrateDYFAMED.txt", sep=""),header=FALSE,fill=TRUE)
       compute_depth=T
       resolx=52
       resoly=30
       source(paste(o_dir,"Scripts/extract_data.R", sep=""))
       nitrate<-climat
       datos <- read.delim2(paste(p_dir,"nitriteDYFAMED.txt", sep=""),header=FALSE,fill=TRUE)
       compute_depth=F
       source(paste(o_dir,"Scripts/extract_data.R", sep=""))
       nitrite<-climat
       nitrite[is.na(nitrite)]<-0
       nit_total<-nitrate+nitrite
       nit_total[is.na(nitrate)]<-NA
       nit_total[nit_total>8]<-8
       image2D(x=month, y=prof, z=nit_total, ylim=c(200,0), zlim=c(0,8), las=1,col=colores_tre,
               ylab="depth (m)", colkey=F, cex.lab=1.2, xaxs="i", xlim=c(4,360), xaxt="n")
       #axis(1, at=seq(0,350, by=50) ,labels=F)
       axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=1.2)
       #mtext(3, at=0, adj=0, text="nitrate+nitrite")
       mtext(2, at=0, line=2, text="(c)", cex=cexletter, font=1, las=1)
       
       # Nutricline
       myfun_iso<-function(x, n){
         res<-which(abs(x-n)==min(abs(x-n),na.rm=T))
         res<-unname(res)
         if (identical(res, integer(0)))  res<-NA
         if (length(res)>1) res<-res[1]
         return(res)}      

       # Find isoline
       isopic<-apply(nit_total, MARGIN=1, FUN=myfun_iso, n=2)    # these are indexes not depths
       length(isopic)
       isopic2<-unlist(isopic,use.names = F)
       length(isopic2)
       #points(x=tiempo, y=depth_center[nlevs-dcm], type="l", col="white", lwd=1)
       pic_depth<-prof[isopic]
       points(x=month,pic_depth,col="white", type="l", lwd=3)
       #legend(x="bottomright", legend=expression(paste("COLOUR: N (",mu,"M)  LINE: nitracline ")), cex=1.2, text.col="black",bty="n")
       legend(x=165,y=130, legend=c(expression(paste("COLOUR: nitrate",sep="")),expression(paste("LINE: nitracline",sep=""))),
              cex=1.2, text.col="black",bty="n")
       
       
   datos <- read.delim2(paste(p_dir,"phosphateDYFAMED.txt", sep=""),header=FALSE,fill=TRUE)
       source(paste(o_dir,"Scripts/extract_data.R", sep=""))
       climat[climat>0.5]<-0.5
       image2D(x=month, y=prof, z=climat, ylim=c(200,0), zlim=c(0,0.5), las=1,col=colores_tre,
               ylab="depth (m)", colkey=F,cex.lab=1.2, xaxs="i", xlim=c(4,360), xaxt="n")
       #axis(1, at=seq(0,350, by=50) ,labels=T, cex.axis=1.2)
       axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=1.2)
       #legend(x="bottomright", legend=expression(paste("P (",mu,"M) ")), cex=1.2, text.col="black",bty="n")
       legend(x=185,y=150, legend=expression(paste("phosphate")), cex=1.2, text.col="black",bty="n")
       
       mtext(2, at=0, line=2, text="(e)", cex=cexletter, font=1, las=1)
   #############
       
############
 
    mtext(3,line=1,at=c(0.26,0.72), text=c("Observations","Model"), outer=T, cex=cextitle)  
       
dev.off() 









#################
#### FIGURE 4 ###
#################

png(file=paste(path_figures,"Figure4_profiles_logChla.png",sep=""),width=1200, height =1000, pointsize=24, family="Helvetica") 
#pdf(file=paste(path_figures,"Figure4_profiles_logChla.pdf",sep=""),width=1200/100, height =1000/100, pointsize=17, family="Helvetica") 

  layout(matrix(c(5:8,1:4),ncol=2, byrow=F), widths = c(0.9,1,1))
  par(mar=c(3,3.6,0,4))
  par(oma=c(0,0.5,2.5,0))
  nombres[elegidos]
  #layout.show(n=8)
  cexletter=1.4
  cexaxis=0.9
  cextitle=1.1
  
    i=elegidos[1]
    ################# 
    archivo<-paste(nombres[i],"/result.nc",sep="")
    filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
    filenc <- open.nc(filename)
    filerc <- read.nc(filenc)
    tiempo<-filerc$time/86400
    meses<-tiempo/30
    mid_meses<-seq(-2192+15, 4382-15, length=18*12)
    meses<-c(1:(18*12))
    etiquetas_meses<-c(paste("2003",c(1:12),sep="-"),paste("2004",c(1:12),sep="-"),paste("2005",c(1:12),sep="-"),paste("2006",c(1:12),sep="-"),
                       paste("2007",c(1:12),sep="-"),paste("2008",c(1:12),sep="-"),paste("2009",c(1:12),sep="-"),paste("2010",c(1:12),sep="-"),
                       paste("2011",c(1:12),sep="-"),paste("2012",c(1:12),sep="-"),paste("2013",c(1:12),sep="-"),paste("2014",c(1:12),sep="-"),
                       paste("2015",c(1:12),sep="-"),paste("2016",c(1:12),sep="-"),paste("2017",c(1:12),sep="-"),paste("2018",c(1:12),sep="-"),
                       paste("2019",c(1:12),sep="-"),paste("2020",c(1:12),sep="-")) 
    
    myfun<-function(x){ res<-which(x==max(x,na.rm=T))
    if (length(res)>1) res<-res[1]
    return(res)}

      ## tChla
      ########      
      TChlao<-filerc$P1_Chl+filerc$P2_Chl+filerc$P3_Chl+filerc$P4_Chl
            TChla_2011<-TChlao[,(361*2):(361*3)]
            # 2012
            TChla_2012<-TChlao[,(361*3):(361*4)]    
            # 2013
            TChla_2013<-TChlao[,((361*4)+1):((361*5)+1)]    
            # 2014
            TChla_2014<-TChlao[,((361*5)+1):((361*6)+1)]    
            TChla<-abind(TChla_2011,TChla_2012,TChla_2013,TChla_2014,along=0.5)
            #dim(TChla)
            TChla_show<-apply(TChla, MARGIN=c(2,3), FUN=mean, na.rm=T)
      modell<-TChla_show
      #TChla_show[TChla_show>1.0]<-1.0
      TChla_show<-log10(TChla_show)
      TChla_show[TChla_show<(-3)]<-(-3)
      image2D(y=depth_center, x=c(1:362), z=t(TChla_show), las=1, cex.lab=1.2, col=colores_chla,
              ylab="depth (m)", xlab="", xaxt="n", ylim=c(200,0),
              zlim=c(-3,0.48),main="", mgp=c(2.5,1,0), 
              colkey=list(at=log10(c(0.001,0.003,0.01,0.03,0.1,0.3,1,3)), labels=as.character(c(0.001,0.003,0.01,0.03,0.1,0.3,1,3))   )  )
      mtext(4, at=100, line=2, text=expression(paste("mg Chl-a ",m^-3,sep="")), outer=F, cex=0.7)
      #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
      axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
      #text(x=mid_meses[seq(1,length(mid_meses),by=6)][13:20], y = par("usr")[3] + 30,
      #     labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][13:20], xpd = NA,srt = 40,adj = 0.965,cex = 0.75) 
      contour(y=depth_center[196:1], x=c(1:362), z=t(fliplr(TChla_show)),
               levels=c(log10(0.2)), labels=c(0.2), col="white", add=T)
      
      # Find DCM
      dcm<-apply(TChlao, MARGIN=2, FUN=myfun)      # these are indexes not depths
      #points(x=tiempo, y=depth_center[nlevs-dcm], type="l", col="white", lwd=1)
      dcm_depth<-depth_center[dcm]
                # 2011
                dcm_2011<-dcm_depth[(361*2):(361*3)]
                # 2012
                dcm_2012<-dcm_depth[(361*3):(361*4)]    
                # 2013
                dcm_2013<-dcm_depth[((361*4)+1):((361*5)+1)]    
                # 2014
                dcm_2014<-dcm_depth[((361*5)+1):((361*6)+1)]
                dcm<-abind(dcm_2011,dcm_2012,dcm_2013,dcm_2014,along=0.5)
                dcm_show<-colMeans(dcm, na.rm=T)    
      points(x=c(1:362),dcm_show, col="white", type="l", lwd=2)
      ################# 
      legend(x=165,y=130, legend=c(expression(paste("COLOUR: TChl-a",sep="")),expression(paste("LINE: ",z[DCM], sep=""))),
             cex=1.2, text.col="white",bty="n")
      mtext(2, at=0, line=2, text="(e)", cex=cexletter, font=1, las=1)

      ## pico
      ###########
      TChla<-filerc$P3_Chl
            TChla_2011<-TChla[,(361*2):(361*3)]
            # 2012
            TChla_2012<-TChla[,(361*3):(361*4)]    
            # 2013
            TChla_2013<-TChla[,((361*4)+1):((361*5)+1)]    
            # 2014
            TChla_2014<-TChla[,((361*5)+1):((361*6)+1)]    
            TChla<-abind(TChla_2011,TChla_2012,TChla_2013,TChla_2014,along=0.5)
      TChla_show<-apply(TChla, MARGIN=c(2,3), FUN=mean, na.rm=T)
      #TChla_show[TChla_show>0.4]<-0.4
      TChla_show<-log10(TChla_show)
      TChla_show[TChla_show<(-3)]<-(-3)
      image2D(y=depth_center, x=c(1:362), z=t(TChla_show), las=1, cex.lab=1.2, col=colores_chla,
              ylab="depth (m)", xlab="", xaxt="n", ylim=c(200,0),
              zlim=c(-3,0.48),main="", mgp=c(2.5,1,0),
              colkey=list(at=log10(c(0.001,0.003,0.01,0.03,0.1,0.3,1,3)), labels=as.character(c(0.001,0.003,0.01,0.03,0.1,0.3,1,3))   )  )
      mtext(4, at=100, line=2, text=expression(paste("mg Chl-a ",m^-3,sep="")), outer=F, cex=0.7)
      #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
      axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
      #text(x=mid_meses[seq(1,length(mid_meses),by=6)][13:20], y = par("usr")[3] + 30,
      #     labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][13:20], xpd = NA,srt = 40,adj = 0.965,cex = 0.75) 
      contour(y=depth_center[196:1], x=c(1:362), z=t(fliplr(TChla_show)),levels=log10(c(0.1,0.2)),labels=c(0.1,0.2), col="white", add=T)
      ###########      
      legend(x=230,y=140, legend=expression(paste(P["Chl-a"]^"(PICO)")), cex=1.2, text.col="white",bty="n")
      mtext(2, at=0, line=2, text="(f)", cex=cexletter, font=1, las=1)
      
      ## nano
      ############
      TChla<-filerc$P2_Chl
              #2011
              TChla_2011<-TChla[,(361*2):(361*3)]
              # 2012
              TChla_2012<-TChla[,(361*3):(361*4)]    
              # 2013
              TChla_2013<-TChla[,((361*4)+1):((361*5)+1)]    
              # 2014
              TChla_2014<-TChla[,((361*5)+1):((361*6)+1)]    
      TChla<-abind(TChla_2011,TChla_2012,TChla_2013,TChla_2014,along=0.5)
      TChla_show<-apply(TChla, MARGIN=c(2,3), FUN=mean, na.rm=T)
      #TChla_show[TChla_show>0.4]<-0.4
      TChla_show<-log10(TChla_show)
      TChla_show[TChla_show<(-3)]<-(-3)
      image2D(y=depth_center, x=c(1:362), z=t(TChla_show), las=1, cex.lab=1.2, col=viridis(100),  #col=rainbow(100)[85:1],
              ylab="depth (m)", xlab="", xaxt="n", ylim=c(200,0),
              zlim=c(-3,0.48),main="", mgp=c(2.5,1,0),
              colkey=list(at=log10(c(0.001,0.003,0.01,0.03,0.1,0.3,1,3)), labels=as.character(c(0.001,0.003,0.01,0.03,0.1,0.3,1,3))   ))
      mtext(4, at=100, line=2, text=expression(paste("mg Chl-a ",m^-3,sep="")), outer=F, cex=0.7)
      #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
      axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
      #text(x=mid_meses[seq(1,length(mid_meses),by=6)][13:20], y = par("usr")[3] + 30,
      #     labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][13:20], xpd = NA,srt = 40,adj = 0.965,cex = 0.75) 
      contour(y=depth_center[196:1], x=c(1:362), z=t(fliplr(TChla_show)),levels=log10(c(0.1,0.2)),labels=c(0.1,0.2), col="white", add=T)
      ##########      
      legend(x=230,y=140, legend=expression(paste(P["Chl-a"]^"(NANO)")), cex=1.2, text.col="white",bty="n")
      mtext(2, at=0, line=2, text="(g)", cex=cexletter, font=1, las=1)
      
      ## micro
      #############
      TChla<-filerc$P1_Chl+filerc$P4_Chl
              #2011
              TChla_2011<-TChla[,(361*2):(361*3)]
              # 2012
              TChla_2012<-TChla[,(361*3):(361*4)]    
              # 2013
              TChla_2013<-TChla[,((361*4)+1):((361*5)+1)]    
              # 2014
              TChla_2014<-TChla[,((361*5)+1):((361*6)+1)]    
      TChla<-abind(TChla_2011,TChla_2012,TChla_2013,TChla_2014,along=0.5)
      TChla_show<-apply(TChla, MARGIN=c(2,3), FUN=mean, na.rm=T)
      #TChla_show[TChla_show>0.4]<-0.4
      TChla_show<-log10(TChla_show)
      TChla_show[TChla_show<(-3)]<-(-3)
      image2D(y=depth_center, x=c(1:362), z=t(TChla_show), las=1, cex.lab=1.2, col=viridis(100), #col=rainbow(100)[85:1],
              ylab="depth (m)", xlab="", xaxt="n", ylim=c(200,0),
              zlim=c(-3,0.48),main="", mgp=c(2.5,1,0),
              colkey=list(at=log10(c(0.001,0.003,0.01,0.03,0.1,0.3,1,3)), labels=as.character(c(0.001,0.003,0.01,0.03,0.1,0.3,1,3))   ))
      mtext(4, at=100, line=2, text=expression(paste("mg Chl-a ",m^-3,sep="")), outer=F, cex=0.7)
      #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
      axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
      #text(x=mid_meses[seq(1,length(mid_meses),by=6)][13:20], y = par("usr")[3] + 30,
      #     labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][13:20], xpd = NA,srt = 40,adj = 0.965,cex = 0.75) 
      contour(y=depth_center[196:1], x=c(1:362), z=t(fliplr(TChla_show)),levels=log10(c(0.1,0.2)),labels=c(0.1,0.2), col="white", add=T)
      ###########      
      legend(x=196,y=140, legend=expression(paste(P["Chl-a"]^"(DIATOM)"," + ",P["Chl-a"]^"(DINO)")), cex=1.2, text.col="white",bty="n")
      mtext(2, at=0, line=2, text="(h)", cex=cexletter, font=1, las=1)
      
    ##### Climatologies Observations
    par(mar=c(3,4,0,2)) 

      ## CHLA BOUSSOLE
      #############
      ## Tabla datos Boussole HPLC
      p_dir <- paste(o_dir,"BOUSSOLE_DATA/", sep="")
      tabla_total<-read.csv(paste(p_dir,"Boussole_HPLC.csv", sep=""))
      #colnames(tabla_total)
      valor1<-tabla_total$Tchla
      resolx=52
      resoly=40
      source(paste(o_dir,"Scripts/extract_data_BOUSSOLE.R",sep=""))
      observedd<-climat
      #climat[climat>1.0]<-1.0
      climat<-log10(climat)
      climat[climat<(-3)]<-(-3)
      climat[climat>(0.48)]<-0.48
      climat[observedd==0]<-NA
      image2D(x=month, y=prof, z=climat, ylim=c(200,0), zlim=c(-3,0.48), las=1,
              ylab="depth (m)", colkey=F, cex.lab=1.2, xaxs="i", xlim=c(4,360),
              col=colores_chla, xaxt="n")
      contour(x=month, y=prof, z=climat,levels=log10(c(0.2)), labels=c(0.2), col="white", add=T)
      axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
      ############
      legend(x="bottomright", legend=expression(paste("TChl-a            ")), cex=1.2, text.col="white",bty="n")
      mtext(2, at=0, line=2, text="(a)", cex=cexletter, font=1, las=1)

      # Pico
      ######
      valor1<-tabla_total$X.PicoChl_Uitz.
      source(paste(o_dir,"Scripts/extract_data_BOUSSOLE.R", sep=""))
      climat<-log10(climat)
      climat[climat<(-3)]<-(-3)
      climat[climat>(0.48)]<-0.48
      climat[observedd==0]<-NA
      image2D(x=month, y=prof, z=climat, ylim=c(200,0), zlim=c(-3,0.48), las=1,
              ylab="depth (m)", colkey=F,cex.lab=1.2, xaxs="i", xlim=c(4,360),col=colores_chla, xaxt="n")
      axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
      contour(x=month, y=prof, z=climat,levels=log10(c(0.1)), labels=c(0.1), col="white", add=T)
      ##########
      legend(x="bottomright", legend=expression(paste("pico Chl-a          ")), cex=1.2, text.col="white",bty="n")
      mtext(2, at=0, line=2, text="(b)", cex=cexletter, font=1, las=1)      

      # Nano
      ##########
      valor1<-tabla_total$X.NanoChl_Uitz.
      source(paste(o_dir,"Scripts/extract_data_BOUSSOLE.R", sep=""))
      climat<-log10(climat)
      climat[climat<(-3)]<-(-3)
      climat[climat>(0.48)]<-0.48
      climat[observedd==0]<-NA
      image2D(x=month, y=prof, z=climat, ylim=c(200,0), zlim=c(-3,0.48), las=1,
              ylab="depth (m)", colkey=F,cex.lab=1.2, xaxs="i", xlim=c(4,360),col=colores_chla, xaxt="n")
      axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
      contour(x=month, y=prof, z=climat,levels=log10(c(0.1)), labels=c(0.1), col="white", add=T)
      ###########
      legend(x="bottomright", legend=expression(paste("nano Chl-a          ")), cex=1.2, text.col="white",bty="n")
      mtext(2, at=0, line=2, text="(c)", cex=cexletter, font=1, las=1)

      # Micro
      ##########
      valor1<-tabla_total$X.MicroChl_Uitz.
      source(paste(o_dir,"Scripts/extract_data_BOUSSOLE.R", sep=""))
      climat<-log10(climat)
      climat[climat<(-3)]<-(-3)
      climat[climat>(0.48)]<-0.48
      climat[observedd==0]<-NA
      image2D(x=month, y=prof, z=climat, ylim=c(200,0), zlim=c(-3,0.48), las=1,
              ylab="depth (m)", colkey=F,cex.lab=1.2, xaxs="i", xlim=c(4,360),col=colores_chla, xaxt="n")
      axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
      contour(x=month, y=prof, z=climat,levels=log10(c(0.1)), labels=c(0.1), col="white", add=T)  
      ##########
      legend(x="bottomright", legend=expression(paste("micro Chl-a          ")), cex=1.2, text.col="white",bty="n")
      mtext(2, at=0, line=2, text="(d)", cex=cexletter, font=1, las=1)
 
mtext(3,line=1,at=c(0.26,0.72), text=c("Observations","Model"), outer=T, cex=cextitle)  

dev.off() 





       
#################
#### FIGURE 5 ###
#################        
        
png(file=paste(path_figures,"Figure5_profiles_DOC_CDOM.png",sep=""),width=1200, height = 300, pointsize=18, family="Helvetica") 
#pdf(file=paste(path_figures,"Figure5_profiles_DOC_CDOM.pdf",sep=""),width=1200/100, height = 300/100, pointsize=14, family="Helvetica") 
    #par(mfcol=c(3,1))
    layout(matrix(c(1,2),ncol=2, byrow=F))
        par(mar=c(3,3.6,0,4))
        par(oma=c(0,0.5,2.5,0))
        nombres[elegidos]
        #layout.show(n=2)
        cexletter=1.6
        cexaxis=0.9
        cextitle=1.1
        
        i=elegidos[1]
        ################# 
        archivo<-paste(nombres[i],"/result.nc",sep="")
        filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
        filenc <- open.nc(filename)
        filerc <- read.nc(filenc)
        tiempo<-filerc$time/86400
        meses<-tiempo/30
        mid_meses<-seq(-2192+15, 4382-15, length=18*12)
        meses<-c(1:(18*12))
        etiquetas_meses<-c(paste("2003",c(1:12),sep="-"),paste("2004",c(1:12),sep="-"),paste("2005",c(1:12),sep="-"),paste("2006",c(1:12),sep="-"),
                           paste("2007",c(1:12),sep="-"),paste("2008",c(1:12),sep="-"),paste("2009",c(1:12),sep="-"),paste("2010",c(1:12),sep="-"),
                           paste("2011",c(1:12),sep="-"),paste("2012",c(1:12),sep="-"),paste("2013",c(1:12),sep="-"),paste("2014",c(1:12),sep="-"),
                           paste("2015",c(1:12),sep="-"),paste("2016",c(1:12),sep="-"),paste("2017",c(1:12),sep="-"),paste("2018",c(1:12),sep="-"),
                           paste("2019",c(1:12),sep="-"),paste("2020",c(1:12),sep="-")) 
        
        myfun<-function(x){ res<-which(x==max(x,na.rm=T))
        if (length(res)>1) res<-res[1]
        return(res)}
        
        ## DOC
        ########
        TChla<-filerc$R1_c+filerc$R2_c+filerc$R3_c+filerc$X1_c+filerc$X2_c+filerc$X3_c
        #2011
        TChla_2011<-TChla[,(361*2):(361*3)]
        # 2012
        TChla_2012<-TChla[,(361*3):(361*4)]    
        # 2013
        TChla_2013<-TChla[,((361*4)+1):((361*5)+1)]    
        # 2014
        TChla_2014<-TChla[,((361*5)+1):((361*6)+1)]    
        TChla<-abind(TChla_2011,TChla_2012,TChla_2013,TChla_2014,along=0.5)
        TChla_show<-(apply(TChla, MARGIN=c(2,3), FUN=mean, na.rm=T))/12
        TChla_notshow<-TChla_show
        TChla_show[TChla_show>70]<-70
        TChla_show[TChla_show<40]<-40
        image2D(y=depth_center, x=c(1:362), z=t(TChla_show), las=1, cex.lab=1.2, col=viridis(100), # col=viridis(100), 
                ylab="depth (m)", xlab="", xaxt="n", ylim=c(200,0), zlim=c(40,70),main="", mgp=c(2.5,1,0))
        mtext(4, at=100, line=2, text=expression(paste("mmol C ",m^-3,sep="")), outer=F, cex=1.0)
        #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        #text(x=mid_meses[seq(1,length(mid_meses),by=6)][17:25], y = par("usr")[3] + 30,
        #labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][17:25], xpd = NA,srt = 40,adj = 0.965,cex = 0.75) 
        contour(y=depth_center[196:1], x=c(1:362), z=t(fliplr(TChla_notshow)),levels=c(38,40,45,50,60), col="white", add=T)
        
        ########### 
        #legend(x=225,y=150, legend=expression(paste("DOC (",mu,"M",")")), cex=1.2, text.col="white",bty="n")
        legend(x=225,y=150, legend=expression(paste("DOC ")), cex=1.2, text.col="white",bty="n")
        mtext(2, at=0, line=2, text="(a)", cex=cexletter, font=1, las=1)
        
        
    ## CDOM
    ########      
        TChla<-filerc$X1_c+filerc$X2_c+filerc$X3_c
        #2011
        TChla_2011<-TChla[,(361*2):(361*3)]
        # 2012
        TChla_2012<-TChla[,(361*3):(361*4)]    
        # 2013
        TChla_2013<-TChla[,((361*4)+1):((361*5)+1)]    
        # 2014
        TChla_2014<-TChla[,((361*5)+1):((361*6)+1)]    
        TChla<-abind(TChla_2011,TChla_2012,TChla_2013,TChla_2014,along=0.5)
        TChla_show<-apply(TChla, MARGIN=c(2,3), FUN=mean, na.rm=T)  
        TChla_notshow<-TChla_show
        TChla_show[TChla_show<1]<-1
        TChla_show[TChla_show>2.4]<-2.4
        image2D(y=depth_center, x=c(1:362), z=t(TChla_show), las=1, cex.lab=1.2, col=colores_chla,
                ylab="depth (m)", xlab="", xaxt="n", ylim=c(200,0), zlim=c(1,2.4),main="", mgp=c(2.5,1,0))
        mtext(4, at=100, line=2, text=expression(paste("mg C ",m^-3,sep="")), outer=F, cex=1.0)
        #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        #text(x=mid_meses[seq(1,length(mid_meses),by=6)][17:25], y = par("usr")[3] + 35,
        #labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][17:25], xpd = NA,srt = 40,adj = 0.965,cex = 1.2) 
        contour(y=depth_center[196:1], x=c(1:362), z=t(fliplr(TChla_notshow)),levels=c(1.2,1.5,1.8,2.0), col="white", add=T)
        

      #### zeu
        par<-filerc$lightspectral_PAR_tot
        zeu<-apply(par[196:1,], MARGIN=2, FUN=find_eudepth, prof=depth_center[196:1], dif=0.01)
        # 2011
        zeu_2011<-zeu[(361*2):(361*3)]
        # 2012
        zeu_2012<-zeu[(361*3):(361*4)]    
        # 2013
        zeu_2013<-zeu[((361*4)+1):((361*5)+1)]    
        # 2014
        zeu_2014<-zeu[((361*5)+1):((361*6)+1)]
        zeu<-abind(zeu_2011,zeu_2012,zeu_2013,zeu_2014,along=0.5)
        zeu_show<-colMeans(zeu, na.rm=T)        
      #points(x=c(1:362), zeu_show, type="l", col="red", las=1, ylim=c(200,0))
      #points(x=tiempo, zeu/4.6, type="l", col="green", las=1, ylim=c(200,0))
      ###########        
      #legend(x=170,y=150, legend=expression(paste("CDOM (","mg ", m^-3,")",sep="")), cex=1.2, text.col="white",bty="n")
      legend(x=220,y=150, legend=expression(paste("CDOM ",sep="")), cex=1.2, text.col="white",bty="n")
      mtext(2, at=0, line=2, text="(b)", cex=cexletter, font=1, las=1)
        
      ## Thresholds for CDOM bleaching
        TChla<-(filerc$lightspectral_PAR_tot)/86400
        #2011
        TChla_2011<-TChla[,(361*2):(361*3)]
        # 2012
        TChla_2012<-TChla[,(361*3):(361*4)]    
        # 2013
        TChla_2013<-TChla[,((361*4)+1):((361*5)+1)]    
        # 2014
        TChla_2014<-TChla[,((361*5)+1):((361*6)+1)]    
        TChla<-abind(TChla_2011,TChla_2012,TChla_2013,TChla_2014,along=0.5)
        TChla_show<-apply(TChla, MARGIN=c(2,3), FUN=mean, na.rm=T)  
        TChla_notshow<-TChla_show
        #TChla_show[TChla_show<1]<-1
        #TChla_show[TChla_show>2.4]<-2.4
        #image2D(y=depth_center, x=c(1:362), z=t(TChla_show), las=1, cex.lab=1.2, col=colores_chla, ylab="depth (m)", xlab="", xaxt="n", ylim=c(200,0),zlim=c(0,60),main="", mgp=c(2.5,1,0))
      contour(y=depth_center[196:1], x=c(1:362), z=t(fliplr(TChla_notshow)),levels=c(50,60), col="white", add=T, drawlabels=F)
        
      dev.off() 
        
        
        
        

        
#################
#### FIGURE 6 ###
#################      
           
png(file=paste(path_figures,"Figure6_profiles_IOPs.png",sep=""), width=1200, height =800, pointsize=24, family="Helvetica") 
#pdf(file=paste(path_figures,"Figure6_profiles_IOPs.pdf",sep=""), width=1200/100, height =800/100, pointsize=18, family="Helvetica") 
        layout(matrix(c(4:6,1:3),ncol=2, byrow=F), widths = c(0.9,1))
        par(mar=c(3,4,0,4))
        par(oma=c(2,1,2.5,1))
        nombres[elegidos]
        #layout.show(n=6)
        cexletter=1.4
        cextitle=1.1
        cexaxis=1.2
        
        i=elegidos[1]
        nombres[elegidos]
        ################# 
        archivo<-paste(nombres[i],"/result.nc",sep="")
        filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
        filenc <- open.nc(filename)
        filerc <- read.nc(filenc)
        tiempo<-filerc$time/86400
        meses<-tiempo/30
        mid_meses<-seq(-2192+15, 4382-15, length=18*12)
        meses<-c(1:(18*12))
        etiquetas_meses<-c(paste("2003",c(1:12),sep="-"),paste("2004",c(1:12),sep="-"),paste("2005",c(1:12),sep="-"),paste("2006",c(1:12),sep="-"),
                           paste("2007",c(1:12),sep="-"),paste("2008",c(1:12),sep="-"),paste("2009",c(1:12),sep="-"),paste("2010",c(1:12),sep="-"),
                           paste("2011",c(1:12),sep="-"),paste("2012",c(1:12),sep="-"),paste("2013",c(1:12),sep="-"),paste("2014",c(1:12),sep="-"),
                           paste("2015",c(1:12),sep="-"),paste("2016",c(1:12),sep="-"),paste("2017",c(1:12),sep="-"),paste("2018",c(1:12),sep="-"),
                           paste("2019",c(1:12),sep="-"),paste("2020",c(1:12),sep="-")) 
        
        myfun<-function(x){ res<-which(x==max(x,na.rm=T))
        if (length(res)>1) res<-res[1]
        return(res)}
        
        ## aph
        #############
        TChla<-filerc$lightspectral_aph450
              #2011
              TChla_2011<-TChla[,(361*2):(361*3)]
              # 2012
              TChla_2012<-TChla[,(361*3):(361*4)]    
              # 2013
              TChla_2013<-TChla[,((361*4)+1):((361*5)+1)]    
              # 2014
              TChla_2014<-TChla[,((361*5)+1):((361*6)+1)]    
        TChla<-abind(TChla_2011,TChla_2012,TChla_2013,TChla_2014,along=0.5)
        TChla_show<-apply(TChla, MARGIN=c(2,3), FUN=mean, na.rm=T)
        TChla_show[TChla_show>0.05]<-0.05
        image2D(y=depth_center, x=c(1:362), z=t(TChla_show), las=1, cex.lab=1.2, col=colores_iops,
                ylab="depth (m)", xlab="", xaxt="n", ylim=c(200,0), zlim=c(0,0.05),main="", mgp=c(2.5,1,0))
        mtext(4, at=100, line=2.4, text=expression(paste(m^-1,sep="")), outer=F, cex=0.7, las=1)
        #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        #text(x=mid_meses[seq(1,length(mid_meses),by=6)][13:20], y = par("usr")[3] + 30,
        #     labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][13:20], xpd = NA,srt = 40,adj = 0.965,cex = 0.75) 
        ###########
        contour(y=depth_center[196:1], x=c(1:362), z=t(fliplr(TChla_show)),levels=c(0.005,0.02,0.03), col="white", add=T)        
        legend(x=230,y=150, legend=expression(paste(a[PH],"(450)  ")), cex=1.2, text.col="white",bty="n")
        mtext(2, at=0, line=2, text="(b)", cex=cexletter, font=1, las=1)
        
        ## anap
        #############
        TChla<-filerc$lightspectral_anap450
              #2011
              TChla_2011<-TChla[,(361*2):(361*3)]
              # 2012
              TChla_2012<-TChla[,(361*3):(361*4)]    
              # 2013
              TChla_2013<-TChla[,((361*4)+1):((361*5)+1)]    
              # 2014
              TChla_2014<-TChla[,((361*5)+1):((361*6)+1)]    
              TChla<-abind(TChla_2011,TChla_2012,TChla_2013,TChla_2014,along=0.5)
        TChla_show<-apply(TChla, MARGIN=c(2,3), FUN=mean, na.rm=T)
        TChla_show[TChla_show>0.03]<-0.03
        image2D(y=depth_center, x=c(1:362), z=t(TChla_show), las=1, cex.lab=1.2, col=colores_iops,
                ylab="depth (m)", xlab="", xaxt="n", ylim=c(200,0), zlim=c(0,0.03),main="", mgp=c(2.5,1,0))
        mtext(4, at=100, line=2.4, text=expression(paste(m^-1,sep="")), outer=F, cex=0.7, las=1)
        #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        #text(x=mid_meses[seq(1,length(mid_meses),by=6)][13:20], y = par("usr")[3] + 30,
        #     labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][13:20], xpd = NA,srt = 40,adj = 0.965,cex = 0.75) 
        ###########
        contour(y=depth_center[196:1], x=c(1:362), z=t(fliplr(TChla_show)),levels=c(0.001, 0.01), col="white", add=T)        
        legend(x=230,y=150, legend=expression(paste(a[NAP],"(450)  ")), cex=1.2, text.col="white",bty="n")
        mtext(2, at=0, line=2, text="(d)", cex=cexletter, font=1, las=1)
        
        ## acdom
        #############
        TChla<-filerc$lightspectral_acdom450
                  #2011
                  TChla_2011<-TChla[,(361*2):(361*3)]
                  # 2012
                  TChla_2012<-TChla[,(361*3):(361*4)]    
                  # 2013
                  TChla_2013<-TChla[,((361*4)+1):((361*5)+1)]    
                  # 2014
                  TChla_2014<-TChla[,((361*5)+1):((361*6)+1)]    
        TChla<-abind(TChla_2011,TChla_2012,TChla_2013,TChla_2014,along=0.5)
        TChla_show<-apply(TChla, MARGIN=c(2,3), FUN=mean, na.rm=T)
        TChla_notshow<-TChla_show
        TChla_show[TChla_show>0.07]<-0.07
        #TChla_show[TChla_show<0.01]<-0.01
        image2D(y=depth_center, x=c(1:362), z=t(TChla_show), las=1, cex.lab=1.2, col=colores_iops,
                ylab="depth (m)", xlab="", xaxt="n", ylim=c(200,0), zlim=c(0.00,0.05),main="", mgp=c(2.5,1,0))
        mtext(4, at=100, line=2.4, text=expression(paste(m^-1,sep="")), outer=F, cex=0.7, las=1)
        #axis(1, at=mid_meses, labels=F, las=1, cex.axis=0.8)
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        #text(x=mid_meses[seq(1,length(mid_meses),by=6)][17:25], y = par("usr")[3] + 35,
             #labels = etiquetas_meses[seq(1,length(etiquetas_meses),by=6)][17:25], xpd = NA,srt = 40,adj = 0.965,cex = 1) 
        #legend(x="bottomright", legend=expression(paste(a[CDOM],"(450) (", m^-1,")")), cex=1.2, text.col="white",bty="n")
        ###########
        contour(y=depth_center[196:1], x=c(1:362), z=t(fliplr(TChla_notshow)),levels=c(0.018,0.020,0.025), col="white", add=T)        
        legend(x=150,y=130, legend=c(expression(paste("COLOUR:" ,a[CDOM],"(450)  ", sep="")),
                                         expression(paste("LINE: ",z[YSM], sep="")) ),
                                         cex=1.2, text.col="white",bty="n")
        mtext(2, at=0, line=2, text="(f)", cex=cexletter, font=1, las=1)
        
        # Find the 1028.88 isopicnal
        ###########        
        #range(filerc$rho)
        rho<-filerc$rho
        #dim(rho)
        #image2D(y=depth_center, x=tiempo, z=t(rho), las=1, cex.lab=1.2,
        #        ylab="depth", xlab="month", xaxt="n", ylim=c(300,0), zlim=c(1025,1029))
        #contour(y=depth_center[196:1], x=tiempo, z=t(fliplr(rho)),
        #las=1, add=T, levels=c(1028.88),col="purple", lwd=3)
        
        myfun_iso<-function(x, n){
          res<-which(abs(x-n)==min(abs(x-n),na.rm=T))
          if (length(res)>1) res<-res[1]
          return(res)}      
        
        # Find isopicnal
        isopic<-apply(rho, MARGIN=2, FUN=myfun_iso, n=1028.88)      # these are indexes not depths
        #points(x=tiempo, y=depth_center[nlevs-dcm], type="l", col="white", lwd=1)
        pic_depth<-depth_center[isopic]
        #points(x=tiempo,pic_depth,col="black", type="l")            
        
        # Find YSM
        Acdom<-filerc$lightspectral_acdom400
        for (i in c(1:ncol(Acdom))) Acdom[1:isopic[i],i]<-NA
        
        myfun_ysm<-function(x){
          if (sum(!is.na(x))!=0) {  
            res<-which(x==max(x,na.rm=T))
            if (length(res)>1) res<-NA
          } else {res<-NA}
          return(res)}      
        
        ysm<-apply(Acdom, MARGIN=2, FUN=myfun_ysm)      # these are indexes not depths
        ysm_depth<-depth_center[ysm]
                  # 2011
        ysm_2011<-ysm_depth[(361*2):(361*3)]
                  # 2012
        ysm_2012<-ysm_depth[(361*3):(361*4)]    
                  # 2013
        ysm_2013<-ysm_depth[((361*4)+1):((361*5)+1)]    
                  # 2014
        ysm_2014<-ysm_depth[((361*5)+1):((361*6)+1)]
        ysm<-abind(ysm_2011,ysm_2012,ysm_2013,ysm_2014,along=0.5)
        ysm_show<-colMeans(ysm, na.rm=T)        
        points(x=c(1:362),ysm_show,col="white", type="l", lwd=3)
        ########################        
       
        
##### Climatologies Observations
par(mar=c(3,4,0,2))         
        
   # Aph
   ##############
      p_dir <- paste(OS,"Datos/Dat_SGlobal_depth/Boussole/",sep="")
      tabla_total<-read.csv(paste(p_dir,"BOUSSOLE_APHYT_new.csv", sep=""), sep=",", header=T)
      colnames(tabla_total)
      valor1<-rowMeans(tabla_total[,149:173])
      fechita<-paste(tabla_total$year,tabla_total$month,tabla_total$day, sep="-")
      lati<-NA   # plot(long, lati)
      long<-NA
      depth<-(tabla_total$prof.m.)
      datetime <- as.Date(fechita, format="%Y-%m-%d")      
      valor1[valor1=="Inf"]<-NA
      valor1[is.nan(valor1)]<-NA
      datetime<-datetime[!is.na(valor1)]
      depth<-depth[!is.na(valor1)]
      valor1<-valor1[!is.na(valor1)]
      resolx=52
      resoly=40
      source(paste(OS,"Programas/23_FABM/extract_data_BOUSSOLE_optics.R",sep=""))
      climat[climat>0.05]<-0.05
      image2D(x=month, y=prof, z=climat, ylim=c(200,0), zlim=c(0,0.05), las=1,
              ylab="depth (m)", colkey=F, cex.lab=1.2, xaxs="i", xlim=c(4,360),col=colores_iops, xaxt="n")
      axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
   ##############
      contour(x=month, y=prof, z=climat, levels=c(0.005), col="white", add=T)
      legend(x=230,y=150, legend=expression(paste(a[PH],"(450)  ")), cex=1.2, text.col="white",bty="n")
      mtext(2, at=0, line=2, text="(a)", cex=cexletter, font=1, las=1)
      
              
   # Anap
   ##############
   p_dir <- paste(OS,"Datos/Dat_SGlobal_depth/Boussole/",sep="")
   tabla_total<-read.csv(paste(p_dir,"BOUSSOLE_ANAP.csv", sep=""), sep=",", header=T)
      colnames(tabla_total)
      valor1<-rowMeans(tabla_total[,149:173])
      fechita<-paste(tabla_total$year,tabla_total$month,tabla_total$day, sep="-")
      lati<-NA   # plot(long, lati)
      long<-NA
      depth<-(tabla_total$prof.m.)
      datetime <- as.Date(fechita, format="%Y-%m-%d")      
      valor1[valor1=="Inf"]<-NA
      valor1[is.nan(valor1)]<-NA
      datetime<-datetime[!is.na(valor1)]
      depth<-depth[!is.na(valor1)]
      valor1<-valor1[!is.na(valor1)]
      resolx=52
      resoly=40
      source(paste(OS,"Programas/23_FABM/extract_data_BOUSSOLE_optics.R",sep=""))
      climat[climat>0.03]<-0.03
      image2D(x=month, y=prof, z=climat, ylim=c(200,0), zlim=c(0,0.03), las=1,
              ylab="depth (m)", colkey=F, cex.lab=1.2, xaxs="i", xlim=c(4,360),col=colores_iops, xaxt="n")
      axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
      ############## 
      contour(x=month, y=prof, z=climat, levels=c(0.01), col="white", add=T)
      legend(x=230,y=150, legend=expression(paste(a[NAP],"(450)  ")), cex=1.2, text.col="white",bty="n")
      mtext(2, at=0, line=2, text="(c)", cex=cexletter, font=1, las=1)
      

  # Acdom
  ##############
      p_dir <- paste(OS,"Datos/Dat_SGlobal_depth/Boussole/",sep="")
      tabla_total<-read.csv(paste(p_dir,"BOUSSOLE_ACDOM.csv", sep=""), sep=",", header=T)
      #colnames(tabla_total)
      valor1<-rowMeans(tabla_total[,258:282])
      fechita<-paste(tabla_total$year,tabla_total$month,tabla_total$day, sep="-")
      lati<-tabla_total$Lat   # plot(long, lati)
      long<-tabla_total$Long
      depth<-(tabla_total$depth..m.)
      datetime <- as.Date(fechita, format="%Y-%m-%d")      
      valor1[valor1=="Inf"]<-NA
      valor1[is.nan(valor1)]<-NA
      datetime<-datetime[!is.na(valor1)]
      depth<-depth[!is.na(valor1)]
      valor1<-valor1[!is.na(valor1)]
      resolx=52
      resoly=40
      source(paste(OS,"Programas/23_FABM/extract_data_BOUSSOLE_optics.R",sep=""))
      #climat[climat<0.01]<-0.01
      climat[climat>0.05]<-0.05
      image2D(x=month, y=prof, z=climat, ylim=c(200,0), zlim=c(0.00,0.05), las=1,
              ylab="depth (m)", colkey=F, cex.lab=1.2, xaxs="i", xlim=c(4,360),col=colores_iops, xaxt="n")
      axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
  ##############
      contour(x=month, y=prof, z=climat, levels=c(0.020,0.03), col="white", add=T) 
      legend(x=220,y=150, legend=expression(paste(a[CDOM],"(450)  ", sep="")), cex=1.2, text.col="white",bty="n")
      mtext(2, at=0, line=2, text="(e)", cex=cexletter, font=1, las=1)
      
      #mtext(1,line=2,at=365/2, text=c("day of the year"), outer=F, cex=0.8)       
      mtext(3,line=1,at=c(0.25,0.73), text=c("Observations","Model"), outer=T, cex=cextitle)  

            
      dev.off() 
        
        
        