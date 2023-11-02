if (Sys.info()['sysname']=="Windows") {OS<-"C:/"} else {OS<-paste("/Users/",Sys.info()['user'],"/", sep="")}

### Figures hypothesis-testing experiments (Section 3.4)

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
   
    ## Model runs
    experimentos <- read.csv(paste(o_dir,"run_log_ALL3.csv", sep=""), sep=",")    
    nombres<-experimentos$file 
    carpetas<-experimentos$folder
       
    
    
       
### Compute DOC and CDOM fluxes      
        
        myfun<-function(x){ res<-which(x==max(x,na.rm=T))
        if (length(res)>1) res<-res[1]
        return(res)}

        elegidos<-c(7,9,10)
        nombres[elegidos]
        colores[elegidos]
        
    TOT2<-matrix(NA, nrow=2169, ncol=length(elegidos))
    TOT_DCM2<-TOT2
    SEA2<-TOT2
    SEA_DCM2<-TOT2
    CDOM2<-TOT2
    CDOM_DCM2<-TOT2
    DOC_PHYTO2<-TOT2
    DOC_PHYTO_DCM2<-TOT2   
    
for (i in elegidos){
    archivo<-paste(nombres[i],"/result.nc",sep="")
    filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
    filenc <- open.nc(filename)
    filerc <- read.nc(filenc)
    tiempo<-filerc$time/86400
    mid_meses<-seq(-2192+15, 4382-15, length=18*12)
    meses<-c(1:(18*12))
    etiquetas_meses<-c(paste("2003",c(1:12),sep="-"),paste("2004",c(1:12),sep="-"),paste("2005",c(1:12),sep="-"),paste("2006",c(1:12),sep="-"),
                       paste("2007",c(1:12),sep="-"),paste("2008",c(1:12),sep="-"),paste("2009",c(1:12),sep="-"),paste("2010",c(1:12),sep="-"),
                       paste("2011",c(1:12),sep="-"),paste("2012",c(1:12),sep="-"),paste("2013",c(1:12),sep="-"),paste("2014",c(1:12),sep="-"),
                       paste("2015",c(1:12),sep="-"),paste("2016",c(1:12),sep="-"),paste("2017",c(1:12),sep="-"),paste("2018",c(1:12),sep="-"),
                       paste("2019",c(1:12),sep="-"),paste("2020",c(1:12),sep="-")) 
    mesitos<-tiempo[(365*2):max(tiempo)]
    anitos<-rep(mesitos[1:365],length=length(mesitos))
  
    # find dcm
    TChla<-filerc$P1_Chl+filerc$P2_Chl+filerc$P3_Chl+filerc$P4_Chl
    dcm<-apply(TChla, MARGIN=2, FUN=myfun)      # these are indexes not depths
    dcm_depth<-depth_center[dcm]

      #### fluxes DOC (mgC m-3 d-1)
      ############### 
      # production of DOC
      R0<-(filerc$P1_flPIR2c_tot)+(filerc$P2_flPIR2c_tot)+(filerc$P3_flPIR2c_tot)+(filerc$P4_flPIR2c_tot)
          doc_phyto<-colMeans(R0[171:196,])
          doc_phyto_dcm<-rep(NA,length=ncol(R0))
          for (k in c(1:ncol(R0))) doc_phyto_dcm[k]<-R0[dcm[k],k]
      R0<-filerc$Z5_rr1c+filerc$Z6_rr1c
          doc_zoo<-colMeans(R0[171:196,])
          doc_zoo_dcm<-rep(NA,length=ncol(R0))
          for (k in c(1:ncol(R0))) doc_zoo_dcm[k]<-R0[dcm[k],k]    
      R0<-filerc$B1_reR3c
          doc_bact<-colMeans(R0[171:196,])
          doc_bact_dcm<-rep(NA,length=ncol(R0))
          for (k in c(1:ncol(R0))) doc_bact_dcm[k]<-R0[dcm[k],k]
      # total production of DOC
      R0<-(filerc$P1_flPIR2c_tot)+(filerc$P2_flPIR2c_tot)+(filerc$P3_flPIR2c_tot)+(filerc$P4_flPIR2c_tot) +filerc$B1_reR3c+filerc$Z5_rr1c+filerc$Z6_rr1c           # falta disolucion de R6 ?
      tot_doc<-colMeans(R0[171:196,])
      tot_doc_dcm<-rep(NA,length=ncol(R0))
      for (k in c(1:ncol(R0))) tot_doc_dcm[k]<-R0[dcm[k],k]
      
      # bacterial consumption of DOC
      R0<-filerc$B1_ruR2c+filerc$B1_ruX2c
            deg_doc2<-colMeans(R0[171:196,])
            deg_doc2_dcm<-rep(NA,length=ncol(R0))
            for (k in c(1:ncol(R0))) deg_doc2_dcm[k]<-R0[dcm[k],k]      
      R0<-filerc$B1_ruR1c+filerc$B1_ruX1c
            deg_doc1<-colMeans(R0[171:196,])
            deg_doc1_dcm<-rep(NA,length=ncol(R0))
            for (k in c(1:ncol(R0))) deg_doc1_dcm[k]<-R0[dcm[k],k]       
      R0<-filerc$PelChem_remR3c + filerc$PelChem_remX3c
            deg_doc3<-colMeans(R0[171:196,])
            deg_doc3_dcm<-rep(NA,length=ncol(R0))
            for (k in c(1:ncol(R0))) deg_doc3_dcm[k]<-R0[dcm[k],k]    
      # total consumption of DOC      
      R0<-filerc$B1_ruR2c+filerc$B1_ruX2c + filerc$B1_ruR1c+filerc$B1_ruX1c + filerc$PelChem_remR3c + filerc$PelChem_remX3c
      deg_doc<-colMeans(R0[171:196,])
      deg_doc_dcm<-rep(NA,length=ncol(R0))
      for (k in c(1:ncol(R0))) deg_doc_dcm[k]<-R0[dcm[k],k]
      ############      

      #### fluxes CDOM (mgC m-3 d-1)
      ###############       
      # bacterial consumption of CDOM
      R0<-filerc$B1_ruX1c+filerc$B1_ruX2c+filerc$PelChem_remX3c
      rem_cdom<-colMeans(R0[171:196,])
      # photodegradation CDOM
      R0<-filerc$PelChem_degX3c+filerc$PelChem_degX2c+filerc$PelChem_degX1c
      deg_cdom<-colMeans(R0[171:196,])
      # total degradation CDOM
      R0<-filerc$B1_ruX1c+filerc$B1_ruX2c+filerc$PelChem_remX3c+filerc$PelChem_degX3c+filerc$PelChem_degX2c+filerc$PelChem_degX1c
      tot_deg_cdom<-colMeans(R0[171:196,])
      tot_deg_cdom_dcm<-rep(NA,length=ncol(R0))
      for (k in c(1:ncol(R0))) tot_deg_cdom_dcm[k]<-R0[dcm[k],k]    

      # X2 fluxes
      R0<-(filerc$P1_flPIR2c_tot)+(filerc$P2_flPIR2c_tot)+(filerc$P3_flPIR2c_tot)+(filerc$P4_flPIR2c_tot)
      tot<-colMeans(R0[171:196,])
      tot_dcm<-rep(NA,length=ncol(R0))
      for (k in c(1:ncol(R0))) tot_dcm[k]<-R0[dcm[k],k]
      
      R0<-(filerc$P1_flPIR2c_act)+(filerc$P2_flPIR2c_act)+(filerc$P3_flPIR2c_act)+(filerc$P4_flPIR2c_act)
      sea<-colMeans(R0[171:196,])
      sea_dcm<-rep(NA,length=ncol(R0))
      for (k in c(1:ncol(R0))) sea_dcm[k]<-R0[dcm[k],k]
      
      test<-filerc$P1_f2cdom
      colMeans(test[171:196,])
           
      # This flux is different depending on formulation
      # Optimized: f2cdom=sea*(%*(theta))
      # Only nutrients: f2cdom=sea*%
      if (i<=9) {R0<-(filerc$P1_flPIR2c_act*filerc$P1_f2cdom)+(filerc$P2_flPIR2c_act*filerc$P2_f2cdom)+(filerc$P3_flPIR2c_act*filerc$P3_f2cdom)+(filerc$P4_flPIR2c_act*filerc$P4_f2cdom)}
      
      # Constant: f2cdom=seo*%
      # Only light: f2cdom=seo*(%*(theta))
      if (i>=10) {R0<-(filerc$P1_flPIR2c_tot*filerc$P1_f2cdom)+(filerc$P2_flPIR2c_tot*filerc$P2_f2cdom)+(filerc$P3_flPIR2c_tot*filerc$P3_f2cdom)+(filerc$P4_flPIR2c_tot*filerc$P4_f2cdom)}
      
      cdom<-colMeans(R0[171:196,])
      cdom_dcm<-rep(NA,length=ncol(R0))
      for (k in c(1:ncol(R0))) cdom_dcm[k]<-R0[dcm[k],k]

      # total flux to CDOM      
      R1<- R0+filerc$B1_reR3c*0.042+(filerc$Z5_rr1c*0.030)+(filerc$Z6_rr1c*0.046)
      tot_cdom<-colMeans(R1[171:196,])
      tot_cdom_dcm<-rep(NA,length=ncol(R1))
      for (k in c(1:ncol(R1))) tot_cdom_dcm[k]<-R1[dcm[k],k]        
      ###############        
      
      ##############
      # To save
      cual<-which(elegidos==i)
      DOC_PHYTO2[,cual]<-doc_phyto
      DOC_PHYTO_DCM2[,cual]<-doc_phyto_dcm
      TOT2[,cual]<-tot
      TOT_DCM2[,cual]<-tot_dcm
      SEA2[,cual]<-sea
      SEA_DCM2[,cual]<-sea_dcm
      CDOM2[,cual]<-cdom
      CDOM_DCM2[,cual]<-cdom_dcm
      
} # end loop modelrun                   


    

        
        
###############    
##### FIGURE 12
###############    
##### fluxes to CDOM
        
png(file=paste(path_figures,"Figure12_fluxes_cdom_alternative.png",sep=""), width=1600, height=580, pointsize=26, family="Helvetica")
#pdf(file=paste(path_figures,"Figure12_fluxes_cdom_alternative.pdf",sep=""), width=1600/100, height=580/100, pointsize=20, family="Helvetica")     
        layout(matrix(c(1,2,4,1,3,5),ncol=3, byrow=T), widths = c(1,0.9,0.9))
        par(mar=c(3,4.5,1,2))
        par(oma=c(0,0,1,3))
        #layout.show(n=5)
        anchol=3
        cexletter=1.6
        cexaxis=1.05
        cexlabs=0.65  
        cexleter=1.2
        
###### % CDOM in total exudation
        plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
             main="", ylab=expression(paste(italic(f)["R2"]^"X2", sep="  ")), xlab="",
             xlim=c(1,363), xaxs="i", ylim=c(0,0.07), yaxs="i", mgp=c(3,1,0), xaxt="n")
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        mtext(3,at=0,outer=F,line=0.5,text="(a)",cex=cexleter, font=2)
        
        ### Optimized
        ####################               
        aph<-(CDOM2[,1]/TOT2[,1])[(365*2):max(tiempo)]
        time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
        groups<-split(aph, time_clases, drop = FALSE)
        media<-sapply(groups,median)
        points(x=c(1:365), y=media, type="l", col=colores[elegidos[1]], lty=2, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
        
        aph<-(CDOM_DCM2[,1]/TOT_DCM2[,1])[(365*2):max(tiempo)]
        time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
        groups<-split(aph, time_clases, drop = FALSE)
        media<-sapply(groups,median)
        points(x=c(1:365), y=media, type="l", col=colores[elegidos[1]], lty=1, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
        #################### 
        
        ### Experiments
        ################ 
        aph<-(CDOM2[,3]/TOT2[,3])[(365*2):max(tiempo)]
        time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
        groups<-split(aph, time_clases, drop = FALSE)
        media<-sapply(groups,median)
        points(x=c(1:365), y=media, type="l", col=colores[elegidos[3]], lty=2, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
        
        aph<-(CDOM_DCM2[,3]/TOT_DCM2[,3])[(365*2):max(tiempo)]
        time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
        groups<-split(aph, time_clases, drop = FALSE)
        media<-sapply(groups,median)
        points(x=c(1:365), y=media, type="l", col=colores[elegidos[3]], lty=1, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
        
        aph<-(CDOM2[,2]/TOT2[,2])[(365*2):max(tiempo)]
        time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
        groups<-split(aph, time_clases, drop = FALSE)
        media<-sapply(groups,median)
        points(x=c(1:365), y=media, type="l", col=colores[elegidos[2]], lty=2, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
        
        aph<-(CDOM_DCM2[,2]/TOT_DCM2[,2])[(365*2):max(tiempo)]
        time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
        groups<-split(aph, time_clases, drop = FALSE)
        media<-sapply(groups,median)
        points(x=c(1:365), y=media, type="l", col=colores[elegidos[2]], lty=1, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
        #################
        
        legend(x=84,y=0.065, col=colores[elegidos[c(3,2,1)]], bty="n", lwd=anchol, lty=3, legend=c(" "," ",""))     
        legend(x=120,y=0.065, legend=c(expression(italic("Constant dpp ratio")),expression(italic("Constant leakage ratio")),expression(italic("Optimized"))),
               col=colores[elegidos[c(3,2,1)]], bty="n", lwd=anchol, lty=1)     
        text(x=109,y=0.063, pos=3, label="(0-9m)", cex=cexlabs+0.2, font=1)                
        text(x=150,y=0.063, pos=3, label="DCM",    cex=cexlabs+0.2, font=1)
        
        
##### total fluxes CDOM  (0-9m)    
        plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
             main="", ylab=expression(paste(X["C"]^"(2)"," (mg ",m^-3,"",d^-1,")", sep="  ")), xlab="",
             xlim=c(1,363), xaxs="i", ylim=c(0,0.6), yaxs="i", mgp=c(2.5,1,0), xaxt="n")
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        #axis(4, at=seq(0,1.0,length=6), labels=seq(0,1.0,length=6)*30, las=1, cex.axis=1)
        #mtext(4, at=0.5, line=2.5, text=expression(paste(R["C"]^"(2)"," (mgC ",m^-3,"",d^-1,")", sep="  ")), cex=cexlabs, outer=F)
        mtext(3,at=0,outer=F,line=0.5,text="(b)",cex=cexleter, font=2)
        
        # total DOC exudation
        ####################               
        # aph<-(TOT2[,1])[(365*2):max(tiempo)]
        # time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
        # groups<-split(aph, time_clases, drop = FALSE)
        # media<-sapply(groups,median)
        # points(x=c(1:365), y=media/30, type="l", col=linea_grey, lty=1, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
        #################### 
        
        # CDOM exudation
        ### Optimized
        ####################               
        aph<-(CDOM2[,1])[(365*2):max(tiempo)]
        time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
        groups<-split(aph, time_clases, drop = FALSE)
        media<-sapply(groups,median)
        points(x=c(1:365), y=media, type="l", col=colores[elegidos[1]], lty=1, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
        #################### 
        
        ### Experimentos
        ################
        aph<-(CDOM2[,3])[(365*2):max(tiempo)]
        time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
        groups<-split(aph, time_clases, drop = FALSE)
        media<-sapply(groups,median)
        points(x=c(1:365), y=media, type="l", col=colores[elegidos[3]], lty=1, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
        
        aph<-(CDOM2[,2])[(365*2):max(tiempo)]
        time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
        groups<-split(aph, time_clases, drop = FALSE)
        media<-sapply(groups,median)
        points(x=c(1:365), y=media, type="l", col=colores[elegidos[2]], lty=1, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
        #################              
        
        #legend(x="topright", legend=c(expression(R["C"]^"(2)"), expression(italic("Constant")),expression(italic("Nutrients")),expression(italic("Optimized"))),
        #       col=c(linea_grey, colores[elegidos[c(3,2,1)]]), bty="n", lwd=anchol) 
        legend(x="topright", legend=c(expression(italic("Constant dpp ratio")),expression(italic("Constant leakage ratio")),expression(italic("Optimized"))),
               col=c(colores[elegidos[c(3,2,1)]]), bty="n", lwd=anchol)         
        #legend(x="topleft", legend="(0-9m)", cex=1.2,  bty="n")
        legend(x=-20, y=0.6, legend="(0-9m)", cex=1.2,  bty="n")
        
    # total fluxes DCM     
        plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
             main="", ylab=expression(paste(X["C"]^"(2)"," (mg ",m^-3,"",d^-1,")", sep="  ")), xlab="",
             xlim=c(1,363), xaxs="i", ylim=c(0,0.3), yaxs="i", mgp=c(2.5,1,0), xaxt="n", yaxt="n")
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        axis(2, at=seq(0.0,0.3,by=0.1), las=1)
        #axis(4, at=seq(0,0.6,length=7), labels=seq(0,0.6,length=7)*30, las=1, cex.axis=1)
        #mtext(4, at=0.2, line=2.0, text=expression(paste(R["C"]^"(2)"," (mgC ",m^-3,"",d^-1,")", sep="  ")), cex=cexlabs, outer=F)
        mtext(3,at=0,outer=F,line=0.5,text="(c)",cex=cexleter, font=2)
        
        # total DOC exudation
        ####################               
        # aph<-(TOT_DCM2[,1])[(365*2):max(tiempo)]
        # time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
        # groups<-split(aph, time_clases, drop = FALSE)
        # media<-sapply(groups,median)
        # points(x=c(1:365), y=media/30, type="l", col=linea_grey, lty=1, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
        #################### 
        
        # total CDOM exudation
        ### Experimentos
        ################
        aph<-(CDOM_DCM2[,3])[(365*2):max(tiempo)]
        time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
        groups<-split(aph, time_clases, drop = FALSE)
        media<-sapply(groups,median)
        points(x=c(1:365), y=media, type="l", col=colores[elegidos[3]], lty=1, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
        
        aph<-(CDOM_DCM2[,2])[(365*2):max(tiempo)]
        time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
        groups<-split(aph, time_clases, drop = FALSE)
        media<-sapply(groups,median)
        points(x=c(1:365), y=media, type="l", col=colores[elegidos[2]], lty=1, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
        #################              
        
        ### Optimized
        ####################               
        aph<-(CDOM_DCM2[,1])[(365*2):max(tiempo)]
        time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
        groups<-split(aph, time_clases, drop = FALSE)
        media<-sapply(groups,median)
        points(x=c(1:365), y=media, type="l", col=colores[elegidos[1]], lty=1, lwd=anchol, las=1, cex.lab=1.0, pch=19, cex=0.2)
        #################### 
        
        #legend(x="topright", legend=c(expression(R["C"]^"(2)"), expression(italic("Constant")),expression(italic("Nutrients")),expression(italic("Optimized"))),
        #       col=c(linea_grey, colores[c(37,36,34)]), bty="n", lwd=anchol)     
        legend(x="topright", legend=c(expression(italic("Constant dpp ratio")),expression(italic("Constant leakage ratio")),expression(italic("Optimized"))),
               col=c(colores[elegidos[c(3,2,1)]]), bty="n", lwd=anchol)             
        legend(x=-20, y=0.3, legend="DCM", cex=1.2,  bty="n")
        
      
###### acdom(450)        
        # surface
        plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
             main="", ylab="", xlab="",
             xlim=c(1,365), xaxs="i", ylim=c(0,0.07), yaxs="i", mgp=c(2.5,1,0), xaxt="n", yaxt="n") 
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        axis(2, at=seq(0,0.07,length=8), labels=T, las=1, cex.axis=1)
        #mtext(2, at=0.035, line=3.0, text=expression(m^-1), cex=cexlabs, outer=F, las=0)
        mtext(2, at=0.035, line=3.0, text=expression(paste(a[CDOM],"(450) ",m^-1,sep="")), cex=cexlabs, outer=F, las=0)
        mtext(3,at=0,outer=F,line=0.5,text="(d)",cex=cexleter, font=2)
        legend(x="topright", legend=expression(paste("(0-9 m)")), cex=1.2,bty="n")
        #legend(x=-20, y=0.07, legend="(0-9m)", cex=1.2,  bty="n")
        
        ### Boussole observations <- clim
        #########################   
        p_dir <- paste(o_dir,"BOUSSOLE_DATA/",sep="")
        reflectancia<-read.csv(paste(p_dir,"BOUSSOLE_ACDOM.csv", sep=""), sep=",", header=T)
        #names(reflectancia)
        fechita<-paste(reflectancia$year,reflectancia$month,reflectancia$day, sep="-")
        horita<-paste(reflectancia$Time_CTD_start..UTC.,"00", sep=":")
        # range(horita)        # rrs ya esta filtrada entre 10:00 y 14:00
        fechas <- chron(dates=fechita,times=horita,format=c("y-m-d","h:m:s"), out.format=c("y-m-d","h:m:s")) 
        julianos<- julian(x=month(fechas), d=day(fechas), y=year(fechas), origin.=c(month = 1, day = 1, year = 2009))
        #range(julianos)
        julianos<-yday(fechas)
        #etiquetas_julianas<- julian(x=1, d=1, y=seq(2009,2015,by=1), origin.=c(month = 1, day = 1, year = 2009))
        #fechas2<-fechas#[33:length(fechas)]
        datos2<-rowMeans(reflectancia[,258:282])
        #datos2<-rowMeans(reflectancia[,268:272])
        #length(datos2)
        datos2[reflectancia$depth..m.>9]<-NA
        #fechas2<-fechas2[!is.na(datos2)]
        julianos<-julianos[!is.na(datos2)]
        datos2<-datos2[!is.na(datos2)]
        points(julianos, datos2, type="p", las=1, pch=21, cex=0.6, col="grey30", bg=col_bou)
        ####################
        
        #### Model
        #########     
        for (i in elegidos){
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          
          R0<-filerc$lightspectral_acdom450+filerc$lightspectral_anap450
          aph<-colMeans(R0[171:196,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(aph, time_clases, drop = FALSE)
          media<-sapply(groups,median)
          points(x=c(1:365), y=media, type="l", col=colores[i], lwd=anchol, pch=19, cex=0.6)
        }
        ########################    
        
        # DCM
        plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
             main="", ylab="", xlab="",
             xlim=c(1,365), xaxs="i", ylim=c(0,0.07), yaxs="i", mgp=c(2.5,1,0), xaxt="n", yaxt="n") 
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        axis(2, at=seq(0,0.07,length=8), labels=T, las=1, cex.axis=1)
        #mtext(2, at=0.035, line=3.0, text=expression(m^-1), cex=cexlabs, outer=F, las=0)
        mtext(2, at=0.035, line=3.0, text=expression(paste(a[CDOM],"(450) ",m^-1,sep="")), cex=cexlabs, outer=F, las=0)
        mtext(3,at=0,outer=F,line=0.5,text="(e)",cex=cexleter, font=2)
        legend(x="topright", legend=expression(paste("(40-60 m)")), cex=1.2, bty="n")
        #legend(x=-20, y=0.07, legend="DCM", cex=1.2,  bty="n")
        
        ### Boussole observations
        #########################    
        # Acdom
        p_dir <- paste(o_dir,"BOUSSOLE_DATA/",sep="")
        reflectancia<-read.csv(paste(p_dir,"BOUSSOLE_ACDOM.csv", sep=""), sep=",", header=T)
        #names(reflectancia)
        fechita<-paste(reflectancia$year,reflectancia$month,reflectancia$day, sep="-")
        horita<-paste(reflectancia$Time_CTD_start..UTC.,"00", sep=":")
        # range(horita)        # rrs ya esta filtrada entre 10:00 y 14:00
        fechas <- chron(dates=fechita,times=horita,format=c("y-m-d","h:m:s"), out.format=c("y-m-d","h:m:s")) 
        julianos<- julian(x=month(fechas), d=day(fechas), y=year(fechas), origin.=c(month = 1, day = 1, year = 2009))
        #range(julianos)
        julianos<-yday(fechas)
        #etiquetas_julianas<- julian(x=1, d=1, y=seq(2009,2015,by=1), origin.=c(month = 1, day = 1, year = 2009))
        #fechas2<-fechas#[33:length(fechas)]
        datos2<-rowMeans(reflectancia[,258:282])
        #length(datos2)
        #datos2[reflectancia$depth..m.>9]<-NA
        datos2[reflectancia$depth..m.<40]<-NA
        datos2[reflectancia$depth..m.>60]<-NA        
        #fechas2<-fechas2[!is.na(datos2)]
        julianos<-julianos[!is.na(datos2)]
        datos2<-datos2[!is.na(datos2)]
        #points(julianos, datos2, type="b", las=1, pch=19, cex=0.2, ylim=c(0,0.01),
        #       col="yellow2", ylab=expression(m^-1), xlab="yy-mm",xlim=c(0,max(tiempo)), xaxs="i",yaxs="i", xaxt="n")
        points(julianos, datos2, type="p", pch=21, cex=0.6, col="grey30", bg=col_bou)
        ####################
        
        
        #### Model
        #########     
        for (i in elegidos){
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          
          # find dcm
          TChla<-filerc$P1_Chl+filerc$P2_Chl+filerc$P3_Chl+filerc$P4_Chl
          dcm<-apply(TChla, MARGIN=2, FUN=myfun)      # these are indexes not depths
          dcm_depth<-depth_center[dcm]          
          
          R0<-filerc$lightspectral_acdom450 #+filerc$lightspectral_anap450
          R0<-R0[,(365*2):max(tiempo)]
          adg<-colMeans(R0[147:153,])
          #adg<-rep(NA,length=ncol(R0))
          #for (j in c(1:ncol(R0))) adg[j]<-R0[dcm[j],j]

          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(adg, time_clases, drop = FALSE)
          media<-sapply(groups,median)
          points(x=c(1:365), y=media, type="l", col=colores[i], lwd=anchol, pch=19, cex=0.6)
        }
        ########################
        
        dev.off()
        
        
        
        
        
        
        
        
#################        
### FIGURE 13 ###        
#################
        elegidos<-c(7,12)
        nombres[elegidos]
        colores[elegidos]
        
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
        mesitos<-tiempo[(365*2):max(tiempo)]
        anitos<-rep(mesitos[1:365],length=length(mesitos))        
        
        
        
#png(file=paste(path_figures,"Figure13_bleaching_acdom_profiles.png",sep=""), width=1000, height=1000, pointsize=24, family="Helvetica")
pdf(file=paste(path_figures,"Figure13_bleaching_acdom_profiles.pdf",sep=""), width=1000/100, height=1000/100, pointsize=17.5, family="Helvetica")        
        #layout(matrix(c(rep(1,4),2:5),ncol=2, byrow=F), widths = c(0.5,1))

        layout(matrix(c(1,2,      5,3,6,4, 2, 1, rep(2,6),  # B O B O
                        1,7,    10,8,11,9, 7, 1, rep(7,6),  # B O B O
                        1,12, 15,13,16,14,12, 1, rep(12,6), # B O B O
                        1,17, 20,18,19,21,17, 1, rep(17,6)),  ncol=7, byrow=T), widths = c(0.7,0.1,rep(0.2,4),0.4))
        par(mar=c(2,4,1,4))
        par(oma=c(2,1,1,1))
        #layout.show(n=21)
        cexdates=1
        cexleter=1.2
        cexlegend=1.1
        cexaxis=1.1
        anchol=3
        cexlabel=0.8
        
#### degradation
        plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
             main="", xaxs="i", xlim=c(-5,-1), ylim=c(400,0), yaxs="i", xaxt="n",
             ylab="", xlab="", yaxt="n")
        mtext(2,at=200,outer=F,line=3,text="depth (m)",cex=cexlabel,las=0)
        axis(2,at=c(0,9,40,60,80,100,150,200,300,400),labels=T, las=1)
        axis(1,at=seq(-5,-1, by=1),labels=10^seq(-5,-1, by=1))
        mtext(3,at=-5,outer=F,line=0.5,text="(a)",cex=cexleter, font=2)
        mtext(1,at=-3,outer=F,line=2.5,text=expression(paste(degX[C]^3,"+",remX[C]^3," (mg ", m^-3," ", d^-1, ")",sep="")),cex=0.75)
        
        #### Model
        #########            
        for (i in elegidos){
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          R0<-filerc$PelChem_degX3c+filerc$PelChem_remX3c
          adg<-rowMeans(R0[,])
          points(x=log10(adg), y=depth_center, type="p", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.6) 
        }
        ########################    
        
        legend(x="bottomright",legend=c(expression(italic("Optimized")),expression(italic("Intense bleaching"))),
               col=c(colores[elegidos]), bty="o", pch=c(19,19))                
        
#### surface  acdom(450)
        par(mar=c(2,4,1,1))
        
        plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
             main="", ylab="", xlab="", xlim=c(1,363), xaxs="i", ylim=c(0,0.07), yaxs="i", mgp=c(2.5,1,0), xaxt="n")
        
        mtext(2, at=0.045, outer=F, line=3.0, text=expression(paste(a[CDOM],"(450)",sep="")), cex=cexlabel-0.1, las=1)
        mtext(2, at=0.032, outer=F, line=4.0,   text=expression(m^-1),cex=cexlabel-0.1,las=1)
        
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        text(x=c(360), y=c(0.060), pos=2, labels=c("(0-9 m)"), cex=c(cexlegend+0.2), font=c(2), bty="n")
        mtext(3,at=1,outer=F,line=0.5,text="(b)",cex=cexleter, font=2)
        #abline(v=c(100,280,300))
        
        #### Model
        #########            
        for (i in elegidos){
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          R0<-filerc$lightspectral_acdom450#+filerc$lightspectral_anap450
          adg<-colMeans(R0[171:196,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(adg, time_clases, drop = FALSE)
          media<-sapply(groups,median)
          points(x=c(1:365), y=media, type="p", col=colores[i], lwd=anchol, pch=19, cex=0.6)
        }
        ########################    

        ### Boussole observations
        ###################   
        p_dir <- paste(o_dir,"BOUSSOLE_DATA/",sep="")
        reflectancia<-read.csv(paste(p_dir,"BOUSSOLE_ACDOM.csv", sep=""), sep=",", header=T)
        fechita<-paste(reflectancia$year,reflectancia$month,reflectancia$day, sep="-")
        horita<-paste(reflectancia$Time_CTD_start..UTC.,"00", sep=":")
        # range(horita)        # rrs ya esta filtrada entre 10:00 y 14:00
        fechas <- chron(dates=fechita,times=horita,format=c("y-m-d","h:m:s"), out.format=c("y-m-d","h:m:s")) 
        julianos<- julian(x=month(fechas), d=day(fechas), y=year(fechas), origin.=c(month = 1, day = 1, year = 2009))
        julianos<-yday(fechas)
        #etiquetas_julianas<- julian(x=1, d=1, y=seq(2009,2015,by=1), origin.=c(month = 1, day = 1, year = 2009))
        fechas2<-fechas#[33:length(fechas)]
        datos2<-rowMeans(reflectancia[,258:282])
        datos2[reflectancia$depth..m.>9]<-NA
        fechas2<-fechas2[!is.na(datos2)]
        julianos<-julianos[!is.na(datos2)]
        datos2<-datos2[!is.na(datos2)]
        points(julianos, datos2, type="p", las=1, pch=21, cex=0.6, col="black", bg=col_bou)
        #########################
        
        ### Reactivity pies
        ###################  
        x1=47
        x2=126
        #abline(v=c(x1,x2))  
        segments(x0=x1,x1=70, y0=0.020, y1=0.055)
        segments(x0=x1,x1=30, y0=0.019, y1=0.055)
        segments(x0=x2,x1=205, y0=0.034, y1=0.055)
        segments(x0=x2,x1=160, y0=0.064, y1=0.055)
        text(x=c(18,60,148,190),y=rep(0.07,4), pos=1, labels=c("B","O","B","O"))
        
        #### Model
        i=elegidos[1]
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          R0<-filerc$X3_c
          R0[R0<1e-8]<-NA
          X3<-colMeans(R0[171:196,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X3, time_clases, drop = FALSE)
          mediaX3<-sapply(groups,median, na.rm=T)
          R0<-filerc$X2_c
          R0[R0<1e-8]<-NA
          X2<-colMeans(R0[171:196,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X2, time_clases, drop = FALSE)
          mediaX2<-sapply(groups,median, na.rm=T)          
          R0<-filerc$X1_c
          R0[R0<1e-8]<-NA
          X1<-colMeans(R0[171:196,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X1, time_clases, drop = FALSE)
          mediaX1<-sapply(groups,median, na.rm=T)  
          totalX<-mediaX1+mediaX2+mediaX3
          #pie(c(mediaX1[100],mediaX2[100],mediaX3[100]), labels=c("X1", "X2", "X3"))
          par(mar=c(0,0,0,2))  
          pie(c(mediaX1[x1],mediaX2[x1],mediaX3[x1]), labels=c("", "", ""), main="", cex=0.5, col=c("white", "bisque1", "bisque3"))
          pie(c(mediaX1[x2],mediaX2[x2],mediaX3[x2]), labels=c("", "", ""), main="", cex=0.5, col=c("white", "bisque1", "bisque3"))
          
        i=elegidos[2]
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          R0<-filerc$X3_c
          R0[R0<1e-8]<-NA
          X3<-colMeans(R0[171:196,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X3, time_clases, drop = FALSE)
          mediaX3<-sapply(groups,median, na.rm=T)
          R0<-filerc$X2_c
          R0[R0<1e-8]<-NA
          X2<-colMeans(R0[171:196,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X2, time_clases, drop = FALSE)
          mediaX2<-sapply(groups,median, na.rm=T)          
          R0<-filerc$X1_c
          R0[R0<1e-8]<-NA
          X1<-colMeans(R0[171:196,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X1, time_clases, drop = FALSE)
          mediaX1<-sapply(groups,median, na.rm=T)  
          totalX<-mediaX1+mediaX2+mediaX3
          par(mar=c(0,2,0,0))
          pie(c(mediaX1[x1],mediaX2[x1],mediaX3[x1]), labels=c("",expression(X["C"]^"(2)"),expression(X["C"]^"(3)")),main="", cex=0.5, col=c("white", "bisque1", "bisque3"))
          pie(c(mediaX1[x2],mediaX2[x2],mediaX3[x2]), labels=c(expression(X["C"]^"(1)"), "", ""),main="", cex=0.5, col=c("white", "bisque1", "bisque3"))
        ########################         

          
        
### aCDOM(450)(40-60m)
        par(mar=c(2,4,1,1))
        plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
             main="", ylab="", xlab="",
             xlim=c(1,365), xaxs="i", ylim=c(0,0.07), yaxs="i", mgp=c(2.5,1,0), xaxt="n") 
        mtext(2, at=0.045, outer=F, line=3.0, text=expression(paste(a[CDOM],"(450)",sep="")), cex=cexlabel-0.1, las=1)
        mtext(2, at=0.032, outer=F, line=4.0,   text=expression(m^-1),cex=cexlabel-0.1,las=1)
        
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        #text(x=c(360,360), y=c(0.060,0.045), pos=2,
             #labels=c(expression(paste(a[CDOM],"(450)")),"(40-60m)"),
             #cex=c(cexlegend,cexlegend+0.2), font=c(1,2), bty="n")
        text(x=c(360), y=c(0.060), pos=2, labels=c("(40-60 m)"),cex=c(cexlegend+0.2), font=c(2), bty="n")
        mtext(3,at=1,outer=F,line=0.5,text="(c)",cex=cexleter, font=2)
        
        #### Model
        ################     
        for (i in elegidos){
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          R0<-filerc$lightspectral_acdom450#+filerc$lightspectral_anap450
          adg<-colMeans(R0[147:153,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(adg, time_clases, drop = FALSE)
          media<-sapply(groups,median)
          points(x=c(1:365), y=media, type="p", col=colores[i], lwd=anchol, pch=19, cex=0.6)          
        }
        ########################    

        ### Boussole observations
        #################     
        # Acdom
        p_dir <- paste(o_dir,"BOUSSOLE_DATA/",sep="")
        reflectancia<-read.csv(paste(p_dir,"BOUSSOLE_ACDOM.csv", sep=""), sep=",", header=T)
        #names(reflectancia)
        fechita<-paste(reflectancia$year,reflectancia$month,reflectancia$day, sep="-")
        horita<-paste(reflectancia$Time_CTD_start..UTC.,"00", sep=":")
        # range(horita)        # rrs ya esta filtrada entre 10:00 y 14:00
        fechas <- chron(dates=fechita,times=horita,format=c("y-m-d","h:m:s"), out.format=c("y-m-d","h:m:s")) 
        julianos<- julian(x=month(fechas), d=day(fechas), y=year(fechas), origin.=c(month = 1, day = 1, year = 2009))
        julianos<-yday(fechas)
        etiquetas_julianas<- julian(x=1, d=1, y=seq(2009,2015,by=1), origin.=c(month = 1, day = 1, year = 2009))
        fechas2<-fechas#[33:length(fechas)]
        datos2<-rowMeans(reflectancia[,258:282])
        datos2[reflectancia$depth..m.<40]<-NA
        datos2[reflectancia$depth..m.>60]<-NA        
        fechas2<-fechas2[!is.na(datos2)]
        julianos<-julianos[!is.na(datos2)]
        datos2<-datos2[!is.na(datos2)]
        points(julianos, datos2, type="p", las=1, pch=21, cex=0.6, col="black", bg=col_bou)
        #########################
        
        ## Reactivity pies
        #################    
        x1=127
        x2=240
        #abline(v=c(x1,x2)) 
        segments(x0=x1,x1=30, y0=0.01, y1=0.055)
        segments(x0=x1,x1=70, y0=0.02, y1=0.055)
        segments(x0=x2,x1=160, y0=0.028, y1=0.055)
        segments(x0=x2,x1=205, y0=0.024, y1=0.055)
        text(x=c(18,60,148,190),y=rep(0.07,4), pos=1, labels=c("B","O","B","O"))
        
        #### Model
        i=elegidos[1]
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          R0<-filerc$X3_c
          R0[R0<1e-8]<-NA
          X3<-colMeans(R0[147:153,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X3, time_clases, drop = FALSE)
          mediaX3<-sapply(groups,median, na.rm=T)
          R0<-filerc$X2_c
          R0[R0<1e-8]<-NA
          X2<-colMeans(R0[147:153,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X2, time_clases, drop = FALSE)
          mediaX2<-sapply(groups,median, na.rm=T)          
          R0<-filerc$X1_c
          R0[R0<1e-8]<-NA
          X1<-colMeans(R0[147:153,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X1, time_clases, drop = FALSE)
          mediaX1<-sapply(groups,median, na.rm=T)  
          totalX<-mediaX1+mediaX2+mediaX3
          #pie(c(mediaX1[100],mediaX2[100],mediaX3[100]), labels=c("X1", "X2", "X3"))
          par(mar=c(0,0,0,2))  
          pie(c(mediaX1[x1],mediaX2[x1],mediaX3[x1]), labels=c("", "", ""), main=" ", col=c("white", "bisque1", "bisque3"))
          pie(c(mediaX1[x2],mediaX2[x2],mediaX3[x2]), labels=c("", "", ""), main=" ", col=c("white", "bisque1", "bisque3"))

        i=elegidos[2]
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          R0<-filerc$X3_c
          R0[R0<1e-8]<-NA
          X3<-colMeans(R0[147:153,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X3, time_clases, drop = FALSE)
          mediaX3<-sapply(groups,median, na.rm=T)
          R0<-filerc$X2_c
          R0[R0<1e-8]<-NA
          X2<-colMeans(R0[147:153,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X2, time_clases, drop = FALSE)
          mediaX2<-sapply(groups,median, na.rm=T)          
          R0<-filerc$X1_c
          R0[R0<1e-8]<-NA
          X1<-colMeans(R0[147:153,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X1, time_clases, drop = FALSE)
          mediaX1<-sapply(groups,median, na.rm=T)  
          totalX<-mediaX1+mediaX2+mediaX3
          par(mar=c(0,2,0,0))
          pie(c(mediaX1[x1],mediaX2[x1],mediaX3[x1]), labels=c("", "", ""), main=" ", col=c("white", "bisque1", "bisque3"))
          pie(c(mediaX1[x2],mediaX2[x2],mediaX3[x2]), labels=c("", "", ""), main=" ", col=c("white", "bisque1", "bisque3"))
        ########################         
        
        
### aCDOM(450)(80-100m)
        par(mar=c(2,4,1,1))
        plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
             main="", ylab="", xlab="",
             xlim=c(1,365), xaxs="i", ylim=c(0,0.07), yaxs="i", mgp=c(2.5,1,0), xaxt="n") 
        mtext(2, at=0.045, outer=F, line=3.0, text=expression(paste(a[CDOM],"(450)",sep="")), cex=cexlabel-0.1, las=1)
        mtext(2, at=0.032, outer=F, line=4.0,   text=expression(m^-1),cex=cexlabel-0.1,las=1)
        
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        #text(x=c(360,360), y=c(0.060,0.045), pos=2,
        #     labels=c(expression(paste(a[CDOM],"(450)")),"(80-100m)"),
        #     cex=c(cexlegend,cexlegend+0.2), font=c(1,2), bty="n")
        text(x=c(360), y=c(0.060), pos=2, labels=c("(80-100 m)"),cex=c(cexlegend+0.2), font=c(2), bty="n")
        mtext(3,at=1,outer=F,line=0.5,text="(d)",cex=cexleter, font=2)
        
        #### Model
        #########            
        for (i in elegidos){
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          R0<-filerc$lightspectral_acdom450#+filerc$lightspectral_anap450
          adg<-colMeans(R0[138:142,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(adg, time_clases, drop = FALSE)
          media<-sapply(groups,median)
          points(x=c(1:365), y=media, type="p", col=colores[i], lwd=anchol, pch=19, cex=0.6)           
        }
        ########################    
        
        ### Boussole observations
        ##################    
        # Acdom
        p_dir <- paste(o_dir,"BOUSSOLE_DATA/",sep="")
        reflectancia<-read.csv(paste(p_dir,"BOUSSOLE_ACDOM.csv", sep=""), sep=",", header=T)
        #names(reflectancia)
        fechita<-paste(reflectancia$year,reflectancia$month,reflectancia$day, sep="-")
        horita<-paste(reflectancia$Time_CTD_start..UTC.,"00", sep=":")
        # range(horita)        # rrs ya esta filtrada entre 10:00 y 14:00
        fechas <- chron(dates=fechita,times=horita,format=c("y-m-d","h:m:s"), out.format=c("y-m-d","h:m:s")) 
        julianos<- julian(x=month(fechas), d=day(fechas), y=year(fechas), origin.=c(month = 1, day = 1, year = 2009))
        julianos<-yday(fechas)
        fechas2<-fechas#[33:length(fechas)]
        datos2<-rowMeans(reflectancia[,258:282])
        datos2[reflectancia$depth..m.<80]<-NA
        datos2[reflectancia$depth..m.>100]<-NA        
        fechas2<-fechas2[!is.na(datos2)]
        julianos<-julianos[!is.na(datos2)]
        datos2<-datos2[!is.na(datos2)]
        points(julianos, datos2, type="p", las=1, pch=21, cex=0.6, col="black", bg=col_bou)
        #########################
        
        ## Reactivity pies
        ##########           
        x1=40
        x2=260
        #abline(v=c(x1,x2))  
        segments(x0=x1,x1=70, y0=0.0205, y1=0.055)
        segments(x0=x1,x1=30, y0=0.0165, y1=0.055)
        segments(x0=x2,x1=160, y0=0.008,  y1=0.055)
        segments(x0=x2,x1=205, y0=0.019,  y1=0.055)    
        text(x=c(18,60,148,190),y=rep(0.07,4), pos=1, labels=c("B","O","B","O"))
          
        #### Model
        i=elegidos[1]
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          R0<-filerc$X3_c
          R0[R0<1e-8]<-NA
          X3<-colMeans(R0[138:142,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X3, time_clases, drop = FALSE)
          mediaX3<-sapply(groups,median, na.rm=T)
          R0<-filerc$X2_c
          R0[R0<1e-8]<-NA
          X2<-colMeans(R0[138:142,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X2, time_clases, drop = FALSE)
          mediaX2<-sapply(groups,median, na.rm=T)          
          R0<-filerc$X1_c
          R0[R0<1e-8]<-NA
          X1<-colMeans(R0[138:142,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X1, time_clases, drop = FALSE)
          mediaX1<-sapply(groups,median, na.rm=T)  
          totalX<-mediaX1+mediaX2+mediaX3
          par(mar=c(0,0,0,2))  
          pie(c(mediaX1[x1],mediaX2[x1],mediaX3[x1]), labels=c("", "", ""), main="", col=c("white", "bisque1", "bisque3"))
          pie(c(mediaX1[x2],mediaX2[x2],mediaX3[x2]), labels=c("", "", ""), main="", col=c("white", "bisque1", "bisque3"))
          
        i=elegidos[2]
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          R0<-filerc$X3_c
          R0[R0<1e-8]<-NA
          X3<-colMeans(R0[138:142,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X3, time_clases, drop = FALSE)
          mediaX3<-sapply(groups,median, na.rm=T)
          R0<-filerc$X2_c
          R0[R0<1e-8]<-NA
          X2<-colMeans(R0[138:142,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X2, time_clases, drop = FALSE)
          mediaX2<-sapply(groups,median, na.rm=T)          
          R0<-filerc$X1_c
          R0[R0<1e-8]<-NA
          X1<-colMeans(R0[138:142,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X1, time_clases, drop = FALSE)
          mediaX1<-sapply(groups,median, na.rm=T)  
          totalX<-mediaX1+mediaX2+mediaX3
          par(mar=c(0,2,0,0)) 
          pie(c(mediaX1[x1],mediaX2[x1],mediaX3[x1]), labels=c("", "", ""), main="", col=c("white", "bisque1", "bisque3"))
          pie(c(mediaX1[x2],mediaX2[x2],mediaX3[x2]), labels=c("", "", ""), main="", col=c("white", "bisque1", "bisque3"))
        ########################         
        
                
  
### aCDOM(450)(150-400m)
        par(mar=c(2,4,1,1))
        plot(x=1, y=1, type="n", col=colores[i], lwd=1, las=1, cex.lab=1.0, pch=19, cex=0.2,
             main="", ylab="", xlab="",
             xlim=c(1,365), xaxs="i", ylim=c(0,0.07), yaxs="i", mgp=c(2.5,1,0), xaxt="n") 
        mtext(2, at=0.045, outer=F, line=3.0, text=expression(paste(a[CDOM],"(450)",sep="")), cex=cexlabel-0.1, las=1)
        mtext(2, at=0.032, outer=F, line=4.0,   text=expression(m^-1),cex=cexlabel-0.1,las=1)
        
        axis(1, at=seq(15,362-14,by=30), labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), las=1, cex.axis=cexaxis)
        #text(x=c(360,360), y=c(0.060,0.045), pos=2,
        #     labels=c(expression(paste(a[CDOM],"(450)")),"(150-400m)"),
        #     cex=c(cexlegend,cexlegend+0.2), font=c(1,2), bty="n")
        text(x=c(360), y=c(0.060), pos=2, labels=c("(150-400 m)"),cex=c(cexlegend+0.2), font=c(2), bty="n")
        mtext(3,at=1,outer=F,line=0.5,text="(e)",cex=cexleter, font=2)
        
        #### Model
        #########                    
        for (i in elegidos){
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          R0<-filerc$lightspectral_acdom450#+filerc$lightspectral_anap450
          adg<-colMeans(R0[113:129,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(adg, time_clases, drop = FALSE)
          media<-sapply(groups,median)
          points(x=c(1:365), y=media, type="p", col=colores[i], lwd=anchol, pch=19, cex=0.6)             
        }
        #########################    

        ### Boussole observations
        #########################    
        # Acdom
        p_dir <- paste(o_dir,"BOUSSOLE_DATA/",sep="")
        reflectancia<-read.csv(paste(p_dir,"BOUSSOLE_ACDOM.csv", sep=""), sep=",", header=T)
        #names(reflectancia)
        fechita<-paste(reflectancia$year,reflectancia$month,reflectancia$day, sep="-")
        horita<-paste(reflectancia$Time_CTD_start..UTC.,"00", sep=":")
        # range(horita)        # rrs ya esta filtrada entre 10:00 y 14:00
        fechas <- chron(dates=fechita,times=horita,format=c("y-m-d","h:m:s"), out.format=c("y-m-d","h:m:s")) 
        julianos<- julian(x=month(fechas), d=day(fechas), y=year(fechas), origin.=c(month = 1, day = 1, year = 2009))
        julianos<-yday(fechas)
        etiquetas_julianas<- julian(x=1, d=1, y=seq(2009,2015,by=1), origin.=c(month = 1, day = 1, year = 2009))
        fechas2<-fechas#[33:length(fechas)]
        datos2<-rowMeans(reflectancia[,258:282])
        sort(unique(reflectancia$depth..m.))
        datos2[reflectancia$depth..m.<150]<-NA
        datos2[reflectancia$depth..m.>1000]<-NA        
        fechas2<-fechas2[!is.na(datos2)]
        julianos<-julianos[!is.na(datos2)]
        datos2<-datos2[!is.na(datos2)]
        points(julianos, datos2, type="p", las=1, pch=21, cex=0.6, col="black", bg=col_bou)
        #########################
        
        ### Reactivity pies
        #########  
        x1=120
        x2=120
        #abline(v=c(x1,x2))
        segments(x0=x1,x1=65, y0=0.018,  y1=0.055)
        segments(x0=x1,x1=32, y0=0.012,  y1=0.055)
        text(x=c(18,60),y=rep(0.07,4), pos=1, labels=c("B","O"))
        #segments(x0=x2,x1=160, y0=0.02,  y1=0.055)
        #segments(x0=x2,x1=200, y0=0.005, y1=0.055)
             
        #### Model
        i=elegidos[1]
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          R0<-filerc$X3_c
          R0[R0<1e-8]<-NA
          X3<-colMeans(R0[113:129,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X3, time_clases, drop = FALSE)
          mediaX3<-sapply(groups,median, na.rm=T)
          R0<-filerc$X2_c
          R0[R0<1e-8]<-NA
          X2<-colMeans(R0[113:129,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X2, time_clases, drop = FALSE)
          mediaX2<-sapply(groups,median, na.rm=T)          
          R0<-filerc$X1_c
          R0[R0<1e-8]<-NA
          X1<-colMeans(R0[113:129,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X1, time_clases, drop = FALSE)
          mediaX1<-sapply(groups,median, na.rm=T)  
          totalX<-mediaX1+mediaX2+mediaX3
          par(mar=c(0,0,0,2)) 
          pie(c(mediaX1[x1],mediaX2[x1],mediaX3[x1]), labels=c("", "", ""), main="", col=c("white", "bisque1", "bisque3"))
          #pie(c(mediaX1[x2],mediaX2[x2],mediaX3[x2]), labels=c(" ", " ", " "), main="", col=c("white", "bisque1", "bisque3"))
          plot(1,1,type="n", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
          
        i=elegidos[2]
          archivo<-paste(nombres[i],"/result.nc",sep="")
          filename <- paste(o_dir,carpetas[i], "/",archivo, sep="")
          filenc <- open.nc(filename)
          filerc <- read.nc(filenc)
          tiempo<-filerc$time/86400
          R0<-filerc$X3_c
          R0[R0<1e-8]<-NA
          X3<-colMeans(R0[113:129,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X3, time_clases, drop = FALSE)
          mediaX3<-sapply(groups,median, na.rm=T)
          R0<-filerc$X2_c
          R0[R0<1e-8]<-NA
          X2<-colMeans(R0[113:129,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X2, time_clases, drop = FALSE)
          mediaX2<-sapply(groups,median, na.rm=T)          
          R0<-filerc$X1_c
          R0[R0<1e-8]<-NA
          X1<-colMeans(R0[113:129,(365*2):max(tiempo)])
          time_clases<-cut(anitos,breaks=seq(729.5,1094.5,by=1))
          groups<-split(X1, time_clases, drop = FALSE)
          mediaX1<-sapply(groups,median, na.rm=T)  
          totalX<-mediaX1+mediaX2+mediaX3
          par(mar=c(0,2,0,0)) 
          pie(c(mediaX1[x1],mediaX2[x1],mediaX3[x1]), labels=c("", "", ""), main="", col=c("white", "bisque1", "bisque3"))
          #pie(c(mediaX1[x2],mediaX2[x2],mediaX3[x2]), labels=c(" ", " ", " "), main="", col=c("white", "bisque1", "bisque3"))
        ########################         
        
        dev.off()
        
