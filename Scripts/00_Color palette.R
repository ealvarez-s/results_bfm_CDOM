if (Sys.info()['sysname']=="Windows") {OS<-"C:/"} else {OS<-paste("/Users/",Sys.info()['user'],"/", sep="")}
# importing
library(RSQLite)
library(RNetCDF)
# dates
library(chron)
library(lubridate)
# strings and matrixes
library(abind)
library(stringr)
# interpolation and correlation
library(akima)
library(Hmisc)
# plots
library(plot3D)
library(plotrix)
# colors
library(viridisLite)
library(unikn)

## EDIT
wrkdir<-paste(OS,"GIT_locales/results_bfm_CDOM",sep="")
path_figures<-paste(wrkdir,"/Figures/",sep="")
o_dir<-paste(wrkdir,"/",sep="")


lambda<-c(250.0,325.0,350.0,375.0,400.0,425.0,450.0,475.0,500.0,525.0,550.0,575.0,600.0,625.0,650.0,675.0,700.0,725.0,775.0)
lambda_extremos<-sort(unique(c(lambda[-c(1,length(lambda))]+(diff(lambda)[-1]/2),
                               lambda[1]-((diff(lambda)[1]-diff(lambda)[2]/2)),
                               lambda[1]+((diff(lambda)[1]-diff(lambda)[2]/2)),
                               lambda[length(lambda)]+(diff(lambda)[length(diff(lambda))]/2))))
lambda_extremos2<-c(187.5 ,312.5 ,337.5 ,362.5 ,387.5 ,412.5 ,437.5 ,462.5 ,487.5 ,512.5,537.5 ,562.5 ,587.5 ,
                   612.5 ,637.5 ,662.5 ,687.5 ,712.5 ,750 ,800 ,900 ,1000 ,1100,1200 ,1300 ,1400 ,1500 ,1600 ,
                   1700 ,1800 ,2000 ,2400 ,3400,4000)

## Color palette

        ## Model runs
        #experimentos <- read.csv(paste(OS, o_dir,"run_log_ALL2.csv", sep=""), sep=",") 
        experimentos <- read.csv(paste(o_dir,"run_log_ALL3.csv", sep=""), sep=",")
        nombres<-experimentos$file 
        carpetas<-experimentos$folder
        colores<-rainbow(100)[round(runif(length(nombres), 1, 99),0)]
        
        ## LINES simulations
        col_sat<-"black"
        col_bou<-"grey70"
        elegidos<-c(7,9,10,12)
        # model runs
        colores[7]<-"cadetblue3"
        colores[9]<-"darkorange"
        colores[10]<-"purple2"
        colores[12]<-"magenta3"
        
        ## DEPTH profiles
        ## Temperature & Nutrients
        colores_tre<- cividis(100)
        ## Chlorophyla, DOC, & CDOM
        colores_chla<- viridis(n=100, alpha = 1)
        ## Optics
        colores_iops<- mako(100)
        
        ## LINES state var
        #### Phyto chl    
        mycol_phyto<-usecol(pal_seegruen,n=4,alpha=0.3)
        linea_phyto<-"seagreen4"
        #### acdom 450nm
        mycol_cdom<-usecol(pal_peach,n=4,alpha=0.3)
        linea_cdom<-"sienna3"
        #### Bact
        mycol_bact<-usecol(pal_karpfenblau,n=4,alpha=0.3)
        linea_bact<-"steelblue4"
        #### DOC
        mycol_doc<-usecol(pal_grau,n=4,alpha=0.3)
        linea_doc<-"grey20"
        # anap
        mycol_grey<-usecol(pal_grau,n=4,alpha=0.3)
        linea_grey<-"grey20"
        
        colores_meses<-rep(rep(rainbow(12), each=30),times=4)[-1]
        colores_estaciones<-rep(rep(c("steelblue3","yellow3","orchid3","coral3"), each=90),times=4)[-1]
        
        
#### Figure color palette    
#         png(file=paste(path_figures,"Figure0_color_palette.png",sep=""), width = 800, height = 800, pointsize=24) 
#         layout(matrix(c(1,1:10,10,11,11,11),ncol=3,byrow=F))
#         par(mar=c(2,2,4,2))   
#         layout.show(n=11)
#         
#         # Lines simulations
#         plot(1,1,type="n",ylim=c(0,7), xaxt="n", yaxt="n", bty="n")
#         colores[elegidos]
#         abline(h=c(6:1), col=c(col_sat,col_bou,colores[elegidos]), lwd=5)
#         text(x=1,y=c(6:1), pos=3, labels=c("Sat","Boussole", "Optimized", "Nutrients", "Constant","Bleaching"), font=3)
#         
#         # Profiles
#         mat <- matrix(c(1:100), nrow=100)
#         image(mat, col=colores_tre,  las=1, xaxt="n", yaxt="n", cex.main=1.2, main="Temperature & Nutrients")
#         image(mat, col=colores_chla, las=1, xaxt="n", yaxt="n", cex.main=1.2, main="Chl, DOC & CDOM")
#         #mat <- matrix(c(1:85), nrow=85)
#         #colores_iops<- rainbow(100)[85:1]
#         image(mat, col=colores_iops, las=1, xaxt="n", yaxt="n", cex.main=1.2, main="IOPs")    
#         
#         # Lines state vars
#         mat <- matrix(c(1:4), nrow=4)
#         image(z=mat, las=1, xaxt="n", yaxt="n", cex.main=1.2, col=mycol_phyto, main="PChl & Aph", bty="n")
#         abline(h=0, col=linea_phyto, lwd=5)
#         
#         #### acdom 450nm
#         mat <- matrix(c(1:4), nrow=4)
#         image(z=mat, las=1, xaxt="n", yaxt="n", cex.main=1.2, col=mycol_cdom, main="Acdom", bty="n")
#         abline(h=0, col=linea_cdom, lwd=5)
#         #### Bact
#         mat <- matrix(c(1:4), nrow=4)
#         image(z=mat, las=1, xaxt="n", yaxt="n", cex.main=1.2, col=mycol_bact, main="Bact", bty="n")
#         abline(h=0, col=linea_bact, lwd=5)
#         #### DOC
#         mat <- matrix(c(1:4), nrow=4)
#         image(z=mat, las=1, xaxt="n", yaxt="n", cex.main=1.2, col=mycol_doc, main="DOC", bty="n")
#         abline(h=0, col=linea_doc, lwd=5)
#         # anap
#         mat <- matrix(c(1:4), nrow=4)
#         image(z=mat, las=1, xaxt="n", yaxt="n", cex.main=1.2, col=mycol_grey, main="Anap & mixed", bty="n")
#         abline(h=0, col=linea_grey, lwd=5)
#         
#         plot(1,1,type="n",ylim=c(0,8), xaxt="n", yaxt="n", bty="n")
#         legend(x="topleft",legend=c("Winter(J-M)","Spring(A-J)","Summer(J-S)","Fall(O-D)"),
#                col=unique(colores_estaciones), bty="n", pch=19, cex=1.4)   
#         dev.off()

        
## Various functions
        
        flip<-function(vector)
        {
          vector<-vector[length(vector):1];
          return(vector);
        }
        
        fliplr<-function(matrix)
        {
          matrix<-matrix[nrow(matrix):1,];
          return(matrix);
        }
        flipud<-function(matrix)
        {
          matrix<-matrix[,ncol(matrix):1];
          return(matrix);
        }
        
        
        jet<-function(n)
        {
          m<-n
          n <- max(c(round(m/4),1));
          x = (1:n)/n;
          y = ((n/2):n)/n;
          e = rep(1,times=length(x));
          r = c(0*y, 0*e, x, e, flip(y));
          g = c(0*y, x, e, flip(x), 0*y);
          b = c(y, e, flip(x), 0*e, 0*y);
          J = cbind(r,g,b);
          while (nrow(J) > m)
          {
            J <- J[-c(1),];
            if(nrow(J) > m) J <- J[-c(nrow(J)),];
          }
          newpalette<-rgb(J[,1],J[,2],J[,3])
          return(newpalette);
        }
        
        find_eudepth <- function(x, prof, dif=0.01){
          l_sur <- x[1]
          light <- x[-1]
          light[light<=1e-08] <- 0
          light[light==0] <- NA
          depth <- prof[!is.na(light)]
          light <- light[!is.na(light)]
          if (length(light)>=2){
            #plot(light, depth, las=1, type="b", xlim=c(0,40), ylim=c(-300,0))
            #points(l_sur,0, las=1, pch=19) 
            valor <- (light/l_sur)-0.01
            index <- which(abs(valor)==min(abs(valor)))
            #points(light[index],  depth[index], pch=19, col="red")
            minimos <- sort(abs(valor),na.last=NA)[1:4]
            cuales <- match(minimos,abs(valor))
            cuales <- cuales[!is.na(cuales)]
            #points(light[cuales], depth[cuales], pch=21, col="green", lwd=2)
            valorcitos <- (light/l_sur)[cuales]
            # plot((light/l_sur),depth) 
            # points(valorcitos,depth[cuales], pch=19)
            if (sum(!is.na(valorcitos))>=2){
              res <- approx(x=valorcitos, y=depth[cuales], xout=0.01, rule=2)
              res <- res$y
            } else if (length(index)==1){res <- depth[index]}else{res<-NA}
          } else{res<-NA}
          # abline(h=res)
          return(res)}
