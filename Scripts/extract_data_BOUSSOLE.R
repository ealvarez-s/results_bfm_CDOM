##############################
###  Grid 1D with Boussole ###
##############################
    #colnames(tabla_total)
    #dim(tabla_total)
    lati<-tabla_total$latitude   # plot(long, lati)
    long<-tabla_total$longitude
    depth<-(tabla_total$Depth)
    datetime <- as.Date(tabla_total$fecha3, format="%Y-%m-%d")  
    #range(datetime, na.rm=TRUE)
    YEAR  <- as.numeric(format(datetime,"%Y"))   #range(YEAR, na.rm=TRUE)
    times <- as.numeric(format(datetime,"%m"))   # sort(unique(times))
    DAY   <- as.numeric(format(datetime,"%d"))   # sort(unique(DAY))
    JULIAN <- julian(datetime, origin = as.Date("2000-01-01"))
    #range(JULIAN, na.rm=TRUE)
    
    ### Definir grid depth x tiempo igual al modelo
    profundidades_final<-c(0,-5,-10,-20,-30,-40,-45,-50,-60,-70,-80,-90,-100,-110,-120,-130,-140,-150,-160,
                  -180,-200,-250,-300,-350,-400,-450,-500,-600,-700,-800,-900,-1000,-1100,-1200,-1240,-1250,-1300,
                  -1400,-1500,-1600,-1700,-1800,-1900,-2000,-2010,-2100,-2300,-2383,-12000)
    clases_pro <-sort(unique(c(0,-2.5,profundidades_final[-1]+diff(profundidades_final)/2)),decreasing = T)
    clases_time <-seq(0.5,365.5, by=10)
    #month<-seq(5,360,by=10)
    #length(clases_time)
    #length(month)    

    fechas<-datetime
    times<-yday(fechas)
    
    resul<-interp(x=times, y=depth, z=valor1, xo=seq(0,365,length=resolx),yo=seq(0,300,length=resoly),
                  linear = TRUE, extrap=TRUE, duplicate = "mean", dupfun = NULL, nx = resolx, ny = resoly,
                  jitter = 10^-12, jitter.iter = 6, jitter.random = FALSE)
    climat <- resul$z 
    month <-resul$x
    prof  <-resul$y
    #dimnames(climat)<- list(z=prof, t=month)
    #dimnames(climat)
    #length(month)
    #length(prof)
    #image2D(x=month, y=prof, z=climat, ylim=c(300,0), zlim=c(0,10), las=1, ylab="depth", colkey=F,cex.lab=1.2)
    
    
    
    
    # # Create grid levels
    # indice1 <- c(depth)  # range(indice1, na.rm=TRUE)
    # indice2 <- c(times)  # range(indice2, na.rm=TRUE)
    # factor1 <-cut(indice1, breaks=clases_pro,   include.lowest = TRUE,right = TRUE)
    # factor2 <-cut(indice2, breaks=clases_time,  include.lowest = FALSE,right = TRUE)
    # niveles1<-levels(factor1)
    # niveles2<-levels(factor2)           
    # 
    # ### Fill grid vertical 
    # climat <- array(NA, dim=c(length(niveles1),length(niveles2)),
    #                 dimnames=list(z=niveles1,t=niveles2))
    # experiment <- data.frame(valor1, factor1, factor2)
    # if(sum(complete.cases(experiment))>0){
    #   res <- aggregate(valor1~factor1+factor2,experiment,
    #                    mean, na.rm=TRUE, drop=TRUE, simplify=FALSE,
    #                    na.action = na.omit)
    #   valor <- unlist(res$valor1)
    #   for (i in 1:nrow(res)){
    #     climat[which(niveles1==res[i,1]),
    #            which(niveles2==res[i,2])] <- valor[i]} # end loop i
    # }
    # 
    # prof<-(-profundidades_final[length(profundidades_final):1])
    # dimnames(climat)<-list(z=prof,t=month)    