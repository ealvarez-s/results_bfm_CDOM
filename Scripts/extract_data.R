cabezas<-which(datos[,3]==2)
profundidades<-c(seq(0,-95,by=-5),seq(-100,-2380, by=-10),-2383,seq(-2390,-3000, by=-10))

perfiles<-matrix(NA,nrow=length(cabezas),ncol=length(profundidades)+5)
colnames(perfiles)<-c("YYYY","MM","DD","hora", profundidades, "-12000")
for (i in cabezas){
  header<-datos[i,]
  ano<-as.numeric(substr(header[1],1,4))
  mes<-as.numeric(substr(header[1],6,7))
  dia<-as.numeric(substr(header[1],9,10))
  hora<-as.numeric(substr(header[1],12,13))+(((as.numeric(substr(header[1],15,16)))+((as.numeric(substr(header[1],18,19)))/60))/60)
  press <-as.numeric(datos[(i+1):(i+as.numeric(datos[i,2])),1])
  dato  <-as.numeric(datos[(i+1):(i+as.numeric(datos[i,2])),2])
  press_final<-(-12000)
  dato_final<-as.numeric(datos[(i+as.numeric(datos[i,2])),2])
  posicion<-match(press,profundidades)
  cual<-which(cabezas==i)
  perfiles[cual,posicion+4]<-dato
  perfiles[cual,ncol(perfiles)]<-dato_final
  perfiles[cual,(1:4)]<-c(ano,mes,dia,hora)
}

fullNA<-function(x){sum(is.na(x))==length(x)}    
eliminables<-apply(perfiles, MARGIN=2, FUN=fullNA)
perfiles_final<-perfiles[,which(eliminables==F)]
if (compute_depth==T) {profundidades_final=as.numeric(colnames(perfiles_final)[5:ncol(perfiles_final)])}

## GUARDAR de nuevo ordenado

## Colocar en tabla (x,y,z)
tabla<-matrix(NA,nrow=0,ncol=6)
colnames(tabla)<-c("YYYY","MM","DD","hora","depth","var")

for (i in cabezas){
  header<-datos[i,]
  ano<-as.numeric(substr(header[1],1,4))
  mes<-as.numeric(substr(header[1],6,7))
  dia<-as.numeric(substr(header[1],9,10))
  hora<-as.numeric(substr(header[1],12,13))+(((as.numeric(substr(header[1],15,16)))+((as.numeric(substr(header[1],18,19)))/60))/60)
  press <-as.numeric(datos[(i+1):(i+as.numeric(datos[i,2])),1])
  dato  <-as.numeric(datos[(i+1):(i+as.numeric(datos[i,2])),2])
  press_final<-(-12000)
  dato_final<-as.numeric(datos[(i+as.numeric(datos[i,2])),2])
  tablita<-cbind(rep(ano,length(dato)+1),rep(mes,length(dato)+1),rep(dia,length(dato)+1),rep(hora,length(dato)+1),
                 c(press,press_final),c(dato,dato_final))
  tabla<-rbind(tabla,tablita)
}

#################
###  Grid 1D  ###
#################
clases_pro <-sort(unique(c(0,-2.5,profundidades_final[-1]+diff(profundidades_final)/2)),decreasing = T)
#length(clases_pro)
#length(profundidades_final)
clases_time <-seq(0.5,365.5, by=10)
month<-seq(5,360,by=10)
length(clases_time)
length(month)

fechas<-paste(tabla[,1],tabla[,2],tabla[,3], sep="-")
times<-yday(fechas)
depth=-tabla[,5]
valor1=tabla[,6]
resul<-interp(x=times, y=depth, z=valor1, xo=seq(0,365,length=resolx),yo=seq(0,400,length=resoly),
       linear = TRUE, extrap=TRUE, duplicate = "mean", dupfun = NULL, nx = resolx, ny = resoly,
       jitter = 10^-12, jitter.iter = 6, jitter.random = FALSE)
climat <- resul$z 
month<-resul$x
prof<-resul$y
dim(climat)
dimnames(climat)<-list(t=month,z=prof)
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
#   
#   valor <- unlist(res$valor1)
#   for (i in 1:nrow(res)){
#     climat[which(niveles1==res[i,1]),
#            which(niveles2==res[i,2])] <- valor[i]} # end loop i
# }
# 
# prof<-(-profundidades_final[length(profundidades_final):1])
# dimnames(climat)<-list(z=prof,t=month)