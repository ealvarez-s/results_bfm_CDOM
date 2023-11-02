if (Sys.info()['sysname']=="Windows") {OS<-"C:/"} else {OS<-paste("/Users/",Sys.info()['user'],"/", sep="")}

### Figures Auto-calibration ###

library(RSQLite)
library(stringr)
source(paste(o_dir,"Scripts/00_Color palette.R", sep=""))

###########################
#### DE30 bX3, IX3, a, phi, theta & fX2, fX1, fX3, aparcoeff, bparcoeff vs. fChla, aph, acdom & bbp
###########################
### Re-launched for Review1
    
    ## connect to db: leg1
    ######################
    con <- dbConnect(drv=RSQLite::SQLite(),
                     dbname=paste(o_dir,"CALIBRATION_R1/boussole_annual_DE30_OPTM5_1bis.db", sep=""))
    ## list all tables
    tables <- dbListTables(con)
    ## exclude sqlite_sequence (contains table information)
    tables <- tables[tables != "sqlite_sequence"]
    lDataFrames <- vector("list", length=length(tables))
    ## create a data.frame for each table
    for (i in seq(along=tables)) {
      lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep="")) }
    dbDisconnect(con)
    runs<-lDataFrames[[2]]
    results<-lDataFrames[[1]]
    
    ### Read xml info
    #################  
    xml<-strsplit(runs$description, split="\n")
    xml<-c(unlist(xml[[1]]))
    
    ### Block parameters
    cuales<-str_which(xml, pattern="  <parameter file" )
    #str_detect(xml[cuales], pattern="<parameter file" , negate = FALSE)
    u1<-str_locate(xml[cuales], pattern="\"instances/")[,2]+1
    u2<-str_locate(xml[cuales], pattern="/parameters/")[,1]-1
    u3<-str_locate(xml[cuales], pattern="/parameters/")[,2]+1
    u4<-str_locate(xml[cuales], pattern="\" minimum=")[,1]-1
    instance   <-substring(xml[cuales],u1,u2)     
    parameter  <-substring(xml[cuales],u3,u4)
    u5<-str_locate(xml[cuales], pattern="minimum=\"")[,2]+1
    u6<-str_locate(xml[cuales], pattern="\" maximum=\"")[,1]-1
    u7<-str_locate(xml[cuales], pattern="\" maximum=\"")[,2]+1
    u8<-str_locate(xml[cuales], pattern="\" />")[,1]-1
    minimum <-as.numeric(substring(xml[cuales],u5,u6))    
    maximum <-as.numeric(substring(xml[cuales],u7,u8))
    
    ## Block observations
    cuales<-str_which(xml, pattern="   <variable source" )
    xml[cuales]
    #str_detect(xml[cuales], pattern=" <variable source" , negate = FALSE)
    observations <-c("PChla","NChla","MChla","aph450","anap450","acdom450","Scdom","bbp450")
    ######################
    
    nparams=length(parameter)
    NP<-48*ceiling((nparams*10)/48)
    maxiter=34
    maxiter*NP
    length(results$run) 
    runs1<-results$run
    lnls1<-results$lnlikelihood
    valores1<-strsplit(results$parameters, split=";")
    length(runs1)
    length(valores1)
    extra_outputs1<-results$extra_outputs
    length(extra_outputs1)
    #####################
    
    # last generation is not complete x 33
    runs1<-runs1[1:((maxiter-1)*NP)]
    lnls1<-lnls1[1:((maxiter-1)*NP)]
    valores1<-valores1[1:((maxiter-1)*NP)]
    extra_outputs1<-extra_outputs1[1:((maxiter-1)*NP)]
    length(runs1)
    length(valores1)
    length(extra_outputs1) 
    ######################
    
    # param1<-rep(NA, length=length(runs1))
    # fin<-NP*c(1:maxiter)
    # inicio<-c(1,fin+1)
    # 
    # ### Ranges for restarting calibration
    # #####################################
    # NOMBRE<-rep(NA,length=length(parameter))
    # FINALL<-rep(NA,length=length(parameter))
    # MAXIMA<-rep(NA,length=length(parameter))
    # MINIMA<-rep(NA,length=length(parameter))
    # 
    # for (k in c(1:nparams)){
    #   for (i in c(1:length(runs1))){
    #     param1[i]<-as.numeric(as.character(valores1[[i]][k]))}
    #   x_id<-c(1:length(runs1))
    #   x_gen<-rep(c(1:maxiter),each=NP)[1:length(runs1)]
    #   param_gen<-rep(NA, length=maxiter)
    #   param_max<-rep(NA, length=maxiter)
    #   param_min<-rep(NA, length=maxiter)
    #   for (i in c(1:maxiter)){
    #     param_gen[i]<-mean(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     #param_max[i]<-max(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     #param_min[i]<-min(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     param_max[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.95, na.rm=T)
    #     param_min[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.05, na.rm=T)                
    #     }
    #   NOMBRE[k]<-paste(instance[k],parameter[k],sep="/")
    #   FINALL[k]<-param_gen[length(param_gen[!is.nan(param_gen)==T])]
    #   MAXIMA[k]<-param_max[length(param_gen[!is.nan(param_gen)==T])]
    #   MINIMA[k]<-param_min[length(param_gen[!is.nan(param_gen)==T])]
    # }
    # 
    # ######################################
    # data.frame(NOMBRE,MINIMA,FINALL,MAXIMA)


    
    
    
    
    
    ## connect to db: leg2
    ######################
    con <- dbConnect(drv=RSQLite::SQLite(),
                     dbname=paste(o_dir,"CALIBRATION_R1/boussole_annual_DE30_OPTM5_2bis2.db", sep=""))
    ## list all tables
    tables <- dbListTables(con)
    ## exclude sqlite_sequence (contains table information)
    tables <- tables[tables != "sqlite_sequence"]
    lDataFrames <- vector("list", length=length(tables))
    ## create a data.frame for each table
    for (i in seq(along=tables)) {
      lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep="")) }
    dbDisconnect(con)
    runs<-lDataFrames[[2]]
    results<-lDataFrames[[1]]
    
    NP<-48*ceiling((nparams*10)/48)
    maxiter=33. #33
    maxiter*NP
    length(results$run)
    runs2<-results$run
    lnls2<-results$lnlikelihood
    valores2<-strsplit(results$parameters, split=";")
    length(runs2)
    length(valores2)
    extra_outputs2<-results$extra_outputs
    length(extra_outputs2)    
    
    # # last generation is complete x 33
    # runs2<-runs2[1:((maxiter-1)*NP)]
    # lnls2<-lnls2[1:((maxiter-1)*NP)]
    # valores2<-valores2[1:((maxiter-1)*NP)]
    # extra_outputs2<-extra_outputs2[1:((maxiter-1)*NP)]
    # length(runs2)
    # length(valores2)
    # length(extra_outputs2) 
    ######################
    
    # param1<-rep(NA, length=length(runs2))
    # fin<-NP*c(1:maxiter)
    # inicio<-c(1,fin+1) 
    # 
    # ### Ranges for restarting calibration
    # #####################################
    # NOMBRE<-rep(NA,length=length(parameter))
    # FINALL<-rep(NA,length=length(parameter))
    # MAXIMA<-rep(NA,length=length(parameter))
    # MINIMA<-rep(NA,length=length(parameter))
    # 
    # for (k in c(1:nparams)){
    #   for (i in c(1:length(runs2))){
    #     param1[i]<-as.numeric(as.character(valores2[[i]][k]))}
    #   x_id<-c(1:length(runs2))
    #   x_gen<-rep(c(1:maxiter),each=NP)[1:length(runs2)]
    #   param_gen<-rep(NA, length=maxiter)
    #   param_max<-rep(NA, length=maxiter)
    #   param_min<-rep(NA, length=maxiter)
    #   for (i in c(1:maxiter)){
    #     param_gen[i]<-mean(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     #param_max[i]<-max(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     #param_min[i]<-min(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     param_max[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.95, na.rm=T)
    #     param_min[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.05, na.rm=T)    
    #     }
    #   NOMBRE[k]<-paste(instance[k],parameter[k],sep="/")
    #   FINALL[k]<-param_gen[length(param_gen[!is.nan(param_gen)==T])]
    #   MAXIMA[k]<-param_max[length(param_gen[!is.nan(param_gen)==T])]
    #   MINIMA[k]<-param_min[length(param_gen[!is.nan(param_gen)==T])]
    # }
    # 
    # ######################################
    # data.frame(NOMBRE,MINIMA,FINALL,MAXIMA)
    
    
 
    
    ## connect to db: leg3
    ######################
    con <- dbConnect(drv=RSQLite::SQLite(),
                     dbname=paste(o_dir,"CALIBRATION_R1/boussole_annual_DE30_OPTM5_3bis2.db", sep=""))
    ## list all tables
    tables <- dbListTables(con)
    ## exclude sqlite_sequence (contains table information)
    tables <- tables[tables != "sqlite_sequence"]
    lDataFrames <- vector("list", length=length(tables))
    ## create a data.frame for each table
    for (i in seq(along=tables)) {
      lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep="")) }
    dbDisconnect(con)
    runs<-lDataFrames[[2]]
    results<-lDataFrames[[1]]
    
    NP<-48*ceiling((nparams*10)/48)
    maxiter=32
    maxiter*NP
    length(results$run)
    runs3<-results$run
    lnls3<-results$lnlikelihood
    valores3<-strsplit(results$parameters, split=";")
    length(runs3)
    length(valores3)
    extra_outputs3<-results$extra_outputs
    length(extra_outputs3)      
    ######################
    
    # last generation is complete x 32
    # runs3<-runs3[1:((maxiter-1)*NP)]
    # lnls3<-lnls3[1:((maxiter-1)*NP)]
    # valores3<-valores3[1:((maxiter-1)*NP)]
    # extra_outputs3<-extra_outputs3[1:((maxiter-1)*NP)]
    # length(runs3)
    # length(valores3)
    # length(extra_outputs3) 
    ######################
    
    
    #param1<-rep(NA, length=length(runs3))
    #fin<-NP*c(1:maxiter)
    #inicio<-c(1,fin+1) 
    
    
    # ### Ranges for restarting calibration
    # #####################################
    # NOMBRE<-rep(NA,length=length(parameter))
    # FINALL<-rep(NA,length=length(parameter))
    # MAXIMA<-rep(NA,length=length(parameter))
    # MINIMA<-rep(NA,length=length(parameter))
    # 
    # for (k in c(1:nparams)){
    #   for (i in c(1:length(runs3))){
    #     param1[i]<-as.numeric(as.character(valores3[[i]][k]))}
    #   x_id<-c(1:length(runs3))
    #   x_gen<-rep(c(1:maxiter),each=NP)[1:length(runs3)]
    #   param_gen<-rep(NA, length=maxiter)
    #   param_max<-rep(NA, length=maxiter)
    #   param_min<-rep(NA, length=maxiter)
    #   for (i in c(1:maxiter)){
    #     param_gen[i]<-mean(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     #param_max[i]<-max(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     #param_min[i]<-min(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     param_max[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.95, na.rm=T)
    #     param_min[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.05, na.rm=T)
    #     }
    #   NOMBRE[k]<-paste(instance[k],parameter[k],sep="/")
    #   FINALL[k]<-param_gen[length(param_gen[!is.nan(param_gen)==T])]
    #   MAXIMA[k]<-param_max[length(param_gen[!is.nan(param_gen)==T])]
    #   MINIMA[k]<-param_min[length(param_gen[!is.nan(param_gen)==T])]
    # }
    ######################################
    #data.frame(NOMBRE,MINIMA,FINALL,MAXIMA)
    
    
    
 
    
    ## connect to db: leg4
    ######################
    con <- dbConnect(drv=RSQLite::SQLite(),
                     dbname=paste(o_dir,"CALIBRATION_R1/boussole_annual_DE30_OPTM5_4bis2.db", sep=""))
    ## list all tables
    tables <- dbListTables(con)
    ## exclude sqlite_sequence (contains table information)
    tables <- tables[tables != "sqlite_sequence"]
    lDataFrames <- vector("list", length=length(tables))
    ## create a data.frame for each table
    for (i in seq(along=tables)) {
      lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep="")) }
    dbDisconnect(con)
    runs<-lDataFrames[[2]]
    results<-lDataFrames[[1]]
    
    NP<-48*ceiling((nparams*10)/48)
    maxiter=34
    maxiter*NP
    length(results$run) 
    runs4<-results$run
    lnls4<-results$lnlikelihood
    valores4<-strsplit(results$parameters, split=";")
    length(runs4)
    length(valores4)
    extra_outputs4<-results$extra_outputs
    length(extra_outputs4)      
    ######################
    
    # last generation is complete x 34
    #runs4<-runs4[1:((maxiter-1)*NP)]
    #lnls4<-lnls4[1:((maxiter-1)*NP)]
    #valores4<-valores4[1:((maxiter-1)*NP)]
    #extra_outputs4<-extra_outputs4[1:((maxiter-1)*NP)]
    #length(runs4)
    #length(valores4)
    #length(extra_outputs4) 
    ######################    
    
    
    #param1<-rep(NA, length=length(runs4))
    #fin<-NP*c(1:maxiter)
    #inicio<-c(1,fin+1) 
    
 
    # ### Ranges for restarting calibration
    # #####################################  
    # NOMBRE<-rep(NA,length=length(parameter))
    # FINALL<-rep(NA,length=length(parameter))
    # MAXIMA<-rep(NA,length=length(parameter))
    # MINIMA<-rep(NA,length=length(parameter))
    # 
    # for (k in c(1:nparams)){
    #   for (i in c(1:length(runs4))){
    #     param1[i]<-as.numeric(as.character(valores4[[i]][k]))}
    #   x_id<-c(1:length(runs4))
    #   x_gen<-rep(c(1:maxiter),each=NP)[1:length(runs4)]
    #   param_gen<-rep(NA, length=maxiter)
    #   param_max<-rep(NA, length=maxiter)
    #   param_min<-rep(NA, length=maxiter)
    #   for (i in c(1:maxiter)){
    #     param_gen[i]<-mean(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     #param_max[i]<-max(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     #param_min[i]<-min(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     param_max[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.95, na.rm=T)
    #     param_min[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.05, na.rm=T)        
    #     }
    #   NOMBRE[k]<-paste(instance[k],parameter[k],sep="/")
    #   FINALL[k]<-param_gen[length(param_gen[!is.nan(param_gen)==T])]
    #   MAXIMA[k]<-param_max[length(param_gen[!is.nan(param_gen)==T])]
    #   MINIMA[k]<-param_min[length(param_gen[!is.nan(param_gen)==T])]
    # }
    ######################################
    #data.frame(NOMBRE,MINIMA,FINALL,MAXIMA)
    
    
  
    
    ## connect to db: leg5
    ######################
    con <- dbConnect(drv=RSQLite::SQLite(),
                     dbname=paste(o_dir,"CALIBRATION_R1/boussole_annual_DE30_OPTM5_5bis2.db", sep=""))
    ## list all tables
    tables <- dbListTables(con)
    ## exclude sqlite_sequence (contains table information)
    tables <- tables[tables != "sqlite_sequence"]
    lDataFrames <- vector("list", length=length(tables))
    ## create a data.frame for each table
    for (i in seq(along=tables)) {
      lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep="")) }
    dbDisconnect(con)
    runs<-lDataFrames[[2]]
    results<-lDataFrames[[1]]
    
    NP<-48*ceiling((nparams*10)/48)
    maxiter=33
    maxiter*NP
    length(results$run) # last generation is complete
    runs5<-results$run
    lnls5<-results$lnlikelihood
    valores5<-strsplit(results$parameters, split=";")
    length(runs5)
    length(valores5)
    extra_outputs5<-results$extra_outputs
    length(extra_outputs5)          
    ######################    
    
    #param1<-rep(NA, length=length(runs4))
    #fin<-NP*c(1:maxiter)
    #inicio<-c(1,fin+1) 
    
    # ### Ranges for restarting calibration
    # #####################################  
    # NOMBRE<-rep(NA,length=length(parameter))
    # FINALL<-rep(NA,length=length(parameter))
    # MAXIMA<-rep(NA,length=length(parameter))
    # MINIMA<-rep(NA,length=length(parameter))
    # 
    # for (k in c(1:nparams)){
    #   for (i in c(1:length(runs5))){
    #     param1[i]<-as.numeric(as.character(valores5[[i]][k]))}
    #   x_id<-c(1:length(runs5))  
    #   x_gen<-rep(c(1:maxiter),each=NP)[1:length(runs5)]  
    #   param_gen<-rep(NA, length=maxiter)
    #   param_max<-rep(NA, length=maxiter)
    #   param_min<-rep(NA, length=maxiter)
    #   for (i in c(1:maxiter)){
    #     param_gen[i]<-mean(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     param_max[i]<-max(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     param_min[i]<-min(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
    #     param_max[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.95, na.rm=T)
    #     param_min[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.05, na.rm=T)    
    #     } 
    #   NOMBRE[k]<-paste(instance[k],parameter[k],sep="/")
    #   FINALL[k]<-param_gen[length(param_gen[!is.nan(param_gen)==T])]
    #   MAXIMA[k]<-param_max[length(param_gen[!is.nan(param_gen)==T])]
    #   MINIMA[k]<-param_min[length(param_gen[!is.nan(param_gen)==T])]
    # }
    # 
    # ######################################
    # data.frame(NOMBRE,MINIMA,FINALL,MAXIMA)
    

    ## All together
    maxiter=33+33+32+34+33
    fin<-NP*c(1:maxiter)
    #fin[length(fin)]<-length(runs1)+length(runs2)
    inicio<-c(1,fin+1)
    param1<-rep(NA, length=length(runs1)+length(runs2)+length(runs3)+length(runs4)+length(runs5))
    #length(param1)
    
    lnls<-c(lnls1,lnls2,lnls3,lnls4,lnls5)
    #length(lnls)
    valores<-append(valores1,valores2)
    valores<-append(valores,valores3)
    valores<-append(valores,valores4)
    valores<-append(valores,valores5)

    extra_outputs<-append(extra_outputs1,extra_outputs2)
    extra_outputs<-append(extra_outputs,extra_outputs3)
    extra_outputs<-append(extra_outputs,extra_outputs4)
    extra_outputs<-append(extra_outputs,extra_outputs5)
   
    total_runs<-length(runs1)+length(runs2)+length(runs3)+length(runs4)+length(runs5)
    
 
    save(maxiter,inicio,fin,param1,lnls,valores,extra_outputs,total_runs, file=paste0(o_dir,"CALIBRATION_R1/DE30_OPTM5_2.RData"))
    
    
    
    ### Parameters ###
    ################## 
    parameter_name<-c(expression(b[X(3)]), expression(I[X(3)]),
                      expression(S["X(1)"]^"350-500"),expression(S["X(2)"]^"350-500"),expression(S["X(3)"]^"350-500"),
                      # eps
                      expression(bar("a*")["P(1)"]),expression(bar("a*")["P(2)"]),expression(bar("a*")["P(3)"]),expression(bar("a*")["P(4)"]),
                      # theta max
                      expression(theta["chl"]^"0(1)"),expression(theta["chl"]^"0(2)"),expression(theta["chl"]^"0(3)"),expression(theta["chl"]^"0(4)"),
                      # QY max
                      expression(phi["c"]^"0(1)"),expression(phi["c"]^"0(2)"),expression(phi["c"]^"0(3)"),expression(phi["c"]^"0(4)"),
                      # bps
                      expression(bar("b*")["P(1)"]),expression(bar("b*")["P(2)"]),expression(bar("b*")["P(3)"]),expression(bar("b*")["P(4)"]),
                      # fX2
                      expression(f["P(1)"]^"X2"),expression(f["P(2)"]^"X2"),expression(f["P(3)"]^"X2"),expression(f["P(4)"]^"X2"),
                      # fX3, fX1
                      expression(f["B"]^"X3"),expression(f["Z(5)"]^"X1"),expression(f["Z(6)"]^"X1"),
                      # aNAP, bNAP
                      expression(a["R"]^"440"),expression(b["R"]^"550"))
    parameter_color<-c(rep(linea_cdom,5),rep(linea_phyto,16),rep(linea_cdom,4),rep(linea_cdom,3),rep(linea_grey,2))                  
    parameter_units<-c(expression(paste(d^-1)),
                       expression(paste(mu, E^-1," ",m^-2," ",s^-1,sep="")),
                       rep(expression(nm^-1), 3), #Scdom
                       rep(expression(paste(m^2," ", mgChl^-1, sep="")),4), #eps
                       rep(expression(paste(mgChl," ", mgC^-1, sep="")),4), #theta
                       rep(expression(paste(mgC," ", mu, E^-1, sep="")),4), #qy
                       rep(expression(paste(m^2," ", mgC^-1,   sep="")),4), #bps
                       rep(expression(paste("-",sep="")),7),
                       rep(expression(paste(m^2," ", mgC^-1,   sep="")),2))
    parameter_decimals<-c(5,0,3,3,3,    4,4,4,4,    3,3,3,3,     4,4,4,4,     4,4,4,4,      3,3,3,3,      3,3,3,5,4)
    #length(parameter)
    #length(parameter_units)
    #length(parameter_name)
    parameter_letter<-c("t","y","v","w","x", "a","f","k","p", "c", "h","m","r","b",  "g",  "l","q","m","r",  "3",  "d","d","i","n",  "s",  "e","j","o", "u","5") 
    #length(parameter_letter)
 
       
##############    
## FIGURE 4 ##    
############## 
    
    png(file=paste(path_figures,"FigureA3_params_along_optim_DE30_OPTM5_bis.png",sep=""),
        width = 1200, height = 800, pointsize=20) 
    par(mfcol=c(5,5))
    #layout(matrix(c(1:4,9,5:8,9), ncol=5, byrow=T))
    par(mar=c(0,5,2,1))
    par(oma=c(3,1,1,3))
    layout.show(n=25)
    cexname=1.4
    cexaxis=0.75
    #
    for (k in c(6:9,29,   14:17,3,   10:13,4,   22:25,5,     26:28,1,2)){

    #for (k in c(1:nparams)){
      #for (k in parameter_rank[c(1:7)]){
      #k=which(parameter_rank==h)
      for (i in c(1:total_runs)){
        param1[i]<-as.numeric(as.character(valores[[i]][k]))
      }
      x_id<-c(1:total_runs)  
      length(x_id)
      x_gen<-rep(c(1:maxiter),each=NP)[1:total_runs]  
      length(x_gen)
      #plot(x_id,param1,las=1,ylim=c(minimum[k],maximum[k]),
      #     xaxt="n",xlab="generation", ylab="units", col="grey50",pch=19, cex=0.2,
      #     mgp=c(3.5,1,0),main=paste(instance[k],parameter[k],sep="/"))
      if (k>=14 & k<=17) {plot(x_gen,param1,las=1,#ylim=c(minimum[k],maximum[k]),
                          xaxt="n",xlab="generation", ylab=parameter_units[k], col="grey95",pch=19, cex=0.05,
                          mgp=c(3.8,1,0), main="", yaxt="n", type="n", xaxs="i", yaxs="i")
                      axis(2,at=seq(round(min(param1),5),round(max(param1),5), length=6),
                      labels=format(seq(round(min(param1),5),round(max(param1),5), length=6),scientific=T),
                      las=1)
      } else {plot(x_gen,param1,las=1,#ylim=c(minimum[k],maximum[k]),
                   xaxt="n",xlab="generation", ylab=parameter_units[k], col="grey95",pch=19, cex=0.05,
                   mgp=c(3.8,1,0), main="", type="n", xaxs="i", yaxs="i")}
      #text(x=max(fin), y=minimum[k]+(minimum[k]*0.5), labels=paste(instance[k],parameter[k],sep="/"), pos=2)
      param_gen<-rep(NA, length=maxiter)
      param_max<-rep(NA, length=maxiter)
      param_min<-rep(NA, length=maxiter)
      if (sum(match(c(6:9,14:17,10:13,18:21),k), na.rm=T)>=1) mycol<-mycol_phyto
      if (sum(match(c(29,30),k), na.rm=T)>=1)                 mycol<-mycol_grey
      if (sum(match(c(22:25),k), na.rm=T)>=1)                 mycol<-mycol_cdom
      if (sum(match(c(26:28,1:5),k), na.rm=T)>=1)             mycol<-mycol_cdom
      
      for (i in c(1:maxiter)){
        param_max[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.95, na.rm=T)
        param_min[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.05, na.rm=T)
      }
      
      equis<-c(c(1:maxiter),c(1:maxiter)[length(c(1:maxiter)):1])
      ies<-c(param_max,param_min[length(param_min):1])
      polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol[4], border=NA)
      points(c(1:maxiter),param_max, type="l", lwd=1, col=parameter_color[k])
      points(c(1:maxiter),param_min, type="l", lwd=1, col=parameter_color[k])
      
      position_name<-param_max[1]-(param_max[1]-param_min[1])*0.15
      
      for (i in c(1:maxiter)){
        param_max[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.85, na.rm=T)
        param_min[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.15, na.rm=T)
      }
      ies<-c(param_max,param_min[length(param_min):1])
      polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol[3], border=NA)

      for (i in c(1:maxiter)){
        param_max[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.75, na.rm=T)
        param_min[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.25, na.rm=T)
      }
      ies<-c(param_max,param_min[length(param_min):1])
      polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol[2], border=NA)
      
      for (i in c(1:maxiter)){
        param_gen[i]<-mean(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
        param_max[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.65, na.rm=T)
        param_min[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.35, na.rm=T)
      }
      ies<-c(param_max,param_min[length(param_min):1])
      polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol[1], border=NA)      
      points(c(1:maxiter),param_gen, type="l", lwd=3, col=parameter_color[k], las=1, ylim=c(0.4,0.8))

      text(x=1+(1*0.2),y=position_name,labels=parameter_name[k], pos=4, font=2, cex=cexname)
      mtext(3,at=-22, line=-0.10,text=paste("(",parameter_letter[k],")",sep=""), font=1, cex=cexname-0.2)
      
      if (k==29|k==2|k==3|k==4|k==5) {
        axis(1, at=c(1,seq(50,1000,by=50)), labels=F)
        axis(1, at=c(1,seq(50,1000,by=50)), labels=c(1,seq(50,1000,by=50)))
        } else { axis(1, at=seq(1,1000,by=50), labels=F)  } 
      
      if (k==6 | k==8 | k==11 | k==16) {text(x=maxiter-(maxiter*(parameter_decimals[k]*0.06)),y=param_gen[maxiter],labels=round(param_gen[maxiter],parameter_decimals[k]), pos=1, font=1)
        } else if (k==2) {  text(x=maxiter-(maxiter*(1.8*0.06)),y=param_gen[maxiter],labels=round(param_gen[maxiter],parameter_decimals[k]), pos=1, font=1)                         
        } else {text(x=maxiter-(maxiter*(parameter_decimals[k]*0.06)),y=param_gen[maxiter],labels=round(param_gen[maxiter],parameter_decimals[k]), pos=3, font=1)}
      #legend(x="topleft",legend=paste(instance[k],parameter[k],sep="/"),bty="n")
      #legend(x="topleft",legend=parameter_name[k],bty="n")
    }
    ###################
    mtext(1,line=1.8,at=seq(0.130,0.930,length=5),text=c("generation"),outer=T, cex=cexaxis)
    dev.off()
    
    
    
### Ranges at the end
#####################################  
    NOMBRE<-rep(NA,length=length(parameter))
    FINALL<-rep(NA,length=length(parameter))
    MAXIMA<-rep(NA,length=length(parameter))
    MINIMA<-rep(NA,length=length(parameter))
    PlusMinus<-rep(NA,length=length(parameter))
    
    for (k in c(1:nparams)){
      for (i in c(1:total_runs)){
        param1[i]<-as.numeric(as.character(valores[[i]][k]))}
      x_id<-c(1:total_runs)  
      x_gen<-rep(c(1:maxiter),each=NP)[1:total_runs]  
      param_gen<-rep(NA, length=maxiter)
      param_max<-rep(NA, length=maxiter)
      param_min<-rep(NA, length=maxiter)
      for (i in c(1:maxiter)){
        param_gen[i]<-mean(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
        #param_max[i]<-max(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
        #param_min[i]<-min(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
        param_max[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.95, na.rm=T)
        param_min[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.05, na.rm=T)        
        } 
      NOMBRE[k]<-paste(instance[k],parameter[k],sep="/")
      FINALL[k]<-param_gen[maxiter]
      MAXIMA[k]<-param_max[maxiter]
      MINIMA[k]<-param_min[maxiter]
      PlusMinus[k]<-(param_max[maxiter]-param_min[maxiter])/2
    }
    
######################################
data.frame(NOMBRE,MINIMA,MAXIMA,FINALL,PlusMinus)
    
    
    #### Criterium to stop: relative tolerance of lnlikelihood
    ###########
    x_id<-c(1:total_runs)  
    #length(x_id)
    x_gen<-rep(c(1:maxiter),each=NP)[1:total_runs]  
    #length(x_gen)
    
    par(mfrow=c(1,1))
    plot(x_gen,lnls,las=1,xlab="generation")
    param_gen<-rep(NA, length=maxiter)
    param_max<-rep(NA, length=maxiter)
    param_min<-rep(NA, length=maxiter)

    for (i in c(1:maxiter)){
      param_max[i]<-quantile(as.numeric(lnls[c(inicio[i]:fin[i])]),probs=0.95, na.rm=T)
      param_min[i]<-quantile(as.numeric(lnls[c(inicio[i]:fin[i])]),probs=0.05, na.rm=T)
    }
    
    equis<-c(c(1:maxiter),c(1:maxiter)[length(c(1:maxiter)):1])
    ies<-c(param_max,param_min[length(param_min):1])
    polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol[4], border=NA)
    points(c(1:maxiter),param_max, type="l", lwd=1, col=parameter_color[k])
    points(c(1:maxiter),param_min, type="l", lwd=1, col=parameter_color[k])
    
    ftol<-(param_max[1]-param_min[1])*0.005
    gen_lnls<-which(abs((param_max-param_min)-ftol)==min(abs((param_max-param_min)-ftol)))
    ############
    
    
    
    
    ### Metrics ####
    ################  
    correlation<-matrix(NA, ncol=length(observations), nrow=total_runs)
    rmse<-matrix(NA, ncol=length(observations), nrow=total_runs)
    
    for (i in c(1:total_runs)){
      metrics<-strsplit(extra_outputs[i], split=" ")
      correlation[i,]<-as.numeric(gsub(",","",gsub("[","",gsub("]","",metrics[[1]][2:(length(observations)+1)], fixed=T), fixed=T), fixed=T))
      rmse[i,]<-as.numeric(gsub(",","",gsub("[","",gsub("]}","",metrics[[1]][(length(observations)+3):(length(observations)*2+2)], fixed=T), fixed=T), fixed=T))
    }
    colnames(correlation)<-observations
    x_id<-c(1:total_runs)  
    x_gen<-rep(c(1:maxiter),each=NP)[1:total_runs]
    
    observations_name<-c(expression("pico-Chla"),expression("nano-Chla"),expression("micro-Chla"),
                         expression(paste(a[PH],(450))), expression(paste(a[NAP],(450))),expression(paste(a[CDOM],(450))),
                         expression(S["CDOM"]^"350-500"),expression(paste(b[BP],(450))))   
    observations_units<-c(rep(expression(paste("(",mgChl," ", m^-3,")", sep="")),3),
                          rep(expression(paste("(",m^-1,")", sep="")), 3),
                          expression(paste("(",nm^-1,")", sep="")),
                          expression(paste("(",m^-1,")", sep="")))    
    minimito<-c(0.065,  0.200,  0.145,   0.012,   0.0048,   0.006,  0.001,  0.001)
    maximito<-c(0.120,  0.300,  0.180,   0.020,   0.0068,   0.020,  0.004,  0.002)
    redondeo<-c(3,  3,  3,   3,   4,   4,  3,  5)
    letritas<-c("(a) ",  "(b) ",  "(c) ",   "(d) ",   "(e) ",   "(f) ",   "(g) ",  "(h) ")

        
##############    
## FIGURE 3 ##    
##############    
    
png(file=paste(path_figures,"FigureA1_metrics_along_optim_DE30_OPTM5.png",sep=""),
        width = 1200, height = 600, pointsize=20) 
    par(mfrow=c(2,4))
    #layout(matrix(c(1:3,8,4:5,7,6), ncol=4, byrow=T))
    par(mar=c(1,5,2,1))
    par(oma=c(3,0,1,4))
    layout.show(n=8)
    cexletter=2.0
    cexlabel=1.6
    cexaxis=0.8
    cexunits=1.1
    gen_stop<-rep(NA, length=length(observations))
    final_R<-rep(NA, length=length(observations))
    final_RMSE<-rep(NA, length=length(observations))
    improvement<-rep(NA, length=length(observations))
    
    for (k in c(1:7)){
      
      ## Correlation (R)
      ##################
      plot(x_gen,correlation[,k], las=1,xaxt="n",xlab="", ylab="", col="grey80",pch=19,
           cex=0.05, main="", ylim=c(0,1.0), yaxs="i", cex.lab=1.1, mgp=c(2.5,1,0), type="n", xaxs="i", yaxs="i")
      
      if (k>=5) {
        axis(1, at=c(1,seq(30,300,by=30)), labels=F, cex.axis=cexaxis)
        axis(1, at=c(1,seq(30,300,by=30)), labels=c(1,seq(30,300,by=30)), cex.axis=cexaxis)
      } else { axis(1, at=c(1,seq(30,300,by=30)), labels=F, cex.axis=cexaxis)
               axis(1, at=c(seq(30,300,by=30)), labels=c(seq(30,300,by=30)), cex.axis=cexaxis)} 
      text(x=140, y=0.83, pos=1, labels=observations_units[k], cex=cexunits) # cambiar x
      
      # Lines
      corr_gen<-rep(NA, length=maxiter)
      corr_max<-rep(NA, length=maxiter)
      corr_min<-rep(NA, length=maxiter)
      for (i in c(1:maxiter)){
        corr_gen[i]<-mean(as.numeric(correlation[c(inicio[i]:fin[i]),k]), na.rm=T)
        corr_max[i]<-quantile(as.numeric(correlation[c(inicio[i]:fin[i]),k]),probs=0.95, na.rm=T)
        corr_min[i]<-quantile(as.numeric(correlation[c(inicio[i]:fin[i]),k]),probs=0.05, na.rm=T)
      }
      equis<-c(c(1:maxiter),c(1:maxiter)[length(c(1:maxiter)):1])
      ies<-c(corr_max,corr_min[length(corr_min):1])
      polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[4], border=NA)      
      points(c(1:maxiter),corr_max, type="l", lwd=1, col=linea_grey)
      points(c(1:maxiter),corr_min, type="l", lwd=1, col=linea_grey)  
      #which(min(abs((corr_max-corr_min)-0.01)))
      gen_stop[k]<-which(abs((corr_max-corr_min)-0.02)==min(abs((corr_max-corr_min)-0.02)))
      arrows(x0=gen_stop[k], x1=gen_stop[k], y0=0.1+corr_gen[gen_stop[k]], y1=0.04+corr_gen[gen_stop[k]],length = 0.1)
      
      for (i in c(1:maxiter)){
        corr_max[i]<-quantile(as.numeric(correlation[c(inicio[i]:fin[i]),k]),probs=0.85, na.rm=T)
        corr_min[i]<-quantile(as.numeric(correlation[c(inicio[i]:fin[i]),k]),probs=0.15, na.rm=T)
      }
      equis<-c(c(1:maxiter),c(1:maxiter)[length(c(1:maxiter)):1])
      ies<-c(corr_max,corr_min[length(corr_min):1])
      polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[3], border=NA) 
      
      for (i in c(1:maxiter)){
        corr_max[i]<-quantile(as.numeric(correlation[c(inicio[i]:fin[i]),k]),probs=0.75, na.rm=T)
        corr_min[i]<-quantile(as.numeric(correlation[c(inicio[i]:fin[i]),k]),probs=0.25, na.rm=T)
      }
      equis<-c(c(1:maxiter),c(1:maxiter)[length(c(1:maxiter)):1])
      ies<-c(corr_max,corr_min[length(corr_min):1])
      polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[2], border=NA)
      
      for (i in c(1:maxiter)){
        corr_gen[i]<-mean(as.numeric(correlation[c(inicio[i]:fin[i]),k]), na.rm=T)
        corr_max[i]<-quantile(as.numeric(correlation[c(inicio[i]:fin[i]),k]),probs=0.65, na.rm=T)
        corr_min[i]<-quantile(as.numeric(correlation[c(inicio[i]:fin[i]),k]),probs=0.35, na.rm=T)
      }
      equis<-c(c(1:maxiter),c(1:maxiter)[length(c(1:maxiter)):1])
      ies<-c(corr_max,corr_min[length(corr_min):1])
      polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_grey[1], border=NA)        
      points(c(1:maxiter),corr_gen, type="l", lwd=4, col=linea_grey, las=1, ylim=c(0.4,0.8))

      final_R[k]<-corr_gen[length(corr_gen)]
      ###################
      
      ## Root mean squared error (RMSE)
      ###################
      par(new=T)
      plot(x_gen,rmse[,k], las=1,xaxt="n",yaxt="n",xlab="", ylab="",col="lightyellow", pch=19, cex=0.05,
           main="", ylim=c(minimito[k],maximito[k]), type="n", xaxs="i", yaxs="i")
      axis(4,at=round(seq(minimito[k],maximito[k],length=5),redondeo[k]),las=1, cex.axis=0.9)
      # Lines
      rmse_gen<-rep(NA, length=maxiter)
      rmse_max<-rep(NA, length=maxiter)
      rmse_min<-rep(NA, length=maxiter)
      for (i in c(1:maxiter)){
        rmse_max[i]<-quantile(as.numeric(rmse[c(inicio[i]:fin[i]),k]),probs=0.95, na.rm=T)
        rmse_min[i]<-quantile(as.numeric(rmse[c(inicio[i]:fin[i]),k]),probs=0.05, na.rm=T)
      }
      equis<-c(c(1:maxiter),c(1:maxiter)[length(c(1:maxiter)):1])
      ies<-c(rmse_max,rmse_min[length(rmse_min):1])
      polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_bact[4], border=NA)        
      points(c(1:maxiter),rmse_max, type="l", lwd=1, col=linea_bact)
      points(c(1:maxiter),rmse_min, type="l", lwd=1, col=linea_bact)  
      
      for (i in c(1:maxiter)){
        rmse_max[i]<-quantile(as.numeric(rmse[c(inicio[i]:fin[i]),k]),probs=0.85, na.rm=T)
        rmse_min[i]<-quantile(as.numeric(rmse[c(inicio[i]:fin[i]),k]),probs=0.15, na.rm=T)
      }
      equis<-c(c(1:maxiter),c(1:maxiter)[length(c(1:maxiter)):1])
      ies<-c(rmse_max,rmse_min[length(rmse_min):1])
      polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_bact[3], border=NA)       
      
      for (i in c(1:maxiter)){
        rmse_max[i]<-quantile(as.numeric(rmse[c(inicio[i]:fin[i]),k]),probs=0.75, na.rm=T)
        rmse_min[i]<-quantile(as.numeric(rmse[c(inicio[i]:fin[i]),k]),probs=0.25, na.rm=T)
      }
      equis<-c(c(1:maxiter),c(1:maxiter)[length(c(1:maxiter)):1])
      ies<-c(rmse_max,rmse_min[length(rmse_min):1])
      polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_bact[2], border=NA) 
      
      for (i in c(1:maxiter)){
        rmse_gen[i]<-mean(as.numeric(rmse[c(inicio[i]:fin[i]),k]), na.rm=T)
        rmse_max[i]<-quantile(as.numeric(rmse[c(inicio[i]:fin[i]),k]),probs=0.65, na.rm=T)
        rmse_min[i]<-quantile(as.numeric(rmse[c(inicio[i]:fin[i]),k]),probs=0.35, na.rm=T)
      }
      equis<-c(c(1:maxiter),c(1:maxiter)[length(c(1:maxiter)):1])
      ies<-c(rmse_max,rmse_min[length(rmse_min):1])
      polygon(x=equis[!is.na(ies)], y=ies[!is.na(ies)], col=mycol_bact[1], border=NA)       
      points(c(1:maxiter),rmse_gen, type="l", lwd=4, col=linea_bact, las=1, ylim=c(0.4,0.8))      
      final_RMSE[k]<-rmse_gen[length(rmse_gen)]
      improvement[k]<-(rmse_gen[length(rmse_gen)]/rmse_gen[1])*100
      #legend(x="bottomright", legend=c("corr","rmse"),lwd=2, col=c("grey30","yellow3"),bty="n")
      legend(x="topright", legend=observations_name[k], bty="n", cex=cexlabel)

      #legend(x="topleft", legend=letritas[k], bty="n", cex=cexletter, text.font=2)
      #text(x=5,y=0.975, pos=1, label=letritas[k], cex=cexletter)
      mtext(side=3, at=10, line=0.5, text=letritas[k], cex=cexlabel-0.1, font=2)
      ###################
    }
    ##################### 

    mtext(2, at=c(0.73,0.23), line=-2, text=c("R","R"), outer=T, cex=cexaxis)
    mtext(4, at=c(0.73,0.23), line=c(2.9,-15.4),  text=c("RMSE","RMSE"), outer=T, cex=cexaxis)
    mtext(1,line=c(1.5,1.5,1.5,-15.5),at=seq(0.150,0.900,length=4),text=c("generation"),outer=T, cex=cexaxis) 
    
    dev.off()
    #####################
    
data.frame(observations, gen_stop, final_R, final_RMSE, improvement)
    