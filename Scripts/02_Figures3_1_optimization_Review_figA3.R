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
                           dbname=paste(o_dir,"CALIBRATION_R1/boussole_annual_DE30_OPTM5_1tris.db", sep=""))
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
          
          # last generation is complete x 34
          #runs1<-runs1[1:((maxiter-1)*NP)]
          #lnls1<-lnls1[1:((maxiter-1)*NP)]
          #valores1<-valores1[1:((maxiter-1)*NP)]
          #extra_outputs1<-extra_outputs1[1:((maxiter-1)*NP)]
          #length(runs1)
          #length(valores1)
          #length(extra_outputs1) 
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
                           dbname=paste(o_dir,"CALIBRATION_R1/boussole_annual_DE30_OPTM5_2tris.db", sep=""))
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
          runs2<-results$run
          lnls2<-results$lnlikelihood
          valores2<-strsplit(results$parameters, split=";")
          length(runs2)
          length(valores2)
          extra_outputs2<-results$extra_outputs
          length(extra_outputs2)    
          
          # # last generation is not complete x 32
          runs2<-runs2[1:((maxiter-1)*NP)]
          lnls2<-lnls2[1:((maxiter-1)*NP)]
          valores2<-valores2[1:((maxiter-1)*NP)]
          extra_outputs2<-extra_outputs2[1:((maxiter-1)*NP)]
          length(runs2)
          length(valores2)
          length(extra_outputs2) 
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
                           dbname=paste(o_dir,"CALIBRATION_R1/boussole_annual_DE30_OPTM5_3tris.db", sep=""))
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
          #runs3<-runs3[1:((maxiter-1)*NP)]
          #lnls3<-lnls3[1:((maxiter-1)*NP)]
          #valores3<-valores3[1:((maxiter-1)*NP)]
          #extra_outputs3<-extra_outputs3[1:((maxiter-1)*NP)]
          #length(runs3)
          #length(valores3)
          #length(extra_outputs3) 
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
          # 
          # ######################################
          # data.frame(NOMBRE,MINIMA,FINALL,MAXIMA)
          
          

          ## connect to db: leg4
          ######################
          con <- dbConnect(drv=RSQLite::SQLite(),
                           dbname=paste(o_dir,"CALIBRATION_R1/boussole_annual_DE30_OPTM5_4tris.db", sep=""))
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
          length(results$run) 
          runs4<-results$run
          lnls4<-results$lnlikelihood
          valores4<-strsplit(results$parameters, split=";")
          length(runs4)
          length(valores4)
          extra_outputs4<-results$extra_outputs
          length(extra_outputs4)      
          ######################
          
          # last generation is complete x 33
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
          #   }
          #   NOMBRE[k]<-paste(instance[k],parameter[k],sep="/")
          #   FINALL[k]<-param_gen[length(param_gen[!is.nan(param_gen)==T])]
          #   MAXIMA[k]<-param_max[length(param_gen[!is.nan(param_gen)==T])]
          #   MINIMA[k]<-param_min[length(param_gen[!is.nan(param_gen)==T])]
          # }
          # 
          # ######################################
          # data.frame(NOMBRE,MINIMA,FINALL,MAXIMA)
          
          


          ## connect to db: leg5
          ######################
          con <- dbConnect(drv=RSQLite::SQLite(),
                           dbname=paste(o_dir,"CALIBRATION_R1/boussole_annual_DE30_OPTM5_5tris.db", sep=""))
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
          runs5<-results$run
          lnls5<-results$lnlikelihood
          valores5<-strsplit(results$parameters, split=";")
          length(runs5)
          length(valores5)
          extra_outputs5<-results$extra_outputs
          length(extra_outputs5)          
          ######################    
          
          # last generation is not complete x 33
          runs5<-runs5[1:((maxiter-1)*NP)]
          lnls5<-lnls5[1:((maxiter-1)*NP)]
          valores5<-valores5[1:((maxiter-1)*NP)]
          extra_outputs5<-extra_outputs5[1:((maxiter-1)*NP)]
          length(runs5)
          length(valores5)
          length(extra_outputs5)           
          
 
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
          #    } 
          #   NOMBRE[k]<-paste(instance[k],parameter[k],sep="/")
          #   FINALL[k]<-param_gen[length(param_gen[!is.nan(param_gen)==T])]
          #   MAXIMA[k]<-param_max[length(param_gen[!is.nan(param_gen)==T])]
          #   MINIMA[k]<-param_min[length(param_gen[!is.nan(param_gen)==T])]
          # }
          # 
          # ######################################
          # data.frame(NOMBRE,MINIMA,FINALL,MAXIMA)


          
      ## All together
      maxiter=34+32+32+33+33
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
      
      
      save(maxiter,inicio,fin,param1,lnls,valores,extra_outputs,total_runs, file=paste0(o_dir,"CALIBRATION_R1/DE30_OPTM5_3.RData"))

      
      
      
      
      ### Parameters ###
      ##################
      parameter_name<-c(expression(b[X(3)]), expression(I[X(3)]),
                        expression(S["X(1)"]^"350-500"),expression(S["X(2)"]^"350-500"),expression(S["X(3)"]^"350-500"),
                        # eps
                        expression(bar("a*")["P"^(DIATOM)]),expression(bar("a*")["P"^(NANO)]),expression(bar("a*")["P"^(PICO)]),expression(bar("a*")["P"^(DINO)]),
                        # theta max
                        expression(theta["chl"]^"0(DIATOM)"),expression(theta["chl"]^"0(NANO)"),expression(theta["chl"]^"0(PICO)"),expression(theta["chl"]^"0(DINO)"),
                        # QY max
                        expression(phi["c"]^"0(DIATOM)"),expression(phi["c"]^"0(NANO)"),expression(phi["c"]^"0(PICO)"),expression(phi["c"]^"0(DINO)"),
                        # bps
                        expression(bar("b*")["P(DIATOM)"]),expression(bar("b*")["P(NANO)"]),expression(bar("b*")["P(PICO)"]),expression(bar("b*")["P(DINO)"]),
                        # fX2
                        expression(f["P"^(DIATOM)]^"X2"),expression(f["P"^(NANO)]^"X2"),expression(f["P"^(PICO)]^"X2"),expression(f["P"^(DINO)]^"X2"),
                        # fX3, fX1
                        expression(f["B"]^"X3"),expression(f["Z"^(MICRO)]^"X1"),expression(f["Z"^(HETEN)]^"X1"),
                        # aNAP, bNAP
                        expression(a["R"]^"440"),expression(b["R"]^"550"))
      parameter_color<-c(rep(linea_cdom,5),rep(linea_phyto,16),rep(linea_cdom,4),rep(linea_cdom,3),rep(linea_grey,2))                  
      parameter_units<-c(expression(paste(d^-1)),
                         expression(paste(mu, E^-1," ",m^-2," ",s^-1,sep="")),
                         rep(expression(nm^-1), 3), #Scdom
                         rep(expression(paste(m^2," ", "mgChl-a"^-1, sep="")),4), #eps
                         rep(expression(paste("mgChl-a"," ", mgC^-1, sep="")),4), #theta
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
      
      one_vs2<-rep(NA,length=length(parameter))
      one_vs3<-rep(NA,length=length(parameter))
      
      ##############    
      ## FIGURE 4 ##    
      ############## 
      
      #png(file=paste(path_figures,"FigureA3_params_along_optim_DE30_OPTM5_replicates.png",sep=""), width = 1200, height = 800, pointsize=20) 

      pdf(file=paste(path_figures,"FigureA3_params_along_optim_DE30_OPTM5_replicates.pdf",sep=""),
          width = 1200/90, height = 800/90, pointsize=15, family="Helvetica") 
      
      par(mfcol=c(5,5))
      #layout(matrix(c(1:4,9,5:8,9), ncol=5, byrow=T))
      par(mar=c(1,5,2,1))
      par(oma=c(3,1,1,3))
      #layout.show(n=25)
      cexname=1.22
      cexaxis=0.75
      #
                       #bX3 #IX3    #Scdom                #eps                        #ratio                      #QY                     #bps            #fP                        fZB             #ap    #bp                       
      parameter_min<-c(0.00, 26, 0.0164,0.0155,0.0166,  0.012,0.025,0.030,0.015,      0.016,0.022,0.012,0.016,  7e-4,1.8e-4,5.8e-4,2.4e-4, NA,NA,NA,NA,  0.04,0.040,0.03,0.04,  0.01,0.01,0.02,    0.00052,    NA)
      parameter_max<-c(0.10, 56, 0.0180,0.0180,0.0184,  0.016,0.034,0.046,0.021,      0.028,0.050,0.022,0.022,  1e-3,2.5e-4,1.0e-3,3.4e-4, NA,NA,NA,NA,  0.09,0.064,0.08,0.062,  0.07,0.07,0.07,   0.00062,    NA)  
     
      
      parameter_y  <-c(0.10, 56, 0.0180,0.0180, 0.0173,  0.0134,0.028,0.0355,0.021,   0.020,0.032,0.022,0.022,  1e-3,2.5e-4,7.2e-4,3.4e-4, NA,NA,NA,NA,  0.09,0.047,0.08,0.062,  0.07,0.07,0.07,   0.00062,    NA)  
      parameter_x  <-c(  50, 50,     55,   130,    130,     130,  130,   130,  130,     130,  130,  130,  130,   130,   130,   130,   130, NA,NA,NA,NA,    55,   50, 140, 140,   150, 140, 140,        135,    NA)  
      
      
       
      for (k in c(6:9,29,   14:17,3,   10:13,4,   22:25,5,     26:28,1,2)){
        
        # Rep 1
        load(paste0(o_dir,"CALIBRATION_R1/DE30_OPTM5.RData"))
        for (i in c(1:total_runs)){param1[i]<-as.numeric(as.character(valores[[i]][k]))}
        x_id<-c(1:total_runs)  
        x_gen<-rep(c(1:maxiter),each=NP)[1:total_runs]  
        if (k>=14 & k<=17) {plot(x_gen,param1,las=1,#ylim=c(minimum[k],maximum[k]),
                                 xaxt="n",xlab="generation", ylab=parameter_units[k], col="grey95",pch=19, cex=0.05,
                                 mgp=c(3.8,1,0), main="", yaxt="n", type="n", xaxs="i", yaxs="i",
                                 ylim=c(parameter_min[k],parameter_max[k]))
          axis(2,at=seq(round(min(param1),5),round(max(param1),5), length=6),
               labels=format(seq(round(min(param1),5),round(max(param1),5), length=6),scientific=T),
               las=1)
        } else {plot(x_gen,param1,las=1,#ylim=c(minimum[k],maximum[k]),
                     xaxt="n",xlab="generation", ylab=parameter_units[k], col="grey95",pch=19, cex=0.05,
                     mgp=c(3.8,1,0), main="", type="n", xaxs="i", yaxs="i", ylim=c(parameter_min[k],parameter_max[k]))}
        param_gen<-rep(NA, length=maxiter)
        param_max<-rep(NA, length=maxiter)
        param_min<-rep(NA, length=maxiter)
        for (i in c(1:maxiter)){
          param_max[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.95, na.rm=T)
          param_min[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.05, na.rm=T)
          param_gen[i]<-mean(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
        }
        #points(c(1:maxiter),param_max, type="l", lwd=1, col="grey10")
        #points(c(1:maxiter),param_min, type="l", lwd=1, col="grey10")
        points(c(1:maxiter),param_gen, type="l", lwd=3, col="grey10", las=1, ylim=c(0.4,0.8))
        position_name<-param_max[1]-(param_max[1]-param_min[1])*0.15
        
   replica_1<-param_gen     
        
        # Rep 2
        load(paste0(o_dir,"CALIBRATION_R1/DE30_OPTM5_2.RData"))        
        for (i in c(1:total_runs)){param1[i]<-as.numeric(as.character(valores[[i]][k]))}
        x_id<-c(1:total_runs)  
        x_gen<-rep(c(1:maxiter),each=NP)[1:total_runs]
        param_gen<-rep(NA, length=maxiter)
        param_max<-rep(NA, length=maxiter)
        param_min<-rep(NA, length=maxiter)
        for (i in c(1:maxiter)){
          param_max[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.95, na.rm=T)
          param_min[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.05, na.rm=T)
          param_gen[i]<-mean(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
        }
        #points(c(1:maxiter),param_max, type="l", lwd=1, col="sienna3")
        #points(c(1:maxiter),param_min, type="l", lwd=1, col="sienna3")
        points(c(1:maxiter),param_gen, type="l", lwd=3, col="sienna3", las=1, ylim=c(0.4,0.8))

   replica_2<-param_gen                  
        
        # Rep 3
        load(paste0(o_dir,"CALIBRATION_R1/DE30_OPTM5_3.RData"))
        for (i in c(1:total_runs)){param1[i]<-as.numeric(as.character(valores[[i]][k]))}
        x_id<-c(1:total_runs)
        x_gen<-rep(c(1:maxiter),each=NP)[1:total_runs]
        param_gen<-rep(NA, length=maxiter)
        param_max<-rep(NA, length=maxiter)
        param_min<-rep(NA, length=maxiter)
        for (i in c(1:maxiter)){
          param_max[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.95, na.rm=T)
          param_min[i]<-quantile(as.numeric(param1[c(inicio[i]:fin[i])]),probs=0.05, na.rm=T)
          param_gen[i]<-mean(as.numeric(param1[c(inicio[i]:fin[i])]), na.rm=T)
        }
        #points(c(1:maxiter),param_max, type="l", lwd=1, col="purple3")
        #points(c(1:maxiter),param_min, type="l", lwd=1, col="purple3")
        points(c(1:maxiter),param_gen, type="l", lwd=3, col="purple3", las=1, ylim=c(0.4,0.8))

   replica_3<-param_gen          
        
        #text(x=50,y=parameter_max[k],labels=parameter_name[k], pos=1, font=2, cex=cexname)
        text(x=parameter_x[k],y=parameter_y[k],labels=parameter_name[k], pos=1, font=2, cex=cexname)
        mtext(3,at=-22, line=0.5,text=paste("(",parameter_letter[k],")",sep=""), font=1, cex=cexname-0.2)
        
         if (k==29|k==2|k==3|k==4|k==5) {
           axis(1, at=c(1,seq(50,1000,by=50)), labels=F)
           axis(1, at=c(1,seq(50,1000,by=50)), labels=c(1,seq(50,1000,by=50)))
         } else { axis(1, at=seq(1,1000,by=50), labels=F)  } 
        
        # value
        # if (k==6 | k==8 | k==11 | k==16) {text(x=maxiter-(maxiter*(parameter_decimals[k]*0.06)),y=param_gen[maxiter],labels=round(param_gen[maxiter],parameter_decimals[k]), pos=1, font=1)
        # } else if (k==2) {  text(x=maxiter-(maxiter*(1.8*0.06)),y=param_gen[maxiter],labels=round(param_gen[maxiter],parameter_decimals[k]), pos=1, font=1)                         
        # } else {text(x=maxiter-(maxiter*(parameter_decimals[k]*0.06)),y=param_gen[maxiter],labels=round(param_gen[maxiter],parameter_decimals[k]), pos=3, font=1)}
      

    one_vs2[k]<-(replica_2[163]/replica_1[163])*100               
    one_vs3[k]<-(replica_3[163]/replica_1[163])*100            
        
    #points(150, replica_1[150], col="grey10", pch=4, cex=1.2, lwd=3)
    #points(150, replica_2[150], col="sienna3", pch=4, cex=1.2, lwd=3)
    #points(150, replica_3[150], col="purple3", pch=4, cex=1.2, lwd=3)
    
        }
      ###################
      mtext(1,line=1.3,at=seq(0.130,0.930,length=5),text=c("generation"),outer=T, cex=cexaxis)
      dev.off()
      
      
      
      
      parameter[c(6:8,  29,   14:16,   10:12, 4,   22:24, 5,     26:28, 1,2)] 
      range(c(one_vs2[c(6:8,  29,   14:16,   10:12, 4,   22:24, 5,     26:28, 1,2)],
              one_vs3[c(6:8,  29,   14:16,   10:12, 4,   22:24, 5,     26:28, 1,2)]))
      
      
      
