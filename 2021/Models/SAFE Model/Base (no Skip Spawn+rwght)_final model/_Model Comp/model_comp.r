
  sable_model_comp <- function(comp_group,model_names,dat_num, dir_input, dir_output) {
  
    setwd(dir_output)
    if(dev.cur()>1) dev.off()                     # Close any current graphics device
    
    pdf(paste0(comp_group,".pdf"))
    
    for(i in 1: dat_num) {
      setwd(dir_input)

      read.temp <- dget(paste0(dir_input, "\\tem", i ,".rdat")) 
      
      if(i == 1) {
        
        styear<-as.numeric(min(row.names(read.temp$t.series)))
        endyear<-as.numeric(max(row.names(read.temp$t.series)))
        nyrs<-length(as.numeric(row.names(read.temp$t.series)))           # number of years of assessment
        yrs<-seq(styear,endyear)
        ages<-as.numeric(colnames(read.temp$M))
        nages<-length(ages)
        
        R_vec <- as.numeric(unlist(read.temp$t.series["Recr"]))[1:nyrs-1]  # 2020 recruitment is not estimated
        R_yrs<-seq(styear-2,endyear-3,1)
        R_melt<-data.frame(cbind(R_yrs,R_vec)) %>% 
          dplyr::rename(!!model_names[i]:= R_vec)

         
        sellabs<-c("LL Derby_F","LL Derby_M","Trawl Fish_F" ,"Trawl Fish_M" ,"LL IFQ_F" ,
                   "LL IFQ_M", "LL IFQ Rec_F" , "LL IFQ Rec_M", "Dom LL Surv_F","Dom LL Sur_M", "Dom LL Sur Rec_F",
                   "Dom LL Sur Rec_M","Coop LL Sur_F" ,
                   "Coop LL Sur_M" ,"Trawl Sur_F","Trawl Sur_M" )
        sel_full <- as.numeric(unlist(read.temp$agesel))
        SEL<- matrix(sel_full, byrow = FALSE, nrow = nages, ncol = length(read.temp$agesel[1,]), 
                   dimnames= list(c(ages), c(sellabs)))
        sel_new <- SEL %>% as.data.frame() %>%
          rownames_to_column("Age") %>%
          pivot_longer(-Age, names_to = "Survey", values_to= "Values")  # combo of transpose and melt
        sel_sub <- sel_new %>%
          dplyr::rename(!!model_names[i]:= Values)

        F_vec <- as.numeric(unlist(read.temp$t.series["fmort"]))
        F_melt<-data.frame(cbind(yrs,F_vec)) %>% 
          dplyr::rename(!!model_names[i]:= F_vec)
        
        M_full <- as.numeric(unlist(read.temp$M))
        M<- matrix(M_full, byrow = FALSE, nrow = nyrs, ncol = nages, 
                   dimnames= list(c(yrs), c(ages)))
        year_M<-as.numeric(unlist(M[,15]))
        M_melt_yr<-data.frame(cbind(yrs,year_M)) %>% 
          dplyr::rename(!!model_names[i]:= year_M)
        keep.years <- c(styear,endyear)
        M_new <- M %>% as.data.frame() %>%
          rownames_to_column("Year") %>%
          pivot_longer(-Year, names_to = "Age", values_to= "Values") %>%   # combo of transpose and melt
          mutate(Plot_years= ifelse(Year %in% keep.years, 1, 0),           # keep 
               Age= as.numeric(Age),
                 Year= as.numeric(Year))
        M_sub <- M_new %>% filter(Plot_years == 1) %>%
          dplyr::select(-Plot_years) %>%
          dplyr::rename(!!model_names[i]:= Values)
        
        srvlabs<-c("DOM LL RPW","DOM LL RPN","DOM LL CPUE", "TRAWL SRV Bio" )
        
        srv1.yrs<-as.numeric(row.names(read.temp$obssrv1))
        srv1_obs <- as.numeric(unlist(read.temp$obssrv1["obssrv1"]))
        srv1_pred <- as.numeric(unlist(read.temp$obssrv1["predsrv1"]))
        srv3.yrs<-as.numeric(row.names(read.temp$obssrv3))
        srv3_obs <- as.numeric(unlist(read.temp$obssrv3["obssrv3"]))
        srv3_pred <- as.numeric(unlist(read.temp$obssrv3["predsrv3"]))
        srv5.yrs<-as.numeric(row.names(read.temp$obssrv5))
        srv5_obs <- as.numeric(unlist(read.temp$obssrv5["obssrv5"]))
        srv5_pred <- as.numeric(unlist(read.temp$obssrv5["predsrv5"]))
        srv7.yrs<-as.numeric(row.names(read.temp$obssrv7))
        srv7_obs <- as.numeric(unlist(read.temp$obssrv7["obssrv7"]))
        srv7_pred <- as.numeric(unlist(read.temp$obssrv7["predsrv7"]))
        
        srv1_melt<-data.frame(cbind(srv1.yrs,srv1_obs,srv1_pred)) %>% 
          dplyr::rename(!!"_OBS":= srv1_obs) %>% 
          dplyr::rename(!!model_names[i]:= srv1_pred)
        srv3_melt<-data.frame(cbind(srv3.yrs,srv3_obs,srv3_pred)) %>% 
          dplyr::rename(!!"_OBS":= srv3_obs) %>% 
          dplyr::rename(!!model_names[i]:= srv3_pred)
        srv5_melt<-data.frame(cbind(srv5.yrs,srv5_obs,srv5_pred)) %>% 
          dplyr::rename(!!"_OBS":= srv5_obs) %>% 
          dplyr::rename(!!model_names[i]:= srv5_pred)
        srv7_melt<-data.frame(cbind(srv7.yrs,srv7_obs,srv7_pred)) %>% 
          dplyr::rename(!!"_OBS":= srv7_obs) %>% 
          dplyr::rename(!!model_names[i]:= srv7_pred)
        
        temp.rep<-readLines(paste0(dir_input, "\\sable", i ,".rep"))
        temp.par<-readLines(paste0(dir_input, "\\tem", i ,".par"))
        mod.nam.temp<-model_names[i]
        B40<-round(as.numeric(unlist(strsplit(temp.rep[grep("B_40",temp.rep)+1],split=" "))),digits=2) #Get B40 from report file
        
        B40_vec<-rep(B40,times=nyrs) 
          B40_melt1<- data.frame(cbind(yrs,B40_vec)) %>%
          dplyr::rename(!!model_names[i]:= B40_vec)
     
        SSB_vec <- as.numeric(unlist(read.temp$t.series["spbiom"]))
        SSB_melt<-data.frame(cbind(yrs,SSB_vec)) %>% 
          dplyr::rename(!!model_names[i]:= SSB_vec) 
         
        num_par<-as.numeric(unlist(strsplit(temp.rep[grep("Number parameters estimated",temp.rep)+1],split=" "))) #Get B40 from report file
        F40<-round(as.numeric(unlist(strsplit(temp.rep[grep("F_40",temp.rep)+1],split=" "))), digits=2) 
        F40<-F40[1]
        F.ABC<-round(as.numeric(unlist(strsplit(temp.rep[grep("F_ABC",temp.rep)+1],split=" "))), digits=2)  
        F.ABC<-F.ABC[1]
        mean.rec<-round(as.numeric(unlist(strsplit(temp.rep[grep("Mean_Recruitment",temp.rep)+1],split=" "))), digits=2)  
        mean.rec<-mean.rec[1]
        ABC<-round(as.numeric(unlist(strsplit(temp.rep[grep("ABC for",temp.rep)+1],split=" "))), digits=2)  
        ABC<-ABC[3] # first 2 are the F_ABC values, this grabs ABC for first projection year
        total_lik<-as.numeric(unlist(strsplit(temp.rep[grep("Total likelihood",temp.rep)+1],split=" "))) 
        max_grad<-unlist(strsplit(temp.par[grep("Maximum gradient",temp.par)],split=" "))
        max_grad<-as.numeric(max_grad[length(max_grad)]) # grab the max grad value which is the last value of first line of par file, and turn into numeric value
        term_SSB_tem<-round(as.numeric(unlist(read.temp$t.series["spbiom"])), digits=2) 
        term_SSB<-term_SSB_tem[nyrs]
        B_B40<-round(term_SSB/B40, digits=2) 
        term_F_tem<-round(as.numeric(unlist(read.temp$t.series["fmort"])), digits=2) 
        term_F<-term_F_tem[nyrs]
        F_F40<-round(term_F/F40, digits=2) 
        likes<-as.numeric(unlist(read.temp$likecomp))
        total_lik<-likes[23]
        total_lik<-round(total_lik,digits=2)
        like<-likes[-c(5,8,22,23)]
        model_conv<-file.exists(paste0(dir_input,"\\tem",i,".std"))

        Model_summ<-data.frame(cbind(mod.nam.temp,as.character(model_conv), max_grad,total_lik,num_par))
        B40_melt<-data.frame(cbind(mod.nam.temp,term_SSB,B40, B_B40,mean.rec,term_F, F40,F_F40, F.ABC, ABC))
        
        
      } # end if i=1
      
      else{
        
        R_vec <- as.numeric(unlist(read.temp$t.series["Recr"]))[1:nyrs-1]  # 2020 recruitment is not estimated
        R_melt<-data.frame(cbind(R_yrs,R_vec)) %>% 
          dplyr::rename(!!model_names[i]:= R_vec) %>%
          full_join(R_melt, by= "R_yrs")
        
        sel_full <- as.numeric(unlist(read.temp$agesel))
        SEL<- matrix(sel_full, byrow = FALSE, nrow = nages, ncol = length(read.temp$agesel[1,]), 
                     dimnames= list(c(ages), c(sellabs)))
        sel_new <- SEL %>% as.data.frame() %>%
          rownames_to_column("Age") %>%
          pivot_longer(-Age, names_to = "Survey", values_to= "Values")  # combo of transpose and melt
        sel_sub <- sel_new %>%
          dplyr::rename(!!model_names[i]:= Values) %>%
          full_join(sel_sub, by= c("Age","Survey"))
        
        F_vec <- as.numeric(unlist(read.temp$t.series["fmort"]))
        F_melt<-data.frame(cbind(yrs,F_vec)) %>% 
          dplyr::rename(!!model_names[i]:= F_vec) %>%
          full_join(F_melt, by= "yrs")
        
        M_full <- as.numeric(unlist(read.temp$M))
        M<- matrix(M_full, byrow = FALSE, nrow = nyrs, ncol = nages, 
                   dimnames= list(c(yrs), c(ages)))
        year_M<-as.numeric(unlist(M[,15]))
        M_melt_yr<-data.frame(cbind(yrs,year_M)) %>% 
          dplyr::rename(!!model_names[i]:= year_M)%>%
          full_join(M_melt_yr, by= "yrs")
        M_new <- M %>% as.data.frame() %>%
          rownames_to_column("Year") %>%
          pivot_longer(-Year, names_to = "Age", values_to= "Values") %>%   # combo of transpose and melt
          mutate(Plot_years= ifelse(Year %in% keep.years, 1, 0),           # keep 
                 Age= as.numeric(Age),
                 Year= as.numeric(Year))
        M_sub <- M_new %>% filter(Plot_years == 1) %>%
          dplyr::select(-Plot_years) %>%
          dplyr::rename(!!model_names[i]:= Values )%>%
          full_join(M_sub, by= c("Year","Age"))
        
        srv1_obs <- as.numeric(unlist(read.temp$obssrv1["obssrv1"]))
        srv1_pred <- as.numeric(unlist(read.temp$obssrv1["predsrv1"]))
        srv3_obs <- as.numeric(unlist(read.temp$obssrv3["obssrv3"]))
        srv3_pred <- as.numeric(unlist(read.temp$obssrv3["predsrv3"]))
        srv5_obs <- as.numeric(unlist(read.temp$obssrv5["obssrv5"]))
        srv5_pred <- as.numeric(unlist(read.temp$obssrv5["predsrv5"]))
        srv7_obs <- as.numeric(unlist(read.temp$obssrv7["obssrv7"]))
        srv7_pred <- as.numeric(unlist(read.temp$obssrv7["predsrv7"]))
        
        srv1_melt<-data.frame(cbind(srv1.yrs,srv1_obs,srv1_pred)) %>% 
          dplyr::rename(!!model_names[i]:= srv1_pred) %>%
          full_join(srv1_melt, by= "srv1.yrs")
        srv3_melt<-data.frame(cbind(srv3.yrs,srv3_obs,srv3_pred)) %>% 
          dplyr::rename(!!model_names[i]:= srv3_pred)%>%
          full_join(srv3_melt, by= "srv3.yrs")
        srv5_melt<-data.frame(cbind(srv5.yrs,srv5_obs,srv5_pred)) %>% 
          dplyr::rename(!!model_names[i]:= srv5_pred)%>%
          full_join(srv5_melt, by= "srv5.yrs")
        srv7_melt<-data.frame(cbind(srv7.yrs,srv7_obs,srv7_pred)) %>% 
          dplyr::rename(!!model_names[i]:= srv7_pred)%>%
          full_join(srv7_melt, by= "srv7.yrs")

        temp.rep<-readLines(paste0(dir_input, "\\sable", i ,".rep"))
        temp.par<-readLines(paste0(dir_input, "\\tem", i ,".par"))
        mod.nam.temp<-model_names[i]
        B40<-round(as.numeric(unlist(strsplit(temp.rep[grep("B_40",temp.rep)+1],split=" "))),digits=2) #Get B40 from report file
        
        B40_vec<-rep(B40,times=nyrs) 
        B40_melt1<- data.frame(cbind(yrs,B40_vec)) %>%
          dplyr::rename(!!model_names[i]:= B40_vec)%>%
          full_join(B40_melt1, by= "yrs")
        
        SSB_vec <- as.numeric(unlist(read.temp$t.series["spbiom"]))
        SSB_melt<-data.frame(cbind(yrs,SSB_vec)) %>% 
          dplyr::rename(!!model_names[i]:= SSB_vec) %>%
          full_join(SSB_melt, by= "yrs")
        
        num_par<-as.numeric(unlist(strsplit(temp.rep[grep("Number parameters estimated",temp.rep)+1],split=" "))) #Get B40 from report file
        F40<-round(as.numeric(unlist(strsplit(temp.rep[grep("F_40",temp.rep)+1],split=" "))), digits=2) 
        F40<-F40[1]
        F.ABC<-round(as.numeric(unlist(strsplit(temp.rep[grep("F_ABC",temp.rep)+1],split=" "))), digits=2)  
        F.ABC<-F.ABC[1]
        ABC<-round(as.numeric(unlist(strsplit(temp.rep[grep("ABC for",temp.rep)+1],split=" "))), digits=2)  
        ABC<-ABC[3] # first 2 are the F_ABC values, this grabs ABC for first projection year
        mean.rec<-round(as.numeric(unlist(strsplit(temp.rep[grep("Mean_Recruitment",temp.rep)+1],split=" "))), digits=2)  
        mean.rec<-mean.rec[1]
        total_lik<-as.numeric(unlist(strsplit(temp.rep[grep("Total likelihood",temp.rep)+1],split=" "))) 
        max_grad<-unlist(strsplit(temp.par[grep("Maximum gradient",temp.par)],split=" "))
        max_grad<-as.numeric(max_grad[length(max_grad)]) # grab the max grad value which is the last value of first line of par file, and turn into numeric value
        term_SSB_tem<-round(as.numeric(unlist(read.temp$t.series["spbiom"])), digits=2) 
        term_SSB<-term_SSB_tem[nyrs]
        B_B40<-round(term_SSB/B40, digits=2) 
        term_F_tem<-round(as.numeric(unlist(read.temp$t.series["fmort"])), digits=2) 
        term_F<-term_F_tem[nyrs]
        F_F40<-round(term_F/F40, digits=2) 
        likes<-as.numeric(unlist(read.temp$likecomp))
        total_lik<-likes[23]
        total_lik<-round(total_lik,digits=2)
        like<-likes[-c(5,8,22,23)]
        model_conv<-file.exists(paste0(dir_input,"\\tem",i,".std"))
        
        Model_summ<-data.frame(cbind(mod.nam.temp,as.character(model_conv), max_grad,total_lik,num_par)) %>%
          full_join(Model_summ,by=c("mod.nam.temp","V2", "max_grad","total_lik","num_par"))
        
        B40_melt<-data.frame(cbind(mod.nam.temp,term_SSB,B40, B_B40,mean.rec, term_F,F40,F_F40, F.ABC, ABC)) %>%
          full_join(B40_melt,by=c("mod.nam.temp","term_SSB","B40", "B_B40","mean.rec","term_F", "F40","F_F40", "F.ABC", "ABC"))
       
      } # end else i=1
      
      R_melt_new <- R_melt %>% 
        pivot_longer(-R_yrs, names_to= "Model_Name", values_to= "R_values") %>% 
        as.data.frame()
      
      F_melt_new <- F_melt %>% 
        pivot_longer(-yrs, names_to= "Model_Name", values_to= "F_values") %>% 
        as.data.frame()
      
      M_melt_new <- M_melt_yr %>% 
        pivot_longer(-yrs, names_to= "Model_Name", values_to= "M_values") %>% 
        as.data.frame()
      
      M_melt_age <- M_sub %>% 
        pivot_longer(-c(Age,Year), names_to= "Model_Name", values_to= "M_values") %>% 
        as.data.frame()
      M_melt_age$Year<-factor(M_melt_age$Year, levels= keep.years)
   
      sel_melt <- sel_sub %>% 
        pivot_longer(-c(Age,Survey), names_to= "Model_Name", values_to= "sel_values") %>% 
        mutate(Age= as.numeric(Age)) %>%
        as.data.frame()

      srv1_melt_new <- srv1_melt %>% 
        pivot_longer(-srv1.yrs, names_to= "Model_Name", values_to= "Srv_values") %>% 
        as.data.frame()
      
      srv3_melt_new <- srv3_melt %>% 
        pivot_longer(-srv3.yrs, names_to= "Model_Name", values_to= "Srv_values") %>% 
        as.data.frame()
      
      srv5_melt_new <- srv5_melt %>% 
        pivot_longer(-srv5.yrs, names_to= "Model_Name", values_to= "Srv_values") %>% 
        as.data.frame()
      
      srv7_melt_new <- srv7_melt %>% 
        pivot_longer(-srv7.yrs, names_to= "Model_Name", values_to= "Srv_values") %>% 
        as.data.frame()
      
      B40_melt_new <- B40_melt1 %>% 
        pivot_longer(-yrs, names_to= "Model_Name", values_to= "B40_values") %>% 
        as.data.frame()
        
      SSB_melt_new <- SSB_melt %>% 
        pivot_longer(-yrs, names_to= "Model_Name", values_to= "SSB_values") %>% 
        as.data.frame()
      
      SSB_melt_new1<-B40_melt_new %>% full_join(SSB_melt_new, by= c("yrs","Model_Name")) 
      
      #SSB_melt_new$B40<-B40_melt1

  
      #---------------------------------------------------------------------------------
      # Likelihood Components graphed independently for each model run
      # 
      #---------------------------------------------------------------------------------
      
      par(omi=c(0,0,0,0),mgp=c(2.5,1,0),mai=c(0.8,0.7,0.1,0.1))
      layout(matrix(c(1,1,1,1,1,1,2,2,3,3), 10, 1, byrow = TRUE))
      
      like_names<-c("LL Fish Age","LL Srv Age", "LL Fish Size_F","LL Fish Size_M","TRWL Fish Size_F","TRWL Fish Size_M",
                    "LL Srv Size_F","LL Srv Size_M","Coop Srv Size_F","Coop Srv Size_M", "TRWL Srv Size_F","TRWL Srv Size_M",
                    "LL Srv RPW","Coop Srv RPW","LL Srv RPN","Coop Srv RPN","LL CPUE RPN", "JPN CPUE RPN", "Trawl Survey RPW","Catch",
                    "Recruit_Pen","F_Pen","M_Prior")           
      barplot(like,names.arg=like_names,
              col=c(rep("red",times=2),rep("darkolivegreen",times=10),rep("blue",times=4),rep("deeppink",times=3),"cyan",rep("blueviolet", times=3)),cex.names=.95,las=2)
      legend("topright",ncol=2, c("Age Comps", "Length Comps","Survey Indices","CPUE","Catch","Penalties"), 
             col = c("red","darkolivegreen","blue","deeppink","cyan","blueviolet"),lwd=2,cex=1)
      title("Component Contributions to Objective Function",line=1)
      
      summary<-matrix(c(mod.nam.temp,as.character(model_conv), max_grad,total_lik,num_par),ncol=5,byrow=TRUE) 
      colnames(summary)<-c("Model","Converged?","Max Grad","nLL","# Pars")
      summary<-as.table(summary)
      textplot(summary,show.rownames=FALSE, valign="top",halign="center",cex=1.1)
      
      summary1<-matrix(round(c( term_SSB,B40, B_B40,mean.rec, term_F, F40,F_F40, F.ABC, ABC),digits=2),ncol=9,byrow=TRUE) 
      colnames(summary1)<-c(paste0(endyear," SSB (kt)"),
                            "SSB_40 (kt)",paste0(endyear," SSB/SSB_40"),"Mean_Recruit", paste0(endyear," F"),"F_40", paste0(endyear," F/F_40"),"F_ABC",paste0(endyear+1," ABC (kt)"))
      summary1<-as.table(summary1)
      textplot(summary1,show.rownames=FALSE, valign="top",halign="center",cex=0.9)
      
      
    } # end read in dat_files
    
    
    
    colnames(Model_summ)<-c("Model","Converged?","Max Grad","nLL","# Pars")
    colnames(B40_melt)<-c("Model",paste0(endyear," SSB (kt)"),
                          "SSB_40 (kt)",paste0(endyear," SSB/SSB_40"),"Mean_Recruit", paste0(endyear," F"),"F_40", paste0(endyear," F/F_40"),"F_ABC",paste0(endyear+1," ABC (kt)"))
   
    model_names_temp <- as.factor(model_names)
    
    
    
    
    plot.new()
    layout(matrix(c(1,2), 2, 1, byrow = TRUE))
    Model_summ <- Model_summ %>% arrange(factor(Model, levels= model_names_temp))
    B40_melt<-B40_melt %>% arrange(factor(Model, levels= model_names_temp))
    
    grid.table(Model_summ, theme=ttheme_default(base_size=7.75),rows=NULL) 
    #ggsave(paste0(dir_output , "//nll Table.png"),width=8,height=5,dpi=300) #plot=grid.table(Model_summ, theme=ttheme_default(base_size=7.75),rows=NULL)
    
    grid.newpage()
    grid.table(B40_melt,theme=ttheme_default(base_size=6.25), rows=NULL)
    
    #---------------------------------------------------------------------------------
    # SSB
    #
    #---------------------------------------------------------------------------------
    
    SSB_plot <- ggplot(SSB_melt_new1,aes(x=yrs,y= SSB_values, group= Model_Name, color= Model_Name)) +
      geom_line(lwd=1.)+
      geom_line(aes(y=B40_values, group= Model_Name, color= Model_Name),linetype=2,lwd=1.) +
      #geom_hline(SSB_melt_new1,aes(x=yrs,y= B40_values))+ #, group= Model_Name, color= Model_Name, type= Model_Name))+
      ylim(0,1.1*max(SSB_melt_new$SSB_values))+
      ylab("SSB (kt)")+  
      scale_x_continuous(breaks = seq(styear,endyear, by = 5))+
      xlab("Year")+
      ggtitle("SSB (kt) Comparison") +
      annotate(geom= "text", label= "B40", x= endyear-1, y= (max(SSB_melt_new1$B40_values, na.rm=T)*1.075), size=6)+ 
      theme(legend.text=element_text(size=6))+ scale_fill_jco()+ scale_color_jco()
    #SSB_plot  
    ggsave(paste0(dir_output , "//SSB_Comp.png"),width=8,height=5,dpi=300,plot=SSB_plot)
    
    SSB_plot2 <- ggplot(SSB_melt_new1,aes(x=yrs,y= SSB_values, group= Model_Name, color= Model_Name)) +
      geom_line(lwd=1.)+
      #geom_line(aes(y=B40_values, group= Model_Name, color= Model_Name),linetype=2,lwd=1.5) +
      #geom_hline(SSB_melt_new1,aes(x=yrs,y= B40_values))+ #, group= Model_Name, color= Model_Name, type= Model_Name))+
      ylim(0,1.1*max(SSB_melt_new$SSB_values))+
      ylab("SSB (kt)")+  
      scale_x_continuous(breaks = seq(styear,endyear, by = 5))+
      xlab("Year")+
      ggtitle("SSB (kt) Comparison") +
      #annotate(geom= "text", label= "B40", x= endyear-1, y= (max(SSB_melt_new1$B40_values, na.rm=T)*1.075), size=6)+ 
      theme(legend.text=element_text(size=6))+ scale_fill_jco()+ scale_color_jco()
    #SSB_plot  
    ggsave(paste0(dir_output , "//SSB_Comp_alt_no_B40.png"),width=8,height=5,dpi=300,plot=SSB_plot2)
    #---------------------------------------------------------------------------------
    # Recruitment
    #
    #---------------------------------------------------------------------------------
    
    R_plot <- ggplot(R_melt_new,aes(x=R_yrs,y= R_values, group= Model_Name, color= Model_Name, type= Model_Name)) +
      geom_line(lwd=1.)+
      ylim(0,1.05*max(R_melt_new$R_values))+
      ylab("Recruitment (Millions of Fish)")+  
      scale_x_continuous(breaks = seq(styear-2,endyear-3, by = 5))+
      xlab("Year Class")+
      ggtitle("Recruitment (Millions of Fish) Comparison")+ 
      theme(legend.text=element_text(size=6))+ scale_fill_jco()+ scale_color_jco()
    #R_plot  
    ggsave(paste0(dir_output , "//Recruit_Comp.png"),width=8,height=5,dpi=300,plot=R_plot)
    
    
    #---------------------------------------------------------------------------------
    # Fully Selected Fishing MORTALITY RATE
    #---------------------------------------------------------------------------------
    
    par(omi=c(0,0.1,0,0.25),mgp=c(2.5,1,0))
    F_plot <- ggplot(F_melt_new,aes(x=yrs,y= F_values, group= Model_Name, color= Model_Name, type= Model_Name)) +
      geom_line(lwd=1.)+
      ylim(0,1.1*max(F_melt_new$F_values))+
      ylab("F")+  
      scale_x_continuous(breaks = seq(styear,endyear, by = 5))+
      xlab("Year")+
      ggtitle("Fully Selected Fishing Mortality Comparison")+ 
      theme(legend.text=element_text(size=6))+ scale_fill_jco()+ scale_color_jco()
    #F_plot  
    ggsave(paste0(dir_output , "//Fully_Selected_F_Comp.png"),width=8,height=5,dpi=300,plot=F_plot)
    

    #---------------------------------------------------------------------------------
    # Natural MORTALITY RATE
    #
    #---------------------------------------------------------------------------------
    
    par(omi=c(0,0.1,0,0.25),mgp=c(2.5,1,0))
    
    M_plot_yr <- ggplot(M_melt_new,aes(x=yrs,y= M_values, group= Model_Name, color= Model_Name, type= Model_Name)) +
      geom_line(lwd=1.)+
      ylab("M at Age-16")+ 
      ylim(0,1.1*max(M_melt_new$M_values))+
      scale_x_continuous(breaks = seq(styear,endyear, by = 5))+
      xlab("Year")+
      ggtitle("Natural Mortality Comparison by Year")+ 
      theme(legend.text=element_text(size=6))+ scale_fill_jco()+ scale_color_jco()
    ggsave(paste0(dir_output , "//M_Year_Comp.png"),width=8,height=5,dpi=300,plot=M_plot_yr)
    
    
    M_plot_age <- ggplot(M_melt_age,aes(x=Age,y= M_values,  color= Model_Name, linetype= Year)) +
      geom_line(lwd=1.)+
      ylab("M")+ 
      ylim(0,1.1*max(M_melt_age$M_values))+
      scale_x_continuous(breaks = seq(2,nages, by = 4))+
      xlab("Age")+
      ggtitle("Natural Mortality Comparison by Age")+ 
      theme(legend.text=element_text(size=6))+ scale_fill_jco()+ scale_color_jco()
    ggsave(paste0(dir_output , "//M_Age_Comp.png"),width=8,height=5,dpi=300,plot=M_plot_age)
    
    #---------------------------------------------------------------------------------
    # Selectivity
    #
    #---------------------------------------------------------------------------------
    
    par(omi=c(0,0.1,0,0.25),mgp=c(2.5,1,0))
    
    
    SEL_plot <- ggplot(sel_melt,aes(x=Age,y= sel_values, group= Model_Name, color= Model_Name)) +
      geom_line(lwd=1.)+
      facet_wrap(.~Survey)+
      ylab("Selectivity")+ 
      ylim(0,1.01)+
      #scale_x_continuous(breaks = seq(2,nages, by = 4))+
      xlab("Age")+
      ggtitle("Selectivity Comparison")+ 
      theme(legend.text=element_text(size=6))+ scale_fill_jco()+ scale_color_jco()
    ggsave(paste0(dir_output , "//Selectivity_Comp.png"),plot=SEL_plot) #width=8,height=8,dpi=600,
    
    
    #---------------------------------------------------------------------------------
    # Survey Fits
    #---------------------------------------------------------------------------------
    srvlabs<-c("DOM LL RPW","DOM LL RPN","DOM LL CPUE", "TRAWL RPW" )
    
    par(omi=c(0,0.1,0,0.25),mgp=c(2.5,1,0))
    Srv1_plot <- ggplot(srv1_melt_new,aes(x=srv1.yrs,y= Srv_values, group= Model_Name, color= Model_Name, type= Model_Name)) +
      geom_line(data=  subset(srv1_melt_new, Model_Name %in% model_names),lwd=1.)+
      geom_point(data=  subset(srv1_melt_new, Model_Name %in% "_OBS"),cex=3)+
      ylim(0,1.05*max(srv1_melt_new$Srv_values))+
      ylab("RPW (weight)")+  
      #scale_x_continuous(breaks = seq(styear,endyear, by = 5))+
      xlab("Year")+
      ggtitle("Domestic Longline Survey Relative Population Weight")+ 
      theme(legend.text=element_text(size=6))+ scale_fill_jco()+ scale_color_jco()
    
    #F_plot  
    ggsave(paste0(dir_output , "//DOM_LL_RPW_Comp.png"),width=8,height=5,dpi=300,plot=Srv1_plot)
    
    Srv3_plot <- ggplot(srv3_melt_new,aes(x=srv3.yrs,y= Srv_values, group= Model_Name, color= Model_Name, type= Model_Name)) +
      geom_line(data=  subset(srv3_melt_new, Model_Name %in% model_names),lwd=1.)+
      geom_point(data=  subset(srv3_melt_new, Model_Name %in% "_OBS"),cex=3)+
      ylim(0,1.05*max(srv3_melt_new$Srv_values))+
      ylab("RPN (numbers)")+  
      #scale_x_continuous(breaks = seq(styear,endyear, by = 5))+
      xlab("Year")+
      ggtitle("Domestic Longline Survey Relative Population Numbers")+ 
      theme(legend.text=element_text(size=6))+ scale_fill_jco()+ scale_color_jco()
    
    #F_plot  
    ggsave(paste0(dir_output , "//DOM_LL_RPN_Comp.png"),width=8,height=5,dpi=300,plot=Srv3_plot)
    
    
    Srv5_plot <- ggplot(srv5_melt_new,aes(x=srv5.yrs,y= Srv_values, group= Model_Name, color= Model_Name, type= Model_Name)) +
      geom_line(data=  subset(srv5_melt_new, Model_Name %in% model_names),lwd=1.)+
      geom_point(data=  subset(srv5_melt_new, Model_Name %in% "_OBS"),cex=3)+
      ylim(0,1.05*max(srv5_melt_new$Srv_values))+
      ylab("CPUE")+  
      #scale_x_continuous(breaks = seq(styear,endyear, by = 5))+
      xlab("Year")+
      ggtitle("Domestic Longline Fishery CPUE")+ 
      theme(legend.text=element_text(size=6))+ scale_fill_jco()+ scale_color_jco()
    
    #F_plot  
    ggsave(paste0(dir_output , "//DOM_LL_CPUE_Comp.png"),width=8,height=5,dpi=300,plot=Srv5_plot)
    
    Srv7_plot <- ggplot(srv7_melt_new,aes(x=srv7.yrs,y= Srv_values, group= Model_Name, color= Model_Name, type= Model_Name)) +
      geom_line(data=  subset(srv7_melt_new, Model_Name %in% model_names),lwd=1.)+
      geom_point(data=  subset(srv7_melt_new, Model_Name %in% "_OBS"),cex=3)+
      ylim(0,1.05*max(srv7_melt_new$Srv_values))+
      ylab("Biomass (kt)")+  
      #scale_x_continuous(breaks = seq(styear,endyear, by = 5))+
      xlab("Year")+
      ggtitle("Trawl Survey Biomass (kt)")+ 
      theme(legend.text=element_text(size=6))+ scale_fill_jco()+ scale_color_jco()
    
    #F_plot  
    ggsave(paste0(dir_output , "//Trawl_Survey_Bio_Comp.png"),width=8,height=5,dpi=300,plot=Srv7_plot)
    
    
    print(SSB_plot) # SSB plot
    print(SSB_plot2) # SSB plot
    
    print(R_plot) # R plot
    
    print(F_plot) # F plot
    print(M_plot_yr) # M Year plot
    print(M_plot_age) # M Age plot
    print(SEL_plot) # Selectivity plot
    
    grid.arrange(Srv1_plot,Srv3_plot,Srv5_plot,Srv7_plot,nrow=2)
    print(Srv1_plot)
    print(Srv3_plot)
    print(Srv5_plot)
    print(Srv7_plot)
    
  dev.off() # turn off recording of plots
  
  plot.new()
  png(paste0(dir_output , "//nll Table.png"),res=300,width=8,height=5, units="in")
  layout(matrix(c(1,2), 2, 1, byrow = TRUE))
  grid.table(Model_summ, theme=ttheme_default(base_size=7.75),rows=NULL) 
  dev.off()
  
  #ggsave(paste0(dir_output , "//nll Table.png"),width=8,height=5,dpi=300) #plot=grid.table(Model_summ, theme=ttheme_default(base_size=7.75),rows=NULL)
  
  plot.new()
  png(paste0(dir_output , "//B40 Table.png"),width=8,height=5,res=300, units="in")
  layout(matrix(c(1,2), 2, 1, byrow = TRUE))
  grid.table(B40_melt,theme=ttheme_default(base_size=6.25), rows=NULL)
  dev.off()
  dev.off()
  }


