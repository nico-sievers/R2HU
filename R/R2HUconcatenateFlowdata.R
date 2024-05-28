#' @title Function to concatenate raw Flow Cytometry data into the standard KOSMOS data sheet layout
#'
#' @description \code{-BETA-}   Ask Nico if you would like to use this function.
#'
#' @param working_directory,rawfile_folder,rawfile_identifier,writetofile,concatenatedfile_name,writetoexcel,excelfile_name,excelfile_sheet,apply_chlacalibration,setsinsupergroups,apply_lengthcalibration \code{(to be updated)}
#'
#' @return Yields a clean data frame of the standard layout and writes to files if selected.
#'
## @examples
## KOSMOSconcatenateFlowdata()
#'
#' @export
#'
#' @importFrom utils View write.csv
#' @importFrom dplyr bind_rows
#' @importFrom readr read_csv
#' @importFrom openxlsx loadWorkbook writeData saveWorkbook
#' @importFrom readxl read_excel
#' @importFrom stats lm


# for debugging:
# working_directory="./";rawfile_folder="export/set-statistics/";rawfile_identifier="set_statistics_";include_easyclus_metadata=F;easyclus_metadata_filename="KOSMOS_Kiel_spring_2024_metadata-cleaned-EasyClus.csv";writetofile=T;concatenatedfile_name="concatenated_clean_data.csv";writetoexcel=F;excelfile_name="KOSMOS_Kiel_spring_2024_FlowCytometry_Sievers_R.xlsx";excelfile_sheet="Main table";apply_chlacalibration=F;setsinsupergroups=F;apply_lengthcalibration=F

# working_directory="../../FlowCytometry/";include_easyclus_metadata=T;concatenatedfile_name="newfunctionconcatenate.csv";writetoexcel=T;excelfile_name="KOSMOS_Kiel_spring_2024_FlowCytometry_Sievers_R - Copy.xlsx";apply_chlacalibration=T;setsinsupergroups=T;apply_lengthcalibration=T


R2HUconcatenateFlowdata=function(working_directory="./",

                                 rawfile_folder="export/set-statistics/",
                                 rawfile_identifier="set_statistics_",

                                 include_easyclus_metadata=FALSE,
                                 easyclus_metadata_filename="KOSMOS_Kiel_spring_2024_metadata-cleaned-EasyClus.csv",

                                 writetofile=TRUE,
                                 concatenatedfile_name="concatenated_clean_data.csv",

                                 writetoexcel=FALSE,
                                 excelfile_name="KOSMOS_Kiel_spring_2024_FlowCytometry_Sievers_R.xlsx",
                                 excelfile_sheet="Main table",

                                 apply_chlacalibration=FALSE,
                                 exclude_day_from_biomass=NULL,
                                 setsinsupergroups=FALSE,
                                 apply_lengthcalibration=FALSE){



  # assemble treatment combinations
  if(T){
    num_mesos=13
    mesos=c(paste("M",1:(num_mesos-1),sep=""),"Fjord")
    Mineral=c(rep(c("Ca(OH)2","Mg(OH)2"),num_mesos/2),NA)
    Delta_TA=c(450, 750, 0, 300, 600, 0, 150, 450, 750, 150, 300, 600, NA)
    treatment_combinations=array(c(mesos,Mineral,Delta_TA),
                                 dim=c(num_mesos,3),
                                 dimnames=list(1:num_mesos,c("Mesocosm","Mineral","Delta_TA")))
  }


  available_files=list.files(paste0(working_directory,rawfile_folder),rawfile_identifier)

  listofDF=NULL;i=0;for(filename in available_files) {
    newdata=read_csv(paste0(working_directory,rawfile_folder,filename))
    i=i+1;listofDF[[i]]=newdata
  }
  concatenated_data=bind_rows(listofDF)


  # read in more metadata from easyclus
  if(include_easyclus_metadata){
    metadata=read.csv(paste0(working_directory,easyclus_metadata_filename))
    concatenated_data=merge(concatenated_data,metadata,by="Filename",by.y="fname",all=T)
    rm(original_columns,new_columns,metadata)
  }


  # disect the filenames to fill meta data columns
  if(T){
    tmp=strsplit(concatenated_data$Filename," ") #split at spaces

    tmp_beginning=sapply(tmp, "[[", 1) #store first part
    concatenated_data$Date=sapply(tmp, "[[", 2) #use middle part as date
    tmp_end=sapply(tmp, "[[", 3) #store last part

    concatenated_data$Time=sapply(strsplit(tmp_end,".",fixed=T), "[[", 1) #extract the time from the last part

    tmp_beginning=strsplit(tmp_beginning,"_") #split first part at underscores

    concatenated_data$Settings=sapply(tmp_beginning, "[[", 5) #use first part as setting
    tmp_day=sapply(tmp_beginning, "[[", 6) #store second part to get day number
    tmp_mesocosm=sapply(tmp_beginning, "[[", 7) #store last part to get mesocosm number

    concatenated_data$Day=gsub("T", "", tmp_day) #use only the number of the day
    concatenated_data$Mesocosm=gsub("M", "", tmp_mesocosm) #use only the number of the mesocosm
    concatenated_data$Mesocosm_Name=tmp_mesocosm #alt: use the whole "mesocosm"-string for side experiments

    rm(tmp,tmp_beginning,tmp_day,tmp_end,tmp_mesocosm)


    #assign treatments to the mesocosm number
    concatenated_data$Mineral=NA
    concatenated_data$Delta_TA=NA
    for(meso in treatment_combinations[,"Mesocosm"]){
      concatenated_data$Mineral[concatenated_data$Mesocosm_Name==meso]=treatment_combinations[treatment_combinations[,"Mesocosm"]==meso,"Mineral"]
      concatenated_data$Delta_TA[concatenated_data$Mesocosm_Name==meso]=treatment_combinations[treatment_combinations[,"Mesocosm"]==meso,"Delta_TA"]
    }
    rm(meso)
    concatenated_data$Treatment=paste(concatenated_data$Delta_TA,"/",concatenated_data$Mineral)
    concatenated_data$Treat_Meso=paste(concatenated_data$Treatment,"/",concatenated_data$Mesocosm_Name)
    concatenated_data$Treat_Meso[is.na(concatenated_data$Mineral)]=concatenated_data$Mesocosm_Name[is.na(concatenated_data$Mineral)]


    #rearrange columns according to the future excel sheet
    ncols=ncol(concatenated_data)
    concatenated_data=concatenated_data[,c(ncols-6,ncols-5,(ncols-3):ncols,ncols-7,2,3:(ncols-8),ncols-4,1),]
    #,ncols-5,ncols-3,ncols-2,ncols-1,
    rm(ncols)
  }

  # fix datastructure - only what is necessary here
  names(concatenated_data)[11]="Concentration [n/\u00b5l]"
  concatenated_data$Day=factor(concatenated_data$Day,c(1:3,seq(5,33,2)))
  concatenated_data$Delta_TA=as.integer(concatenated_data$Delta_TA)
  concatenated_data$Set=sub("/","_",concatenated_data$Set) #replace any "/" that stupid me used in set names

  #check that there are no duplicates
  if(T){
    tmp=paste(concatenated_data$Filename,concatenated_data$Set,concatenated_data$Settings)
    if(nrow(concatenated_data) != length(unique(tmp))){
      warning("Duplicates in the concatenated data. Check individual export files for overlap!")
      View(concatenated_data[duplicated(tmp),])
    }
    rm(tmp)
  }


  # the various Chla proxies, their references, and normalisations
  if(T){
    # use meanRED*conc to account for volume changes
    concatenated_data$ChlaProxyRaw=concatenated_data$'Concentration [n/\u00b5l]'*concatenated_data$`Mean FL Red Total`
    concatenated_data$ChlaProxyRaw[concatenated_data$'Concentration [n/\u00b5l]'==0]=0

    # exclude a day (T1 for kiel spring) systematically from biomass calculations
    concatenated_data$ChlaProxyRaw[concatenated_data$Day %in% exclude_day_from_biomass]=NA

    if(apply_chlacalibration){
      ### calibrate Chla proxy to other data set

      ###source("./scripts/calibrate_chla.R")
      if(T){
        chla_filter=read_excel(paste0(working_directory,"KOSMOS_Kiel_spring_2024_Chlorophyll_a_25_03_2024_JT.xlsx"),sheet = "Main table",na=c("",NA))
        chla_filter$Day=factor(chla_filter$Day,c(1:3,seq(5,33,2)))
        chlcal=chla_filter


        if(T){
          concatenated_data=left_join(concatenated_data,chlcal)

          colnr=ncol(concatenated_data)
          tocalibrate=concatenated_data[concatenated_data$Set=="All cells" & concatenated_data$Mesocosm!="Fjord",c(1:11,(colnr-15):colnr)]
          names(tocalibrate)[names(tocalibrate)=="Chlorophyll a (\u00b5g/L)"]="ChlorophyllA" # only rename because of the linear models below

          tocalibratelarge=tocalibrate[tocalibrate$Settings=="large",]
          tocalibratesmall=tocalibrate[tocalibrate$Settings=="small",]

          lmlarge=lm(ChlaProxyRaw~ChlorophyllA,tocalibratelarge)
          lmsmall=lm(ChlaProxyRaw~ChlorophyllA,tocalibratesmall)

          concatenated_data$ChlaProxyCalibrated=NA
          #y=mx+t
          #x=(y-t)/m
          concatenated_data$ChlaProxyCalibrated[concatenated_data$Settings=="large"]=(concatenated_data$ChlaProxyRaw[concatenated_data$Settings=="large"]-lmlarge$coefficients[[1]])/lmlarge$coefficients[[2]]
          concatenated_data$ChlaProxyCalibrated[concatenated_data$Settings=="small"]=(concatenated_data$ChlaProxyRaw[concatenated_data$Settings=="small"]-lmsmall$coefficients[[1]])/lmsmall$coefficients[[2]]

          rm(colnr,tocalibrate,tocalibratelarge,tocalibratesmall,lmlarge,lmsmall)
        }

        # QC
        if(T){
          #
          # KOSMOStimeplot(chla_filter,"Chlorophyll a (\u00b5g/L)")
          # KOSMOStimeplot(concatenated_data[concatenated_data$Settings=="small" & concatenated_data$Set=="All cells",],"ChlaProxyRaw")
          # KOSMOStimeplot(concatenated_data[concatenated_data$Settings=="large" & concatenated_data$Set=="All cells",],"ChlaProxyRaw")
          # KOSMOStimeplot(concatenated_data[concatenated_data$Settings=="small" & concatenated_data$Set=="All cells",],"ChlaProxyCalibrated")
          # KOSMOStimeplot(concatenated_data[concatenated_data$Settings=="large" & concatenated_data$Set=="All cells",],"ChlaProxyCalibrated")
          #
          # plot(ChlaProxy~'Chlorophyll a (\u00b5g/L)',tocalibratelarge)
          # abline(lmlarge)
          # plot(ChlaProxy~'Chlorophyll a (\u00b5g/L)',tocalibratesmall)
          # abline(lmsmall)
          #
          # library(ggplot2)
          # ggplot(tocalibratelarge,aes('Chlorophyll a (\u00b5g/L)',ChlaProxy,col=Day))+
          #   geom_point()+
          #   labs(title="large",col="Day")
          # ggplot(tocalibratesmall,aes('Chlorophyll a (\u00b5g/L)',ChlaProxy,col=Day))+
          #   geom_point()+
          #   labs(title="small",col="Day")
          #
          # ggplot(tocalibratesmall,aes('Chlorophyll a (\u00b5g/L)',ChlaProxy,col=`% of total`))+
          #   geom_point()+
          #   labs(title="small",col="Day")
          # ggplot(tocalibratesmall,aes(`% of total`,ChlaProxy,col=Day))+
          #   geom_point()+
          #   labs(title="small",col="Day")
          #
          # smcomp=concatenated_data[concatenated_data$Set=="All cells",c("Day","Mesocosm","Settings","ChlaProxy")]
          # library(tidyr)
          # smcomplong=pivot_wider(smcomp,names_from="Settings",values_from="ChlaProxy")
          #
          # ggplot(smcomplong,aes(small,large,col=Day))+
          #   geom_point()+
          #   labs(title="Correlation of ChlaProxy between large and small setting",col="Day")
          #
          # plot(large~small,smcomplong)
          # abline(lm(large~small,smcomplong))
          #
          #
          # # tocalibratelarge=tocalibratelarge[!(tocalibratelarge$Day %in% c(1,2,3)),]
          # #
          # # tmp=tocalibratelarge[tocalibratelarge$Day %in% c(11),]
          # # points(tmp$'Chlorophyll a (\u00b5g/L)',tmp$ChlaProxy,col="blue")
          # #
          # # #plot(tocalibratelarge$'Chlorophyll a (\u00b5g/L)'+1,tocalibratelarge$ChlaProxy,log="xy")
          # # #plot(tocalibratelarge$'Chlorophyll a (\u00b5g/L)',tocalibratelarge$ChlaProxy)
          # # points(tocalibratelarge$'Chlorophyll a (\u00b5g/L)',tocalibratelarge$ChlaProxy,col="blue")
          # # soos=lm(tocalibratelarge$ChlaProxy~tocalibratelarge$'Chlorophyll a (\u00b5g/L)')
          # # abline(soos)
          # # # die fragwürdigen
          # # frage=tocalibratelarge[tocalibratelarge$'Chlorophyll a (\u00b5g/L)'>3.75 & tocalibratelarge$'Chlorophyll a (\u00b5g/L)'<5 & tocalibratelarge$ChlaProxy<10000,]
          # # points(frage$'Chlorophyll a (\u00b5g/L)',frage$ChlaProxy,col="red")
          #
          #
          #
          #
          # #als Kosmosplot
          # library(KOSMOSplotR)
          # meem=concatenated_data[concatenated_data$Settings=="large" & concatenated_data$Set=="All cells",]
          # meem$chladivider=meem$ChlaProxy/meem$'Chlorophyll a (\u00b5g/L)'
          # KOSMOStimeplot(meem,"chladivider",ylimit=c(1000,7000))
          #
          # test2=concatenated_data[,c("Filename","Set","Count","Concentration [n/\u00b5l]","Mean FL Red Total","ChlaProxy")]

        }
      }

      # decide whether to work with the raw or the calibrated proxy for all further calculations
      ###concatenated_data$ChlaProxyUse=concatenated_data$ChlaProxyRaw
      concatenated_data$ChlaProxyUse=concatenated_data$ChlaProxyCalibrated
    }else{
      concatenated_data$ChlaProxyUse=concatenated_data$ChlaProxyRaw
    }

    refset="All cells"
    concatenated_data$ChlaProxyRefSet=NA
    concatenated_data$ChlaProxyRefDayMeso=NA
    concatenated_data$ChlaProxyRefSetting=NA
    for(setting in c("small","large")){
      for(set in unique(concatenated_data$Set[concatenated_data$Settings==setting])){
        # by set
        concatenated_data$ChlaProxyRefSet[concatenated_data$Settings==setting & concatenated_data$Set==set]=max(concatenated_data$ChlaProxyUse[concatenated_data$Settings==setting & concatenated_data$Set==set],na.rm=T)
      }
      for(day in unique(concatenated_data$Day)){
        for(meso in unique(concatenated_data$Mesocosm)){
          # by meso and day, but across sets
          concatenated_data$ChlaProxyRefDayMeso[concatenated_data$Settings==setting & concatenated_data$Day==day & concatenated_data$Mesocosm==meso]=concatenated_data$ChlaProxyUse[concatenated_data$Settings==setting & concatenated_data$Day==day & concatenated_data$Mesocosm==meso & concatenated_data$Set==refset]
        }
      }
      # normalised by setting
      concatenated_data$ChlaProxyRefSetting[concatenated_data$Settings==setting]=max(concatenated_data$ChlaProxyRefDayMeso[concatenated_data$Settings==setting],na.rm=T)
    }
    concatenated_data$ChlaProxyContribution=concatenated_data$ChlaProxyUse/concatenated_data$ChlaProxyRefDayMeso
    concatenated_data$ChlaProxyNormalisedSetting=concatenated_data$ChlaProxyUse/concatenated_data$ChlaProxyRefSetting
    concatenated_data$ChlaProxyNormalisedSet=concatenated_data$ChlaProxyUse/concatenated_data$ChlaProxyRefSet

    ### supergroups
    if(setsinsupergroups){
      concatenated_data$Supergroup=NA
      concatenated_data$Supergroup[concatenated_data$Settings=="large" & concatenated_data$Set %in% c("Multi-celled Microphytoplankton","Microphytoplankton (other)")]="Large Microphytoplankton"
      concatenated_data$Supergroup[concatenated_data$Settings=="large" & concatenated_data$Set %in% c("very high green_orange","Cryptophytes","Mid-green-yellow")]="Cryptophyte-like"
      concatenated_data$Supergroup[concatenated_data$Settings=="large" & concatenated_data$Set %in% c("high SSC","Nanophytoplankton (other)","elongated low-red nano")]="Nanophytoplankton"
      # supergroup subtable
      supergrouptable <- concatenated_data %>%
        group_by(`Day`, `Mesocosm`, `Mineral`, `Delta_TA`, `Treat_Meso`, `Supergroup`) %>%
        summarise(CombinedBiomassContribution = sum(ChlaProxyUse),.groups = "keep")# %>%
      #count()
      supergrouptable=supergrouptable[!is.na(supergrouptable$Supergroup),]
      # create all three relationsships
      supergrouprelationtable <- supergrouptable %>%
        group_by(`Day`, `Mesocosm`, `Mineral`, `Delta_TA`, `Treat_Meso`) %>%
        mutate(red_green = CombinedBiomassContribution[Supergroup=="Large Microphytoplankton"]/CombinedBiomassContribution[Supergroup=="Cryptophyte-like"]) %>%
        mutate(green_blue = CombinedBiomassContribution[Supergroup=="Cryptophyte-like"]/CombinedBiomassContribution[Supergroup=="Nanophytoplankton"]) %>%
        mutate(red_blue = CombinedBiomassContribution[Supergroup=="Large Microphytoplankton"]/CombinedBiomassContribution[Supergroup=="Nanophytoplankton"]) %>%
        distinct(red_green,.keep_all = T) %>%
        select(-c(Supergroup,CombinedBiomassContribution))
    }

    #correlate Chla proxy and concentration to see trends in chl/cell
    concatenated_data$FLredpercell=concatenated_data$`Mean FL Red Total`
    concatenated_data$Chlapercell=concatenated_data$ChlaProxyUse/concatenated_data$'Concentration [n/\u00b5l]'

    concatenated_data=subset(concatenated_data,select=-ChlaProxyUse)
  }


  #apply current length calibration
  if(apply_lengthcalibration){
    slope=0.2502
    intercept=2.9872

    concatenated_data$`raw Min FWS Length`=concatenated_data$`Min FWS Length`
    concatenated_data$`Min FWS Length`=(concatenated_data$`raw Min FWS Length`-intercept)/slope
    concatenated_data$`raw Max FWS Length`=concatenated_data$`Max FWS Length`
    concatenated_data$`Max FWS Length`=(concatenated_data$`raw Max FWS Length`-intercept)/slope
    concatenated_data$`raw Mean FWS Length`=concatenated_data$`Mean FWS Length`
    concatenated_data$`Mean FWS Length`=(concatenated_data$`raw Mean FWS Length`-intercept)/slope

    #doesnt work like this, would need to be percentage of original value
    #concatenated_data$`raw SD FWS Length`=concatenated_data$`SD FWS Length`
    #concatenated_data$`SD FWS Length`=(concatenated_data$`raw SD FWS Length`-intercept)/slope

    rm(intercept,slope)
  }


  #export the summary file
  if(writetofile){
    write.csv(concatenated_data,paste0(working_directory,concatenatedfile_name),row.names = FALSE)
  }

  # write to excel if desired
  if(writetoexcel){
    wb <- loadWorkbook(paste0(working_directory,excelfile_name))
    writeData(wb,excelfile_sheet,concatenated_data)
    saveWorkbook(wb,paste0(working_directory,excelfile_name),overwrite = TRUE)
    rm(wb)
  }

  rm(treatment_combinations,available_files,Delta_TA,filename,mesos,Mineral,num_mesos)
  return(concatenated_data)

}
