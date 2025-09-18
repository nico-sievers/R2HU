#' @title Function to concatenate raw Flow Cytometry data into the standard KOSMOS data sheet layout
#'
#' @description \code{-BETA-}   Ask Nico if you would like to use this function.
#'
#' @param working_directory,rawfile_folder,rawfile_identifier,writetofile,concatenatedfile_name,writetoexcel,excelfile_name,excelfile_sheet,apply_chlacalibration,setsinsupergroups,apply_lengthcalibration,include_easyclus_metadata,easyclus_metadata_filename,chlacalfile_name,chlacalfile_sheet,exclude_day_from_biomass,calculate_percent_change,baseline_days,percent_change_vars \code{(to be updated)}
#'
#' @return Yields a clean data frame of the standard layout and writes to files if selected.
#'
## @examples
## KOSMOSconcatenateFlowdata()
#'
#' @export
#'
#' @importFrom utils View write.csv
#' @importFrom dplyr bind_rows group_by mutate ungroup
#' @importFrom readr read_csv
#' @importFrom openxlsx loadWorkbook writeData saveWorkbook
#' @importFrom readxl read_excel
#' @importFrom stats lm


# for debugging:
# working_directory="./";rawfile_folder="export/set-statistics/";rawfile_identifier="set_statistics_";include_easyclus_metadata=F;easyclus_metadata_filename="KOSMOS_Kiel_spring_2024_metadata-cleaned-EasyClus.csv";writetofile=T;concatenatedfile_name="concatenated_clean_data.csv";writetoexcel=F;excelfile_name="KOSMOS_Kiel_spring_2024_FlowCytometry_Sievers_R.xlsx";excelfile_sheet="Main table";apply_chlacalibration=F;chlacalfile_sheet="Main table";setsinsupergroups=F;exclude_day_from_biomass=NULL;apply_lengthcalibration=F;calculate_percent_change=FALSE;baseline_days=1:3;percent_change_vars=c("Concentration [n/\u00b5l]","ChlaProxyRaw");library(utils);library(dplyr);library(readr);library(openxlsx);library(readxl);library(stats);library(KOSMOSplotR)

# working_directory="../../FlowCytometry/";include_easyclus_metadata=T;concatenatedfile_name="newfunctionconcatenate.csv";writetoexcel=T;excelfile_name="KOSMOS_Kiel_spring_2024_FlowCytometry_Sievers_R - Copy.xlsx";apply_chlacalibration=T;setsinsupergroups=T;apply_lengthcalibration=T

# working_directory="../../KOSMOS_2024_autumn_Kiel_FlowCytometry/";writetoexcel=T;excelfile_name="KOSMOS_Kiel_autumn_2024_FlowCytometry_Sievers_R.xlsx";apply_chlacalibration=T;chlacalfile_name="KOSMOS_Kiel_autumn_2024_Chlorophyll_a.xlsx"

# KOSMOSselect("quartz");working_directory="../../KOSMOS_2024_Kiel_Quartz-experiment_FlowCytometry/";writetoexcel=T;excelfile_name="KOSMOS_Kiel_2024_Quartz-side-experiment_FlowCytometry_Sievers_R";apply_chlacalibration=T;chlacalfile_name="KOSMOS_Kiel_2024_Quartz-Side-experiment_Chlorophyll.xlsx"

# KOSMOSselect("PIIP");writetoexcel=T;excelfile_name="OAEPIIP_FlowCytometry_Faucher_R.xlsx"


R2HUconcatenateFlowdata=function(working_directory="./",

                                 rawfile_folder="export/set-statistics/",
                                 rawfile_identifier="set_statistics_",

                                 include_easyclus_metadata=FALSE,
                                 easyclus_metadata_filename="KOSMOS_Kiel_spring_2024_metadata-cleaned-EasyClus.csv",

                                 number_of_name_underscores_total=6

                                 writetofile=TRUE,
                                 concatenatedfile_name="concatenated_clean_data.csv",

                                 writetoexcel=FALSE,
                                 excelfile_name="KOSMOS_Kiel_spring_2024_FlowCytometry_Sievers_R.xlsx",
                                 excelfile_sheet="Main table",

                                 apply_chlacalibration=FALSE,
                                 chlacalfile_name="KOSMOS_Kiel_spring_2024_Chlorophyll_a_25_03_2024_JT.xlsx",
                                 chlacalfile_sheet="Main table",
                                 exclude_day_from_biomass=NULL,
                                 setsinsupergroups=FALSE,

                                 calculate_percent_change=FALSE,
                                 baseline_days=1:3,
                                 percent_change_vars=c("Concentration [n/\u00b5l]","ChlaProxyRaw","FLredpercell"),

                                 apply_lengthcalibration=FALSE){



  # assemble treatment combinations
    # num_mesos=13
    # mesos=c(paste("M",1:(num_mesos-1),sep=""),"Fjord")
    # Mineral=c(rep(c("Ca(OH)2","Mg(OH)2"),num_mesos/2),NA)
    # Delta_TA=c(450, 750, 0, 300, 600, 0, 150, 450, 750, 150, 300, 600, NA)
    # treatment_combinations=array(c(mesos,Mineral,Delta_TA),
    #                              dim=c(num_mesos,3),
    #                              dimnames=list(1:num_mesos,c("Mesocosm","Mineral","Delta_TA")))

    # tmp=KOSMOScurrentStyletable$Mesocosm
    # # need to figure out which order they are in. this is a shit show.
    # tmp=gsub("/","",tmp,)
    # tmp=gsub("  "," ",tmp)
    # tmp=strsplit(tmp," ")
    # #tmp=tmp[lapply(tmp,grepl,pattern="/")]
    # mesopos=grep("^[mM]\\d{1,2}$",tmp[[1]])
    # conpos=grep("^\\d{1,4}$",tmp[[1]][-mesopos])
    # catpos=(1:3)[-c(mesopos,conpos)]
    # Mesocosm=unlist(lapply(tmp,"[",mesopos))
    # CatVar=unlist(lapply(tmp,"[",catpos))
    # ConVar=unlist(lapply(tmp,"[",conpos))
    # treatment_combinations=cbind(Mesocosm,CatVar,ConVar)
    # rm(ConVar,Mesocosm,CatVar)


  available_files=list.files(paste0(working_directory,rawfile_folder),rawfile_identifier)

  listofDF=NULL;i=0;for(filename in available_files) {
    newdata=read_csv(paste0(working_directory,rawfile_folder,filename),show_col_types=F)
    i=i+1;listofDF[[i]]=newdata
  }
  if(i==1){message("One raw file processed.")
  }else if(i>1){message(paste0("Concatenated ",i," raw files."))
  }else{stop("No raw files found!")}

  concatenated_data=bind_rows(listofDF)


  # read in more metadata from easyclus
  if(include_easyclus_metadata){
    metadata=read.csv(paste0(working_directory,easyclus_metadata_filename))
    concatenated_data=merge(concatenated_data,metadata,by="Filename",by.y="fname",all=T)
    rm(original_columns,new_columns,metadata)
  }


  # disect the filenames to fill meta data columns
    tmp=strsplit(concatenated_data$Filename," ") #split at spaces

    tmp_beginning=sapply(tmp, "[[", 1) #store first part
    concatenated_data$Date=sapply(tmp, "[[", 2) #use middle part as date
    tmp_end=sapply(tmp, "[[", 3) #store last part

    concatenated_data$Time=sub("h",":",sapply(strsplit(tmp_end,".",fixed=T), "[[", 1)) #extract the time from the last part


    #### fucked up fix for OAEPIIP filenames
    if(strsplit(tmp_beginning[1],"_")[[1]][1]=="OAEPIIP"){
      tmp_beginning=paste0("place_holder_",tmp_beginning)
    }
    #### fucked up fix for OAEPIIP filenames


    tmp_beginning=strsplit(tmp_beginning,"_") #split first part at underscores

    concatenated_data$Settings=sapply(tmp_beginning, "[[", number_of_name_underscores_total-2 #use first part as setting
    tmp_day=sapply(tmp_beginning, "[[", number_of_name_underscores_total-1) #store second part to get day number
    tmp_mesocosm=sapply(tmp_beginning, "[[", number_of_name_underscores_total) #store last part to get mesocosm number

    concatenated_data$Day=gsub("T", "", tmp_day) #use only the number of the day
    concatenated_data$Mesocosm=gsub("M", "", tmp_mesocosm) #use only the number of the mesocosm
    concatenated_data$Mesocosm_Name=tmp_mesocosm #alt: use the whole "mesocosm"-string for side experiments

    rm(tmp,tmp_beginning,tmp_day,tmp_end,tmp_mesocosm)


    #assign treatments to the mesocosm number
    treatment_combinations=KOSMOScurrentStyletable[,1:4]
    concatenated_data=merge(concatenated_data,treatment_combinations,by.x="Mesocosm",by.y=names(treatment_combinations)[1],all.x=T)


    # concatenated_data[[KOSMOScurrentCategoricalVar]]=NA
    # concatenated_data[[KOSMOScurrentContinuousVar]]=NA
    # for(meso in treatment_combinations[,1]){
    #   concatenated_data[concatenated_data$Mesocosm==meso,KOSMOScurrentCategoricalVar]=treatment_combinations[treatment_combinations[,1]==meso,KOSMOScurrentCategoricalVar]
    #   concatenated_data[concatenated_data$Mesocosm==meso,KOSMOScurrentContinuousVar]=treatment_combinations[treatment_combinations[,1]==meso,KOSMOScurrentContinuousVar]
    # }
    # rm(meso)

    ### this was total crap on my end
    #deal with any control you find by checking what you have in the style cheat
    #controlentries=!(concatenated_data$Mesocosm_Name %in% treatment_combinations$Mesocosm)
    #concatenated_data$Mineral[controlentries]=NA
    #concatenated_data$Delta_TA[controlentries]=NA

    # # paste the rest together based on what you got
    # concatenated_data$Treatment=paste(concatenated_data[[KOSMOScurrentContinuousVar]],"/",concatenated_data[[KOSMOScurrentCategoricalVar]])
    # concatenated_data$Treat_Meso=paste(concatenated_data$Treatment,"/",concatenated_data$Mesocosm_Name)

    ncols=ncol(concatenated_data)
    concatenated_data[is.na(concatenated_data[[ncols]]),ncols]=concatenated_data$Mesocosm_Name[is.na(concatenated_data[[ncols]])]

    #rearrange columns according to the future excel sheet
    #concatenated_data=concatenated_data[,c(ncols-6,ncols-5,(ncols-3):ncols,ncols-7,2,3:(ncols-8),ncols-4,1),]
    concatenated_data=concatenated_data[,c(ncols-4,1,(ncols-2):ncols,ncols-5,3:(ncols-6),ncols-3,2)]
    rm(ncols)

  # fix datastructure - only what is necessary here
  names(concatenated_data)[10]="Concentration [n/\u00b5l]"
  concatenated_data$Day=factor(concatenated_data$Day,levels=sort(as.numeric(unique(concatenated_data$Day))))
  ###concatenated_data[[KOSMOScurrentContinuousVar]]=as.integer(concatenated_data[[KOSMOScurrentContinuousVar]])
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

    # exclude a day (i.e. T1 for kiel spring) systematically from biomass calculations
    concatenated_data$ChlaProxyRaw[concatenated_data$Day %in% exclude_day_from_biomass]=NA

    # get the standard per-cell-proxy
    concatenated_data$FLredpercell=concatenated_data$`Mean FL Red Total`

    if(apply_chlacalibration){
      ### calibrate Chla proxy to other data set

      chlcal=read_excel(paste0(working_directory,chlacalfile_name),sheet=chlacalfile_sheet,na=c("",NA))
      chlcal$Day=factor(chlcal$Day)

      concatenated_data=left_join(concatenated_data,chlcal)

      colnr=ncol(concatenated_data)
      tocalibrate=concatenated_data[concatenated_data$Set=="All cells" & concatenated_data$Mesocosm!="Fjord",c(1:11,(colnr-15):colnr)]
      names(tocalibrate)[length(names(tocalibrate))]="ChlorophyllA" # only rename because of the linear models below

      tocalibratelarge=tocalibrate[tocalibrate$Settings=="large",]
      tocalibratesmall=tocalibrate[tocalibrate$Settings=="small",]

      lmlarge=lm(ChlaProxyRaw~ChlorophyllA,tocalibratelarge)
      lmsmall=lm(ChlaProxyRaw~ChlorophyllA,tocalibratesmall)

      concatenated_data$ChlaProxyCalibrated=NA
      #y=mx+t
      #x=(y-t)/m
      concatenated_data$ChlaProxyCalibrated[concatenated_data$Settings=="large"]=(concatenated_data$ChlaProxyRaw[concatenated_data$Settings=="large"]-lmlarge$coefficients[[1]])/lmlarge$coefficients[[2]]
      concatenated_data$ChlaProxyCalibrated[concatenated_data$Settings=="small"]=(concatenated_data$ChlaProxyRaw[concatenated_data$Settings=="small"]-lmsmall$coefficients[[1]])/lmsmall$coefficients[[2]]

      # QC
      if(T){
        # concatenated_data=concatenated_data[,c(1:11,(ncol(concatenated_data)-15):ncol(concatenated_data))]

        print(ggplot(tocalibratelarge,aes(ChlorophyllA,ChlaProxyRaw,col=Day))+
          geom_point()+
          geom_abline(slope=lmlarge$coefficients["ChlorophyllA"],intercept=lmlarge$coefficients["(Intercept)"])+
          #?scale_y_continuous()
          labs(title="Chlorophyll calibration - large setting",col="Day"))
        print(ggplot(tocalibratesmall,aes(ChlorophyllA,ChlaProxyRaw,col=Day))+
          geom_point()+
          geom_abline(slope=lmsmall$coefficients["ChlorophyllA"],intercept=lmsmall$coefficients["(Intercept)"])+
          labs(title="Chlorophyll calibration - small setting",col="Day"))
      }

      ## calibrate the two settings agains another
      if(T){
        smcomp=concatenated_data[concatenated_data$Set=="All cells",c("Day","Mesocosm","Settings","ChlaProxyRaw")]
        smcomplong=pivot_wider(smcomp,names_from="Settings",values_from="ChlaProxyRaw")
        lmcomp=lm(large~small,smcomplong)
        print(ggplot(smcomplong,aes(small,large,col=Day))+
          geom_point()+
          geom_abline(slope=lmcomp$coefficients["small"],intercept=lmcomp$coefficients["(Intercept)"])+
          labs(title="Correlation of ChlaProxy between large and small setting",col="Day"))
      }

      rm(colnr,tocalibrate,tocalibratelarge,tocalibratesmall,lmlarge,lmsmall)

      #correlate Chla proxy and concentration to see trends in chl/cell
      concatenated_data$Chlapercell=concatenated_data$ChlaProxyCalibrated/concatenated_data$'Concentration [n/\u00b5l]'
      concatenated_data$Chlapercell[concatenated_data$Count==0]=0

    }
    # decide whether to work with the raw or the calibrated proxy for all further calculations
    ###concatenated_data$ChlaProxyUse=concatenated_data$ChlaProxyRaw
    # concatenated_data$ChlaProxyUse=concatenated_data$ChlaProxyCalibrated
    ###tmp this is only because i quickly fixed the problem of normalising around with the negative values. if this doesnt cause any issues its probably the way to go
    concatenated_data$ChlaProxyUse=concatenated_data$ChlaProxyRaw

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
        group_by(`Day`, `Mesocosm`, .(KOSMOScurrentCategoricalVar), .(KOSMOScurrentContinuousVar), `Treat_Meso`, `Supergroup`) %>%
        summarise(CombinedBiomassContribution = sum(ChlaProxyUse),.groups = "keep")# %>%
      #count()
      supergrouptable=supergrouptable[!is.na(supergrouptable$Supergroup),]
      # create all three relationsships
      supergrouprelationtable <- supergrouptable %>%
        group_by(`Day`, `Mesocosm`, .(KOSMOScurrentCategoricalVar), .(KOSMOScurrentContinuousVar), `Treat_Meso`) %>%
        mutate(red_green = CombinedBiomassContribution[Supergroup=="Large Microphytoplankton"]/CombinedBiomassContribution[Supergroup=="Cryptophyte-like"]) %>%
        mutate(green_blue = CombinedBiomassContribution[Supergroup=="Cryptophyte-like"]/CombinedBiomassContribution[Supergroup=="Nanophytoplankton"]) %>%
        mutate(red_blue = CombinedBiomassContribution[Supergroup=="Large Microphytoplankton"]/CombinedBiomassContribution[Supergroup=="Nanophytoplankton"]) %>%
        distinct(red_green,.keep_all = T) %>%
        select(-c(Supergroup,CombinedBiomassContribution))
    }

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

  # calculate the percent change for some parameters relative to a defined baseline
  if(calculate_percent_change){
    for (var in percent_change_vars) {
      baseline_mean_col <- paste0("baseline_mean_", var)
      percent_change_col <- paste0("percent_change_", var)

      concatenated_data <- concatenated_data %>%
        group_by(Mesocosm, Settings, Set) %>%
        mutate(
          !!baseline_mean_col := mean(.data[[var]][Day %in% baseline_days], na.rm = TRUE)
        ) %>%
        mutate(
          !!percent_change_col := ifelse(
            !is.na(.data[[baseline_mean_col]]),
            (.data[[var]] - .data[[baseline_mean_col]]) / .data[[baseline_mean_col]] * 100,
            NA_real_
          )
        )
    }
  }


  #export the summary file
  if(writetofile){
    write.csv(concatenated_data,paste0(working_directory,concatenatedfile_name),row.names = FALSE)
  }

  # write to excel if desired
  if(writetoexcel){
    wb <- loadWorkbook(paste0(working_directory,excelfile_name))

    # # Determine the dimensions of the existing data in the sheet
    # existing_data <- read.xlsx(paste0(working_directory, excelfile_name), sheet = excelfile_sheet)
    # num_existing_rows <- nrow(existing_data)
    # num_existing_cols <- ncol(existing_data)
    # # Clear old data by writing an empty table over it, if necessary
    # if (num_existing_rows > 0 && num_existing_cols > 0) {
    #   blank_data <- matrix(NA, nrow = num_existing_rows, ncol = num_existing_cols)
    #   writeData(wb, excelfile_sheet, blank_data, startCol = 1, startRow = 1)
    # }

    # Check if the sheet exists, and if so, remove it
    if (excelfile_sheet %in% names(wb)) {
      removeWorksheet(wb, excelfile_sheet)
    }
    # Add the worksheet back with the same name
    addWorksheet(wb, excelfile_sheet)
    # Get the current sheet names to adjust the order
    current_sheets <- names(wb)
    # Ensure the new sheet is in position 2
    new_order <- c(current_sheets[1], excelfile_sheet, current_sheets[-1])
    # Set the new order with worksheetOrder
    worksheetOrder(wb) <- match(new_order, current_sheets)

    # # Move the new sheet to position 2
    # moveWorksheet(wb, sheet = excelfile_sheet, position = 2)

    # Write the new data, save, and clean up
    writeData(wb,excelfile_sheet,concatenated_data)
    saveWorkbook(wb,paste0(working_directory,excelfile_name),overwrite=TRUE)
    rm(wb)
  }

  rm(treatment_combinations,available_files,filename)
  return(concatenated_data)

}
