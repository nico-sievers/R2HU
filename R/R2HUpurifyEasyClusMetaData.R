#' @title Function to fix the meta data export tables from EasyClus, where certain column names shift due to our old instrument.
#'
#' @description \code{-BETA-}   Ask Nico if you would like to use this function.
#'
#' @param working_directory,rawfile_folder,rawfile_identifier,writetofile,concatenatedfile_name,writetoexcel,excelfile_name,excelfile_sheet,apply_chlacalibration,setsinsupergroups,apply_lengthcalibration \code{(to be updated)}
#'
#' @return Returns the corrected table.
#'
## @examples
## R2HUpurifyEasyClusMetaData()
#'
#' @export
#'
#' @importFrom utils write.csv
#' @importFrom readr read_delim
#' @importFrom dplyr %>% group_by summarise mutate distinct select
#' @importFrom tidyr separate pivot_longer pivot_wider

# for debugging:
#rawfilename="C:/Program Files/CytoBuoy B.V/EasyClusv215results/datafiles/sampleINFO-23-May-2024_14u08u35.txt"



R2HUpurifyEasyClusMetaData=function(rawfilename="./*.txt",separator="XXX",writetocsv=T,newfilename="./metadata_fixed.csv"){

  # load data and headers separately
  headers=as.vector(unlist(read_delim(rawfilename,delim=separator,trim_ws=TRUE,n_max=1,col_names=F)[1,]))
  data=read_delim(rawfilename,delim=separator,trim_ws=TRUE,skip=1,col_names=F)

  # fix headers
  if(T){
    positionbeforewhichtoelongate=which(headers=="nrofimages")
    headers=c(headers[1:(positionbeforewhichtoelongate-1)],paste0("unknown",1:4),headers[positionbeforewhichtoelongate:length(headers)])
    positionbeforewhichtoelongate=which(headers=="pixelsize")
    headers=c(headers[1:(positionbeforewhichtoelongate-1)],paste0("unknown",5:8),headers[positionbeforewhichtoelongate:length(headers)])
  }

  # merge headers onto data unless they mismatch
  if(length(headers)!=ncol(data)){stop("Length of headers and data doesn't match after the fixing attempt")}
  names(data)=headers

  # kill completely empty rows
  data=data[apply(data,2,function(x){!all(is.na(x))})]


  # calculate the percentage of analysed volume
  positionpumped=which(headers=="pumpedvolume")
  data$percentanalyzedvolume=data$analyzedvolume/data$pumpedvolume
  data=data[,c(1:positionpumped,ncol(data),(positionpumped+1):(ncol(data)-1))]


  # get the PMTs into separate columns
  if(T){
    # Step 1: Identify the original columns
    original_columns <- setdiff(names(data), "detectorsPMT")
    # Step 2: Separate and pivot longer
    data <- data %>%
      # Separate the strings by ":"
      separate(detectorsPMT, into = paste0("detectorsPMT_", 1:4), sep = ":") %>%
      # Gather the parts into key-value pairs
      pivot_longer(cols = starts_with("detectorsPMT_"), names_to = "detectorsPMT_", values_to = "key_value") %>%
      # Separate the key_value pairs into two columns
      separate(key_value, into = c("key", "value"), sep = "-") %>%
      # Remove the intermediate column
      select(-detectorsPMT_)
    # Step 3: Pivot wider
    data <- data %>%
      pivot_wider(names_from = key, values_from = value)
    # Step 4: Identify new columns
    new_columns <- setdiff(names(data), original_columns)
    # Step 5: Rename new columns with the "detectorsPMT_" prefix
    data <- data %>%
      rename_with(~ paste0("detectorsPMT_", .), all_of(new_columns))
    # Step 6: Convert the new columns to numeric
    data <- data %>%
      mutate(across(starts_with("detectorsPMT_"), as.integer))
  }


  if(writetocsv){write.csv(data,newfilename)}
  return(data)

}
