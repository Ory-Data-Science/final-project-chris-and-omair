#Predownload data from medicaid website or use the fetch function.
#Link: https://www.medicaid.gov/medicaid/prescription-drugs/state-drug-utilization-data/index.html
#Analysis used national data from available years (1991 to 2017)
#Note: 2017 is incomplete data running up to current date.

require(tidyverse)
require(stringr)
require(scales)
setwd('~/GitHub/final-project-chris-and-omair')

#refining function is below.
#refine takes one argument:
#PATTERN = a text pattern you want to search the medicaid data by: e.g. "State" will pick up all, "201" will pick up since 2010
#You can also concatenate for multiple searches

by_year <- function(PATTERN) {
  
  #A separate FDA dataset is used to obtain product data, including drug strengths and controlled status.
  
  List <- read_csv('./source/product.csv')
  
  #In order to unify the products within the Medicaid dataset and the reference product dataset, the data is transformed.
  #The column "PRODUCTNDC" is separated into "Labeler.Code" and "Product.Code" to match the Medicaid dataset.
  
  List <- separate(List, PRODUCTNDC, c('Labeler.Code', 'Product.Code'))
  List$NONPROPRIETARYNAME <- toupper(List$NONPROPRIETARYNAME) 
  
  #"Labeler.Code" is extended to a string of 5 digits utilizing preceding zeroes.
  
  List$Labeler.Code <- as.numeric(List$Labeler.Code)
  List$Labeler.Code <- sprintf('%05d', List$Labeler.Code)
  
  #"Product.Code" is extended to a string of 4 digits utilizing preceding zeroes.
  
  List$Product.Code <- as.numeric(List$Product.Code)
  List$Product.Code <- sprintf('%04d', List$Product.Code)
  
  #Detecting common opiate strings, these drugs are marked 'YES' when they match.
  #Also fixes some scheduling list errors in original file.
  
  List <- List%>%
    mutate(Narcotic = '')%>%
    mutate(Narcotic=replace(Narcotic, str_detect(NONPROPRIETARYNAME,
                                                 'OXYCODONE|MORPHINE|FENTAN|HYDROCODONE|MORPHONE|BUPENORPHINE|LEVORPHANOL|METHADONE|TRAMADOL'), 'YES'))%>%
    mutate(Narcotic=replace(Narcotic, str_detect(NONPROPRIETARYNAME,
                                                 'ACETAMINOPHEN|NAPROXEN|GABAPENTIN|INDOMETHACIN|PIROXICAM|DICLOFENAC|IBUPROFEN|CYCLOBENZAPRINE|
                                                 CARISOPRODOL|MELOXICAM|ORPHENADRINE|METHOCARBAMOL|METAXALONE'), 'NO'))
  
  filenames <- list.files(path = './source', pattern = PATTERN, full.names = TRUE)
  
  central_data <- data.frame(State = character(), Narcotic = integer(), NonNarcotic = integer(), Year = character(),
                             Percentage = numeric(), From.Yearly.Mean = numeric())
  
  for (f in filenames) {
    
    #Intial read stores base data as variable dataframe "Drug".
    
    Drug <- read_csv(f)
    
    #Remove " " from column names, replacing with "."
    
    names(Drug) <- gsub(x = names(Drug), pattern = " ", replacement = ".")
    
    #"Labeler.Code" is extended to a string of 5 digits utilizing preceding zeroes.
    #"Product.Code" is extended to a string of 4 digits utilizing preceding zeroes.
    #Remove NAs for NDC info.
    
    Drug$Labeler.Code <- as.numeric(Drug$Labeler.Code)
    Drug$Product.Code <- as.numeric(Drug$Product.Code)
    
    Drug <- Drug%>%
      filter(!is.na(Labeler.Code), !is.na(Product.Code))
    
    Drug$Labeler.Code <- sprintf('%05d', Drug$Labeler.Code)
    Drug$Product.Code <- sprintf('%04d', Drug$Product.Code)
    
    #Following code filters data for incomplete data and zero'd data using the "Units Reimbursed" column
    #This removes any data where data was withheld or the amount of product dispensed was "0".
    #The sorted data is stored as variable dataframe "Paid".
    
    Paid <- Drug%>%filter(!is.na(Units.Reimbursed) & Units.Reimbursed!=0)
    
    #Product names are then set to uppercase for the sake of consistency.
    
    Paid$Product.Name <- toupper(Paid$Product.Name)
    
    #A combined dataframe is generated using inner join based on Paid and List.
    #Filtered on State and whether or not it is a Narcotic painkiller
    #Exclude nonpainkillers based on whether or not it is a NSAID or muscle relaxant for non-narcotics.
    
    RawData <- inner_join(Paid, List)%>%
      group_by(State, Narcotic)%>%
      summarize(Number.of.Prescriptions = sum(Number.of.Prescriptions))%>%
      filter(Narcotic != '')
    
    Narcotic <- RawData%>%
      filter(Narcotic == "YES")%>%
      select(State, Number.of.Prescriptions)
    
    NonNarcotic <- RawData%>%
      filter(Narcotic == "NO")%>%
      select(State, Number.of.Prescriptions)
      
    Data <- inner_join(Narcotic, NonNarcotic, by = "State")
    Data <- Data%>%rename(Narcotic = Number.of.Prescriptions.x, NonNarcotic = Number.of.Prescriptions.y)
    
    #Add year, percentage narcotic vs all painkillers, and percentage points off of yearly national mean.
    
    f <- gsub('[.csv]', '', f)
    namelist <- strsplit(f, '_')
    name <- unlist(namelist)
    
    Data <- Data%>%
      mutate(Year = name[5])
    
    Mean <- 100*sum(Data$Narcotic)/(sum(Data$Narcotic)+sum(Data$NonNarcotic))
    
    Data <- Data%>%
      mutate(Percentage = Narcotic / (Narcotic + NonNarcotic) * 100)%>%
      mutate(From.Yearly.Mean = Percentage-Mean)
    
    #Generate combined dataframe in global environment for narcotic vs non-narcotic painkillers
    
    central_data <- bind_rows(central_data, Data)
    
    assign('central_data', central_data, envir = .GlobalEnv)
    
  }
  
  for (i in 1991:2017) {
    
    #Sort data by year and reorder the dataframe
    
    Data <- central_data%>%
      filter(Year == i)
    
    Data <- Data[c(4,1,2,3,5,6)]
    
    #CSV written named "Year_[YEAR].csv" created in final project folder.
    
    dataname <- paste('./data/by_year/Data_', i, '.csv', sep = '')
  
    write_csv(Data, dataname)
  
  }
}

#Use skip_step_one function to obtain a summary dataset of all years processed in step one (if you already did it before)

skip_step_one <- function() {
  
  filenames <- list.files(path = './data/by_year', pattern = 'Data', full.names = TRUE)
  
  central_data <- data.frame(State = character(), Narcotic = numeric(), NonNarcotic = numeric(), Year = numeric(), Percentage = numeric(), From.Yearly.Mean = numeric())
  
  for (f in filenames) {
  
    Data <- read_csv(f)
    
    central_data <- bind_rows(central_data, Data)
    
    assign('central_data', central_data, envir = .GlobalEnv)  
  }
}

#You can then sort by State.

by_state <- function() {
  
  States <- central_data$State
  
  for(S in States){
    
    Data <- central_data%>%
      filter(State == S)
    
    Data <- Data[c(1,4,2,3,5,6)]
    
    dataname <- paste('./data/by_state/Data_', S, '.csv', sep = '')
    
    write_csv(Data, dataname)
  }
}  

#This function generates two heatmaps.
heatmap <- function() {
  
  ggplot(Graphing) +
    geom_tile(aes(x = Year, y = State, fill = Percentage)) +
    scale_fill_gradientn(colours=c('white', 'red'), na.value = 'grey98',  values = rescale(c(0, 40))) +
    ggtitle('Painkillers that are Narcotics', subtitle = 'as a percent of total Painkillers') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

  ggplot(Graphing) +
    geom_tile(aes(x = Year, y = State, fill = From.Yearly.Mean)) +
    scale_fill_gradientn(colours=c('blue', 'white', 'red'), na.value = 'grey98', values = rescale(c(-15, 0, 25))) +
    ggtitle('Painkillers that are Narcotics', subtitle = 'Percentage Points off of Mean for Year') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
}

#Nonfunctional
fetch <- function(){
  data <- read_csv('https://data.medicaid.gov/api/views/q7kf-kjqz/rows.csv?accessType=DOWNLOAD')
  write_csv(data, './source/State_Drug_Utilization_Data_1991.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/7t83-bmdc.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_1992.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/d4zb-hchj.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_1993.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/y3rw-ibat.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_1994.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/aaka-kdax.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_1995.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/crtc-n6kh.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_1996.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/adb9-bvf8.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_1997.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/mpxd-47jf.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_1998.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/8car-a85u.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_1999.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/xemf-shqf.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2000.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/7dxf-vdi8.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2001.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/npac-bg4r.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2002.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/yfr2-g35g.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2003.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/azm2-epd3.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2004.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/hxu8-ag9g.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2005.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/admb-uh5m.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2006.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/rpeh-qcu8.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2007.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/psbv-t9xz.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2008.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/5ncx-akys.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2009.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/gyfr-saxn.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2010.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/au58-3g3e.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2011.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/jzhb-tr7x.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2012.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/hcg7-jjb2.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2013.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/qpz6-74iw.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2014.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/m4ab-dkvc.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2015.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/neai-csgh.csv')
  write.csv(data, './source/State_Drug_Utilization_Data_2016.csv')
  
  data <- read.socrata('https://data.medicaid.gov/resource/f8sh-7iwd.csv')
  write.csv(data, "./source/State_Drug_Utilization_Data_2017.csv")
}