#Predownload data from medicaid website.
#Link: https://www.medicaid.gov/medicaid/prescription-drugs/state-drug-utilization-data/index.html
#Analysis used national data from available years (1991 to 2017)
#Note: 2017 is incomplete data running up to current date.

require(tidyverse)
require(tidycensus)
require(stringr)

#Data is stored in separate GitHub folder due to file size

setwd('~/GitHub/final-project-chris-and-omair-data')

#A separate FDA dataset is used to obtain product data, including drug strengths and controlled status.

List <- read_csv('product.csv')

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
  mutate(OPIATE = '', NSAID = '')%>%
  mutate(OPIATE=replace(OPIATE,str_detect(NONPROPRIETARYNAME,
                                          'OXYCODONE|MORPHINE|FENTAN|HYDROCODONE'), 'YES'))%>%
  mutate(NSAID=replace(NSAID,str_detect(NONPROPRIETARYNAME,
                                          'ACETAMINOPHEN|NAPROXEN|DICLOFENAC|IBUPROFEN|TRAMADOL|CYCLOBENZAPRINE|CARISOPRODOL|MELOXICAM'), 'YES'))%>%
  mutate(NSAID=replace(OPIATE))

#Actual function is below.

refine <- function(pattern) {
  
  filenames <-list.files(path = "~/GitHub/final-project-chris-and-omair-data",
                         pattern = pattern, full.names = TRUE)
  
  for (f in filenames) {
    
    setwd("~/GitHub/final-project-chris-and-omair-data")
    
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
    
    #Paid is then curated for selected data by setting columns to NULL.
    #The line may be modified as needed.
    #Product names are then set to uppercase for the sake of consistency.
    
    Paid$Product.Name <- toupper(Paid$Product.Name)
    
    #Paid is grouped according to "Labeler Code" and Product Code" which elminates duplicates of the same active ingredient and strength.
    #Both must be true for a product to share the same product code.
    #The data is summed for Units Reimbursed, Number of Prescriptions.
    #Resultant dataframe is named "Group".
    
    Group <- Paid%>%
      group_by(Labeler.Code, Product.Code)%>%
      summarize(Units.Reimbursed = sum(Units.Reimbursed), Number.of.Prescriptions = sum(Number.of.Prescriptions))
    
    #A separate variable dataframe is established to list all the unique products, maintaining their labeler code, product code, and product name.
    
    Unique <- Drug%>%
      distinct(Product.Code, .keep_all=TRUE)%>%
      select(Labeler.Code, Product.Code, Product.Name, Package.Size)
    
    #A combined dataframe named "Product" is the result of an inner join of the unique products from the Medicaid dataset and the reference product dataset.
    #Joined on "Labeler.Code" and "Product.Code".
    #Retained data: "Product.Name", "Labeler.Code", "Product.Code", "SUBSTANCENAME", "ACTIVE_NUMERATOR_STRENGTH", "DEASCHEDULE"
    #Some columns are renamed for ease of reading.
    #Filtered out N/A product name matches.
    #Removed "Product.Name" column after filtering due to repetition with "Substance.Name".
    
    Product <- inner_join(Unique, List)
    
    Product <- Product%>%
      select(Product.Name, Labeler.Code, Product.Code, NONPROPRIETARYNAME,
             ACTIVE_NUMERATOR_STRENGTH, DEASCHEDULE, OPIATE)%>%
      filter(!is.na(Product.Name))
    
    Product$DEASCHEDULE[is.na(Product$DEASCHEDULE)] <- ''
    
    Product <- Product%>%
      rename(DEA.Schedule = DEASCHEDULE, Product = NONPROPRIETARYNAME,
             Strength = ACTIVE_NUMERATOR_STRENGTH, Opiate = OPIATE)
    
    #"National" dataframe contains merged data from filtered "Product" dataframe (identifiers for products) and harvested "Group" data from Medicaid dataset.
    #Joined on "Labeler.Code" and "Product.Code"
    #Set "Product.Code" to a string of 4 digits using preceding zeroes.
    #Capitalize all product names in "Product" column
    #Remove Product.Name column
    
    National <- inner_join(Product, Group)
    National$Product <- toupper(National$Product)
    National[1] = NULL
    
    #Separate dataset showing opiates vs non-opiates by state.
    
    Opiate <- inner_join(Product, Paid)%>%
      group_by(State, Opiate)%>%
      summarize(Number.of.Prescriptions = sum(Number.of.Prescriptions))
    
    NSAIDs <- inner_join(Product, Paid)%>%
      group_by(State, NSAID)%>%
      summarize(Number.of.Prescriptions = sum(Number.of.Prescriptions))
      
    #Establishing file name.
    #dataframe named "NationalData[YEAR]" and "OpiateRXs[YEAR]" created in global environment for outputted data.
    #CSV written named "NationalData[YEAR].csv" and "OpiateRXs[YEAR].csv" created in final project folder.
    
    namelist <- strsplit(f, '_')
    name <- unlist(namelist)
    
    assign(paste('National', name[5], sep = ''), National, envir = .GlobalEnv)
    assign(paste('OpiateRXs', name[5], sep = ''), Opiate, envir = .GlobalEnv)
    
    nationalname <- paste('NationalData', name[5], '.csv', sep = '')
    opiatename <- paste('OpiateRXs', name[5], '.csv', sep = '')
    NSAIDname <- paste('NSAIDRXs', name[5], '.csv', sep = '')
    
    setwd('~/GitHub/final-project-chris-and-omair')
    
    write_csv(National, nationalname)
    write_csv(Opiate, opiatename)
    write_csv(NSAIDs, NSAIDname)
  }
}