
Refine <- function(pattern) {
  
  require(tidyverse)
  require(tidycensus)
  
  #Predownload data from medicaid website.
  #Link: https://www.medicaid.gov/medicaid/prescription-drugs/state-drug-utilization-data/index.html
  #Analysis used national data from available years (1991 to 2017)
  #Note: 2017 is incomplete data running up to current date.
  
  #Data is stored in separate GitHub folder due to file size
  
  setwd("~/GitHub/final-project-chris-and-omair-data")
  
  #A separate FDA dataset is used to obtain product data, including drug strengths and controlled status.
  #This is parsed into the dataframe "List".
  
  FDAList <- read_csv("product.csv")
  
  #In order to unify the products within the Medicaid dataset and the reference product dataset, the data is transformed.
  #The column "PRODUCTNDC" is separated into "Labeler.Code" and "Product.Code" to match the Medicaid dataset.
  #"Labeler.Code" is extended to a string of 5 digits utilizing preceding zeroes.
  #"Product.Code" is extended to a string of 4 digits utilizing preceding zeroes.
  
  FDAList <- separate(FDAList, PRODUCTNDC, c("Labeler.Code", "Product.Code"))
  FDAList$Labeler.Code <- sprintf("%05s", FDAList$Labeler.Code)
  FDAList$Product.Code <- sprintf("%04s", FDAList$Product.Code)
  
  #Manually curated list of CII's based on FDA product list.
  #Joined FDA list with extra manually curated information.
  #Set curated "Opiate" column as capital letters for ease of reading.
  
  CII <- read_csv("CII.csv")
  CII$Labeler.Code <- sprintf("%05s", CII$Labeler.Code)
  CII$Product.Code <- sprintf("%04s", CII$Product.Code)
  List <- right_join(CII, FDAList, by = NULL, all.x=TRUE)
  List$OPIATE <- toupper(List$OPIATE)
  
  #Actual function is below.
  
  filenames <-list.files(path = "~/GitHub/final-project-chris-and-omair-data", pattern = pattern, full.names = TRUE)
    for (f in filenames) {
      
      setwd("~/GitHub/final-project-chris-and-omair-data")
      Drug <- read_csv(f)
      Drug$Labeler.Code <- sprintf("%05s", Drug$Labeler.Code)
      Drug$Product.Code <- sprintf("%04s", Drug$Product.Code)
      
  #Intial read stores base data as variable dataframe "Drug".
  #Following code filters data for incomplete data and zero'd data using the "Units Reimbursed" column
      #This removes any data where data was withheld or the amount of product dispensed was "0".
  #The sorted data is stored as variable dataframe "Paid".
                  
      Paid <- Drug%>%filter(!is.na(Units.Reimbursed)&Units.Reimbursed!=0)
      
  #Paid is then curated for selected data by setting columns to NULL.
      #The line may be modified as needed.
  #Product names are then set to uppercase for the sake of consistency.
          
      Paid[,(c(1,6:7,9,12:19))] = NULL
      Paid$Product.Name <- toupper(Paid$Product.Name)
  
  #Paid is grouped according to "Product Code", which elminates duplicates of the same active ingredient and strength.
      #Both must be true for a product to share the same product code.
  #The data is summed for Units Reimbursed, Number of Prescriptions, Total Reimbursement, Medicaid Amount Reimbursed, and Private (non-Medicaid) Reimbursement.
      #Resultant dataframe is named "Group".
      
      Group <- Paid%>%
        group_by(Labeler.Code, Product.Code)%>%
        summarize(Units.Reimbursed = sum(Units.Reimbursed), Number.of.Prescriptions = sum(Number.of.Prescriptions))
  
'  #A separate variable dataframe is established to list all the unique products, maintaining their labeler code, product code, and product name.
      
      Unique <- Drug%>%
        distinct(Product.Code, .keep_all=TRUE)%>%
        select(Labeler.Code, Product.Code, Product.Name, Package.Size)
      
  #A combined dataframe named "Product" is the result of an inner join of the unique products from the Medicaid dataset and the reference product dataset.
      #Joined on "Labeler.Code" and "Product.Code".
      #Retained data: "Product.Name", "Labeler.Code", "Product.Code", "SUBSTANCENAME", "ACTIVE_NUMERATOR_STRENGTH", "DEASCHEDULE"
      #Some columns are renamed for ease of reading.
      #Filtered out N/A product name matches.
      #Removed "Product.Name" column after filtering due to repetition with "Substance.Name".
      
      Product <- inner_join(Unique, List, by = NULL)
      Product <- Product%>%
        select(Product.Name, Labeler.Code, Product.Code, NONPROPRIETARYNAME, ACTIVE_NUMERATOR_STRENGTH, DEASCHEDULE, OPIATE)%>%
        filter(!is.na(Product.Name))
      Product$DEASCHEDULE[is.na(Product$DEASCHEDULE)] <- "Not Control"
      Product <- Product%>%rename(DEA.Schedule = DEASCHEDULE)
      Product <- Product%>%rename(Product = NONPROPRIETARYNAME)
      Product <- Product%>%rename(Strength = ACTIVE_NUMERATOR_STRENGTH)
      Product <- Product%>%rename(Opiate = OPIATE)
  
  #"Final" dataframe contains merged data from filtered "Product" dataframe (indentifiers for products) and harvested "Group" data from Medicaid dataset.
      #Joined on "Labeler.Code" and "Product.Code"
      #Set "Product.Code" to a string of 4 digits using preceding zeroes.
      #Capitalize all product names in "Product" column
      #Remove Product.Name column
      
      Final <- inner_join(Product, Group, by = NULL)
      Final$Product <- toupper(Final$Product)
      Final[1] = NULL

  #Establishing file name:
      #dataframe named "Data[YEAR]" created in global environment for inputted data.
      #dataframe named "Drug[YEAR]" created in global environment for outputted data.
      #CSV written named "Drug[YEAR].csv created in final project folder.
      
      namelist <- strsplit(f, "_")
      name <- unlist(namelist)
      assign(paste("Data", name[5], sep = ""), Drug, envir = .GlobalEnv)
      assign(paste("Drug", name[5], sep = ""), Final, envir = .GlobalEnv)
      filename <- paste("Drug", name[5], ".csv", sep = "")
      setwd("~/GitHub/final-project-chris-and-omair")
      write_csv(Final, filename)'
  }
}