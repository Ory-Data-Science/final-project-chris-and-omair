
Output <- function(year) {
  
  require(tidyverse)
  setwd("~/GitHub/final-project-chris-and-omair-data")
  
  data <- paste("State_Drug_Utilization_Data_", year, ".csv", sep = "")
  Drug <- read_csv(data)
  
  Paid <- Drug%>%filter(!is.na(Units.Reimbursed)&Units.Reimbursed!=0)
  Paid[,(c(1:2,6:7,9,15:19))] = NULL
  Paid$Product.Name <- toupper(Paid$Product.Name)
  
  Group <- Paid%>%group_by(Product.Code)%>%summarize(
    Prescriptions = sum(Number.of.Prescriptions), Total.Reimbursed = sum(Total.Amount.Reimbursed),
    Medicaid.Reimbursed = sum(Medicaid.Amount.Reimbursed), Private.Reimbursed = sum(Non.Medicaid.Amount.Reimbursed))
  
  Unique <- Paid%>%distinct(Product.Code, .keep_all=TRUE)%>%select(Labeler.Code, Product.Code, Product.Name)
  
  List <- read_csv("product.csv")
  
  List <- separate(List, PRODUCTNDC, c("Labeler.Code", "Product.Code"))
  List$Labeler.Code <- as.numeric(List$Labeler.Code)
  List$Product.Code <- as.numeric(List$Product.Code)
  List$Labeler.Code <- sprintf("%05d", List$Labeler.Code)
  List$Product.Code <- sprintf("%04d", List$Product.Code)
  Product <- inner_join(Unique, List, by = NULL)
  Product <- Product%>%
    select(Product.Name, Labeler.Code, Product.Code, SUBSTANCENAME, ACTIVE_NUMERATOR_STRENGTH, DEASCHEDULE)%>%
    filter(!is.na(Product.Name))
  Product$DEASCHEDULE[is.na(Product$DEASCHEDULE)] <- "Not Control"
  Product <- Product%>%rename(DEA.Schedule = DEASCHEDULE)
  Product <- Product%>%rename(Substance.Name = SUBSTANCENAME)
  Product <- Product%>%rename(Strength = ACTIVE_NUMERATOR_STRENGTH)
  Product[1] = NULL
  
  Final <- merge(Product, Group, all=TRUE)
  Final[6] = NULL
  Final$Product.Code <- sprintf("%04s", Final$Product.Code)
  assign(paste("Drug", year, sep = ""), Final, envir = .GlobalEnv)
  filename <- paste("Drug", year, ".csv", sep = "")
  setwd("~/GitHub/final-project-chris-and-omair")
  write_csv(Final, filename)
}
