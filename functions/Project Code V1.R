#Initial SetUp
require(tidyverse)
setwd("~/GitHub/final-project-chris-and-omair-data")

#Parse Data
Drug2014 <- read_csv("State_Drug_Utilization_Data_2014.csv")
Drug2015 <- read_csv("State_Drug_Utilization_Data_2015.csv")
Drug2016 <- read_csv("State_Drug_Utilization_Data_2016.csv")
Drug2017 <- read_csv("State_Drug_Utilization_Data_2017.csv")

#Get Paid Data Only
Paid2014 <- Drug2014%>%filter(!is.na(Units.Reimbursed)&Units.Reimbursed!=0)
Paid2015 <- Drug2015%>%filter(!is.na(Units.Reimbursed)&Units.Reimbursed!=0)
Paid2016 <- Drug2016%>%filter(!is.na(Units.Reimbursed)&Units.Reimbursed!=0)
Paid2017 <- Drug2017%>%filter(!is.na(Units.Reimbursed)&Units.Reimbursed!=0)

#Clean Data
Paid2014[,(c(1:2,6:7,9,15:19))] = NULL
Paid2015[,(c(1:2,6:7,9,15:19))] = NULL
Paid2016[,(c(1:2,6:7,9,15:19))] = NULL
Paid2017[,(c(1:2,6:7,9,15:19))] = NULL
Paid2014$Product.Name <- toupper(Paid2014$Product.Name)
Paid2015$Product.Name <- toupper(Paid2015$Product.Name)
Paid2016$Product.Name <- toupper(Paid2016$Product.Name)
Paid2017$Product.Name <- toupper(Paid2017$Product.Name)

#Group by Product.Code
Group2014 <- Paid2014%>%group_by(Product.Code)%>%summarize(
      Prescriptions = sum(Number.of.Prescriptions), Total.Reimbursed = sum(Total.Amount.Reimbursed),
      Medicaid.Reimbursed = sum(Medicaid.Amount.Reimbursed), Private.Reimbursed = sum(Non.Medicaid.Amount.Reimbursed))
Group2015 <- Paid2015%>%group_by(Product.Code)%>%summarize(
      Prescriptions = sum(Number.of.Prescriptions), Total.Reimbursed = sum(Total.Amount.Reimbursed),
      Medicaid.Reimbursed = sum(Medicaid.Amount.Reimbursed), Private.Reimbursed = sum(Non.Medicaid.Amount.Reimbursed))
Group2016 <- Paid2016%>%group_by(Product.Code)%>%summarize(
      Prescriptions = sum(Number.of.Prescriptions), Total.Reimbursed = sum(Total.Amount.Reimbursed),
      Medicaid.Reimbursed = sum(Medicaid.Amount.Reimbursed), Private.Reimbursed = sum(Non.Medicaid.Amount.Reimbursed))
Group2017 <- Paid2017%>%group_by(Product.Code)%>%summarize(
      Prescriptions = sum(Number.of.Prescriptions), Total.Reimbursed = sum(Total.Amount.Reimbursed),
      Medicaid.Reimbursed = sum(Medicaid.Amount.Reimbursed), Private.Reimbursed = sum(Non.Medicaid.Amount.Reimbursed))

#Names by Product.Code
Product2014 <- Paid2014%>%distinct(Product.Code, .keep_all=TRUE)%>%select(Labeler.Code, Product.Code, Product.Name)
Product2015 <- Paid2015%>%distinct(Product.Code, .keep_all=TRUE)%>%select(Labeler.Code, Product.Code, Product.Name)
Product2016 <- Paid2016%>%distinct(Product.Code, .keep_all=TRUE)%>%select(Labeler.Code, Product.Code, Product.Name)
Product2017 <- Paid2017%>%distinct(Product.Code, .keep_all=TRUE)%>%select(Labeler.Code, Product.Code, Product.Name)
Product <- rbind(Product2014, Product2015, Product2016, Product2017)%>%distinct(Product.Code, .keep_all=TRUE)


#Read Product Data CSV
List <- read_csv("product.csv")

#Define Products
List <- separate(List, PRODUCTNDC, c("Labeler.Code", "Product.Code"))
List$Labeler.Code <- sprintf("%05s", List$Labeler.Code)
Product <- merge(Product, List, all=TRUE)
Product <- Product%>%filter(!is.na(Product.Name))%>%
      select(Labeler.Code, Product.Code, Product.Name, DEASCHEDULE)
Product$DEASCHEDULE[is.na(Product$DEASCHEDULE)] <- "Not Control"
Product <- Product%>%rename(DEA.Schedule = DEASCHEDULE)

#Final Tables
Final2014 <- merge(Group2014, Product, all=TRUE)
Final2015 <- merge(Group2015, Product, all=TRUE)
Final2016 <- merge(Group2016, Product, all=TRUE)
Final2017 <- merge(Group2017, Product, all=TRUE)
Final2014[6] = NULL
Final2015[6] = NULL
Final2016[6] = NULL
Final2017[6] = NULL
Final2014$Product.Code <- sprintf("%04s", Final2014$Product.Code)
Final2015$Product.Code <- sprintf("%04s", Final2015$Product.Code)
Final2016$Product.Code <- sprintf("%04s", Final2016$Product.Code)
Final2017$Product.Code <- sprintf("%04s", Final2017$Product.Code)

#Export
setwd("~/GitHub/final-project-chris-and-omair")
write_csv(Final2014, "Drugs2014.csv")
write_csv(Final2015, "Drugs2015.csv")
write_csv(Final2016, "Drugs2016.csv")
write_csv(Final2017, "Drugs2017.csv")