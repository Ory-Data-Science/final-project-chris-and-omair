require(tidyverse)
setwd("~/GitHub/final-project-chris-and-omair-data")

List <- read_csv("product.csv")

List <- separate(List, PRODUCTNDC, c("Labeler.Code", "Product.Code"))
List$NONPROPRIETARYNAME <- toupper(List$NONPROPRIETARYNAME)

CII <- List%>%select(Labeler.Code, Product.Code, NONPROPRIETARYNAME, ACTIVE_NUMERATOR_STRENGTH, DEASCHEDULE)


CII$Labeler.Code <- as.numeric(CII$Labeler.Code)
CII$Labeler.Code <- sprintf("%05d", CII$Labeler.Code)

CII$Product.Code <- as.numeric(CII$Product.Code)
CII$Product.Code <- sprintf("%04d", CII$Product.Code)

Opiate <- CII%>%
  mutate(OPIATE = "")%>%
  mutate(OPIATE=replace(OPIATE,str_detect(NONPROPRIETARYNAME, 'OXYCODONE|MORPHINE|FENTAN|HYDROCODONE|MORPHONE|CODEINE|TAPENTADOL|CODEINE'),'YES'))
  

setwd("~/GitHub/final-project-chris-and-omair")
write_csv(CII, "CII.csv")