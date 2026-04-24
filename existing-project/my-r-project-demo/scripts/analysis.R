setwd("C:/Users/jzhu5/OneDrive - Transport for NSW/Regression")
library("readxl")
library("rio")
library("dplyr")
library("plyr")
library(writexl)

## a new version
data_list <- import_list("Input R.xlsx")

## Read data
Business <- read_excel("Input R.xlsx", sheet = "Business_Input")
Business_2 <- read_excel("Input R - 16 July.xlsx", sheet = "Business_Input_2")
Residentials <- read_excel("Input R.xlsx", sheet = "Residentials_Input_2")
Population <- read_excel("Input R.xlsx", sheet = "Population_Input_2")
Employee_Number <- read_excel("Input R.xlsx", sheet = "Employee_Number_2")

library(writexl)
sheets <- list("Business_Input_2" = Business, "Residentials_Input_2" = Residentials, "Population_Input_2" = Population, "Employee_Number_2" = Employee_Number) #assume sheet1 and sheet2 are data frames
write_xlsx(sheets)

write.csv(Business, file="A1.csv")
write.csv(Residentials, file="A2.csv")
write.csv(Population, file="A3.csv")
write.csv(Employee_Number, file="A4.csv")

Business <- read_excel("Input R - Growth.xlsx", sheet = "Business_Input_2")
Business<-Business[!(Business$count_of_businesses_2 %in% 0),]
Business<-Business[!(Business$count_of_businesses_2 < -100),]
reg_business6=lm(count_of_businesses_2~Treat_LGA+Total_Land_Value_by_Year+Land_Value_by_LGA, data=Business)
summary(reg_business6)
reg_business6_2017=lm(count_of_businesses_2~Treat_LGA*Post2017+Total_Land_Value_by_Year+Land_Value_by_LGA, data=Business)
summary(reg_business6_2017)
reg_business6_2018=lm(count_of_businesses_2~Treat_LGA*Post2018+Total_Land_Value_by_Year+Land_Value_by_LGA, data=Business)
summary(reg_business6_2018)
reg_business6_2019=lm(count_of_businesses_2~Treat_LGA*Post2019+Total_Land_Value_by_Year+Land_Value_by_LGA, data=Business)
summary(reg_business6_2019)


Residentials <- read_excel("Input R - Growth.xlsx", sheet = "Residential_Input_2")
Residentials<-Residentials[!(Residentials$number_of_residential_2 %in% 0),]
Residentials<-Residentials[!(Residentials$number_of_residential_2 < -100),]
Residentials6=lm(number_of_residential_2~Treat_LGA+Total_Land_Value_by_Year+Land_Value_by_LGA, data=Residentials)
summary(Residentials6)
Residentials6_2017=lm(number_of_residential_2~Treat_LGA*Post2017+Total_Land_Value_by_Year+Land_Value_by_LGA, data=Residentials)
summary(Residentials6_2017)
Residentials6_2018=lm(number_of_residential_2~Treat_LGA*Post2018+Total_Land_Value_by_Year+Land_Value_by_LGA, data=Residentials)
summary(Residentials6_2018)
Residentials6_2019=lm(number_of_residential_2~Treat_LGA*Post2019+Total_Land_Value_by_Year+Land_Value_by_LGA, data=Residentials)
summary(Residentials6_2019)

Population <- read_excel("Input R - Growth.xlsx", sheet = "Population_Input_2")
Population<-Population[!(Population$population_2 %in% 0),]
Population<-Population[!(Population$population_2 < -100),]
Population6=lm(population_2~Treat_LGA+Total_Land_Value_by_Year+Land_Value_by_LGA, data=Population)
summary(Population6)
Population6_2017=lm(population_2~Treat_LGA*Post2017+Total_Land_Value_by_Year+Land_Value_byLGA_2017, data=Population)
summary(Population6_2017)
Population6_2018=lm(population_2~Treat_LGA*Post2018+Total_Land_Value_by_Year+Land_Value_byLGA_2018, data=Population)
summary(Population6_2018)
Population6_2019=lm(population_2~Treat_LGA*Post2019+Total_Land_Value_by_Year+Land_Value_byLGA_2019, data=Population)
summary(Population6_2019)

Employee <- read_excel("Input R - Growth.xlsx", sheet = "Employee_Input_2")
Employee<-Employee[!(Employee$Employee_Number_2 %in% 0),]
Employee<-Employee[!(Employee$Employee_Number_2 < -100),]
Employee6=lm(Employee_Number_2~Treat_LGA+Total_Land_Value_by_Year+Land_Value_by_LGA, data=Employee)
summary(Employee6)
Employee6_2017=lm(Employee_Number_2~Treat_LGA*Post2017+Total_Land_Value_by_Year+Land_Value_byLGA_2017, data=Employee)
summary(Employee6_2017)
Employee6_2018=lm(Employee_Number_2~Treat_LGA*Post2018+Total_Land_Value_by_Year+Land_Value_byLGA_2018, data=Employee)
summary(Employee6_2018)
Employee6_2019=lm(Employee_Number_2~Treat_LGA*Post2019+Total_Land_Value_by_Year+Land_Value_byLGA_2019, data=Employee)
summary(Employee6_2019)

Residentials <- read_excel("Input R - Growth.xlsx", sheet = "Residential_Input_2")
Residentials<-Residentials[!(Residentials$number_of_residential_2 %in% 0),]
Residentials<-Residentials[!(Residentials$number_of_residential_2 < -100),]
Residentials6=lm(number_of_residential_2~Treat_LGA+Total_Land_Value_by_Year+Land_Value_by_LGA, data=Residentials)
summary(Residentials6)
Residentials6_2017=lm(number_of_residential_2~Treat_LGA*Post2017+Total_Land_Value_by_Year+Land_Value_byLGA_2017, data=Residentials)
summary(Residentials6_2017)
Residentials6_2018=lm(number_of_residential_2~Treat_LGA*Post2018+Total_Land_Value_by_Year+Land_Value_byLGA_2018, data=Residentials)
summary(Residentials6_2018)
Residentials6_2019=lm(number_of_residential_2~Treat_LGA*Post2019+Total_Land_Value_by_Year+Land_Value_byLGA_2019, data=Residentials)
summary(Residentials6_2019)

## pre-processing
setwd("C:/Users/jzhu5/OneDrive - Transport for NSW/Project - Economic Uplift")

library("readxl")
library("rio")
library("dplyr")
library(writexl)

Business <- read_excel("Input R - Growth.xlsx", sheet = "Business_Input_2")
Business <- read_excel("Input R - Growth.xlsx", sheet = "Business_Input_2")
temp1<-Business[Business$period=="2022",]
temp2<-Business[Business$period=="2022",]
temp1[,"period"]<-2023
temp2[,"period"]<-2024

final<-bind_rows(Business, temp1, temp2)
final<-arrange(final,lga_name,period)

write.csv(final, file="A1.csv")

--------------------------------------------------------------
Business <- read_excel("Input R - Growth.xlsx", sheet = "Bus by Industry")
Business <- read_excel("Input R - Growth.xlsx", sheet = "Bus by Industry")
temp1<-Business[Business$period=="2022",]
temp2<-Business[Business$period=="2022",]
temp1[,"period"]<-2023
temp2[,"period"]<-2024

final<-bind_rows(Business, temp1, temp2)
final<-arrange(final,lga_name,period)

write.csv(final, file="A2.csv")

---------------------------------------------------------------------
Residential <- read_excel("Input R - Growth.xlsx", sheet = "Residential_Input_2")
Residential <- read_excel("Input R - Growth.xlsx", sheet = "Residential_Input_2")
temp1<-Residential[Residential$period=="2021",]
temp2<-Residential[Residential$period=="2021",]
temp1[,"period"]<-2022
temp2[,"period"]<-2023

final<-bind_rows(Residential, temp1, temp2)
final<-arrange(final,lga_name,period)

final[final$period=="2022","Total_Land_Value_by_Year"]<-2.84713E+12
final[final$period=="2023","Total_Land_Value_by_Year"]<-2.80073E+12

write.csv(final, file="A3.csv")

-------------------------------------------------------------------
Population <- read_excel("Input R - Growth.xlsx", sheet = "Population_Input_2")
temp1<-Population[Population$period=="2021",]
temp2<-Population[Population$period=="2021",]
temp1[,"period"]<-2022
temp2[,"period"]<-2023

final<-bind_rows(Population, temp1, temp2)
final<-arrange(final,lga_name...2,period)

final[final$period=="2022","Total_Land_Value_by_Year"]<-2.84713E+12
final[final$period=="2023","Total_Land_Value_by_Year"]<-2.80073E+12

write.csv(final, file="A4.csv")

-------------------------------------------------------------------
Approval <- read_excel("Input R - Growth.xlsx", sheet = "Building_Approval_Input_3")
temp1<-Approval[Approval$period=="2022",]
temp2<-Approval[Approval$period=="2022",]
temp1[,"period"]<-2023
temp2[,"period"]<-2024

final<-bind_rows(Approval, temp1, temp2)
final<-arrange(final,lga_name,period)

final[final$period=="2023","Total_Land_Value_by_Year"]<-2.80073E+12
final[final$period=="2024","Total_Land_Value_by_Year"]<-2.9816E+12

write.csv(final, file="A5.csv")

-------------------------------------------------------------------
Income <- read_excel("Input R - Growth.xlsx", sheet = "Employee_Income_Mean_2")
temp1<-Income[Income$period=="2021",]
temp1[,"period"]<-2022

final<-bind_rows(Income, temp1)
final<-arrange(final,lga_name, period)

final[final$period=="2022","Total_Land_Value_by_Year"]<-2.84713E+12

write.csv(final, file="A6.csv")

-------------------------------------------------------------------
Employee <- read_excel("Input R - Growth.xlsx", sheet = "Employee_Input_2")
temp1<-Employee[Employee$period=="2021",]
temp1[,"period"]<-2022

final<-bind_rows(Employee, temp1)
final<-arrange(final,lga_name, period)

final[final$period=="2022","Total_Land_Value_by_Year"]<-2.84713E+12

write.csv(final, file="A7.csv")



# Experimental: trying alternative method
