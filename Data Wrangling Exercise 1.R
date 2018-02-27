library(dplyr)
library(tidyr)
library(readxl)
library(readr)
refine <- read_excel("C:/Users/Antstoe/Downloads/refine.xlsx")
write.csv(refine, "refine_original.csv", row.names=T)
refine_original <- read.csv(file="refine_original.csv", header=TRUE, sep=",")

#Clean Up Brand Names (Error turning everything into numbers )
refine_original <- refine_original %>% 
  mutate(company = ifelse(grepl("Phillips", company, ignore.case = TRUE), "Philips", company)) %>%
  mutate(company = ifelse(grepl("phillips", company, ignore.case = TRUE), "Philips", company)) %>%
  mutate(company = ifelse(grepl("phllips", company, ignore.case = TRUE), "Philips", company)) %>%
  mutate(company = ifelse(grepl("fillips", company, ignore.case = TRUE), "philips", company)) %>%
  mutate(company = ifelse(grepl("phlips", company, ignore.case = TRUE), "Philips", company)) %>%
  mutate(company = ifelse(grepl("akz0", company, ignore.case = TRUE), "Akzo", company)) %>%
  mutate(company = ifelse(grepl("ak zo", company, ignore.case = TRUE), "Akzo", company)) %>%
  mutate(company = ifelse(grepl("unilver", company, ignore.case = TRUE), "Unilever", company)) 

#Clean Up Brand Names
refine_original <-transform(refine_original, company=tolower(company))
refine_original$company[refine_original$company=="Phillips"] <- "Philips"
refine_original$company[refine_original$company=="phillips"] <- "Philips"
refine_original$company[refine_original$company=="phllips"] <- "Philips"
refine_original$company[refine_original$company=="fillips"] <- "Philips"
refine_original$company[refine_original$company=="phlips"] <- "Philips"
refine_original$company[refine_original$company=="akz0"] <- "Akzo"
refine_original$company[refine_original$company=="ak zo"] <- "Akzo"
refine_original$company[refine_original$company=="unilver"] <- "Unilever"

#Seperate Product Code and Number
refine_original <- refine_original %>% 
  separate('Product.code...number', into=c("Product_code", "Product_number"), sep = "-")

#Add Product Categories
refine_original <- refine_original %>% 
  mutate("Product_category" = ifelse(Product_code == "p", "Smartphone", "")) %>% 
  mutate("Product_category" = ifelse(Product_code == "x", "Laptop", Product_category)) %>%
  mutate("Product_category" = ifelse(Product_code == "v", "TV", Product_category)) %>%
  mutate("Product_category" = ifelse(Product_code == "q", "Tablet", Product_category))


#Easily Geocoded
refine_original <- refine_original %>%
  unite(Full_address, c(address, city, country), sep = ",")

#Create Dummy Variable
refine_original <- refine_original %>% 
  mutate(company_philips = ifelse(company =="philips", 1, 0)) %>%
  mutate(company_akzo = ifelse(company == "akzo", 1, 0)) %>%
  mutate(company_van_houten = ifelse(company == "van_houten", 1, 0)) %>%
  mutate(company_unilever = ifelse(company == "unilever", 1, 0))

refine_original <- refine_original %>% 
  mutate(product_smartphone = ifelse(Product_category == "Smartphone", 1, 0)) %>%
  mutate(product_TV = ifelse(Product_category == "TV", 1, 0)) %>%
  mutate(product_laptop = ifelse(Product_category == "Laptop", 1, 0)) %>%
  mutate(product_tablet = ifelse(Product_category == "Tablet", 1, 0))



View(refine_original)
write.csv(refine_original,'refine_original.csv', row.names=T)