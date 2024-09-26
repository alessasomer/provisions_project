
# read in provisions sales data from 2023 and 2024

#install.packages("tidyverse")

library(tidyverse)

username <- Sys.getenv("USERNAME")
mypath <- paste0("C:\\Users\\", username, "\\Dropbox\\provisions\\")


## 2023

raw.2023 <- read_csv(paste0(mypath, "raw\\transactions-2023-01-01-2024-01-01.csv")) 

data.2023 <- raw.2023 %>% 
  # refunds/exchanges (N=33), non-register transaction (N=93)
  filter(`Event Type` == "Payment", 
         Source == "Register") %>% 
  # drop columns with no variation 
  select(where(~ n_distinct(.) != 1)) %>%  
  # keep first names of customers, when available
  mutate(`Customer Name` = ifelse(str_detect(`Customer Name`, "@"), NA, `Customer Name`)) %>% 
  mutate(`Customer Name` = ifelse(str_detect(`Customer Name`, ","), NA, `Customer Name`)) %>% 
  separate_wider_delim(`Customer Name`, 
                       " ", 
                       names = c("first.name", "last.name"), 
                       too_few = "align_start", 
                       too_many = "merge") %>% 
  # drop personally-identifiable information
  select(!`Staff Name` & !last.name) %>% 
  # remove dollar signs and commas so that we can convert character vectors to amounts to numeric
  mutate(across(where(is.character), ~ str_replace(., "\\$", ""))) %>% 
  mutate(across(where(is.character), ~ str_replace(., ",", ""))) %>% 
  # clean up names 
  rename(date = Date, 
       time = Time, 
       customer.id = `Customer ID`, 
       gross.sales = `Gross Sales`, 
       discount = Discounts, 
       net.sales = `Net Sales`, 
       gift.card = `Gift Card Sales`,
       tax = Tax, 
       amt.total = `Total Collected`, 
       amt.card = Card, 
       amt.cash = Cash, 
       amt.gift.card = `Square Gift Card`,
       amt.other = `Other Tender`, 
       fees = Fees, 
       amt.net = `Net Total`, 
       card.type = `Card Brand`, 
       items = Description, 
       discount.type = `Discount Name`) %>% 
  # drop things we don't care about 
  select(!`Card Entry Methods` 
         & !`Other Tender Type` 
         & !`Other Tender Note` 
         & !`PAN Suffix` 
         & !`Transaction ID`
         & !`PAN Suffix`
         & !`Payment ID` 
         & !`Device Name` 
         & !`Staff ID` 
         & !Details 
         & !`Customer Reference ID` 
         & !`Device Nickname` 
         & !`Deposit ID` 
         & !`Deposit Date` 
         & !`Deposit Details` 
         & !`Fee Percentage Rate` 
         & !`Fee Fixed Rate` 
         & !`Free Processing Applied`) %>% 
  # not sure what Cash App is but it only pertains to three transactions so dropping them 
  mutate(`Cash App` = as.numeric(`Cash App`)) %>% 
  filter(`Cash App` == 0) %>% 
  select(!`Cash App`) %>% 
  # convert character variables that are dollars to numeric 
  mutate(across(c(gross.sales, discount, net.sales, gift.card, tax, amt.total, fees, amt.net), ~ as.numeric(.))) %>% 
  mutate(across(c(amt.card, amt.cash, amt.gift.card, amt.other), ~ as.numeric(.))) 

## code for checking observations that don't convert to numeric
# which(is.na(as.numeric(data$gross.sales)) != is.na(data$gross.sales)) # gives a list of problematic rows
# filter(data[2978,])

# write the data
write_csv(data.2023, paste0(mypath, "ECON370-provisions-transactions-2023.csv"))


## 2024

raw.2024 <- read_csv(paste0(mypath, "raw\\transactions-2024-01-01-2025-01-01.csv")) 

data.2024 <- raw.2024 %>% 
  # refunds/exchanges (N=33), non-register transaction (N=93)
  filter(`Event Type` == "Payment", 
         Source == "Register") %>% 
  # drop columns with no variation 
  select(where(~ n_distinct(.) != 1)) %>%  
  # keep first names of customers, when available
  mutate(`Customer Name` = ifelse(str_detect(`Customer Name`, "@"), NA, `Customer Name`)) %>% 
  mutate(`Customer Name` = ifelse(str_detect(`Customer Name`, ","), NA, `Customer Name`)) %>% 
  separate_wider_delim(`Customer Name`, 
                       " ", 
                       names = c("first.name", "last.name"), 
                       too_few = "align_start", 
                       too_many = "merge") %>% 
  # drop personally-identifiable information
  select(!`Staff Name` & !last.name) %>% 
  # remove dollar signs and commas so that we can convert character vectors to amounts to numeric
  mutate(across(where(is.character), ~ str_replace(., "\\$", ""))) %>% 
  mutate(across(where(is.character), ~ str_replace(., ",", ""))) %>% 
  # clean up names 
  rename(date = Date, 
         time = Time, 
         customer.id = `Customer ID`, 
         gross.sales = `Gross Sales`, 
         discount = Discounts, 
         net.sales = `Net Sales`, 
         gift.card = `Gift Card Sales`,
         tax = Tax, 
         amt.total = `Total Collected`, 
         amt.card = Card, 
         amt.cash = Cash, 
         amt.gift.card = `Square Gift Card`,
         amt.other = `Other Tender`, 
         fees = Fees, 
         amt.net = `Net Total`, 
         card.type = `Card Brand`, 
         items = Description, 
         discount.type = `Discount Name`) %>% 
  # drop things we don't care about 
  select(!`Card Entry Methods` 
         & !`PAN Suffix` 
         & !`Transaction ID`
         & !`PAN Suffix`
         & !`Payment ID` 
         & !Details 
         & !`Customer Reference ID` 
         & !`Deposit ID` 
         & !`Deposit Date` 
         & !`Deposit Details` 
         & !`Fee Percentage Rate` 
         & !`Fee Fixed Rate`) %>% 
  # convert character variables that are dollars to numeric 
  mutate(across(c(gross.sales, discount, net.sales, gift.card, tax, amt.total, fees, amt.net), ~ as.numeric(.))) %>% 
  mutate(across(c(amt.card, amt.cash, amt.gift.card, amt.other), ~ as.numeric(.))) 

# keep only transactions from first half of 2024
data.2024 <- filter(data.2024, month(date) <=6)


# combine data frames from 2023 and 2024

provisions <- rbind(data.2023, data.2024)
  

# export data as a csv

write_csv(provisions, paste0(mypath, "ECON370-provisions-transactions.csv"))

