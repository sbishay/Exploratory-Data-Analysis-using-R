setwd("C:\\Users\\subham b\\Desktop\\Data Analysis\\Case Studies\\R case study 2 (Credit card)")

# Import file
cus_aqu<- read.csv("Customer Acqusition.csv")
repay <- read.csv("Repayment.csv")
spend <- read.csv("spend.csv")

# import library
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

## 1
# a. Incase age is less than 18, replace it with mean of age value?

cus_aqu_age <- ifelse(cus_aqu$Age<18,mean(cus_aqu$Age,na.rm = TRUE),cus_aqu$Age)

# b. Incase spendamount is more then the limit ,replace it with 50% of limit?

cus_aqu_spend <- spend %>%
                 left_join(cus_aqu,by = "Customer")
View(spend)

cus_aqu_spend_amount <- ifelse(cus_aqu_spend$Amount>cus_aqu_spend$Limit,
                        cus_aqu_spend$Limit/2,
                        cus_aqu_spend$Amount) 



# c. Incase the repayment amount is more than the limit, replace the repayment with the limit

cus_aqu_repay <- repay %>%
                 left_join(cus_aqu,by = "Customer")
View(repay)
cus_aqu_repay_amount <- ifelse(cus_aqu_repay$Amount>cus_aqu_repay$Limit,
                               cus_aqu_repay$Limit,
                               cus_aqu_repay$Amount)



## 2.
# a. How many distinct customers exist?

dis_customer <- unique(cus_aqu$Customer)
length(dis_customer)

# b. How many distinct categories exist?  

dis_cat <- unique(cus_aqu$Product)
length(dis_cat)

# c. Average monthly spend by customer?

spend$Month <- dmy(spend$Month)
spend_cust <- spend %>% 
              group_by(month=month(spend$Month)) %>%
              dplyr::summarize(avg_spend=mean(Amount,na.rm = TRUE))

# d. What is the monthly repayment by customers?

repay$Month <- dmy(repay$Month)
repay_cust <- repay %>% 
              group_by(month=month(repay$Month)) %>%
              dplyr::summarize(avg_repay=mean(Amount,na.rm = TRUE))


# e. if the monthly rate of interest is 2.9% what is the profit for the bank for each month?

account_month <- repay_cust %>% 
                 left_join(spend_cust,by=c("month","month"))
account_month$profit_month <- case_when (
                              account_month$avg_repay - account_month$avg_spend >= 0 ~ 
                              account_month$avg_repay - account_month$avg_spend,
                              account_month$avg_repay - account_month$avg_spend <0 ~ 0
                              )
interest <- 2.9/100
account_month$profit <- account_month$profit_month*interest
account_month$profit_cum <- cumsum(account_month$profit_month)
View(account_month)

# f. What are the top 5 product type?

top_prod <- dplyr::arrange(spend %>% 
                           group_by(Type) %>% 
                           dplyr::summarize(totalamount=sum(Amount)),
                           desc(totalamount))
top_prod[1:5,1]

# g. Which city is having maximum spend?

city_spend <- dplyr::arrange(cus_aqu_spend %>%
                             group_by(City) %>% 
                             dplyr::summarise(totalamount=sum(Amount)),
                             desc(totalamount))
city_spend[1,1]

# h. Which age group is spend more money?

age_spend <- dplyr::arrange(cus_aqu_spend %>%
                            group_by(Age) %>% 
                            dplyr::summarise(totalamount=sum(Amount)),
                            desc(totalamount))
age_spend[1,1]


# i. Who are the top 10 customer in terms of repayment?

top10_cus <- dplyr::arrange(cus_aqu_repay %>%
                            group_by(Customer) %>% 
                            dplyr::summarise(totalamount=sum(Amount,na.rm=T)),
                            desc(totalamount))
top10_cus[1:10,1]


## 3. Calculate the city wise spend on each product on yearly basis.
##    Also include a graphical representation for the same?

cus_aqu_spend$Year <- year(dmy(spend$Month))
city_spend_Year <- cus_aqu_spend %>%
              group_by(City,Year) %>% 
              dplyr::summarise(totalamount=sum(Amount))

ggplot(city_spend_Year)+aes(City,totalamount)+geom_bar(stat = "identity",position = "dodge")+facet_grid(.~Year)
plotly::ggplotly(plot1)

## 4.
# a. Monthly comparison of total spends,city wise

spend$Month <- dmy(spend$Month)
city_spend_month <- cus_aqu_spend %>%
                    group_by(City,Month=month(spend$Month)) %>% 
                    dplyr::summarise(total.spend=sum(Amount,na.rm = TRUE))

# b. Comparison of yearly spend on air tickets?

spend$Year <- year(spend$Month)
tckt_year <- spend %>% 
             group_by(Year,Type) %>% 
             dplyr::summarise(year.spend=sum(Amount))
tckt_year[tckt_year$Type=="AIR TICKET",]


# c. Comparison of monthly spend for each product 

prod_spend_month <- spend %>% 
                    group_by(Type,Month=month(spend$month)) %>% 
                    dplyr::summarise(totalamount=sum(Amount)) 

plot(prod_spend_month$totalamount,type = "l", lty = 1) # There is no any seasonality in Data.

## 5. Write user defined R function to perform the following analysis: 
##    You need to find top 10 customers for each city in terms of their repayment 
##    amount by different products and by different time periods i.e. year or month.


top10_customers <- function(time_period=NULL,product=NULL)
  

{
  
  if (time_period=="Month") 
    
  {
    
    cus_aqu_repay$Month <- dmy(spend$Month)
    top <- cus_aqu_repay %>% group_by(Customer,City,Product,Month=month(spend$Month)) %>% 
                  dplyr::summarise(totalrepay=sum(Amount))
    top <- top[top$Product==product,]
    top <- dplyr::arrange(top,desc(totalrepay))
    top <- top[1:10,]
    
  }
  
  else if (time_period=="Year") 
  
  {
    
    cus_aqu_repay$Year <- year(dmy(spend$Month))
    top <- cus_aqu_repay %>% group_by(Customer,City,Product,Year) %>% 
                  dplyr::summarise(totalrepay=sum(Amount))
    top <- top[top$Product==product,]
    top <- dplyr::arrange(top,desc(totalrepay))
    top <- top[1:10,]
  
  }
  
  return(top)
  
}

##    The user should be able to specify the product 
##    (Gold/Silver/Platinum) and time period (yearly or monthly) and the function 
##    should automatically take these inputs while identifying the top 10 
##    customers.

top10_customers(time_period = "Year",product = "Gold") 
top10_customers(time_period = "Year",product = "Silver")
top10_customers(time_period = "Year",product = "Platinum")

top10_customers(time_period = "Month",product = "Gold")
top10_customers(time_period = "Month",product = "Silver")
top10_customers(time_period = "Month",product = "Platinum")


save.image(file = "~/Credit Card Case Study.RData")