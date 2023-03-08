                                         #R CASE STUDY 2:
                                #DATA EXPLORATORY ANALYSIS FOR CREDIT CARD DATA


getwd()

#custacq <- read.csv("C:/Users/0000/Desktop/ANA LABS 2021 COURSES/R assignments/R case study 2 (Credit card)')


#custacq <- read.csv("C:\\Users\\0000\\Desktop\\R case study 2 (Credit card).csv",TRUE,"," )
#custacq <- NULL
cust_acq <- read.csv("Customer Acqusition.csv",TRUE,"," )

cust_spend <- read.csv("spend.csv",TRUE,"," )

cust_repay <- read.csv("Repayment.csv",TRUE,"," )


data_list <- list(cust_acq,cust_spend,cust_repay)



"
table_a1 <- merge(x=custo,y=transa,
                by.x = "customer_Id",
                by.y = "cust_id")
Customer_Final <- merge(x=tablea,y=prodcat,by = "prod_cat_code", all = TRUE)


table_a1 <- merge(x=custo,y=transa,
                  by.x = "customer_Id",
                  by.y = "cust_id")
Customer_Final2 <- merge(x=tablea,y=prodcat,by = "prod_cat_code", all = TRUE,no.dups = TRUE)



table_a1 <- merge(x=cust_acq,y=cust_spend,
                  by.x = "Customer",
                  by.y = "Customer")

Customer_Finala2 <- merge(x=table_a1,y=cust_repay,by = "Customer", all = TRUE )




table_a3 <- merge(x=cust_repay,y=cust_spend,
                  by.x = "SL.No.",
                  by.y = "Sl.No.")

Customer_Finala2 <- merge(x=table_a1,y=cust_repay,by = "Customer", all = TRUE )
"

#table_a3 <- full_join(custo,transa, by=c("customer_Id"="cust_id"))
"
table_a3 <- full_join(cust_repay,cust_spend, by=c("SL.No."="Sl.No."))
#Customer_Final2 <- full_join(table1,prodcat, by="prod_cat_code")

Customer_Finala4 <- full_join(table_a3,cust_acq, by=c("SL.No."="No"))



table_a1 <- merge(x=cust_acq,y=cust_spend,
                  by.x = "Customer",
                  by.y = "Customer")
Customer_Finala1 <- merge(x=table_a1,y=cust_repay,by = "Customer", all = FALSE,na.rm = TRUE)

Customer_Finala1 <- merge(x=table_a1,y=cust_repay,by = "Customer", all = FALSE)
Customer_Finala1 <- merge(x=table_a1,y=cust_repay,by = "Customer", all = FALSE,na.rm = TRUE)


table_a2 <- full_join(cust_acq,cust_spend)
Customer_Finala2 <- full_join(table_a2,cust_repay,no.dups = TRUE)

#Customer_Finala2 <- full_join(table_a2,cust_repay,no.dups = TRUE,na.rm = TRUE)

na.rm = TRUE

#data[ ,':='(id=NULL)]  

Customer_Finala1[,':='(X=NULL)]

#data[ ,`:=`(x1 = NULL, x2 = NULL)]       # Remove columns

"
"
Customer_Finala1$X <- NULL


table_a2 <- full_join(cust_acq,cust_spend)
Customer_Finala3 <- full_join(table_a2,cust_repay,no.dups = TRUE,na.rm=TRUE)
Customer_Finala3$X <- NULL
Customer_Finala3$SL.No. <- NULL

na.rm
"
"
"


table_a3 <- full_join(cust_repay,cust_spend, by=c("SL.No."="Sl.No."))
#Customer_Final2 <- full_join(table1,prodcat, by="prod_cat_code")

Customer_Finala4 <- full_join(table_a3,cust_acq, by=c("SL.No."="No"),na.rm=TRUE)      #2nd alternative  

Customer_Finala4$X <- NULL

Customer_Finala4 <- na.omit(Customer_Finala4)    #imp #correct   #2nd alternative

mean(Customer_Finala4$Age,na.rm=TRUE) #correct







tablea4 <- merge(x=cust_repay,y=cust_spend,      #imp #correct   #FINAL ANS
                by.x = "SL.No.",
                by.y = "Sl.No.")
Customer_Finala5 <- merge(x=tablea4,y=cust_acq,by.x = "SL.No.",
                                               by.y="No", all = TRUE)

Customer_Finala5$X <- NULL


Customer_Finala5 <- na.omit(Customer_Finala5)   #imp #correct  #FINAL ANS #USED thought the CASE STUDY 



#customer tables Customer_Finala5 and Customer_Finala4 give the same results #imp



#Q1.	In the above dataset,
#a.	Incase age is less than 18, replace it with mean of age values.

mean_age <- mean(Customer_Finala1$Age)
mean_age

mean(Customer_Finala1$Age)

#Customer_Finala1$Age <-  (Customer_Finala1$Age<18=mean(Customer_Finala2$Age))

#Customer_Finala1$Age <-  Customer_Finala1$Age<18=46


mean(Customer_Finala4$Age,na.rm=TRUE) #correct



select(Customer_Finala1)  


str(Customer_Finala1)


Customer_Finala1[Age] <- sapply(Customer_Finala1[Age],
                                   function(x) replace(x, x %in% x<18,mean(Customer_Finala2$Age ))) 



#Traffic$Amount <- ifelse(Traffic$Amount < 18,18, Traffic$Amount)



Customer_Finala1$Age <- ifelse(Customer_Finala1$Age<18,43.24941,Customer_Finala1$Age)

Customer_Finala2$Age <- ifelse(Customer_Finala2$Age<18,43,Customer_Finala2$Age)

str(Customer_Finala1$Age)

print(Customer_Finala2$Age<18)

#data$num1[data$num1 == 1] <- 99               # Replace 1 by 99

Customer_Finala2$Age[Customer_Finala2$Age<18] <- 44




Customer_Finala4$Age[Customer_Finala2$Age<18] <- 44




Customer_Finala4$Age <- ifelse(Customer_Finala4$Age<18,46,Customer_Finala1$Age) #correct 


#b b.	In case spend amount is more than the limit, replace it with 50% of that customer's limit. 
#(customer's limit provided in acquisition table is the per transaction limit on his card)


head(Customer_Finala4$Limit,5)

head(Customer_Finala4,5)


#Customer_Finala4$Age <- ifelse(Customer_Finala4$Age<18,46,Customer_Finala1$Age) #correct 

#Customer_Finala5$Amount.y <- ifelse(Customer_Finala5$Amount.y>Customer_Finala5$Limit,0.5*Customer_Finala5$Limit,Customer_Finala5$Amount.y)

Customer_Finala4$Amount.y <- ifelse(Customer_Finala4$Amount.y>Customer_Finala4$Limit,0.5*Customer_Finala4$Limit,Customer_Finala5$Amount.y)




#c.	Incase the repayment amount is more than the limit, replace the repayment with the limit.     #correct final ans 
#.x is repayment  .y is amount spent    


#Customer_Finala4$Age <- ifelse(Customer_Finala4$Age<18,46,Customer_Finala1$Age) #correct 


Customer_Finala4$Amount.x <- ifelse(Customer_Finala4$Amount.x>Customer_Finala4$Limit,Customer_Finala4$Limit,Customer_Finala4$Amount.x)      #correct final ans 






#2.	From the above dataset create the following summaries:


#a.	How many distinct customers exist?
  
#distinct(Customer_Finala4$Customer)



Customer_Finala4 %>%
  select(Customer)  %>%
  distinct()

print("99 customers are distint" )


#b.	How many distinct categories exist?

Customer_Finala4 %>%
  select(Segment)  %>%
  distinct()

print("5 categories  are distint" )




#c.	What is the average monthly spend by customers?     
str(Customer_Finala4$Month.x)
str(Customer_Finala4$Month.y)

Customer_Finala4$Month.x <- as.Date(parse_date_time(Customer_Finala4$Month.x, c('dmy','dym','mdy','myd','ydm','ymd')))
class((Customer_Finala4$Month.x))

#df$month<- format(as.Date(df$date, format="%Y-%m-%d"),"%m")

Customer_Finala4$monthly <- format(as.Date(Customer_Finala4$Month.x,format="%Y-%m-%d"),"%B")




Customer_Finala4$Yearly <- format(as.Date(Customer_Finala4$Month.x,format="%Y-%m-%d"),"%y")







"
Customer_Finala4  %>%
  select(monthly,Amount.y) %>%
 # mean(Amount.y)
summarise(Amount.y, mean) %>%
ungroup()

"


#data_without_na %>%
 # summarise(across(where(is.numeric), mean))

#aggregate( X2 ~ Month + Year , df1 , mean )

X2 <- aggregate( Amount.y ~ monthly + Yearly , Customer_Finala4 , mean ) #CORRECT FINAL
X2
print("the avg monthly spend by customers is X2")





#d.	What is the average monthly repayment by customers? 
#.x is repayment  .y is amount spent    


X3 <- aggregate( Amount.x ~ monthly + Yearly , Customer_Finala4 , mean ) #CORRECT FINAL
X3

print("the avg monthly spend by customers is X3")









#e.	If the monthly rate of interest is 2.9%, what is the profit for the bank for each month?             
#(Profit is defined as interest earned on Monthly Profit. Monthly Profit = Monthly repayment - Monthly spend.
#Interest is earned only on positive profits and not on negative amounts)



#.x is repayment  .y is amount spent    

"
Customer_Finala4$profit

Customer_Finala4$Amount.x <- ifelse(Customer_Finala4$Amount.x>Customer_Finala4$Limit,Customer_Finala4$Limit,Customer_Finala4$Amount.x)      #correct final ans 


Customer_Finala4$profit <- ifelse(Customer_Finala4$Amount.x-Customer_Finala4$Amount.y>0,Customer_Finala4$profit*0.029,Customer_Finala4$profit)      #correct final ans 

Customer_Finala5$profit

Customer_Finala5$profit <- ifelse(Customer_Finala5$Amount.x-Customer_Finala5$Amount.y>0,Customer_Finala5$profit*0.029,Customer_Finala5$profit)      

Customer_Finala5$profit <- ifelse(Customer_Finala5$Amount.x-Customer_Finala5$Amount.y>0,Customer_Finala5$profit,Customer_Finala5$profit)      

"



#SIR ANS 
"
table_a1 <- merge(x=cust_acq,y=cust_spend,
                  by.x = "Customer",
                  by.y = "Customer")

Customer_Finala2 <- merge(x=table_a1,y=cust_repay,by = "Customer" )

Customer_Finala2$Amount.x <- ifelse(Customer_Finala2$Amount.x>Customer_Finala2$Limit,Customer_Finala2$Limit,Customer_Finala2$Amount.x)      

Customer_Finala2$profit =  Customer_Finala2$Amount.x-Customer_Finala2$Amount.y

Customer_Finala2$profit_bank <- ifelse(Customer_Finala2$profit>0,Customer_Finala2$profit*0.029,Customer_Finala2$profit)

"


Customer_Finala4$profit=Customer_Finala4$Amount.x-Customer_Finala4$Amount.y

Customer_Finala4$profit_bank <- ifelse(Customer_Finala4$profit>0,Customer_Finala4$profit*0.029,Customer_Finala4$profit)  #CORRECT FINAL

#vec_new <- vec[vec >= 0]             # Remove negative vector elements

#Customer_Finala4$profit_bank <-  Customer_Finala4$profit_bank[Customer_Finala4$profit_bank>0,na.rm=TRUE]

#data_new1[data_new1 < 0] <- NA       


Customer_Finala4$profit_bank[Customer_Finala4$profit_bank <0] <- NA     #CORRECT FINAL REMOVED NEAGTIVE VALUES 



#Q f.	What are the top 5 product types?
  #.x is repayment  .y is amount spent



#head(Customer_Finala4$Amount.y & Customer_Finala4$Amount.y)



"
Customer_Finala4 %>%
  select(Amount.y,Product)  %>%
  group_by(Product)  %>%
  head(Amount.y,5)  %>%
  distinct()  %>%
    ungroup()
"


#head(Customer_Final[order(-Customer_Final$total_amt),], 10)

x4 <- head(Customer_Finala4[order(-Customer_Finala4$Amount.y),],5)   #correct final

print("the top 5 product type is given by table  x4")






#g.	Which city is having maximum spend?                

head(Customer_Finala4[order(Customer_Finala4$City),],1)


top

#max(Customer_Finala4$)

x5 <- Customer_Finala4    %>%
     select(City,Amount.y)    %>%
     group_by(City)    %>%
     head()    %>%
     distinct()    %>%
     ungroup
     

print("Bombay city has max spend amount of 484342.47")    #correct final





#h.	Which age group is spending more money?          # correct check 





is.numeric(Customer_Finala4$Age)
str(Customer_Finala4$Age)
distinct(Customer_Finala4$Age)


x6 <- Customer_Finala4 %>%
    select(Age,Amount.y) %>%
     group_by(Age)%>%
     head()%>%
distinct()%>%
ungroup()


print("76 age group spends more money ")     




#i.	Who are the top 10 customers in terms of repayment?


 x7 <- head(Customer_Finala4[order(-Customer_Finala4$Amount.x),],10)   #correct final

 print("the top 10 customers in terms of repayment  is given by table  x7")
 
 
 
 
 
 
 #3.	Calculate the city wise spend on each product on yearly basis.  # correct 
 #Also include a graphical representation for the same.
 
 
 
 #.x is repayment  .y is amount spent
 
 
 x9 <-Customer_Finala4   %>%                             #correct final
      select(City,Yearly,Amount.y,Product)  %>%
       group_by(City,Product)    %>%
   summarise(sum1=sum(Amount.y),Yearly,Product) %>%
   distinct()     %>%
       ungroup()
 
 
 print("the city wise spend on each year is given by table x9")   #correct final
 




  ggplot(x9)+ aes(x= City , y=sum1, fill=Product ) + geom_bar(stat = "identity", position = "dodge" ,width = 0.5) #correct final
 
 
 
 
 
 #Q4.	Create graphs for
 #a.	Monthly comparison of total spends, city wise              # correct 
 
 x10 <- Customer_Finala4    %>%                             
 select(City,Amount.y,monthly)     %>%                             
 group_by(City,monthly)          %>%
 summarise(sumc=sum(Amount.y),City,monthly)  %>%
 distinct() %>%
 ungroup()
 
 


# ggplot(x10)+ aes(x= City, y=sumc, fill=monthly ) + geom_bar(stat = "identity", position = "dodge",beside = FALSE )   #check stacked not coming 
                                                             
 ggplot(x10)+ aes(x= City, y=sumc, fill=monthly ) + geom_bar(stat = "identity", position = "dodge",beside = FALSE )   #correct final 
 

 #ggplot(mpg) + 
#   geom_bar(aes(x = class, fill = factor(cyl)))
 
# ggplot(x10) + 
  # geom_bar(aes(x = City, fill = factor(monthly)))       #correct final
 
 
 #b.	Comparison of yearly spend on air tickets
 #.x is repayment  .y is amount spent
 
 x_11 <- Customer_Finala4      %>%                            
         select(Yearly,Amount.y,Type)             %>%                            
       group_by(Type="AIR TICKET",Yearly)      %>%                            
       summarise(sumyat=sum(Amount.y),Yearly)       %>%                            
       distinct()
       ungroup()
 
 
       ggplot(x_11)+ aes(x= Yearly, y=sumyat, fill=Type ) + geom_bar(stat = "identity", position = "dodge") #correct final
       
 
 
 
 
 #c.	Comparison of monthly spend for each product (look for any seasonality that exists in terms of spend)   
 
 "
       x_12 <- Customer_Finala4        %>%                               #correct final
   select(monthly,Amount.y,Type)           %>% 
    group_by(monthly,Type)        %>% 
    summarise(summp=sum(Amount.y),Type)        %>% 
      distinct()        %>% 
      ungroup()
  
 "
 
 x_12a<- Customer_Finala4        %>%                               #correct final
   select(monthly,Amount.y,Product)           %>% 
   group_by(monthly,Product)        %>% 
   summarise(summp1=sum(Amount.y),Product)        %>% 
   distinct()        %>% 
   ungroup()
 
 #ggplot(Orange) +
  # geom_line(aes(x = age, y = circumference, color = Tree))
 
 
 #ggplot(x_12) +                                                   #not complete
   #geom_line(aes(x =monthly , y =summp , color = Type))
 
 
 #ggplot(x_12) +                                                     #not complete
   #geom_line(aes(x =monthly , y =summp , 
     #                          linetype = Type))
 
 
 
 
 #ggplot(Orange) +
   #geom_line(aes(x = age, y = circumference, linetype = Tree))
 
 
 
 ggplot(x_12a)+ aes(x= Product, y=summp1, fill=monthly ) + geom_bar(stat = "identity", position = "dodge") #correct final check
 
 
 
 print("June has high sales")
 
 
 #Q5.	Write user defined R function to perform the following analysis:             #not complete
 
 # You need to find top 10 customers for each city in terms of their repayment amount by different products 
 # and by different time periods i.e. year or month. 
 # The user should be able to specify the product (Gold/Silver/Platinum) 
 # and time period (yearly or monthly) and the function should automatically take these inputs while identifying the top 10 customers.
 
 
 
 
 #.x is repayment  .y is amount spent
 
 
 
 #USE DATAEditR PACKAGE 
 
 "
 x_13 <- Customer_Finala4        %>%                               #not correct
   select(City,Amount.x,Type,Yearly,monthly,Product,Customer)           %>% 
   group_by(Product,Type)        %>% 
   #summarise(summp=sum(Amount.y),Type)        %>% 
   distinct()        %>% 
   ungroup()
 "
 
 
 x_13a <- Customer_Finala4 %>%
 select(City,Amount.x,Yearly,monthly,Product,Customer)    %>% 
   group_by(Product,Customer)         %>% 
   head(10) %>% 
   distinct()   %>%
 ungroup()
 
 
 
 x_14A <- Customer_Finala4 %>%
   select(City,Amount.x,Yearly,Product,Customer)    %>% 
   group_by(Product,Customer)         %>% 
   head(10) %>% 
   distinct()   %>%
   ungroup()
 
 
 
 
 x_15A <- Customer_Finala4 %>%
   select(City,Amount.x,monthly,Product,Customer)    %>% 
   group_by(Product,Customer)         %>% 
   head(10) %>% 
   distinct()   %>%
   ungroup()
 
 
 
 
 
 x_prod <- Customer_Finala4 %>%
   select(City,Amount.x,Product,Customer)    %>% 
   group_by(Product,Customer)         %>% 
   head(10) %>% 
   distinct()   %>%
   ungroup()
 
 
 
 
 
 
 
 
 DataEditR::x_13a
 
 
 
 
 check <- function(prod,time_period) {

      if(time_period=="monthly"
        Customer_Finala4 %>%
          select(City,Amount.x,monthly,Product,Customer)    %>% 
          group_by(Product,Customer)         %>% 
          head(10) %>% 
          distinct()   %>%
          ungroup()
      )
        
      result=x_14A
        {   
   
   else if(time_period=="Yearly"){
     
     Customer_Finala4 %>%
       select(City,Amount.x,Yearly,Product,Customer)    %>% 
       group_by(Product,Customer)         %>% 
       head(10) %>% 
       distinct()   %>%
       ungroup()
     
     
   }
   return(result)
 }
   
   
   
   
 
    check2 <- function(prod,time_period) {
        
        if(time_period=="monthly" ){
          
          result=x_14A
        }
          
          else if(time_period=="Yearly"){
            
          
          result=x_15A
        }}
      
 
 
 
 
 
        
        
        
 check3 <- function(produ,time_period) {
   
   if(time_period=="monthly" ){
     
     result=x_14A
   }
   
   else if(time_period=="Yearly"){
     
     
     result=x_15A
   
   }
     
     else if(produ=="Product"){
       
       result=x_prod
     
     }}
    
    
 

 
 
 customer_spend_repayment <- Customer_Finala4 %>%
                            select( Product,City,Amount.x,Customer.x,monthly,Yearly)  %>%
                              group_by(Customer.x,City,monthly)%>%
                               summarise(Avg_spend=mean(Amount.x),Product,Yearly) %>%
                                 head(10) %>% 
                                    distinct()   %>%
                                    ungroup()
                                 
    
 time_period <- Customer_Finala4 %>%
    select( Product,City,Amount.x,Customer.x,monthly,Yearly)  %>%
    distinct()   %>%
    ungroup()
 
    
    
    #result=customer_spend_repayment[customer_spend_repayment$Product.x==Product_category,]%>%
    #dplyr::group_by(Customer,City.x,monthly_spend)%>%
    #summarise(Avg_spend=mean(Amount.x))
 
 
 
 #x_prod <- Customer_Finala4 %>%
  #  select(City,Amount.x,Product,Customer.x)    %>% 
   # group_by(Product,Customer.x)         %>% 
    #head(10) %>% 
    #distinct()   %>%
    #ungroup()
 
 
 
 
 
 
 
 
 
 
 
 
 
 
  Top10customers=function(customer_spend_repayment,Product_category,time_period)
 {
    if (time_period=="monthly"){
       result=customer_spend_repayment[customer_spend_repayment$Product.x==Product_category,]%>%
          dplyr::group_by(Customer,City.x,monthly_spend)%>%summarise(Avg_spend=mean(Amount.x))
    }
    else if (time_period=="yearly"){
       result=customer_spend_repayment[customer_spend_repayment$Product.x==Product_category,]%>%
          dplyr::group_by(Customer,City.x,yearly_spend)%>%summarise(Avg_spend=mean(Amount.x))
    }
    return(result)
 }
 
 
 
 
  Top10customers2=function(customer_spend_repayment,Product_category,time_period)
  {
     if (time_period=="monthly"){
        result=customer_spend_repayment[customer_spend_repayment$Product]%>%
           dplyr::group_by(Customer,City.x,monthly_spend)
        %>%summarise(Avg_spend=mean(Amount.x))
     }
     else if (time_period=="yearly"){
        result=customer_spend_repayment[customer_spend_repayment$Product.x==Product_category,]%>%
           dplyr::group_by(Customer,City.x,yearly_spend)
        %>%summarise(Avg_spend=mean(Amount.x))
     }
     return(result)
  }
  
 
 Top10customers(05)
 
 
 
 Product_category <- Customer_Finala4 %>%
 select(Product)  %>%
 #group_by()%>%
 distinct()%>%
 ungroup()
 
 
 
 Top10customers3=function(Customer_Finala4,Product_category,time_period)
 {
    if (time_period=="monthly"){
       result=customer_spend_repayment
    }
    else if (time_period=="yearly"){
       result=customer_spend_repayment
    }
    return(result)
 }
 
 "
 Top10customers3=function(Customer_Finala4,Product_category,time_period)
 {
    if (time_period=="monthly"){
       result=customer_spend_repayment[customer_spend_repayment$Product.x==Product_category,]%>%
          dplyr::group_by(Customer,City.x,monthly_spend)%>%
          summarise(Avg_spend=mean(Amount.x))
    }
    else if (time_period=="yearly"){
       result=customer_spend_repayment[customer_spend_repayment$Product.x==Product_category,]%>%
          dplyr::group_by(Customer,City.x,yearly_spend)%>%
          summarise(Avg_spend=mean(Amount.x))
    }
    return(result)
 }
 
 "
 
 Top10customers3(03)
 
 
 
 
 
 
 
 
 
 
 
 
 check4 <- function(prod,time_period) {
    
    if(time_period=="monthly" ){
       
       result=x_14A
    }
    
    else if(time_period=="Yearly"){
       
       
       result=x_15A
    }}
 
 
 
 
 

 
 
    x_14A <- Customer_Finala4 %>%                                  #CORRECT FINAL
    select(City,Amount.x,Yearly,Product,Customer)    %>% 
    group_by(Product,Customer)         %>% 
    head(10) %>% 
    distinct()   %>%
    ungroup()
 
 
 
 
 x_15A <- Customer_Finala4 %>%                                          #CORRECT FINAL
    select(City,Amount.x,monthly,Product,Customer)    %>% 
    group_by(Product,Customer)         %>% 
    head(10) %>% 
    distinct()   %>%
    ungroup()
 
 
 
 
 
 x_prod <- Customer_Finala4 %>%                                            #CORRECT FINAL
    select(City,Amount.x,Product,Customer)    %>% 
    group_by(Product,Customer)         %>% 
    head(10) %>% 
    distinct()   %>%
    ungroup()
 
 
 
 
 
 check5 <- function(produ,time_period) {                                       #CORRECT FINAL
    
    if(time_period=="monthly" ){
       
       result=x_14A
    }
    
    else if(time_period=="Yearly"){
       
       
       result=x_15A
       
    }
    
    else if(produ=="Product"){
       
       result=x_prod
       
    }}
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
