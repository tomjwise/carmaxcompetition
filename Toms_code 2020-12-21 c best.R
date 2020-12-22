library(dplyr)
library(ggplot2)

### read and preprocess data ------------------------------------------------
### read data
data_df <- read.csv("CaseCompetitionData2021.csv", na.strings = "?")
Year_Make_Model_1990_2013 <- readxl::read_excel("Year-Make-Model-1990-present.xls")

### turn categorical customer_age into numeric "ave_age"
grepl("-", unique(data_df$customer_age))
unique(data_df$customer_age)
data_df$ave_age <- sapply(data_df$customer_age, function(x) mean(as.numeric(unlist(strsplit(x, " - ")))))

### turn categorical purchase_price into numeric "price"
grepl("-", unique(data_df$purchase_price))
unique(data_df$purchase_price)
data_df$price <- sapply(data_df$purchase_price, function(x) mean(as.numeric(unlist(strsplit(x, " - ")))))

### change "Model" to "purchase_model"
names(Year_Make_Model_1990_2013)[3] <- "purchase_model"
### change "Make" to "purchase_make"
names(Year_Make_Model_1990_2013)[2] <- "purchase_make"

### purchase_model to lower case
Year_Make_Model_1990_2013$purchase_model <- tolower(Year_Make_Model_1990_2013$purchase_model)
data_df$purchase_model <- tolower(data_df$purchase_model)

### purchase_make to lower case
Year_Make_Model_1990_2013$purchase_make <- tolower(Year_Make_Model_1990_2013$purchase_make)
data_df$purchase_make <- tolower(data_df$purchase_make)

### Delete unused columns from Year_Make_Model_1990_2013
Year_Make_Model_1990_2013_2 <- Year_Make_Model_1990_2013
Year_Make_Model_1990_2013_2 <- subset(Year_Make_Model_1990_2013_2, select = -c(1,4,6) )

### Remove duplicate rows Year_Make_Model_1990_2013_2
Year_Make_Model_1990_2013_2 <- Year_Make_Model_1990_2013_2 %>% distinct()

### Merge the two data
data_df <- merge(data_df, Year_Make_Model_1990_2013_2, by = c("purchase_make", "purchase_model"), all.x = TRUE)

### Turn NA Class to Unknown
data_df$Class[is.na(data_df$Class)] <- "Unkown"

### create Total Sale Value (tot_sale_value)
data_df$tot_sale_value <- data_df$price * 0.11 +
    ifelse(data_df$trade_in == 1, data_df$price * 0.054, 0) + ifelse(data_df$vehicle_financing == 1, data_df$price * 0.06, 0) + ifelse(data_df$vehicle_warranty_used == 1, data_df$price * 0.025, 0)

### create Total Customer Value (tot_customer_value)
tmp <- data_df$price * 0.11 +
    ifelse(data_df$trade_in == 1, data_df$price * 0.054, 0) + ifelse(data_df$vehicle_financing == 1, data_df$price * 0.06, 0) + ifelse(data_df$vehicle_warranty_used == 1, data_df$price * 0.025, 0)
data_df$tot_customer_value <- tmp * (data_df$subsequent_purchases + 1 + data_df$customer_previous_purchase)


data_df$profit_price <- data_df$price * 0.11 
data_df$profit_trade_in <- ifelse(data_df$trade_in == 1, data_df$price * 0.054, 0)
data_df$profit_vehicle_financing <- ifelse(data_df$vehicle_financing == 1, data_df$price * 0.06, 0)
data_df$profit_vehicle_warranty <- ifelse(data_df$vehicle_warranty_used == 1, data_df$price * 0.025, 0)

data_df$cust_profit_price <- data_df$price * 0.11 * (data_df$subsequent_purchases + 1 + data_df$customer_previous_purchase)
data_df$cust_profit_trade_in <- ifelse(data_df$trade_in == 1, data_df$price * 0.054, 0) * (data_df$subsequent_purchases + 1 + data_df$customer_previous_purchase)
data_df$cust_profit_vehicle_financing <- ifelse(data_df$vehicle_financing == 1, data_df$price * 0.06, 0) * (data_df$subsequent_purchases + 1 + data_df$customer_previous_purchase)
data_df$cust_profit_vehicle_warranty <- ifelse(data_df$vehicle_warranty_used == 1, data_df$price * 0.025, 0) * (data_df$subsequent_purchases + 1 + data_df$customer_previous_purchase)



# Histograms of Total sales & Customer Value ------------------------------
hist(data_df$tot_customer_value,
     main="Histogram for tot_customer_value",
     xlab="total customer value",
     border="blue",
     col="green",
     xlim=c(100,30000),
     las=1,
     breaks=250)

hist(data_df$tot_sale_value,
     main="Histogram for tot_sale_value",
     xlab="total sale value",
     border="blue",
     col="green",
     xlim=c(100,12000),
     las=1,
     breaks=40) #normal?

#qqnorm(data_df$tot_sale_value)
#qqline(data_df$tot_sale_value)
# Not normal


options(scipen = 999)

# which Car Brands have highest Total Sale Value? -------------------------
tmp1 <- tmp
median_tot_sale_val <- tapply(data_df$tot_sale_value, data_df$purchase_make, function(x) median(x, na.rm = TRUE))
ordered_makes <- names(sort(median_tot_sale_val))
tmp$purchase_make <- factor(data_df$purchase_make, levels = ordered_makes)

g <- ggplot(data_df, aes(x = purchase_make, y = tot_sale_value))
g <- g + geom_boxplot()
g
# No big suprise: bmw & merc are highest. Only suprising thing to me is how even everything else is....




### Sum of tot_sale_value per make ------------------------------------------

sum_vals <- tapply(data_df$tot_sale_value, data_df$purchase_make, function(var) sum(var, na.rm = TRUE))
barplot(sort(sum_vals))

for_order <- names(sort(sum_vals))

for_stacked_bar_df <- data.frame(price = tapply(data_df$profit_price, data_df$purchase_make, function(var) sum(var, na.rm = TRUE)),
                                 trade_in = tapply(data_df$profit_trade_in, data_df$purchase_make, function(var) sum(var, na.rm = TRUE)),
                                 vehicle_financing = tapply(data_df$profit_vehicle_financing, data_df$purchase_make, function(var) sum(var, na.rm = TRUE)),
                                 vehicle_warranty = tapply(data_df$profit_vehicle_warranty, data_df$purchase_make, function(var) sum(var, na.rm = TRUE)))
for_stacked_bar_df$make <- rownames(for_stacked_bar_df)
for_stacked_bar_df <- reshape2::melt(for_stacked_bar_df, id.vars = "make")

for_stacked_bar_df$make <- factor(for_stacked_bar_df$make, levels = for_order)

g <- ggplot(for_stacked_bar_df, aes(x = make, y = value, fill = variable))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
g <- g + scale_y_continuous(labels = scales::comma, expand = c(0, 0))
g
# The proportions within the bins just reflect the numbers that I chose for the 
# four profit variables. However with real data for these categories this graphic would become meaningful.



### sum of tot. customer value by age group ---------------------------------

tmp_df <- data_df[data_df$customer_age != "101+", ]
for_plot_df <- data.frame(price = tapply(tmp_df $cust_profit_price, tmp_df $customer_age, function(x) sum(x, na.rm = TRUE)),
                            trade_in = tapply(tmp_df $cust_profit_trade_in, tmp_df $customer_age, function(x) sum(x, na.rm = TRUE)),
                            vehicle_financing = tapply(tmp_df $cust_profit_vehicle_financing, tmp_df $customer_age, function(x) sum(x, na.rm = TRUE)),
                            vehicle_warranty = tapply(tmp_df $cust_profit_vehicle_warranty, tmp_df $customer_age, function(x) sum(x, na.rm = TRUE)))
for_plot_df$age_group <- rownames(for_plot_df)
for_plot_df <- reshape2::melt(for_plot_df, id.vars = "age_group")

g <- ggplot(for_plot_df, aes(x = age_group, y = value, fill = variable))
g <- g + geom_bar(stat="identity")
g <- g + scale_y_continuous(labels = scales::comma, expand = c(0, 0))
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g




### sum of tot. cust.val.  by gender ----------------------------------------

tmp_df <- data_df[data_df$customer_gender == "M", ]
tmp_df <- tmp_df[tmp_df$customer_age != "101+", ]
for_plot_df_M <- data.frame(price = tapply(tmp_df$cust_profit_price, tmp_df$customer_age, function(x) sum(x, na.rm = TRUE)),
                            trade_in = tapply(tmp_df$cust_profit_trade_in, tmp_df$customer_age, function(x) sum(x, na.rm = TRUE)),
                            vehicle_financing = tapply(tmp_df$cust_profit_vehicle_financing, tmp_df$customer_age, function(x) sum(x, na.rm = TRUE)),
                            vehicle_warranty = tapply(tmp_df$cust_profit_vehicle_warranty, tmp_df$customer_age, function(x) sum(x, na.rm = TRUE)))

tmp_df <- data_df[data_df$customer_gender == "F", ]
tmp_df <- tmp_df[tmp_df$customer_age != "101+", ]
for_plot_df_F <- data.frame(price = tapply(tmp_df$cust_profit_price, tmp_df$customer_age, function(x) sum(x, na.rm = TRUE)),
                            trade_in = tapply(tmp_df$cust_profit_trade_in, tmp_df$customer_age, function(x) sum(x, na.rm = TRUE)),
                            vehicle_financing = tapply(tmp_df$cust_profit_vehicle_financing, tmp_df$customer_age, function(x) sum(x, na.rm = TRUE)),
                            vehicle_warranty = tapply(tmp_df$cust_profit_vehicle_warranty, tmp_df$customer_age, function(x) sum(x, na.rm = TRUE)))

tmp_df <- data_df[data_df$customer_gender == "U", ]
tmp_df <- tmp_df[tmp_df$customer_age != "101+", ]
for_plot_df_U <- data.frame(price = tapply(tmp_df$cust_profit_price, tmp_df$customer_age, function(x) sum(x, na.rm = TRUE)),
                            trade_in = tapply(tmp_df$cust_profit_trade_in, tmp_df$customer_age, function(x) sum(x, na.rm = TRUE)),
                            vehicle_financing = tapply(tmp_df$cust_profit_vehicle_financing, tmp_df$customer_age, function(x) sum(x, na.rm = TRUE)),
                            vehicle_warranty = tapply(tmp_df$cust_profit_vehicle_warranty, tmp_df$customer_age, function(x) sum(x, na.rm = TRUE)))

for_plot_df_M$gender <- "M"
for_plot_df_F$gender <- "F"
for_plot_df_U$gender <- "U"

for_plot_df_M$age_group <- rownames(for_plot_df_M)
for_plot_df_F$age_group <- rownames(for_plot_df_F)
for_plot_df_U$age_group <- rownames(for_plot_df_U)

for_plot_df <- rbind(for_plot_df_M, for_plot_df_F, for_plot_df_U)

for_plot_df <- reshape2::melt(for_plot_df, id.vars = c("age_group", "gender"))

g <- ggplot(for_plot_df, aes(x = age_group, y = value, fill = variable))
g <- g + geom_bar(stat="identity")
g <- g + facet_wrap(gender~.)
g <- g + scale_y_continuous(labels = scales::comma, expand = c(0, 0))
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

# women are the same, but spend less - are there fewer female sales or are they spending less per purchase?





### Boxplot for Total Sale Value / Car Year ---------------------------------

boxplot(data_df$tot_sale_value ~ data_df$purchase_vehicle_year,
        main="Boxplot for Total Sale Value / Car Year",
        xlab="Car Year",
        ylab="Total Sale Value",
        border="blue",
        col="green")
# Total Sale Value is very steady for 5 or so years...



### Boxplot for Total Customer Value / Customer Age -------------------------

# remove over 101+ years age
tmp1 <- data_df[!data_df$customer_age == "101+", ]

boxplot(tmp1$tot_customer_value ~ tmp1$customer_age,
        main="Boxplot for Total Customer Value / Customer Age",
        xlab="Customer Age",
        ylab="Total Customer Value",
        border="blue",
        col="green")
# Rise and fall of total Customer Value around age 41-50, but not easy to see variation in the medians so:

### Bar plot of median Total Customer Value / Customer Age ------------------
tapply(tmp1$tot_customer_value, tmp1$customer_age, function(x) median(x, na.rm = TRUE))
barplot(tapply(tmp1$tot_customer_value, tmp1$customer_age, function(x) median(x, na.rm = TRUE)))
# More profit comes from 30-60 age group. No big surprise... 


### Distribution of Customer Age --------------------------------------------

tmp1 <- data_df[!data_df$customer_age == "101+", ]
table(tmp1$customer_age)
table(tmp1$customer_age) / sum(table(tmp1$customer_age)) * 100
barplot(table(tmp1$customer_age) / sum(table(tmp1$customer_age)) * 100)
hist(tmp1$ave_age)
# Most customers are between 30 and 50- no surprise....



### Boxplot of tot_customer_value and customer_income -----------------------

data_df$customer_income <- factor(data_df$customer_income, levels = c("0 - 20000", 
                                                                      "20001 - 40000", 
                                                                      "40001 - 60000", 
                                                                      "60001 - 80000",
                                                                      "80001 - 100000", 
                                                                      "100001 - 120000",
                                                                      "120001 - 140000", 
                                                                      "140001 - 160000",
                                                                      "160001 - 180000", 
                                                                      "180001 - 200000", 
                                                                      "200001+"))

boxplot(data_df$tot_customer_value ~ data_df$customer_income)

# sum of tot Cust value, by customer income split by gender ---------------
tmp_df <- data_df[data_df$customer_gender == "M", ]
for_plot_df_M <- data.frame(price = tapply(tmp_df$cust_profit_price, tmp_df$customer_income, function(x) sum(x, na.rm = TRUE)),
                            trade_in = tapply(tmp_df$cust_profit_trade_in, tmp_df$customer_income, function(x) sum(x, na.rm = TRUE)),
                            vehicle_financing = tapply(tmp_df$cust_profit_vehicle_financing, tmp_df$customer_income, function(x) sum(x, na.rm = TRUE)),
                            vehicle_warranty = tapply(tmp_df$cust_profit_vehicle_warranty, tmp_df$customer_income, function(x) sum(x, na.rm = TRUE)))

tmp_df <- data_df[data_df$customer_gender == "F", ]
for_plot_df_F <- data.frame(price = tapply(tmp_df$cust_profit_price, tmp_df$customer_income, function(x) sum(x, na.rm = TRUE)),
                            trade_in = tapply(tmp_df$cust_profit_trade_in, tmp_df$customer_income, function(x) sum(x, na.rm = TRUE)),
                            vehicle_financing = tapply(tmp_df$cust_profit_vehicle_financing, tmp_df$customer_income, function(x) sum(x, na.rm = TRUE)),
                            vehicle_warranty = tapply(tmp_df$cust_profit_vehicle_warranty, tmp_df$customer_income, function(x) sum(x, na.rm = TRUE)))

tmp_df <- data_df[data_df$customer_gender == "U", ]
for_plot_df_U <- data.frame(price = tapply(tmp_df$cust_profit_price, tmp_df$customer_income, function(x) sum(x, na.rm = TRUE)),
                            trade_in = tapply(tmp_df$cust_profit_trade_in, tmp_df$customer_income, function(x) sum(x, na.rm = TRUE)),
                            vehicle_financing = tapply(tmp_df$cust_profit_vehicle_financing, tmp_df$customer_income, function(x) sum(x, na.rm = TRUE)),
                            vehicle_warranty = tapply(tmp_df$cust_profit_vehicle_warranty, tmp_df$customer_income, function(x) sum(x, na.rm = TRUE)))

for_plot_df_M$gender <- "M"
for_plot_df_F$gender <- "F"
for_plot_df_U$gender <- "U"

for_plot_df_M$income_group <- rownames(for_plot_df_M)
for_plot_df_F$income_group <- rownames(for_plot_df_F)
for_plot_df_U$income_group <- rownames(for_plot_df_U)

for_plot_df <- rbind(for_plot_df_M, for_plot_df_F, for_plot_df_U)

for_plot_df <- reshape2::melt(for_plot_df, id.vars = c("income_group", "gender"))

for_plot_df$income_group <- factor(for_plot_df$income_group, levels = c("0 - 20000", 
                                                                        "20001 - 40000", 
                                                                        "40001 - 60000", 
                                                                        "60001 - 80000",
                                                                        "80001 - 100000", 
                                                                        "100001 - 120000",
                                                                        "120001 - 140000", 
                                                                        "140001 - 160000",
                                                                        "160001 - 180000", 
                                                                        "180001 - 200000", 
                                                                        "200001+"))

g <- ggplot(for_plot_df, aes(x = income_group, y = value, fill = variable))
g <- g + geom_bar(stat="identity")
g <- g + facet_wrap(gender~.)
g <- g + scale_y_continuous(labels = scales::comma, expand = c(0, 0))
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
g
# most profit comes from the 40-60 year olds of both females and males



### Boxplot of tot_customer_value and customer_gender -----------------------

boxplot(data_df$tot_customer_value ~ data_df$customer_gender)
# profit per sale is very similar between males and feales?


### Barplot of vehicle Class / % of Class  ----------------------------------
# I need to order this and make it clearer ############################

table(data_df$Class, exclude = FALSE)
table(data_df$Class, exclude = FALSE) / sum(table(data_df$Class, exclude = FALSE)) * 100

barplot(table(data_df$Class, exclude = FALSE) / sum(table(data_df$Class, exclude = FALSE)) * 100)

### write data_df to .csv
write.csv(data_df, file = "CarMax_R_data_df.csv")


### investigate Class
car_class <- unique(data_df$Class) 
car_class <- as.data.frame(table(data_df$Class))
car_class <- car_class[order(car_class$Freq,decreasing = TRUE),]
car_class

test1 <- as.data.frame(table(data_df$purchase_model))
test1

test1 <- data_df %>% dplyr::filter(data_df$purchase_model == 'PANAMERA' 
                                   & purchase_vehicle_year == '2013')
test1 <- data_df %>% dplyr::filter(purchase_model == 'PANAMERA')
test1
test1 <- filter(data_df$purchase_model  == 'PANAMERA')
test1 <- dplyr::filter(data_df, !grepl("PANAMERA", ))
