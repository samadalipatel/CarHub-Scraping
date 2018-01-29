#read in the sheet
library(readxl)
full_sheet <- read_excel('~/Downloads/RealPriceCheckJuly2017.xlsx', col_names = TRUE)

#load dplyr to filter
library(dplyr)
work_sheet <- filter(.data = full_sheet, Year == 2016 | Year == 2017)

#we want min three of all makes
length(unique(work_sheet$Make))
table_df <- table(work_sheet$Make) %>% as.data.frame(stringsAsFactors = FALSE)
three_ <- table_df[table_df$Freq >=3,1]
not_three_ <- table_df$Var1[table_df$Freq<3]

#the following is the limit for our while statement
length(three_) * 3

#write loop to generate all 81 randomly selected rows
df <- as.data.frame(matrix(nrow = 81, ncol = 10))
set.seed(81)
i <- 1
while (i<82){
  for (j in three_){
    df[i:(i+2), ] <- sample_n(work_sheet[work_sheet$Make == j, ], size = 3, replace = FALSE)
    i <- i+3
  }
}

#the not_three_ obs
not_three_df <- work_sheet[work_sheet$Make %in% not_three_, ]

#merge the two after changing df colnames
colnames(df) <- colnames(not_three_df)
df <- rbind(df, not_three_df)
head(df)
tail(df)

#export df to csv to generate kbb and cargurus
write.csv(x = df, file = 'df.csv', row.names = FALSE)

#now need to generate the get_make and make objects for api query
#generate get_make objects 
library(httr)
makes <- unique(df$Make) %>% tolower() %>% 
  gsub(pattern = ' ',replacement = '') %>% 
  gsub(pattern = '-', replacement = '')

for(make in makes){
  name <- paste('get_', make, sep = '')
  assign(name, GET(paste('https://api.edmunds.com/api/vehicle/v2/',
                         make,
                         '/models?fmt=json&api_key=mhqgys3zf3ne3e4f7bsjyecy',
                         sep = '')))
}

#generate make objects
for (make in makes){
  assign(make, 
         content(get(paste('get_', make, sep = ''))))
}

#insert functions to be used for edmunds manual query
mod_year <- function(Make, Price_Sheet){
  num_models <- Make$modelsCount
  
  for (i in 1:nrow(Price_Sheet)){
    for (j in 1:num_models){
      if (Make$models[[j]]$name == Price_Sheet[i, 2]){
        mod <- j
      }
    }
    #print(paste('The number', i, 'model is', mod))
    
    for (k in 1:length(Make$models[[mod]]$years)){
      if (Make$models[[mod]]$years[[k]]$year == Price_Sheet[i,5]){
        yr_index <- k
      }
    }  
    #print(paste('The number', i, 'year is', yr_index))  
  }
  return_vec <- c(mod, yr_index)
  return(return_vec)
}


styles <- function(mod_index, yr_index, Make){
  df <- data.frame(matrix(NA, nrow = 5, ncol = 2))
  for (i in 1:length(Make$models[[mod_index]]$years[[yr_index]]$styles)){
    
    df[i, 1] <- Make$models[[mod_index]]$years[[yr_index]]$styles[[i]]$name
    df[i, 2] <- Make$models[[mod_index]]$years[[yr_index]]$styles[[i]]$id
  }
  return(df)
}

myfilt <- function(df, filter_string){
  library(dplyr)
  library(stringr)
  return_df <- filter(df, str_detect(df$X1, filter_string))
  return(return_df)
}

#implementation
styles(mod_index = mod_year(honda, work_sheet[3, ])[1],
       yr_index = mod_year(honda, work_sheet[3, ])[2],
       Make = honda)

#check models for a brand 
for (i in 1:length(mclaren$models)){
  print(mclaren$models[[i]]$name)
}


for (i in 1:length(astonmartin$models[[3]]$years)){
  print(astonmartin$models[[3]]$years[[i]]$year)
}

#by now I have inputted the id's in the df.xls file
#read the file, take that column 
IDs <- read_excel('~/Downloads/df.xls') %>% select("Edmund's Price")

#insert function to query edmunds api 
edmunds_price <- function(id_df){
  #load needed packages
  library(httr)
  
  #make our price vector to store price values
  price <- c(rep(NA, nrow(id_df)))
  
  for(i in 1:nrow(id_df)){
    
    #for NA ID, return NA price
    if (is.na(id_df[i, 1])){
      price[i] <- NA
    }
    
    #for actual ID's, use httr
    else{
      id <- id_df[i,1]
      price_query <- GET('https://api.edmunds.com/v1/api/tmv/tmvservice/calculateusedtmv',
                         query = list(styleid = id,
                                      condition = 'Outstanding',
                                      mileage = 0,
                                      zip = 90212,
                                      fmt = 'json',
                                      api_key='mhqgys3zf3ne3e4f7bsjyecy'))
      
      price_query_content <- content(price_query)
      print(paste('here\'s ur data for ID#', price_query_content$tmv$totalWithOptions$usedTradeIn, 'u impatient fuck'))
      if (is.null(price_query_content$tmv$totalWithOptions$usedTradeIn))
        price[i] <- NA
      else
        price[i] <- price_query_content$tmv$totalWithOptions$usedTradeIn
    }
  }
  return(price)
}

prices <- edmunds_price(IDs)

#fix the NA's
prices[which(prices==0)] <- NA
prices

#put this row into df 
df <- read_excel('~/Downloads/df.xls')
df[,"Edmund's Price"] <- prices
write.csv(x = df, file = '~/Downloads/QA/2016_2017.csv', row.names = FALSE)



