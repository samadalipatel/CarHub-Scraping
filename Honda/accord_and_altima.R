#read in the sheet
library(readxl)
full_sheet <- read_excel('~/Downloads/RealPriceCheckJuly2017.xlsx', col_names = TRUE)

#load dplyr to filter
library(dplyr)
work_sheet <- filter(.data = full_sheet, (Make == 'Nissan' & Model == 'Altima') |
                       Make == 'Honda' & Model == 'Accord')

#order the years of work_sheet
hondas <- slice(work_sheet, 1:34)
nissans <- slice(work_sheet, 35:64)
hondas <- hondas[order(hondas$Year), ]
nissans <- nissans[order(nissans$Year),]

#create new work_sheet
work_sheet <- rbind(hondas, nissans)

#write to csv 
write.csv(x = work_sheet, file = 'altima_and_accord.csv')


#generate honda info
makes <- c('honda', 'nissan')
library(httr)

#too lazy to copy and paste link, will just run this code chunk from earlier lol 
for(make in makes){
  name <- paste('get_', make, sep = '')
  assign(name, GET(paste('https://api.edmunds.com/api/vehicle/v2/',
                         make,
                         '/models?fmt=json&api_key=mhqgys3zf3ne3e4f7bsjyecy',
                         sep = '')))
}
honda <- content(get_honda)
nissan <- content(get_nissan)

#load necessary functions to find style id's
#these functions have been edited to reflect the columns in the work_sheet
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

IDs <- read_excel('~/Downloads/accord_altima_IDs.xlsx', col_names = 'ID')

#edmunds scraping for after generating style_id
edmunds_price <- function(id_df){
  #load needed packages
  library(httr)
  
  #make our price vector to store price values
  price <- c(rep(NA, 149))
  
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
      print(price_query_content$tmv$totalWithOptions$usedTradeIn)
      if (is.null(price_query_content$tmv$totalWithOptions$usedTradeIn))
        price[i] <- NA
      else
        price[i] <- price_query_content$tmv$totalWithOptions$usedTradeIn
    }
  }
  return(price)
}

#find style_ids
prices <- edmunds_price(IDs)

#now load the edited work_sheet and put in prices for edmunds column
new_work_sheet <- read_excel('~/Downloads/altima_and_accord.xls', col_names = TRUE)
new_work_sheet <- new_work_sheet[,-c(1)]
new_work_sheet[,"Edmund's Price"] <- prices

#export data
write.csv(x = new_work_sheet, file = 'altima_and_accord.csv')

