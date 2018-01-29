library(dplyr)
library(httr)
# Load the data
price_samp2 <- read.csv('~/Downloads/QA/price_sample.csv')
price_samp2 <- price_samp2[,-c(1,8,9)]
names(price_samp2)[6] <- 'Edmund\'s Price'

makes <- unique(price_samp$Make) %>% tolower() %>% 
  gsub(pattern = ' ',replacement = '') %>% 
  gsub(pattern = '-', replacement = '')


amgeneral <- content(get_amgeneral)
rm(get_amgeneral)

ferrari <- content(get_ferrari)
rm(get_ferrari)

maserati <- content(get_maserati)
rm(get_maserati)

plymouth <- content(get_plymouth)
rm(get_plymouth)

plymouth <- content(get_plymouth)
rm(get_plymouth)



# Generate get_make objects 
for(make in makes){
  name <- paste('get_', make, sep = '')
  assign(name, GET(paste('https://api.edmunds.com/api/vehicle/v2/',
                         make,
                         '/models?fmt=json&api_key=mhqgys3zf3ne3e4f7bsjyecy',
                         sep = '')))
}

# Generate the objects themselves 
for (make in makes){
  assign(make, 
         content(get(paste('get_', make, sep = ''))))
}

# 21 to 75 are all 'get_' objects; probably also could have used stringr to detect that fact
rm(ls()[21])

##THIS FUNCTION FAILS ON THE WHOLE ELSE THING - SEE YEAR_EXP   
  
det_year <- function(Make, Price_Sheet){
  num_models <- Make$modelsCount
  
  for (i in 1:nrow(Price_Sheet)){
    for (j in 1:num_models){
      if (Make$models[[j]]$name == Price_Sheet[i, 2]){
        mod <- j
      }
    }
    print(paste('The number', i, 'model is', mod))
    
    for (k in 1:length(Make$models[[mod]]$years)){
      if (Make$models[[mod]]$years[[k]]$year == Price_Sheet[i,4]){
        yr_index <- k
      }
      else
        yr_index <- 0
    }  
    print(paste('The number', i, 'year is', yr_index))  
    
    #create list to save all dfs
    df_list <- list()
    #create empty data frame, fill in with values of style and id number 
    df <- data.frame(matrix(NA, nrow = 1000, ncol = 2))
    
    if (yr_index > 0){
      for (l in 1:length(Make$models[[mod]]$years[[yr_index]]$styles)){
        df[l, 1] <- Make$models[[mod]]$years[[yr_index]]$styles[[l]]$name
        df[l, 2] <- Make$models[[mod]]$years[[yr_index]]$styles[[l]]$id
      }
    }
    
    if (yr_index == 0){
      df[1, 1] <- paste(Price_Sheet[i, 1], Price_Sheet[i, 2])
      df[1, 2] <- NA
    }
    
    #now save this df into the corresponding i value of df_list 
    df_list[[i]] <- df
    
  }
  return(df_list)
}  

################################################################################

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
      if (Make$models[[mod]]$years[[k]]$year == Price_Sheet[i,4]){
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

#how to do it 
styles(mod_index = mod_year(amgeneral, price_samp[3, ])[1],
       yr_index = mod_year(amgeneral, price_samp[3, ])[2],
       Make = amgeneral)

#ram has too many values, narrow down
library(dplyr)
library(stringr)
df <- styles(mod_index = mod_year(ram, price_samp[71, ])[1],
             yr_index = mod_year(ram, price_samp[71, ])[2],
             Make = ram)
df <- filter(df, str_detect(string = df$X1, pattern = 'SLT'))
df <- filter(df, str_detect(string = df$X1, pattern = 'Crew Cab'))
df <- filter(df, str_detect(string = df$X1, pattern = '5.6'))
df <- filter(df, !str_detect(string = df$X1, pattern = '4WD'))


df <- styles(mod_index = mod_year(ram, price_samp[72, ])[1],
             yr_index = mod_year(ram, price_samp[72, ])[2],
             Make = ram)
df <- filter(df, str_detect(string = df$X1, pattern = 'Regular Cab'))
df <- filter(df, str_detect(string = df$X1, pattern = 'Lone Star'))


df <- filter(df, str_detect(df$X1, 'AcuraWatch'))

#create own filter function for narrowing 
myfilt <- function(df, filter_string){
  library(dplyr)
  return_df <- filter(df, str_detect(df$X1, filter_string))
  return(return_df)
}

#check models for a brand 
for (i in 1:length(jaguar$models)){
  print(jaguar$models[[i]]$name)
}

#now we have our id's 
library(readxl)
style_id <- read_excel('~/Downloads/QA/style_ids.xlsx', col_names = 'ID', na = 'NA')
# pseudo
# price <- c(rep(NA, 150))
# if style_id[i, 1] == NA
#   price[i] <- NA
# else
#   run tmv stuff


#tmv stuff
tmv_exp_2 <- GET('https://api.edmunds.com/v1/api/tmv/tmvservice/calculateusedtmv',
                 query = list(styleid=10135,
                              condition='Outstanding',
                              mileage=0,
                              zip=90212,
                              fmt='json',
                              api_key='mhqgys3zf3ne3e4f7bsjyecy'))
tmv_content_2 <- content(tmv_exp_2)
tmv_content_2$tmv$totalWithOptions$usedPrivateParty



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

#run the function
prices <- edmunds_price(style_id)
#it appears as though many of them are 0, check the models of these cars 
zeros <- c( 5,6,7,8,9,16,19,20,21,23,24,145,146,147,149)
price_samp[zeros, c("Make", 'Model', "Year")]
#what happens to the price_query_content for these? 
#check one for instance, index 145 
id <- style_id[145,1]
price_query <- GET('https://api.edmunds.com/v1/api/tmv/tmvservice/calculateusedtmv',
                   query = list(styleid = id,
                                condition = 'Outstanding',
                                mileage = 0,
                                zip = 90212,
                                fmt = 'json',
                                api_key='mhqgys3zf3ne3e4f7bsjyecy'))

price_query_content <- content(price_query)
#looks like these cars are too new for edmunds to have data on them - let's change them to NA
prices[which(prices==0)] <- NA
prices

#now it's time to put this in the file with the other prices
#file name is toedit.xls
price_sample <- read_excel('~/Downloads/toedit.xls', na = 'NA',col_names = TRUE)
price_sample <- price_sample[,-c(1)]
write.csv(x = price_sample, '~/Downloads/QA/price_sample.csv')



