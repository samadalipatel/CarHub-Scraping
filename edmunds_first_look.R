
#edmunds api dealer 
library(dplyr)
library(httr)
key <- 'mhqgys3zf3ne3e4f7bsjyecy'
zip <- '90064'
format <- 'json'
sample <- GET('http://api.edmunds.com/v1/api/', query = list(zipcode = zip, api_key = key, fmt = format))

#edmunds api cars 


sample <- GET('https://api.edmunds.com/api/vehicle/v2/makes?fmt=json&api_key=mhqgys3zf3ne3e4f7bsjyecy')
result <- content(sample)

#need to get style ID for car, then can get TMV 

#LOOP TO GET STYLES FOR EACH CAR OF EACH YEAR, NEED TO PUT IN API KEY 
models <- c('CL', 'ILX')
for(model in models){
  
  if (model == 'CL'){
    year <- c('2003', '2002', '2001')
    for(yr in year){
      url <- paste('https://api.edmunds.com/api/vehicle/v2/acura/', 
                   model,'/', year, '/fmt=json', sep = '')
      print(url)
    }
  }

  if (model == 'ILX'){
  year <- c('2016', '2013', '2015', 2017)
  for(yr in year){
    url <- paste('https://api.edmunds.com/api/vehicle/v2/acura/', 
                 model,'/', year, '/fmt=json', sep = '')
    print(url)
    }
  }
}



#MIGHT ALREADY HAVE INFO NEEDED TO GET STYLE ID WITHOUT QUERYING EDMUNDS IN result OBJECT
get_all_cars <- GET('https://api.edmunds.com/api/vehicle/v2/makes?fmt=json&api_key=mhqgys3zf3ne3e4f7bsjyecy')
all_cars <- content(sample)

#result is basically all info on all cars lmao, list in list of list of list 

#this list might be too annoying/complicated to scrape through, next line of code just looks at one company
#might be simpler 
get_honda <- GET('https://api.edmunds.com/api/vehicle/v2/honda/models?fmt=json&api_key=mhqgys3zf3ne3e4f7bsjyecy')
honda <- content(get_honda)

#it looks as though getting info for a single company is too much, might be instead worth it to learn from get_all_cars
#let's see if id in all_cars matches style ID from the example

length(all_cars$makes)
#find out which value is Honda
for (i in 1:62){
  if (all_cars$makes[[i]]$name == 'Honda')
    print(i)
}
all_cars$makes[[24]]$models[[1]]$years[[12]]


#need to find ACCORD EX 4DR SEDAN (2.3L 4CYL 4A)
for(i in 1:(length(honda$models[[1]]$years[[1]]$styles))){
  if (honda$models[[1]]$years[[1]]$styles[[i]]$name == 'EX 4dr Sedan')
    print(i)
}
honda$models[[1]]$years[[1]]$styles[[3]]$id



#seeing tmv in action
tmv_exp <- GET('https://api.edmunds.com/v1/api/tmv/tmvservice/calculateusedtmv?styleid=100001210&condition=Outstanding&mileage=0&zip=90069&fmt=json&api_key=mhqgys3zf3ne3e4f7bsjyecy')
tmv_content <- content(tmv_exp)
tmv_content$tmv$totalWithOptions$usedPrivateParty

#seeing with the id i got
tmv_exp_2 <- GET('https://api.edmunds.com/v1/api/tmv/tmvservice/calculateusedtmv?styleid=3875&condition=Outstanding&mileage=0&zip=90069&fmt=json&api_key=mhqgys3zf3ne3e4f7bsjyecy')

tmv_content_2 <- content(tmv_exp_2)
tmv_content_2$tmv$totalWithOptions$usedPrivateParty

#i want to get the excel file in R
library(readxl)
price_sheet <- read_excel(path = '~/Downloads/RealPriceCheckJuly2017.xlsx',col_names = TRUE)


##################################################
#########HONDA#############
honda_sheet <- price_sheet[price_sheet$Make=='Honda', c(2,3,5)]
#all cars can't work because it doesn't provide trim

library(stringr)

for (i in 1:length(honda$models[[1]]$years[[13]]$styles)){
  print(honda$models[[1]]$years[[13]]$styles[[i]]$name)
}

#probably gonna have to use str_locate() to determine if trims are okay 
for (i in 1:length(honda$models[[1]]$years[[13]]$styles)){
  if (str_detect(string = honda$models[[1]]$years[[13]]$styles[[i]]$name, pattern = 'LX'))
    print(honda$models[[1]]$years[[13]]$styles[[i]]$name)
}


honda_sheet[(honda_sheet$Model=='Accord') & (str_detect(string = honda_sheet$Style, pattern = 'LX')), 2]


tmv_exp_2 <- GET('https://api.edmunds.com/v1/api/tmv/tmvservice/calculateusedtmv',
                 query = list(styleid=17424,
                              condition='Outstanding',
                              mileage=0,
                              zip=90212,
                              fmt='json',
                              api_key='mhqgys3zf3ne3e4f7bsjyecy'))
tmv_content_2 <- content(tmv_exp_2)
tmv_content_2$tmv$totalWithOptions$usedPrivateParty








#so let's say year is 2002, code needs to find the value that returns 2002
for (k in 1:nrow(df_model)){
  print(df_model[[2]][[k]])
  for(l in 1:length(honda$models[[1]]$years)){
    if (honda$models[[1]]$years[[l]]$year == df_model[[2]][[k]]){
      yr_index <- l
      yr <- honda$models[[1]]$years[[l]]$year
    }
  }
  print(yr_index)
  print(yr)
  
  
}



models <- unique(honda_sheet$Model)
for (i in 1:length(models)){
  if (models[i] == 'Accord'){
    
    #find j for honda$models[j]
    for (j in 1:length(honda$models)){
      if (honda$models[[j]]$name == models[i]){
        mod <- j
        print(mod)
      }
    }
    
    #create model subset 
    df_model <- honda_sheet[honda_sheet$Model == models[i], c(2,3)]
    
    #let's take these years and find styles 
    lirst <- list()
    for (t in 1:1000){
      for (k in 1:nrow(df_model)){
        for(l in 1:length(honda$models[[mod]]$years)){
          if (honda$models[[mod]]$years[[l]]$year == df_model[[2]][[k]])
            yr_index <- l
          lirst[t] <- honda$models[[mod]]$years[[l]]$styles[[1]]$name
        }
        
        
      }
    }
  }
  
}
  

#want function that takes info from the spreadsheet, returns the year mod number and year number and shows all styles 












