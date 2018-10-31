library(stringi)

# Compile all polls for their csv files
if (exists('dataset')){
  rm('dataset')
}

folder <- "data"
file_list <- list.files(folder)

pollDate <- read.csv('pollDates.csv')
data$District <- as.character(data$District)

for (file in file_list){
  if (!exists("dataset")){
    dataset <- read.csv(file.path(folder,file))
    dataset$District <- stri_sub(str_split_fixed(file,'-', 3)[3],1,-5)
    # dataset <- merge(dataset, pollDate, by='District', all.x=TRUE)
  }
  
  if (exists("dataset")){
    temp_dataset <-read.csv(file.path(folder,file))
    temp_dataset$District <- stri_sub(str_split_fixed(file,'-', 3)[3],1,-5)
    # temp_dataset <- merge(temp_dataset, pollDate, by='District', all.x=TRUE)
    dataset<-bind_rows(dataset, temp_dataset)
    rm(temp_dataset)
  }
}
alreadyV <- dataset[dataset$likely =='Already voted', ]

alreadyVI <- alreadyV[alreadyV$partyid=='Independent (No party)', ]
