library(plumber)
library(R6)
library(future)
library(dplyr)
# library(pls)
# library(caret)
utilities <- new.env()
sys.source("./utilities/utilities_v2.R", envir = utilities, toplevel.env = utilities)

# Load the serialized model from S3
download_and_load_model <- function(url) {
  temp_file <- tempfile(fileext = ".rds")
  download.file(url, temp_file)
  loaded_model <- readRDS(temp_file)
  return(loaded_model)
}
# 
# ##Detach non-core packages:
# detachAllPackages <- function() {
#   
#   basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
#   
#   package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
#   
#   package.list <- setdiff(package.list,basic.packages)
#   
#   if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
#   
# }

# detachAllPackages()


Saved_Model<-readRDS("/home/user/Documents/projects/r.reproduce.foodspoilage.pls/chicken_reapplication_prediction_API_R_v3.RDS")
saved_model<- Saved_Model$new("/home/user/Documents/projects/r.reproduce.foodspoilage.pls/chicken_beef_decision_fusion/chicken_reapplication_models.RDS")

# Define the plumber API
#* @apiTitle BoilerPlate ML Prediction API
#* @apiDescription An API for serving a serialized model
#* @apiParam url The URL of the serialized model
#* @param file_list:[file]
#* @param predictor:name

#* @post /train
function(file_list, predictor=NULL) {
  # read_file<-  read.table(text = file_list[[1]], sep =",", header = TRUE, stringsAsFactors = FALSE)
  # loaded_model <- readRDS(url)
  read_files<- list()
  for (i in 1:length(file_list)){
    read_files[[names(file_list)[i]]]<- read.table(text = file_list[[i]], sep =",", header = TRUE, stringsAsFactors = FALSE)
    if (!is.null(predictor)){
      read_files[[names(file_list)[i]]]<-read_files[[names(file_list)[i]]][,!names(read_files[[names(file_list)[i]]]) %in% 
                                                                             c(predictor)]
    }  
  }
  prediction <- "SamplePredictionResult"
  
  return(list(prediction = prediction, read_files = read_files, body = body))
}

#* @post /predict
#* @param file_list:[file]
#* @param predictor:name
#* @serializer csv
function(file_list, predictor=NULL) {
  future::future({
    saved_model$execute(file_list, predictor)
  })
  # read_files.x.y.concat<-utilities$drop_NAs_join(read_files.x, predictor_vector)
}

#* @get /predict/dimensions
function(req, res) {
  dimensions <- loaded_model$metadata$dimensions
  
  return(list(dimensions = dimensions))
}

