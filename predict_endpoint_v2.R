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


Saved_Model<-readRDS("/home/user/Documents/projects/r.reproduce.foodspoilage.pls/chicken_reapplication_prediction_API_R6.RDS")
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
    read_files<- list()
    transformed_files<- list()
    for (i in 1:length(file_list)){
      read_files[[names(file_list)[i]]]<- read.table(text = file_list[[i]], sep =",", header = TRUE, stringsAsFactors = FALSE)
      colnames(read_files[[names(file_list)[i]]])[1]<-"Sample"
      cat("COLNAMES:\n")
      cat(colnames(read_files[[names(file_list)[i]]]))
      cat("\n")
      
      
      ###transform raw files to transformed state.
      transformed_files[[i]]<-saved_model$pre_proc[[i]](saved_model,read_files[[i]])
    }
    model_metadata<- list()
    model_metadata$xlabels<-list()
    predictor_vector<- NULL
    read_files.x<- list()
    read_files.y<- list()
    if (!is.null(predictor) && length(read_files)){
      predictor_vector<- read_files[[names(file_list)[1]]][,c("Sample",predictor)]
    }
    if(length(read_files)){
      for(i in 1:length(read_files)){
        read_files.x[[i]]<-read_files[[names(read_files)[i]]][,!names(read_files[[names(read_files)[i]]]) %in% c(predictor)]
        read_files.y[[i]]<-read_files[[names(read_files)[i]]][,names(read_files[[names(read_files)[i]]]) %in% c(predictor)]
        model_metadata$xlabels[[i]]<-names(read_files[[names(read_files)[i]]])
      }
    }
    
    cat("DFs:\n")
    for(file in read_files){
      print(file)
    }
    cat("\n Predictor_Vector:\n")
    print(predictor_vector)
    cat("\n File X Labels\n")
    print(model_metadata$xlabels)
    cat("---------\n")
    cat("\n Read Files X Values\n")
    print(read_files.x)
    cat("---------\n")
    cat("\n Read Files Y Values\n")
    print(read_files.y)
    cat("---------\n")
    cat("\n")
    as.data.frame(saved_model$main_proc(transformed_files))
    
  })
  # read_files.x.y.concat<-utilities$drop_NAs_join(read_files.x, predictor_vector)
}

#* @get /predict/dimensions
function(req, res) {
  dimensions <- loaded_model$metadata$dimensions
  
  return(list(dimensions = dimensions))
}

