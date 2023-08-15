library(plumber)
utilities <- new.env()
sys.source("./utilities/utilities_v2.R", envir = utilities, toplevel.env = utilities)

# Load the serialized model from S3
download_and_load_model <- function(url) {
  temp_file <- tempfile(fileext = ".rds")
  download.file(url, temp_file)
  loaded_model <- readRDS(temp_file)
  return(loaded_model)
}


# Create a mock model with metadata
mock_model <- list(
  metadata = list(
    dimensions = c("feature1", "feature2", "feature3")
  ),
  trained_model = "YourTrainedModelObject"
)

# Save the mock model as an RDS file
saveRDS(mock_model, "mock_model.rds")

library(plumber)

# Load the serialized model
loaded_model <- readRDS("mock_model.rds")


# Define the plumber API
#* @apiTitle BoilerPlate ML Prediction API
#* @apiDescription An API for serving a serialized model
#* @apiParam url The URL of the serialized model
#* @param file_list:[file]
#* @param predictor:name

#* @post /predict
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

#* @post /train
#* @param file_list:[file]
#* @param predictor:name
#* @serializer csv
function(file_list, predictor=NULL) {
  read_files<- list()
  for (i in 1:length(file_list)){
    # cat(file_list[[i]])
    read_files[[names(file_list)[i]]]<- read.table(text = file_list[[i]], sep =",", header = TRUE, stringsAsFactors = FALSE)
    colnames(read_files[[names(file_list)[i]]])[1]<-"Sample"
    cat("COLNAMES:\n")
    cat(colnames(read_files[[names(file_list)[i]]]))
    cat("\n")
  }
  model_metadata<- list()
  model_metadata$xlabels<-list()
  predictor_vector<- NULL
  if (!is.null(predictor) && length(read_files)){
    predictor_vector<- read_files[[names(file_list)[1]]][,c("Sample",predictor)]
  }
  if(length(read_files)){
    for(i in 1:length(read_files)){
      read_files[[names(read_files)[i]]]<-read_files[[names(read_files)[i]]][,!names(read_files[[names(read_files)[i]]]) %in% c(predictor)]
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
  cat("End file X labels\n")
  cat("\n")
  concatenated_files<-utilities$drop_NAs_join(read_files, predictor_vector)
}

#* @get /predict/dimensions
function(req, res) {
  dimensions <- loaded_model$metadata$dimensions
  
  return(list(dimensions = dimensions))
}

