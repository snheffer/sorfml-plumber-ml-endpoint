library(plumber)

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
#* @post /predict
function(file_list) {
  read_file<-  read.table(text = file_list[[1]], sep =",", header = TRUE, stringsAsFactors = FALSE)
  # loaded_model <- readRDS(url)
  read_files<- list()
  # for (file in file_list){
  #   read_files[]
  # }
  for (i in 1:length(file_list)){
    cat(file_list[[i]])
    read_files[[names(file_list)[i]]]<- read.table(text = file_list[[i]], sep =",", header = TRUE, stringsAsFactors = FALSE)
  }
  prediction <- "SamplePredictionResult"
  
  return(list(prediction = prediction, file = read_file, read_files = read_files, body = body))
}

#* @post /train
#* @param file_list:[file]
function(file_list) {
  read_files<- list()
  for (i in 1:length(file_list)){
    cat(file_list[[i]])
    read_files[[names(file_list)[i]]]<- read.table(text = file_list[[i]], sep =",", header = TRUE, stringsAsFactors = FALSE)
  }
}

#* @get /predict/dimensions
function(req, res) {
  dimensions <- loaded_model$metadata$dimensions
  
  return(list(dimensions = dimensions))
}

