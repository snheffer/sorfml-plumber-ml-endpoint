library("dplyr")
drop_NAs_join <- function (read_files, predictor_vector=NULL){
  #Drop rows with NAs from each data table
  cleaned_tables <- lapply(read_files, function(dt) na.omit(dt))
  #Join data tables by sample name
  merged_data <- cleaned_tables %>%
    Reduce(function(x, y) transform(merge(x,y,by=0,all=TRUE), row.names=Row.names, Row.names=NULL), .)
  if(!is.null(predictor_vector)){
    merged_data <- transform(merge(merged_data, predictor_vector, by=0, all.x = TRUE), row.names=Row.names, Row.names=NULL)
  }
  print(merged_data)
  return(merged_data)
}