library("dplyr")
drop_NAs_join <- function (read_files){
  #Drop rows with NAs from each data table
  cleaned_tables <- lapply(read_files, function(dt) na.omit(dt))
  #Join data tables by sample name
  merged_data <- cleaned_tables %>%
    Reduce(function(x, y) left_join(x, y, by = names(x)[[1]]), .)
  return(merged_data)
}