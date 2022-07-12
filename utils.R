# Define a function to avoid repeating code
checkPaperQuants <- function(data, outcomeType){
  stopifnot("Data has no outcome variable" = "outcome" %in% names(data))
  data %>% 
    filter(outcome == outcomeType) %>% 
    summary()
}