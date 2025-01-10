#### build functions for Project 9 ----
descriptev_stat <- function (data, subject_start = NULL, subject_end = NULL) {
 # stoping thefunction if the data is < 10 
  
   if (nrow (data) < 10) {
    stop ("data is too short")
   }

  if (!is.null(subject_start) & !is.null(subject_end)) {
    if (!"subject_id" %in% names(data)) {
      stop("The data must include a 'subject_id' column to filter by subject range.")
    }
    
    data <- data[data$subject_id >= subject_start & data$subject_id <= subject_end, ]
  
  variable_names <- names(data)
  
  results <- data.frame(
    Variable = character(),
    Statistic = character(),
    Value = character(),
    stringsAsFactors = FALSE
  )
  
  for (variable in variable_names) {
    
    if (class(data[[variable]]) %in%
        c('factor', 'character')) {
      levels_counts <- table(data[[variable]])
      for (level in names(levels_counts)) {
        results <- rbind(results, data.frame(
          Variable = variable,
          Statistic = paste('Level:', level),
          Value = levels_counts[level]
        ))
      }
    } else if (class(data[[variable]]) %in%
               c('numeric', 'integer')) {
      results <- rbind(results, data.frame(
        Variable = variable,
        Statistic = 'Mean',
        Value = mean(data[[variable]], na.rm = TRUE)
      ))
      results <- rbind(results, data.frame(
        Variable = variable,
        Statistic = 'Standard Deviation',
        Value = sd(data[[variable]], na.rm = TRUE)
      ))
      results <- rbind(results, data.frame(
        Variable = variable,
        Statistic = 'Minimum',
        Value = min(data[[variable]], na.rm = TRUE)
      ))
      results <- rbind(results, data.frame(
        Variable = variable,
        Statistic = 'Maximum',
        Value = max(data[[variable]], na.rm = TRUE)
      ))
    }}
  return(results)
}}

