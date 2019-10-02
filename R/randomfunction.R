randomize <- function(df){
  df2 <- df
  type <- sapply(df,class)
  for(i in 1:length(type)){
    if (type[i] == "character")
      df2[i] <- sample(1:nrow(unique(df[i])),nrow(df[i]), replace = TRUE)
    if (type[i]=="integer")
      df2[i] <- sample(min(df[i],na.rm = TRUE):max(df[i],na.rm = TRUE),nrow(df[i]), replace = TRUE)
    if(type[i]=="Date"){ 
      origin <- min(df[[1]], na.rm = TRUE)
      df2[i] <- sample(1:(max(df[[i]],na.rm = TRUE)-origin),nrow(df[i]), replace = TRUE)
      df2[i] <- as.Date(df2[[i]], origin = origin)
    }
    
    
  }
  df2<<-df2
}