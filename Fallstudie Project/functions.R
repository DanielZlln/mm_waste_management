percent_na <- function(df) {
  for (i in 1:ncol(df)) {
    if (is.numeric(df[[i]])) {
      print(paste0(colnames(df[i]),": ", 
                   round(mean(is.na(df[i])),3)))
    }
  }
}

exclude_na <- function(df) {
  for (i in 1:ncol(df)) {
    if (anyNA(df[[i]]) & is.numeric(df[[i]])) {
      for (j in 1:length(df[,i])) {
        if (is.na(df[j,i])) {
          df[j,i] <- mean(df[,i], na.rm = T) 
        }
      }
    }
  }
  return(df)
}

exclude_chr_rows <- function(df) {
  for (i in 1:ncol(df)) {
    for (j in 1:length(df[[i]])) {
      if (is.na(df[j,i])) {
        df[j,] <- df[-j,]
      }
    }
  }
  return(df)
}


exclude_chr_row <- function(df) {
  df %>% 
    filter(complete.cases(.))
}


count_dist_dimesnion <- function(df, dimension_in_quotation_marks) {
  df %>% 
    summarise(dist_dimension = n_distinct(df[[dimension_in_quotation_marks]]))
}


circlePlot <- function(df, column, label){
  
  pck <- circleProgressiveLayout(df[[column]], sizetype = "area")
  
  mydata <- cbind(df, pck)
  
  myplotcord <- circleLayoutVertices(pck)
  
  p1 <- ggplot() + 
    geom_polygon(data = myplotcord, 
                 aes(x,y, group = id, 
                     fill = as.factor(id))) +
    geom_text(data = mydata, aes(x,y, size = df[[column]], 
                                 label = paste0(df[[label]]))) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "none") +
    labs(title = "Durchschnittliche Abfallmenge pro Provinz")
  p1
}


chr_to_factor <- function(df) {
  for (i in 1:ncol(df)) {
    if (is.character(df[[i]])) {
      df[[i]] <- as.factor(df[[i]])
    }
  }
  return(df)
}


fa_rotation <- function(df, factor, fa_rotation) {
  fa_df <- factanal(df, 
                    factors = factor,
                    scores = "Bartlett",
                    rotation = fa_rotation)
  return(fa_df)
}
