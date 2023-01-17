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


circlePlot <- function(df, column, label, own_title){
  
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
    labs(title = own_title)
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


all_distance <- function(df,dist_method, fusio_method,kluster) {
  
  distance <- dist(scale(df, 
                         center = TRUE, 
                         scale = TRUE),
                   method = dist_method,
                   p = 2)
  
  
  # Hierarchische Clusteranalyse
  h <- hclust(distance, method = fusio_method)
  
  # Dendrogramm
  plot(h, main = paste("Distanzberechnung:", dist_method, 
                       "\nFusionierungsmethode:", fusio_method))
  
  # Cluster im Dendogramm makieren
  rect.hclust(h, k= kluster, border = "red")
  
}

all_distance_daisy <- function(df,metric_dist, metrics,fusio_method,kluster) {
  
  distance_daisy <- daisy(scale(df),
                          metric = metric_dist,
                   stand = F)
  
  
  # Hierarchische Clusteranalyse
  dia_agnes <- agnes(distance_daisy, 
                     diss = T,
                     metric = metrics,
                     stand = T,
                     method = fusio_method)
  
  # Dendrogramm
  plot(dia_agnes, main = paste("Methode Distanzberechnung:", metric_dist, 
                               "\nMetrix Distanzberechnung:", metrics,
                                "\nFusionierungsmethode:", fusio_method),
       ask = F)
  
  # Cluster im Dendogramm makieren
  rect.hclust(dia_agnes, k= kluster, border = "red")
  
}

plot_groups <- function(df) {
  list_agg <- c("mean", "sum", "median", "min", "max")
  
  for (i in 1:length(list_agg)) {
    if(list_agg[i] == "mean"){
      df_agg <- df %>% group_by(cluster) %>%  summarise_all(mean)
    } else if (list_agg[i] == "sum"){
      df_agg <- df %>% group_by(cluster) %>%  summarise_all(sum)
    } else if(list_agg[i] == "median"){
      df_agg <- df %>% group_by(cluster) %>%  summarise_all(median)
    } else if(list_agg[i] == "min"){
      df_agg <- df %>% group_by(cluster) %>%  summarise_all(min)
    } else if(list_agg[i] == "max"){
      df_agg <- df %>% group_by(cluster) %>%  summarise_all(max)
    }
    
    df_plot <- pivot_longer(df_agg, -cluster, 
                            names_to="variable", values_to="value")
    
    pgg <-  ggplot(df_plot,aes(x = cluster,y = value)) + 
      geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
      ggtitle(paste0(list_agg[i], " per cluster"))
    
    print(pgg)
  }
}


# fix von stackoverflow, iwo in der function soll ein bug sein,
# braucht aber sehr lange
# fviz_nbclust <- function (x, FUNcluster = NULL, method = c("silhouette", "wss",
#                                                            "gap_stat"), diss = NULL, k.max = 10, nboot = 100, verbose = interactive(),
#                           barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue",
#                           print.summary = TRUE, ...)
# {
#   set.seed(123)
#   if (k.max < 2)
#     stop("k.max must bet > = 2")
#   method = match.arg(method)
#   if (!inherits(x, c("data.frame", "matrix")) & !("Best.nc" %in%
#                                                   names(x)))
#     stop("x should be an object of class matrix/data.frame or ",
#          "an object created by the function NbClust() [NbClust package].")
#   if (inherits(x, "list") & "Best.nc" %in% names(x)) {
#     best_nc <- x$Best.nc
#     if (any(class(best_nc) == "numeric") )
#       print(best_nc)
#     else if (any(class(best_nc) == "matrix") )
#       .viz_NbClust(x, print.summary, barfill, barcolor)
#   }
#   else if (is.null(FUNcluster))
#     stop("The argument FUNcluster is required. ", "Possible values are kmeans, pam, hcut, clara, ...")
#   else if (!is.function(FUNcluster)) {
#     stop("The argument FUNcluster should be a function. ",
#          "Check if you're not overriding the specified function name somewhere.")
#   }
#   else if (method %in% c("silhouette", "wss")) {
#     if (is.data.frame(x))
#       x <- as.matrix(x)
#     if (is.null(diss))
#       diss <- stats::dist(x)
#     v <- rep(0, k.max)
#     if (method == "silhouette") {
#       for (i in 2:k.max) {
#         clust <- FUNcluster(x, i, ...)
#         v[i] <- .get_ave_sil_width(diss, clust$cluster)
#       }
#     }
#     else if (method == "wss") {
#       for (i in 1:k.max) {
#         clust <- FUNcluster(x, i, ...)
#         v[i] <- .get_withinSS(diss, clust$cluster)
#       }
#     }
#     df <- data.frame(clusters = as.factor(1:k.max), y = v,
#                      stringsAsFactors = TRUE)
#     ylab <- "Total Within Sum of Square"
#     if (method == "silhouette")
#       ylab <- "Average silhouette width"
#     p <- ggpubr::ggline(df, x = "clusters", y = "y", group = 1,
#                         color = linecolor, ylab = ylab, xlab = "Number of clusters k",
#                         main = "Optimal number of clusters")
#     if (method == "silhouette")
#       p <- p + geom_vline(xintercept = which.max(v), linetype = 2,
#                           color = linecolor)
#     return(p)
#   }
#   else if (method == "gap_stat") {
#     extra_args <- list(...)
#     gap_stat <- cluster::clusGap(x, FUNcluster, K.max = k.max,
#                                  B = nboot, verbose = verbose, ...)
#     if (!is.null(extra_args$maxSE))
#       maxSE <- extra_args$maxSE
#     else maxSE <- list(method = "firstSEmax", SE.factor = 1)
#     p <- fviz_gap_stat(gap_stat, linecolor = linecolor,
#                        maxSE = maxSE)
#     return(p)
#   }
# }
# 
# .viz_NbClust <- function (x, print.summary = TRUE, barfill = "steelblue",
#                           barcolor = "steelblue")
# {
#   best_nc <- x$Best.nc
#   if (any(class(best_nc) == "numeric") )
#     print(best_nc)
#   else if (any(class(best_nc) == "matrix") ) {
#     best_nc <- as.data.frame(t(best_nc), stringsAsFactors = TRUE)
#     best_nc$Number_clusters <- as.factor(best_nc$Number_clusters)
#     if (print.summary) {
#       ss <- summary(best_nc$Number_clusters)
#       cat("Among all indices: \n===================\n")
#       for (i in 1:length(ss)) {
#         cat("*", ss[i], "proposed ", names(ss)[i],
#             "as the best number of clusters\n")
#       }
#       cat("\nConclusion\n=========================\n")
#       cat("* According to the majority rule, the best number of clusters is ",
#           names(which.max(ss)), ".\n\n")
#     }
#     df <- data.frame(Number_clusters = names(ss), freq = ss,
#                      stringsAsFactors = TRUE)
#     p <- ggpubr::ggbarplot(df, x = "Number_clusters",
#                            y = "freq", fill = barfill, color = barcolor) +
#       labs(x = "Number of clusters k", y = "Frequency among all indices",
#            title = paste0("Optimal number of clusters - k = ",
#                           names(which.max(ss))))
#     return(p)
#   }
# }
# # assign them to the factoextra namespace
# environment(fviz_nbclust) <- asNamespace("factoextra")
# assignInNamespace("fviz_nbclust",fviz_nbclust,"factoextra")
# environment(.viz_NbClust) <- asNamespace("factoextra")
# assignInNamespace(".viz_NbClust",.viz_NbClust,"factoextra")
# 
# # run NbClust
# nb<-NbClust(waste_management_numeric, diss=NULL,
#             distance="euclidean",
#             method="average")
# # run the corrected version of fviz_nbclust
# fviz_nbclust(nb)

