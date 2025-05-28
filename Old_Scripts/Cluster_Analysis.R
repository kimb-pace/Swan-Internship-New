#load packages 
library(cluster)
library(vegan)
library(ggplot2)
library(dendextend)
library(factoextra)
library(NbClust)
library(ggpubr)

#Load example data 
#Alpine plots only 
      alpine_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_Quickload/alpine_df_vasc_filtered.xlsx"))
      alpine_env <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_Quickload/alpine_env_vasc_filtered.xlsx"))
      alpine_lichen_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_Quickload/alpine_df_lichen_filtered.xlsx"))
      alpine_lichen_env <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_Quickload/alpine_env_lichen_filtered.xlsx"))
      alpine_nonvasc_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_Quickload/alpine_df_nonvasc_filtered.xlsx"))
      alpine_nonvasc_env <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_Quickload/alpine_env_nonvasc_filtered.xlsx"))

#Needleleaf forest plots only 
      needleleaf_vascular_comp <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_Quickload/forest_df_vasc_filtered.xlsx"))
      needleleaf_vascular_env <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_Quickload/forest_env_vasc_filtered.xlsx"))


      
      
#Initial exploration of methods using needleleaf forest data 

#Create matrix for adonis2 in vegan package 
    needleleaf_vascular_comp <- needleleaf_vascular_comp[,c(8:261)]
    needleleaf_vascular_comp <- as.matrix(needleleaf_vascular_comp) 

#Create distance matrix 
    dist_matrix <- vegdist(needleleaf_vascular_comp, method = "bray")

#Different ways of visualizing optimal numbers of clusters: 
clustering <- hclust(dist_matrix, method = "ward.D2")
plot(clustering, main = "Cluster Dendrogram - Ward D2", xlab = "Plots", ylab = "Dissimilarity")
sil <- silhouette(cutree(clustering, k = 3), dist_matrix)
plot(sil, main = "Silhuette Plot")

#Bootstrapping on matrix to determine the reliability of the determined number of clusters.
#IE do the same number of clusters consistently form when the data is replicated? 
fviz_nbclust(needleleaf_vascular_comp, FUN = hcut, method = "gap_stat")

#Elbow method
fviz_nbclust(needleleaf_vascular_comp, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

#Silhouette method
fviz_nbclust(needleleaf_vascular_comp, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#Gap statistic
#recommended value: nboot= 500, can use 50 for speed of computing 
set.seed(123)
fviz_nbclust(needleleaf_vascular_comp, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

#Gap statistic to estimate optimal clusters:  
#It compares the total within intra-cluster variation for different values of k with their expected values under 
#null reference distribution of the data. the estimate of the optimal clusters will be the value that maximizes the gap statistic
#ie that yields the largest gap statistic. 

#color overlay of viereck class on a dendrogram 
dist_matrix <- vegdist(needleleaf_vascular_comp, method = "bray")
clustering <- hclust(dist_matrix, method = "ward.D2")
viereck_classes <- as.factor(needleleaf_vascular_env$Viereck.3)
veg_colors <- setNames(rainbow(length(unique(viereck_classes))), unique(viereck_classes))
dend <- as.dendrogram(clustering)
labels_colors(dend) <- veg_colors[viereck_classes]
plot(dend, main = "Dendrogram Colored by Viereck 3 Classes - Vascular Species")
legend("topright", legend = names(veg_colors), fill = veg_colors, title = "Viereck Class")


#Choose number of clusters using the above methods to help you decide how many
#once you've decided the optimal number, you can make some plots with that number of clusters as the overlay 
k <- 3
#create clusters 
clusters <- cutree(clustering, k = k)

#Use clusters as overlay on an NMDS plot 
nmds <- metaMDS(needleleaf_vascular_comp, distance = "bray", k = 3)
scores_df <- data.frame(nmds$points, Cluster = as.factor(clusters))
ggplot(scores_df, aes(MDS1, MDS2, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() + 
  labs(title = "NMDS Cluster Visualization")

#Another NMDS option with cluster overlay as point colors 
nmds <- metaMDS(needleleaf_vascular_comp, distance = "bray", k = 3)
nmds_scores <- data.frame(nmds$points)
nmds_scores$Cluster <- as.factor(clusters)
nmds_scores$Viereck.3 <- needleleaf_vascular_env$Viereck.3
nmds_scores$Park <- needleleaf_vascular_env$Park

#point shapes based on viereck class 
ggplot(nmds_scores, aes(x = MDS1, y = MDS2, color = Cluster, shape = Viereck.3)) +
  geom_point(size = 4) +
  scale_color_manual(values = rainbow(length(unique(clusters)))) +
  theme_minimal() + 
  labs(title = "NMDS Cluster Visualization - By Class", x = "NMDS Axis1", y = "NMDS Axis2") +
  theme(legend.position = "right")

#point shapes based on park 
ggplot(nmds_scores, aes(x = MDS1, y = MDS2, color = Cluster, shape = Park)) +
  geom_point(size = 4) +
  scale_color_manual(values = rainbow(length(unique(clusters)))) +
  theme_minimal() + 
  labs(title = "NMDS Cluster Visualization - By Park", x = "NMDS Axis1", y = "NMDS Axis2") +
  theme(legend.position = "right")






#Alpine community cluster analysis 

#create matrices for analysis 
    alpine_composition <- alpine_df[,c(8:261)]
    alpine_composition <- as.matrix(alpine_composition) 
    
    alpine_lichen_composition <- alpine_lichen_df[,c(10:167)]
    alpine_lichen_composition <- as.matrix(alpine_lichen_composition) 
    
    alpine_nonvasc_composition <- alpine_nonvasc_df[,c(10:210)]
    alpine_nonvasc_composition <- as.matrix(alpine_nonvasc_composition) 


#Vascular species 

    #convert to a distance matrix 
    dist_matrix <- vegdist(alpine_composition, method = "bray")
    
    #start with D2 clustering method and calculate CCC statistic to determine if this method is a good fit 
    clustering_ward <- hclust(dist_matrix, method = "ward.D2")
    coph_corr <- cor(cophenetic(clustering_ward), dist_matrix)
    coph_corr
    #if cophenetic correlation coefficient (CCC) is >0.08 it means the clustering structure is a good fit
    #if its <0.06 that means it might not capture meaningful groups 
    
    #try other methods to compare CCC statistics 
    clustering_centroid <- hclust(dist_matrix, method = "centroid")
    coph_corr <- cor(cophenetic(clustering_centroid), dist_matrix)
    coph_corr
    
    clustering_median <- hclust(dist_matrix, method = "median")
    coph_corr <- cor(cophenetic(clustering_median), dist_matrix)
    coph_corr
    
    clustering_average <- hclust(dist_matrix, method = "average")
    coph_corr <- cor(cophenetic(clustering_average), dist_matrix)
    coph_corr
    
    clustering_single <- hclust(dist_matrix, method = "single")
    coph_corr <- cor(cophenetic(clustering_single), dist_matrix)
    coph_corr

#plot the different clustering methods as dendrograms 
    plot(clustering_ward, main = "Ward D2 Clustering Method - Vascular Species, Alpine Community", xlab = "Plots", ylab = "Dissimilarity")
    plot(clustering_centroid, main = "Centroid Clustering Method - Vascular Species, Alpine Community", xlab = "Plots", ylab = "Dissimilarity")
    plot(clustering_median, main = "Median Clustering Method - Vascular Species, Alpine Community", xlab = "Plots", ylab = "Dissimilarity")
    plot(clustering_average, main = "Average Clustering Method - Vascular Species, Alpine Community", xlab = "Plots", ylab = "Dissimilarity")
    plot(clustering_single, main = "Single Clustering Method - Vascular Species, Alpine Community", xlab = "Plots", ylab = "Dissimilarity")

#create silhuette plot to visualize clustering 
    sil <- silhouette(cutree(clustering, k = 4), dist_matrix)
    plot(sil, main = "Silhuette Plot - Alpine Vascular Species")


#Defining optimal number of clusters using three different methods 

  #Elbow method
  fviz_nbclust(alpine_composition, kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)+
    labs(subtitle = "Elbow method - Alpine Vascular")

  #Silhouette method
  fviz_nbclust(alpine_composition, kmeans, method = "silhouette")+
    labs(subtitle = "Silhouette method - Alpine Vascular")

  #Gap statistic method 
  fviz_nbclust(alpine_composition, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
    labs(subtitle = "Gap statistic method")


#Dendrogram colored by viereck class using different methods of clustering 
  #types of clustering - 
  #ward.D2 (minimizes within cluster variance, sensitive to outliers) 
  #single (smallest minimum distance between any two points) 
  #complete (largest maximum distance between two points)
  #average (average distance between all pairs of points in two clusters)
  #centroid - uses centroid of each cluster for merging
  #median (uses the median of all distances betwen points in diff clusters)
  
  
  viereck_classes <- as.factor(alpine_env$Viereck.3)
  veg_colors <- c("darkorange", "blue4", "seagreen3", "firebrick3")
  names(veg_colors) <- unique(alpine_env$Viereck.3)
  
  #centroid 
  dend_centroid <- as.dendrogram(clustering_centroid)
  labels_colors(dend_centroid) <- veg_colors[viereck_classes]
  plot(dend_centroid, main = "Alpine Vascular Species - Centroid Method")
  legend("topright", legend = names(veg_colors), fill = veg_colors, title = "Site Classification", cex = 0.7)
  centroid_dend <- recordPlot()
  
  #ward 
  dend_ward <- as.dendrogram(clustering_ward)
  labels_colors(dend_ward) <- veg_colors[viereck_classes]
  plot(dend_ward, main = "Alpine Vascular Species - Ward Method")
  legend("topright", legend = names(veg_colors), fill = veg_colors, title = "Site Classification", cex = 0.7)
  ward_dend <- recordPlot()
  
  #median 
  dend_median <- as.dendrogram(clustering_median)
  labels_colors(dend_median) <- veg_colors[viereck_classes]
  plot(dend_median, main = "Alpine Vascular Species - Median Method")
  legend("topright", legend = names(veg_colors), fill = veg_colors, title = "Site Classification", cex = 0.7)
  median_dend <- recordPlot()
  
  #average
  dend_average <- as.dendrogram(clustering_average)
  labels_colors(dend_average) <- veg_colors[viereck_classes]
  plot(dend_average, main = "Alpine Vascular Species - Average Method")
  legend("topright", legend = names(veg_colors), fill = veg_colors, title = "Site Classification", cex = 0.7)
  average_dend <- recordPlot()
  
  #single
  dend_single <- as.dendrogram(clustering_single)
  labels_colors(dend_single) <- veg_colors[viereck_classes]
  plot(dend_single, main = "Alpine Vascular Species - Single Method")
  legend("topright", legend = names(veg_colors), fill = veg_colors, title = "Site Classification", cex = 0.7)
  single_dend <- recordPlot()
  
#quickview of plots 
  centroid_dend
  ward_dend
  average_dend
  median_dend
  single_dend

  
  
#choose number of clusters after deciding based on the methods for determining how many 
  #then you can overlay the clusters on an NMDS ordination 

#centroid method 
#nmds colored by cluster only 
  k <- 4
  clusters_centroid <- cutree(clustering_centroid, k = k)
  nmds <- metaMDS(alpine_composition, distance = "bray", k = 3)
  nmds$stress
  scores_df_centroid <- data.frame(nmds$points, Cluster = as.factor(clusters_centroid))
  
  ggplot(scores_df_centroid, aes(MDS1, MDS2, color = Cluster)) +
    geom_point(size = 3) +
    theme_minimal() + 
    labs(title = "NMDS Cluster Visualization - Centroid")
  
#nmds colored by cluster and viereck class 
  nmds_scores <- data.frame(nmds$points)
  nmds_scores$Cluster <- as.factor(clusters_centroid)
  nmds_scores$Viereck.3 <- alpine_env$Viereck.3
  nmds_scores$Park <- alpine_env$Park
  
centroid_vascular <-  ggplot(nmds_scores, aes(x = MDS1, y = MDS2, color = Cluster, shape = Viereck.3)) +
    geom_point(size = 3) +
    scale_color_manual(values = rainbow(length(unique(clusters_centroid)))) +
    theme_minimal() + 
    labs(title = "Centroid Method", x = "NMDS Axis1", y = "NMDS Axis2") +
    theme(legend.position = "right")+
    geom_label(aes(x = Inf, y = Inf, label = "1"), 
                hjust = 1.2, vjust = 1.2, 
                size = 5, 
                label.size = 0.5, 
                fill = "white", 
                color = "black")

 #Ward method 
  #nmds colored by cluster only 
  k <- 4
  clusters_ward <- cutree(clustering_ward, k = k)
  alpine_composition$Cluster <- clusters_ward
  nmds <- metaMDS(alpine_composition, distance = "bray", k = 3)
  scores_df_ward <- data.frame(nmds$points, Cluster = as.factor(clusters_ward))

  ggplot(scores_df_ward, aes(MDS1, MDS2, color = Cluster)) +
    geom_point(size = 3) +
    theme_minimal() + 
    labs(title = "NMDS Cluster Visualization (Ward) - Alpine Vascular Species")

  #nmds colored by cluster and viereck class 
  nmds_scores <- data.frame(nmds$points)
  nmds_scores$Cluster <- as.factor(clusters_ward)
  nmds_scores$Viereck.3 <- alpine_env$Viereck.3
  nmds_scores$Park <- alpine_env$Park

  ward_vascular <- ggplot(nmds_scores, aes(x = MDS1, y = MDS2, color = Cluster, shape = Viereck.3)) +
    geom_point(size = 3) +
    scale_color_manual(values = rainbow(length(unique(clusters_ward)))) +
    theme_minimal() + 
    labs(title = "Ward D2 Method", x = "NMDS Axis1", y = "NMDS Axis2") +
    theme(legend.position = "right")+
    geom_label(aes(x = Inf, y = Inf, label = "2"), 
               hjust = 1.2, vjust = 1.2, 
               size = 5, 
               label.size = 0.5, 
               fill = "white", 
               color = "black")

#Average method 
  #nmds colored by cluster only 
  k <- 4
  clusters_average <- cutree(clustering_average, k = k)
  nmds <- metaMDS(alpine_composition, distance = "bray", k = 3)
  scores_df_average <- data.frame(nmds$points, Cluster = as.factor(clusters_average))
  
  ggplot(scores_df_average, aes(MDS1, MDS2, color = Cluster)) +
    geom_point(size = 3) +
    theme_minimal() + 
    labs(title = "NMDS Cluster Visualization (Average) - Alpine Vascular Species")
  
  #nmds colored by cluster and viereck class 
  nmds_scores <- data.frame(nmds$points)
  nmds_scores$Cluster <- as.factor(clusters_average)
  nmds_scores$Viereck.3 <- alpine_env$Viereck.3
  nmds_scores$Park <- alpine_env$Park
  
  average_vascular <- ggplot(nmds_scores, aes(x = MDS1, y = MDS2, color = Cluster, shape = Viereck.3)) +
    geom_point(size = 3) +
    scale_color_manual(values = rainbow(length(unique(clusters_average)))) +
    theme_minimal() + 
    labs(title = "Average Method", x = "NMDS Axis1", y = "NMDS Axis2") +
    theme(legend.position = "right")+
    geom_label(aes(x = Inf, y = Inf, label = "3"), 
               hjust = 1.2, vjust = 1.2, 
               size = 5, 
               label.size = 0.5, 
               fill = "white", 
               color = "black")

#median method 
  #nmds colored by cluster only 
  k <- 4
  clusters_median <- cutree(clustering_median, k = k)
  nmds <- metaMDS(alpine_composition, distance = "bray", k = 3)
  scores_df_median <- data.frame(nmds$points, Cluster = as.factor(clusters_median))
  
  ggplot(scores_df_median, aes(MDS1, MDS2, color = Cluster)) +
    geom_point(size = 3) +
    theme_minimal() + 
    labs(title = "NMDS Cluster Visualization (Median) - Alpine Vascular Species")
  
  #nmds colored by cluster and viereck class 
  nmds_scores <- data.frame(nmds$points)
  nmds_scores$Cluster <- as.factor(clusters_median)
  nmds_scores$Viereck.3 <- alpine_env$Viereck.3
  nmds_scores$Park <- alpine_env$Park
  
  median_vascular <-  ggplot(nmds_scores, aes(x = MDS1, y = MDS2, color = Cluster, shape = Viereck.3)) +
    geom_point(size = 3) +
    scale_color_manual(values = rainbow(length(unique(clusters_median)))) +
    theme_minimal() + 
    labs(title = "Median Method", x = "NMDS Axis1", y = "NMDS Axis2") +
    theme(legend.position = "right")+
    geom_label(aes(x = Inf, y = Inf, label = "4"), 
               hjust = 1.2, vjust = 1.2, 
               size = 5, 
               label.size = 0.5, 
               fill = "white", 
               color = "black")


#single method 
  #nmds colored by cluster only 
  k <- 4
  clusters_single <- cutree(clustering_single, k = k)
  nmds <- metaMDS(alpine_composition, distance = "bray", k = 3)
  scores_df_single <- data.frame(nmds$points, Cluster = as.factor(clusters_single))
  
  ggplot(scores_df_single, aes(MDS1, MDS2, color = Cluster)) +
    geom_point(size = 3) +
    theme_minimal() + 
    labs(title = "NMDS Cluster Visualization (Single) - Alpine Vascular Species")
  
  #nmds colored by cluster and viereck class 
  nmds_scores <- data.frame(nmds$points)
  nmds_scores$Cluster <- as.factor(clusters_single)
  nmds_scores$Viereck.3 <- alpine_env$Viereck.3
  nmds_scores$Park <- alpine_env$Park
  
 single_vascular <- ggplot(nmds_scores, aes(x = MDS1, y = MDS2, color = Cluster, shape = Viereck.3)) +
    geom_point(size = 3) +
    scale_color_manual(values = rainbow(length(unique(clusters_single)))) +
    theme_minimal() + 
    labs(title = "Single Method", x = "NMDS Axis1", y = "NMDS Axis2") +
    theme(legend.position = "right")+
   geom_label(aes(x = Inf, y = Inf, label = "5"), 
              hjust = 1.2, vjust = 1.2, 
              size = 5, 
              label.size = 0.5, 
              fill = "white", 
              color = "black")

#Combined plot showing all different methods together 
  Vascular_Cluster_Plot <- ggarrange(
    plots = centroid_vascular, ward_vascular, average_vascular, median_vascular, single_vascular,
    ncol = 2,      
    nrow = 3,
    common.legend = TRUE,  
    legend = "bottom")
  Vascular_Cluster_Plot <- annotate_figure(
    Vascular_Cluster_Plot,
    top = text_grob("Cluster Visualization - Alpine Vascular Species", face = "bold", size = 14))
  Vascular_Cluster_Plot


#Flexible beta clustering method 
  #a type of agglomerative clustering 
  #it's basically a compromise between chaining and clumping linkages 
  #more negative the beta parameter is, the more conservative. clusters form later
  #if it's closer to 0: merges more easily, more permissive 
  #to interpret: look for natural groupings, identify outliers (items that join the cluster tree
  #at high dissimilarity) and test different height cuts to decide how many meaningful groups exist
set.seed(123)
dist_matrix_vasc <- vegdist(alpine_composition, method = "bray")
flex_beta_cluster <- agnes(dist_matrix_vasc, method = "flexible", par.method = -0.05)
plot(flex_beta_cluster, main = "Flexible Beta Clustering (β = -0.05)")






#Lichen species cluster analysis 
dist_matrix <- vegdist(alpine_lichen_composition, method = "bray")
clustering <- hclust(dist_matrix, method = "ward.D2")
#A couple initial example plots 
    plot(clustering, main = "Cluster Dendrogram - Lichen Species, Alpine Community", xlab = "Plots", ylab = "Dissimilarity")
    sil <- silhouette(cutree(clustering, k = 4), dist_matrix)
    plot(sil, main = "Silhuette Plot - Alpine Lichen Species")

#Methods for determining the optimal number of clusters 
#Elbow method
fviz_nbclust(alpine_lichen_composition, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method - Alpine Lichen")

#Silhouette method
fviz_nbclust(alpine_lichen_composition, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#Gap statistic
set.seed(123)
fviz_nbclust(alpine_lichen_composition, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


#Choose number of clusters and assign them 
#For example, two: 
k <- 2
clusters <- cutree(clustering, k = k)

#Viereck class coloring on Dendrogram, can choose different coloring mechanism if you want 
viereck_classes <- as.factor(alpine_lichen_env$Viereck.3)
veg_colors <- c("darkorange", "blue4", "seagreen3", "firebrick3", "skyblue2")
names(veg_colors) <- unique(alpine_lichen_env$Viereck.3)
dend <- as.dendrogram(clustering)
labels_colors(dend) <- veg_colors[viereck_classes]
plot(dend, main = "Viereck Tier 3 Classes (Lichen Species)")
legend("topright", legend = names(veg_colors), fill = veg_colors, title = "Site Classification", cex = 0.7)

#NMDS plot with cluster overlay 
nmds <- metaMDS(alpine_lichen_composition, distance = "bray", k = 3)
nmds_scores <- data.frame(nmds$points)
nmds_scores$Cluster <- as.factor(clusters)
nmds_scores$Viereck.3 <- alpine_lichen_env$Viereck.3
nmds_scores$Park <- alpine_lichen_env$Park

ggplot(nmds_scores, aes(x = MDS1, y = MDS2, color = Cluster, shape = Viereck.3)) +
  geom_point(size = 3) +
  scale_color_manual(values = rainbow(length(unique(clusters)))) +
  theme_minimal() + 
  labs(title = "NMDS Cluster Visualization - Alpine Lichen Species", x = "NMDS Axis1", y = "NMDS Axis2") +
  theme(legend.position = "right")

#Nonvascular species cluster analysis 
      dist_matrix <- vegdist(alpine_nonvasc_composition, method = "bray")
      clustering <- hclust(dist_matrix, method = "ward.D2")
    #A few example plots 
      plot(clustering, main = "Cluster Dendrogram - Nonvascular Species, Alpine Community", xlab = "Plots", ylab = "Dissimilarity")
      sil <- silhouette(cutree(clustering, k = 2), dist_matrix)
      plot(sil, main = "Silhuette Plot - Alpine Nonvascular Species")

#Determining the numbber of clusters 
#Elbow method
fviz_nbclust(alpine_nonvasc_composition, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method - Alpine Nonvascular")

#Silhouette method
fviz_nbclust(alpine_nonvasc_composition, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#Gap Statistic Method 
fviz_nbclust(alpine_nonvasc_composition, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method - Nonvascular Species")

#Choose number of clusters 
k <- 2
clusters <- cutree(clustering, k = k)

#Viereck class dendrogram 
viereck_classes <- as.factor(nonvasc_env_alpine$Viereck.3)
veg_colors <- c("darkorange", "blue4", "seagreen3", "firebrick3", "skyblue2")
names(veg_colors) <- unique(nonvasc_env_alpine$Viereck.3)
dend <- as.dendrogram(clustering)
labels_colors(dend) <- veg_colors[viereck_classes]
plot(dend, main = "Viereck Tier 3 Classes (Nonvascular Species)")
legend("topright", legend = names(veg_colors), fill = veg_colors, title = "Site Classification", cex = 0.7)

nmds <- metaMDS(alpine_nonvasc_composition, distance = "bray", k = 3)
nmds_scores <- data.frame(nmds$points)
nmds_scores$Cluster <- as.factor(clusters)
nmds_scores$Viereck.3 <- nonvasc_env_alpine$Viereck.3
nmds_scores$Park <- lichens_env_alpine$Park

ggplot(nmds_scores, aes(x = MDS1, y = MDS2, color = Cluster, shape = Viereck.3)) +
  geom_point(size = 3) +
  scale_color_manual(values = rainbow(length(unique(clusters)))) +
  theme_minimal() + 
  labs(title = "NMDS Cluster Visualization - Alpine Nonvascular Species", x = "NMDS Axis1", y = "NMDS Axis2") +
  theme(legend.position = "right")



























