library(cluster)
library(vegan)
library(ggplot2)
library(dendextend)
library(factoextra)
library(NbClust)
library(ggpubr)
 
vasc_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/vasc_df_balanced.xlsx")
nonvasc_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/nonvasc_df_balanced.xlsx")
lichens_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/lichens_df_balanced.xlsx")

alpine_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/alpine_df.xlsx")



#needleleaf forest 

needleleaf_vascular_comp <- vasc_df %>% filter(Viereck.2 %in% c("Needleleaf Forest"))
needleleaf_vascular_env <- vasc_env %>% filter(Viereck.2 %in% c("Needleleaf Forest"))

needleleaf_vascular_env <- needleleaf_vascular_env %>%
  arrange(Plot, Sample_Year) %>%
  group_by(Plot) %>%
  mutate(Visit = paste0("visit_", row_number())) %>%
  ungroup()

needleleaf_vascular_comp <- needleleaf_vascular_comp[,c(11:280)]
needleleaf_vascular_comp <- as.matrix(needleleaf_vascular_comp) 

dist_matrix <- vegdist(needleleaf_vascular_comp, method = "bray")
clustering <- hclust(dist_matrix, method = "ward.D2")
plot(clustering, main = "cluster dendrogram", xlab = "Plots", ylab = "Dissimilarity")
sil <- silhouette(cutree(clustering, k = 3), dist_matrix)
plot(sil, main = "Silhuette Plot")

#gap statistic to estimate optimal clusters 
#compares the total within intra-cluster variation for different values of k with their expected values under 
#null reference distribution of the data. the estimate of the optimal clusters will be the value that maximizes the gap statistic
#ie that yields the largest gap statistic. 
fviz_nbclust(needleleaf_vascular_comp, FUN = hcut, method = "gap_stat")
fviz_nbclust(needleleaf_vascular_comp, FUNcluster, method = c("silhouette", "wss", "gap_stat"))

# Elbow method
fviz_nbclust(needleleaf_vascular_comp, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(needleleaf_vascular_comp, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(needleleaf_vascular_comp, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")



#choose number of clusters 
k <- 3
clusters <- cutree(clustering, k = k)
needleleaf_vascular_comp$Cluster <- clusters
nmds <- metaMDS(needleleaf_vascular_comp, distance = "bray", k = 3)
scores_df <- data.frame(nmds$points, Cluster = as.factor(clusters))

ggplot(scores_df, aes(MDS1, MDS2, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() + 
  labs(title = "NMDS Cluster Visualization")


dist_matrix <- vegdist(needleleaf_vascular_comp, method = "bray")
clustering <- hclust(dist_matrix, method = "ward.D2")

viereck_classes <- as.factor(needleleaf_vascular_env$Viereck.3)
veg_colors <- setNames(rainbow(length(unique(viereck_classes))), unique(viereck_classes))
dend <- as.dendrogram(clustering)
labels_colors(dend) <- veg_colors[viereck_classes]
plot(dend, main = "Dendrogram colored by viereck tier 3 classes - Vascular Species")
legend("topright", legend = names(veg_colors), fill = veg_colors, title = "Viereck Class")


#types of clustering - ward.D2 (minimizes within cluster variance), ward.D, single (smallest minimum distance between any two points), 
#complete (largest maximum distance between two points), average (average distance between all pairs of points in two clusters),
#centroid - uses centroid of each cluster for merging, median (uses the median of all distances betwen points in diff clusters)

#ward.D2 is sensitive to outliers 

nmds <- metaMDS(needleleaf_vascular_comp, distance = "bray", k = 3)
nmds_scores <- data.frame(nmds$points)
nmds_scores$Cluster <- as.factor(clusters)
nmds_scores$Viereck.3 <- needleleaf_vascular_env$Viereck.3
nmds_scores$Park <- needleleaf_vascular_env$Park

ggplot(nmds_scores, aes(x = MDS1, y = MDS2, color = Cluster, shape = Viereck.3)) +
  geom_point(size = 4) +
  scale_color_manual(values = rainbow(length(unique(clusters)))) +
  theme_minimal() + 
  labs(title = "NMDS Cluster Visualization - By Class", x = "NMDS Axis1", y = "NMDS Axis2") +
  theme(legend.position = "right")

ggplot(nmds_scores, aes(x = MDS1, y = MDS2, color = Cluster, shape = Park)) +
  geom_point(size = 4) +
  scale_color_manual(values = rainbow(length(unique(clusters)))) +
  theme_minimal() + 
  labs(title = "NMDS Cluster Visualization - By Park", x = "NMDS Axis1", y = "NMDS Axis2") +
  theme(legend.position = "right")












#alpine community analysis 


alpine_composition <- alpine_df[,c(7:280)]
alpine_composition <- as.matrix(alpine_composition) 
alpine_env




alpine_lichen_composition <- alpine_lichen_df[,c(13:179)]
alpine_lichen_composition <- as.matrix(alpine_lichen_composition) 
lichens_env_alpine


alpine_nonvasc_composition <- alpine_nonvasc_df[,c(13:219)]
alpine_nonvasc_composition <- as.matrix(alpine_nonvasc_composition) 
nonvasc_env_alpine



#vascular species 
dist_matrix <- vegdist(alpine_composition, method = "bray")

clustering_ward <- hclust(dist_matrix, method = "ward.D2")
coph_corr <- cor(cophenetic(clustering_ward), dist_matrix)
coph_corr
#if cophenetic correlation coefficient (CCC) is >0.08 it means the clustering structure is a good fit
#if its <0.06 that means it might not capture meaningful groups 

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

plot(clustering_ward, main = "Ward D2 Clustering Method - Vascular Species, Alpine Community", xlab = "Plots", ylab = "Dissimilarity")
plot(clustering_centroid, main = "Centroid Clustering Method - Vascular Species, Alpine Community", xlab = "Plots", ylab = "Dissimilarity")
plot(clustering_median, main = "Median Clustering Method - Vascular Species, Alpine Community", xlab = "Plots", ylab = "Dissimilarity")
plot(clustering_average, main = "Average Clustering Method - Vascular Species, Alpine Community", xlab = "Plots", ylab = "Dissimilarity")
plot(clustering_single, main = "Single Clustering Method - Vascular Species, Alpine Community", xlab = "Plots", ylab = "Dissimilarity")


sil <- silhouette(cutree(clustering, k = 4), dist_matrix)
plot(sil, main = "Silhuette Plot - Alpine Vascular Species")


#Defining optimal number of clusters 

  #gap statistic method 
  fviz_nbclust(alpine_composition, FUN = hcut, method = "gap_stat")

  # Elbow method
  fviz_nbclust(alpine_composition, kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)+
    labs(subtitle = "Elbow method - Alpine Vascular")

  # Silhouette method
  fviz_nbclust(alpine_composition, kmeans, method = "silhouette")+
    labs(subtitle = "Silhouette method - Alpine Vascular")

  # Gap statistic method 
  fviz_nbclust(alpine_composition, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
    labs(subtitle = "Gap statistic method")

  
#Dendrogram colored by viereck class 
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
  
centroid_dend
ward_dend
average_dend
median_dend
single_dend

  
  
#choose number of clusters after deciding based on the methods for determining 

#centroid method 
#nmds colored by cluster only 
  k <- 4
  clusters_centroid <- cutree(clustering_centroid, k = k)
  alpine_composition$Cluster <- clusters_centroid
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

centroid_vascular

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
  alpine_composition$Cluster <- clusters_average
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
  alpine_composition$Cluster <- clusters_median
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
  alpine_composition$Cluster <- clusters_single
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

#combined plot 
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

  
  
  
  
  
#Flexible beta clustering 
set.seed(123)
dist_matrix_vasc <- vegdist(alpine_composition, method = "bray")
flex_beta_cluster <- agnes(dist_matrix_vasc, method = "flexible", par.method = -0.05)
plot(flex_beta_cluster, main = "Flexible Beta Clustering (β = -0.05)")






#lichen species 
dist_matrix <- vegdist(alpine_lichen_composition, method = "bray")
clustering <- hclust(dist_matrix, method = "ward.D2")
plot(clustering, main = "Cluster Dendrogram - Lichen Species, Alpine Community", xlab = "Plots", ylab = "Dissimilarity")
sil <- silhouette(cutree(clustering, k = 4), dist_matrix)
plot(sil, main = "Silhuette Plot - Alpine Lichen Species")

fviz_nbclust(alpine_lichen_composition, FUN = hcut, method = "gap_stat")

# Elbow method
fviz_nbclust(alpine_lichen_composition, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method - Alpine Lichen")

# Silhouette method
fviz_nbclust(alpine_lichen_composition, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(alpine_lichen_composition, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

#choose number of clusters 
k <- 2
clusters <- cutree(clustering, k = k)
alpine_lichen_composition$Cluster <- clusters
nmds <- metaMDS(alpine_lichen_composition, distance = "bray", k = 3)
scores_df <- data.frame(nmds$points, Cluster = as.factor(clusters))

ggplot(scores_df, aes(MDS1, MDS2, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() + 
  labs(title = "NMDS Cluster Visualization")


dist_matrix <- vegdist(alpine_lichen_composition, method = "bray")
clustering <- hclust(dist_matrix, method = "ward.D2")

viereck_classes <- as.factor(lichens_env_alpine$Viereck.3)
veg_colors <- c("darkorange", "blue4", "seagreen3", "firebrick3", "skyblue2")
names(veg_colors) <- unique(lichens_env_alpine$Viereck.3)
dend <- as.dendrogram(clustering)
labels_colors(dend) <- veg_colors[viereck_classes]
plot(dend, main = "Viereck Tier 3 Classes (Lichen Species)")
legend("topright", legend = names(veg_colors), fill = veg_colors, title = "Site Classification", cex = 0.7)


nmds <- metaMDS(alpine_lichen_composition, distance = "bray", k = 3)
nmds_scores <- data.frame(nmds$points)
nmds_scores$Cluster <- as.factor(clusters)
nmds_scores$Viereck.3 <- lichens_env_alpine$Viereck.3
nmds_scores$Park <- lichens_env_alpine$Park

ggplot(nmds_scores, aes(x = MDS1, y = MDS2, color = Cluster, shape = Viereck.3)) +
  geom_point(size = 3) +
  scale_color_manual(values = rainbow(length(unique(clusters)))) +
  theme_minimal() + 
  labs(title = "NMDS Cluster Visualization - Alpine Lichen Species", x = "NMDS Axis1", y = "NMDS Axis2") +
  theme(legend.position = "right")




#nonvascular species 
dist_matrix <- vegdist(alpine_nonvasc_composition, method = "bray")
clustering <- hclust(dist_matrix, method = "ward.D2")
plot(clustering, main = "Cluster Dendrogram - Nonvascular Species, Alpine Community", xlab = "Plots", ylab = "Dissimilarity")
sil <- silhouette(cutree(clustering, k = 2), dist_matrix)
plot(sil, main = "Silhuette Plot - Alpine Nonvascular Species")

fviz_nbclust(alpine_nonvasc_composition, FUN = hcut, method = "gap_stat")

# Elbow method
fviz_nbclust(alpine_nonvasc_composition, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method - Alpine Nonvascular")

# Silhouette method
fviz_nbclust(alpine_nonvasc_composition, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(alpine_nonvasc_composition, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method - Nonvascular Species")

#choose number of clusters 
k <- 2
clusters <- cutree(clustering, k = k)
alpine_nonvasc_composition$Cluster <- clusters
nmds <- metaMDS(alpine_nonvasc_composition, distance = "bray", k = 3)
scores_df <- data.frame(nmds$points, Cluster = as.factor(clusters))

ggplot(scores_df, aes(MDS1, MDS2, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() + 
  labs(title = "NMDS Cluster Visualization")


dist_matrix <- vegdist(alpine_nonvasc_composition, method = "bray")
clustering <- hclust(dist_matrix, method = "ward.D2")

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



























