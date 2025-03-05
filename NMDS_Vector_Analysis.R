library(vegan)
library(permute)
library(lattice)
library(dplyr)
library(readxl)
library(writexl)
library(readr)
library(dplyr)
library(tibble)






load("T:\\Users\\KPace\\Quadrat_Freq_Analyses\\Data\\plot_fire.rdata")
loaded_objects <- ls()
print(loaded_objects)

plot_fire_df <- as.data.frame(plot_fire)

file.choose() #to get pathway
library(mapview)
library(sf)
library(dplyr)

plot_elevation_data <- out$plot_loc_summary[c(1,2,3)]
plot_elevation_data

data <- as.data.frame(out$plot_loc_summary)
data2 <- as.data.frame(out$plot_sample)
join <- inner_join(data, data2, by = c("ID" = "Plot"))

join <- alpine_df %>%
  left_join(data %>% select(Plot, Elevation), by = "Plot")

names(data)[names(data) == "ID"] <- "Plot"

#maps
plot(out$plot_loc_summary)
mapview(out$plot_loc_summary)

selected_plots <- c("KATM_2009_02_006", "KATM_2011_03_022")
filtered_data <- out$plot_loc_summary %>% filter(ID %in% selected_plots)
mapview(filtered_data)

write_xlsx(alpine_env, "C:/Users/kmpace/Desktop/alpine_env_sprich.xlsx")


#quick load of prepared dataframes - presence_absence
alpine_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/alpine_df.xlsx")
alpine_lichen_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/alpine_lichen_df.xlsx")
alpine_nonvasc_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/alpine_nonvasc_df.xlsx")
fulldata <- read.csv("T:/Users/KPace/SWAN-Internship-New/Data/Unmodified/Quadrat_Frequency.csv")

summary <- alpine_lichen_df %>%
  select(13:179) %>%
  summarise(
    nonzero_cols = sum(colSums(. != 0) > 0),
    all_zero_cols = sum(colSums(. != 0) == 0)
  )

summary <- alpine_nonvasc_df %>%
  select(13:219) %>%
  summarise(
    nonzero_cols = sum(colSums(. != 0) > 0),
    all_zero_cols = sum(colSums(. != 0) == 0)
  )

summary <- alpine_df %>%
  select(7:280) %>%
  summarise(
    nonzero_cols = sum(colSums(. != 0) > 0),
    all_zero_cols = sum(colSums(. != 0) == 0)
  )


alpine_lichen_matrix <- alpine_lichen_df[,c(13:179)]
alpine_lichen_matrix <- as.matrix(alpine_lichen_matrix)
mds_alpine_lichen <- metaMDS(alpine_lichen_matrix, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
#axis2 scores
axis2_scores <- scores(mds_alpine_lichen, display = "sites")[, 2]
species_cor <- apply(alpine_lichen_matrix, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))






#quad abundance alpine 
lichen_abundance_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/quad_abundance_df_lichen.xlsx")

alpine_lichen_abundance_df <- lichen_abundance_df %>%
  filter(Plot_Year %in% alpine_lichen_df$Plot_Year)

alpine_lichen_abundance_matrix <- alpine_lichen_abundance_df[,c(12:178)]
alpine_lichen_abundance_matrix <- as.matrix(alpine_lichen_abundance_matrix) 

#lichen, colored by viereck 3
mds_alpine_lichen_ab <- metaMDS(alpine_lichen_abundance_matrix, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
#ordination information for caption (#plots, #iterations, stress value, if it converged)
length(unique(alpine_lichen_abundance_df[["Plot"]]))
mds_alpine_lichen_ab$stress
mds_alpine_lichen_ab$iters
mds_alpine_lichen_ab$converged

veg_colors <- c("darkorange", "blue4", "seagreen3", "firebrick3", "skyblue2")
names(veg_colors) <- unique(alpine_lichen_abundance_df$Viereck.3)
dev.off()
ordiplot(mds_alpine_lichen_ab, type = "n")
xlim <- c(-1.5, 1)
ylim <- c(-1.5, 1)
plot(mds_alpine_lichen_ab, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4)
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.10",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "2",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
points(scores(mds_alpine_lichen_ab, display = "sites"),
       col = veg_colors[alpine_lichen_abundance_df$Viereck.3],
       pch = 19)
legend("bottomleft", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19, cex = 1.3)
ordiarrows(mds_alpine_lichen_ab, groups = alpine_lichen_abundance_df$Plot, levels = alpine_lichen_abundance_df$Sample_Year, col = 'blue')
ordihull(mds_alpine_lichen_ab, groups = alpine_lichen_abundance_df$Park, draw ="polygon", label = TRUE)
title("Lichen Species")

#plotID labels
text(mds_alpine_lichen_ab, display = "sites", labels = alpine_lichen_abundance_df$PlotID, col = "black", cex = 0.7, pos = 4)
#adding only select labels 
selected_plots <- c("022", "999", "998")
nmds_scores <- as.data.frame(scores(mds_alpine_lichen_ab, display = "sites"))
nmds_scores$PlotID <- alpine_lichen_abundance_df$PlotID
selected_scores <- nmds_scores[nmds_scores$PlotID %in% selected_plots, ]
text(selected_scores$NMDS1, selected_scores$NMDS2, labels = selected_scores$PlotID,
     col = "black", cex = 0.7, pos = 4)

#record plot 
alpinelichen_plot_viereck <- recordPlot()

#add species vector arrows 
species_fit <- envfit(mds_alpine_lichen_ab, alpine_lichen_abundance_matrix, perm = 999)
species_scores <- as.data.frame(species_fit$vectors$arrows)
species_scores$Species_Code <- rownames(species_scores)

arrows(0,0,species_scores$NMDS1, species_scores$NMDS2, length = 0.1, col = "red")
text(species_scores$NMDS1, species_scores$NMDS2, labels = species_scores$Species_Code, 
     col = "red", pos = c(3, 1, 4), offset = 0.5)

selected_species <- c("CLGR13", "UMHY2", "STERE2", "CLMA12", "UMHYH", "THAMN3", 
                      "FLCU", "BRDI60", "ALOC60", "CEFA60", "STVE60")
filtered_scores <- species_scores %>% filter(Species_Code %in% selected_species)
arrows(0, 0, filtered_scores$NMDS1, filtered_scores$NMDS2, length = 0.1, col = "red")
text(filtered_scores$NMDS1, filtered_scores$NMDS2, labels = filtered_scores$Species_Code, 
     col = "red", pos = c(4), offset = 0.5)

#axis2 scores
axis2_scores <- scores(mds_alpine_lichen_ab, display = "sites")[, 2]
species_cor <- apply(alpine_lichen_abundance_matrix, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))

#axis1 scores 
axis1_scores <- scores(mds_alpine_lichen_ab, display = "sites")[, 1]
species_cor <- apply(alpine_lichen_abundance_matrix, 2, function(species) cor (species, axis1_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))






#quad abundance alpine vascular NMDS 
#quad abundance alpine 
vasc_abundance_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/quad_abundance_df_vascular.xlsx")

alpine_vasc_abundance_df <- vasc_abundance_df %>%
  filter(Plot_Year %in% alpine_df$Plot_Year)

alpine_vasc_abundance_matrix <- alpine_vasc_abundance_df[,c(8:281)]
alpine_vasc_abundance_matrix <- as.matrix(alpine_vasc_abundance_matrix) 

mds_alpine_vasc_ab <- metaMDS(alpine_vasc_abundance_matrix, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
#ordination information for caption (#plots, #iterations, stress value, if it converged)
length(unique(alpine_vasc_abundance_df[["Plot"]]))
mds_alpine_vasc_ab$stress
mds_alpine_vasc_ab$iters
mds_alpine_vasc_ab$converged

veg_colors <- c("darkorange", "blue4", "seagreen3", "firebrick3")
names(veg_colors) <- unique(alpine_vasc_abundance_df$Viereck.3)
dev.off()
ordiplot(mds_alpine_vasc_ab, type = "n")
xlim <- c(-1, 1.5)
ylim <- c(-1, 1)
plot(mds_alpine_vasc_ab, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4)

text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.10",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "1",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
points(scores(mds_alpine_vasc_ab, display = "sites"),
       col = veg_colors[alpine_vasc_abundance_df$Viereck.3],
       pch = 19)
legend("bottomright", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19, cex = 1.3)
ordiarrows(mds_alpine_vasc_ab, groups = alpine_vasc_abundance_df$Plot, levels = alpine_vasc_abundance_df$Sample_Year, col = 'blue')
ordihull(mds_alpine_vasc_ab, groups = alpine_vasc_abundance_df$Park, draw ="polygon", label = TRUE)
title("Vascular Species")

#plotID labels
text(mds_alpine_vasc_ab, display = "sites", labels = alpine_vasc_abundance_df$PlotID, col = "black", cex = 0.7, pos = 4)
#adding only select labels 
selected_plots <- c("022", "999", "998")
nmds_scores <- as.data.frame(scores(mds_alpine_vasc_ab, display = "sites"))
nmds_scores$PlotID <- alpine_vasc_abundance_df$PlotID
selected_scores <- nmds_scores[nmds_scores$PlotID %in% selected_plots, ]
text(selected_scores$NMDS1, selected_scores$NMDS2, labels = selected_scores$PlotID,
     col = "black", cex = 0.7, pos = 4)

#record plot 
alpinelichen_plot_viereck <- recordPlot()

#add species vector arrows 
species_fit <- envfit(mds_alpine_vasc_ab, alpine_vasc_abundance_matrix, perm = 999)
species_scores <- as.data.frame(species_fit$vectors$arrows)
species_scores$Species_Code <- rownames(species_scores)

arrows(0,0,species_scores$NMDS1, species_scores$NMDS2, length = 0.1, col = "red")
text(species_scores$NMDS1, species_scores$NMDS2, labels = species_scores$Species_Code, 
     col = "red", pos = c(3, 1, 4), offset = 0.5)

selected_species <- c("EMNI", "GEGL", "ARARA2", "FEBR", "ANFR", "DROC")
filtered_scores <- species_scores %>% filter(Species_Code %in% selected_species)
arrows(0, 0, filtered_scores$NMDS1, filtered_scores$NMDS2, length = 0.1, col = "red")
text(filtered_scores$NMDS1, filtered_scores$NMDS2, labels = filtered_scores$Species_Code, 
     col = "red", pos = c(4, 3, 1, 2), offset = 0.5)

#axis2 scores
axis2_scores <- scores(mds_alpine_vasc_ab, display = "sites")[, 2]
species_cor <- apply(alpine_vasc_abundance_matrix, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))

#axis1 scores 
axis1_scores <- scores(mds_alpine_vasc_ab, display = "sites")[, 1]
species_cor <- apply(alpine_vasc_abundance_matrix, 2, function(species) cor (species, axis1_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))





#quad abundance alpine 
nonvasc_abundance_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/quad_abundance_df_nonvasc.xlsx")

alpine_nonvasc_abundance_df <- nonvasc_abundance_df %>%
  filter(Plot_Year %in% alpine_nonvasc_df$Plot_Year)

alpine_nonvasc_abundance_matrix <- alpine_nonvasc_abundance_df[,c(12:218)]
alpine_nonvasc_abundance_matrix <- as.matrix(alpine_nonvasc_abundance_matrix) 

#lichen, colored by viereck 3
mds_alpine_nonvasc_ab <- metaMDS(alpine_nonvasc_abundance_matrix, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
#ordination information for caption (#plots, #iterations, stress value, if it converged)
length(unique(alpine_nonvasc_abundance_df[["Plot"]]))
mds_alpine_nonvasc_ab$stress
mds_alpine_nonvasc_ab$iters
mds_alpine_nonvasc_ab$converged

veg_colors <- c("darkorange", "blue4", "seagreen3", "firebrick3", "skyblue2")
names(veg_colors) <- unique(alpine_nonvasc_abundance_df$Viereck.3)
dev.off()
ordiplot(mds_alpine_nonvasc_ab, type = "n")
xlim <- c(-1, 1)
ylim <- c(-1.5, 1)
plot(mds_alpine_nonvasc_ab, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4)
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.13",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "2",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
points(scores(mds_alpine_nonvasc_ab, display = "sites"),
       col = veg_colors[alpine_nonvasc_abundance_df$Viereck.3],
       pch = 19)
legend("bottomleft", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19, cex = 1.3)
ordiarrows(mds_alpine_nonvasc_ab, groups = alpine_nonvasc_abundance_df$Plot, levels = alpine_nonvasc_abundance_df$Sample_Year, col = 'blue')
ordihull(mds_alpine_nonvasc_ab, groups = alpine_nonvasc_abundance_df$Park, draw ="polygon", label = TRUE)
title("Nonvascular Species")

#plotID labels
text(mds_alpine_nonvasc_ab, display = "sites", labels = alpine_nonvasc_abundance_df$PlotID, col = "black", cex = 0.7, pos = 4)
#adding only select labels 
selected_plots <- c("022", "999", "998")
nmds_scores <- as.data.frame(scores(mds_alpine_nonvasc_ab, display = "sites"))
nmds_scores$PlotID <- alpine_nonvasc_abundance_df$PlotID
selected_scores <- nmds_scores[nmds_scores$PlotID %in% selected_plots, ]
text(selected_scores$NMDS1, selected_scores$NMDS2, labels = selected_scores$PlotID,
     col = "black", cex = 0.7, pos = 4)

#record plot 
alpinenonvasc_plot_viereck <- recordPlot()

#add species vector arrows 
species_fit <- envfit(mds_alpine_nonvasc_ab, alpine_nonvasc_abundance_matrix, perm = 999)
species_scores <- as.data.frame(species_fit$vectors$arrows)
species_scores$Species_Code <- rownames(species_scores)

arrows(0,0,species_scores$NMDS1, species_scores$NMDS2, length = 0.1, col = "red")
text(species_scores$NMDS1, species_scores$NMDS2, labels = species_scores$Species_Code, 
     col = "red", pos = c(3, 1, 4), offset = 0.5)

selected_species <- c("CLGR13", "UMHY2", "STERE2", "CLMA12", "UMHYH", "THAMN3", 
                      "FLCU", "BRDI60", "ALOC60", "CEFA60", "STVE60")
filtered_scores <- species_scores %>% filter(Species_Code %in% selected_species)
arrows(0, 0, filtered_scores$NMDS1, filtered_scores$NMDS2, length = 0.1, col = "red")
text(filtered_scores$NMDS1, filtered_scores$NMDS2, labels = filtered_scores$Species_Code, 
     col = "red", pos = c(4), offset = 0.5)

#axis2 scores
axis2_scores <- scores(mds_alpine_nonvasc_ab, display = "sites")[, 2]
species_cor <- apply(alpine_nonvasc_abundance_matrix, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))

#axis1 scores 
axis1_scores <- scores(mds_alpine_nonvasc_ab, display = "sites")[, 1]
species_cor <- apply(alpine_nonvasc_abundance_matrix, 2, function(species) cor (species, axis1_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))






























#quad abundance open low shrub 

openlow_lichen_abundance_df <- lichen_abundance_df %>% filter(Viereck.3 %in% c("Open Low Scrub"))

openlow_lichen_abundance_df <- lichen_abundance_df %>%
  filter(Plot_Year %in% openlow_df_lichen$Plot_Year)

openlow_lichen_abundance_matrix <- openlow_lichen_abundance_df[,c(12:177)]
openlow_lichen_abundance_matrix <- as.matrix(openlow_lichen_abundance_matrix)


#lichen, colored by viereck 3
mds_openlow_lichen_ab <- metaMDS(openlow_lichen_abundance_matrix, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
#ordination information for caption (#plots, #iterations, stress value, if it converged)
length(unique(openlow_lichen_abundance_df[["Plot"]]))
mds_openlow_lichen_ab$stress
mds_openlow_lichen_ab$iters
mds_openlow_lichen_ab$converged

veg_colors <- c("orchid3")
names(veg_colors) <- unique(openlow_lichen_abundance_df$Viereck.3)
dev.off()
xlim <- c(-1.5, 1)
ylim <- c(-1.5, 1.5)
plot(mds_openlow_lichen_ab, type = "n", xlim = xlim, ylim = ylim,cex.axis = 1.5, cex.lab = 1.4, main = "Lichen Species", cex.main = 2)

text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.13",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "1",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
points(scores(mds_openlow_lichen_ab, display = "sites"),
       col = veg_colors[openlow_lichen_abundance_df$Viereck.3],
       pch = 19)
ordiarrows(mds_openlow_lichen_ab, groups = openlow_lichen_abundance_df$Plot, levels = openlow_lichen_abundance_df$Sample_Year, col = 'blue')
ordihull(mds_openlow_lichen_ab, groups = openlow_lichen_abundance_df$Park, draw ="polygon", label = TRUE)
ordihull(mds_openlow_lichen_ab, groups = openlow_lichen_abundance_df$Elevation_Band, draw ="polygon", label = TRUE)
openlowlichen_plot_viereck <- recordPlot()

species_fit <- envfit(mds_openlow_lichen_ab, openlow_lichen_abundance_matrix, perm = 999)
species_scores <- as.data.frame(species_fit$vectors$arrows)
species_scores$Species_Code <- rownames(species_scores)

arrows(0,0,species_scores$NMDS1, species_scores$NMDS2, length = 0.1, col = "red")
text(species_scores$NMDS1, species_scores$NMDS2, labels = species_scores$Species_Code, 
     col = "red", pos = c(3, 1, 4), offset = 0.5)

selected_species <- c("CLGR13", "UMHY2", "STERE2", "CLMA12", "UMHYH", "THAMN3", 
                      "FLCU", "BRDI60", "ALOC60", "CEFA60", "STVE60")
filtered_scores <- species_scores %>% filter(Species_Code %in% selected_species)
arrows(0, 0, filtered_scores$NMDS1, filtered_scores$NMDS2, length = 0.1, col = "red")
text(filtered_scores$NMDS1, filtered_scores$NMDS2, labels = filtered_scores$Species_Code, 
     col = "red", pos = c(4), offset = 0.5)

#axis1 scores 
axis1_scores <- scores(mds_openlow_lichen_ab, display = "sites")[, 1]
species_cor <- apply(openlow_lichen_abundance_matrix, 2, function(species) cor (species, axis1_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))






#turn into matrices for NMDS 
alpine_composition <- alpine_df[,c(7:280)]
alpine_composition <- as.matrix(alpine_composition) 

alpine_lichen_composition <- alpine_lichen_df[,c(13:179)]
alpine_lichen_composition <- as.matrix(alpine_lichen_composition) 

alpine_nonvasc_composition <- alpine_nonvasc_df[,c(13:219)]
alpine_nonvasc_composition <- as.matrix(alpine_nonvasc_composition) 



#vascular, colored by viereck 3
mds_alpine <- metaMDS(alpine_composition, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
#ordination information for caption (#plots, #iterations, stress value, if it converged)
length(unique(alpine_df[["Plot"]]))
mds_alpine$stress
mds_alpine$iters

veg_colors <- c("skyblue2", "darkorange", "blue4", "seagreen3")
names(veg_colors) <- unique(alpine_df$Viereck.3)
dev.off()
ordiplot(mds_alpine, type = "n")
xlim <- c(-1, 1)
ylim <- c(-1.5, 1)
plot(mds_alpine, type = "n", xlim = xlim, ylim = ylim)
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.13",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "1",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
#park_shapes <- c("LACL" = 16, "KATM" = 17)
#shapes <- park_shapes[alpine_df$Park]
points(scores(mds_alpine, display = "sites"),
       col = veg_colors[alpine_df$Viereck.3],
       pch = 19)
#       pch = shapes)
legend("bottomleft", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19)
#legend("bottomright", title = "Park",
#       legend=names(park_shapes),
#       ncol=1,
#       col = park_shapes, pch = shapes)
ordiarrows(mds_alpine, groups = alpine_df$PlotID, levels = alpine_df$Sample_Year, col = 'blue')
ordihull(mds_alpine, groups = alpine_df$Park, draw ="polygon", label = TRUE)
title("Vascular Species")

#plotID labels
text(mds_alpine, display = "sites", labels = alpine_df$PlotID, col = "black", cex = 0.7, pos = 4)
#adding only select labels 
selected_plots <- c("001", "002", "003", "006")
nmds_scores <- as.data.frame(scores(mds_alpine, display = "sites"))
nmds_scores$PlotID <- alpine_df$PlotID
selected_scores <- nmds_scores[nmds_scores$PlotID %in% selected_plots, ]
text(selected_scores$NMDS1, selected_scores$NMDS2, labels = selected_scores$PlotID,
     col = "black", cex = 0.7, pos = 4)

alpinevasc_plot_viereck <- recordPlot()
alpinevasc_plot_viereck

#lichen, colored by viereck 3
mds_alpine_lichen <- metaMDS(alpine_lichen_composition, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
#ordination information for caption (#plots, #iterations, stress value, if it converged)
length(unique(alpine_lichen_df[["Plot"]]))
mds_alpine_lichen$stress
mds_alpine_lichen$iters
mds_alpine_lichen$converged

veg_colors <- c("darkorange", "blue4", "seagreen3", "firebrick3", "skyblue2")
names(veg_colors) <- unique(alpine_lichen_df$Viereck.3)
dev.off()
ordiplot(mds_alpine_lichen, type = "n")
xlim <- c(-1.5, 1)
ylim <- c(-1.5, 1.5)
plot(mds_alpine_lichen, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4)

text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.09",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "1",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
points(scores(mds_alpine_lichen, display = "sites"),
       col = veg_colors[alpine_lichen_df$Viereck.3],
       pch = 19)
legend("bottomleft", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19, cex = 1.3)
ordiarrows(mds_alpine_lichen, groups = alpine_lichen_df$Plot, levels = alpine_lichen_df$Sample_Year, col = 'blue')
ordihull(mds_alpine_lichen, groups = alpine_lichen_df$Park, draw ="polygon", label = TRUE)
title("Lichen Species - Presence/Absence")
alpinelichen_plot_viereck <- recordPlot()


#strong positive correlations to axis 2: increasing as plots shift upward 
#strong negative correlations to axis 2: decreasing as plots shift upward 




axis2_scores <- scores(mds_alpine_lichen, display = "sites")[, 2]
species_cor <- apply(alpine_lichen_composition, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
str(fulldata)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))

species_fit <- envfit(mds_alpine_lichen, alpine_lichen_composition, perm = 999)
species_scores <- as.data.frame(species_fit$vectors$arrows)
species_scores$Species_Code <- rownames(species_scores)
selected_species <- c("CLGR13", "UMHY2", "STERE2", "CLMA12", "CLPY60", "UMHYH")
filtered_scores <- species_scores %>% filter(Species_Code %in% selected_species)
ordiplot(mds_alpine_lichen, type = "n")
arrows(0, 0, filtered_scores$NMDS1, filtered_scores$NMDS2, length = 0.1, col = "red")
text(filtered_scores$NMDS1, filtered_scores$NMDS2, labels = filtered_scores$Species_Code, 
     col = "red", pos = c(3, 1, 4), offset = 0.5)











#nonvascular, colored by viereck 3
mds_alpine_nonvasc <- metaMDS(alpine_nonvasc_composition, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)

#ordination information for caption (#plots, #iterations, stress value, if it converged)
length(unique(alpine_nonvasc_df[["Plot"]]))
mds_alpine_nonvasc$stress
mds_alpine_nonvasc$iters

veg_colors <- c("darkorange", "blue4", "seagreen3", "firebrick3", "skyblue2")
names(veg_colors) <- unique(alpine_nonvasc_df$Viereck.3)
dev.off()
ordiplot(mds_alpine_nonvasc, type = "n")

xlim <- c(-1.5, 1)
ylim <- c(-1.5, 1.5)
plot(mds_alpine_nonvasc, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4)
     
     
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.12",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "3",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
points(scores(mds_alpine_nonvasc, display = "sites"),
       col = veg_colors[alpine_nonvasc_df$Viereck.3],
       pch = 19)
legend("bottomleft", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19, cex = 1.3)


#only katmai arrows 
ordiarrows(mds_alpine_nonvasc, 
           groups = alpine_nonvasc_df$PlotID[alpine_nonvasc_df$Park == "KATM"], 
           levels = alpine_nonvasc_df$Sample_Year[alpine_nonvasc_df$Park == "KATM"], col = 'blue')
#all arrows 
ordiarrows(mds_alpine_nonvasc, 
           groups = alpine_nonvasc_df$PlotID, 
           levels = alpine_nonvasc_df$Sample_Year, col = 'blue')
ordihull(mds_alpine_nonvasc, groups = alpine_nonvasc_df$Park, draw ="polygon", label = TRUE)
title("Nonvascular Species")
alpinenonvasc_plot_viereck <- recordPlot()


#axis2 scores
axis2_scores <- scores(mds_alpine_nonvasc, display = "sites")[, 2]
species_cor <- apply(alpine_nonvasc_composition, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))



species_fit <- envfit(mds_alpine_nonvasc, alpine_nonvasc_composition, perm = 999)
species_scores <- as.data.frame(species_fit$vectors$arrows)
species_scores$Species_Code <- rownames(species_scores)

selected_species <- c("CLGR13", "UMHY2", "STERE2", "CLMA12", "CLPY60", "UMHYH")
filtered_scores <- species_scores %>% filter(Species_Code %in% selected_species)
arrows(0, 0, filtered_scores$NMDS1, filtered_scores$NMDS2, length = 0.1, col = "red")
text(filtered_scores$NMDS1, filtered_scores$NMDS2, labels = filtered_scores$Species_Code, 
     col = "red", pos = c(3, 1, 4), offset = 0.5)

#axis1 scores 
axis1_scores <- scores(mds_alpine_nonvasc, display = "sites")[, 1]
species_cor <- apply(alpine_nonvasc_composition, 2, function(species) cor (species, axis1_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))














dwarfscrub_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/dwarfscrub_df.xlsx")
dwarfscrub_df_lichens <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/dwarfscrub_df_lichens.xlsx")
dwarfscrub_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/dwarfscrub_df_nonvasc.xlsx")


dwarfscrub_composition <- dwarfscrub_df[,c(7:280)]
dwarfscrub_composition <- as.matrix(dwarfscrub_composition) 

dwarfscrub_composition_lichens <- dwarfscrub_df_lichens[,c(13:179)]
dwarfscrub_composition_lichens <- as.matrix(dwarfscrub_composition_lichens) 

dwarfscrub_composition_nonvasc <- dwarfscrub_df_nonvasc[,c(13:219)]
dwarfscrub_composition_nonvasc <- as.matrix(dwarfscrub_composition_nonvasc) 


#vascular, but colored by viereck 3
mds_dwarfscrub <- metaMDS(dwarfscrub_composition, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(dwarfscrub_df[["Plot"]]))
mds_dwarfscrub$stress
mds_dwarfscrub$iters #163
veg_colors <- c("blue4", "seagreen3")
names(veg_colors) <- unique(dwarfscrub_df$Viereck.3)
dev.off()
ordiplot(mds_dwarfscrub, type = "n")
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.10",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "1",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
points(scores(mds_dwarfscrub, display = "sites"),
       col = veg_colors[dwarfscrub_df$Viereck.3],
       pch = 19)
legend("bottomleft", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19)
ordiarrows(mds_dwarfscrub, 
           groups = dwarfscrub_df$PlotID, 
           levels = dwarfscrub_df$Sample_Year, col = 'blue')
ordihull(mds_dwarfscrub, groups = dwarfscrub_df$Park, draw ="polygon", label = TRUE)
title("Vascular Species")
dwarfscrubvasc_plot_viereck <- recordPlot()
dwarfscrubvasc_plot_viereck

#lichen, but colored by viereck 3
mds_dwarfscrub_lichen <- metaMDS(dwarfscrub_composition_lichens, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(dwarfscrub_df_lichens[["Plot"]]))
mds_dwarfscrub_lichen$stress
mds_dwarfscrub_lichen$iters #129 
veg_colors <- c("blue4", "seagreen3", "firebrick")
names(veg_colors) <- unique(dwarfscrub_df_lichens$Viereck.3)
dev.off()
ordiplot(mds_dwarfscrub_lichen, type = "n")
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.10",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "2",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
points(scores(mds_dwarfscrub_lichen, display = "sites"),
       col = veg_colors[dwarfscrub_df_lichens$Viereck.3],
       pch = 19)
legend("bottomleft", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19)
ordiarrows(mds_dwarfscrub_lichen, 
           groups = dwarfscrub_df_lichens$PlotID, 
           levels = dwarfscrub_df_lichens$Sample_Year, col = 'blue')
ordihull(mds_dwarfscrub_lichen, groups = dwarfscrub_df_lichens$Park, draw ="polygon", label = TRUE)
title("Lichen Species")
dwarfscrublichen_plot_viereck <- recordPlot()

#axis2 scores
axis2_scores <- scores(mds_dwarfscrub_lichen, display = "sites")[, 2]
species_cor <- apply(dwarfscrub_composition_lichens, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))

#axis1 scores 
axis1_scores <- scores(mds_dwarfscrub_lichen, display = "sites")[, 1]
species_cor <- apply(dwarfscrub_composition_lichens, 2, function(species) cor (species, axis1_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))



#nonvascular, but colored by viereck 3
mds_dwarfscrub_nonvasc <- metaMDS(dwarfscrub_composition_nonvasc, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(dwarfscrub_df_nonvasc[["Plot"]]))
mds_dwarfscrub_nonvasc$stress
mds_dwarfscrub_nonvasc$iters #72 
veg_colors <- c("blue4", "seagreen3", "firebrick")
names(veg_colors) <- unique(dwarfscrub_df_nonvasc$Viereck.3)
dev.off()
ordiplot(mds_dwarfscrub_nonvasc, type = "n")
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.13",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "3",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
points(scores(mds_dwarfscrub_nonvasc, display = "sites"),
       col = veg_colors[dwarfscrub_df_nonvasc$Viereck.3],
       pch = 19)
legend("bottomleft", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19)
ordiarrows(mds_dwarfscrub_nonvasc, 
           groups = dwarfscrub_df_nonvasc$PlotID, 
           levels = dwarfscrub_df_nonvasc$Sample_Year, col = 'blue')
ordihull(mds_dwarfscrub_nonvasc, groups = dwarfscrub_df_nonvasc$Park, draw ="polygon", label = TRUE)
title("Nonvascular Species")
dwarfscrubnonvasc_plot_viereck <- recordPlot()

#axis2 scores
axis2_scores <- scores(mds_dwarfscrub_nonvasc, display = "sites")[, 2]
species_cor <- apply(dwarfscrub_composition_nonvasc, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))

#axis1 scores 
axis1_scores <- scores(mds_dwarfscrub_nonvasc, display = "sites")[, 1]
species_cor <- apply(dwarfscrub_composition_nonvasc, 2, function(species) cor (species, axis1_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))













#quick load of prepared dataframes 
forest_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/forest_df.xlsx")
forest_df_lichens <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/forest_df_lichens.xlsx")
forest_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/forest_df_nonvasc.xlsx")


#turn into matrices for NMDS 
forest_composition <- forest_df[,c(7:280)]
forest_composition <- as.matrix(forest_composition) 

forest_composition_lichens <- forest_df_lichens[,c(13:179)]
forest_composition_lichens <- as.matrix(forest_composition_lichens) 

forest_composition_nonvasc <- forest_df_nonvasc[,c(13:219)]
forest_composition_nonvasc <- as.matrix(forest_composition_nonvasc) 



mds_forest <- metaMDS(forest_composition, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(forest_df[["Plot"]])) #25 
mds_forest$stress 
mds_forest$iters #92
veg_colors <- c("blue4", "seagreen3")
names(veg_colors) <- unique(forest_df$Viereck.3)
dev.off()
ordiplot(mds_forest, type = "n")
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.10",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "1",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
points(scores(mds_forest, display = "sites"),
       col = veg_colors[forest_df$Viereck.3],
       pch = 19)
legend("bottomleft", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19)
ordiarrows(mds_forest, 
           groups = forest_df$PlotID, 
           levels = forest_df$Sample_Year, col = 'blue')
ordihull(mds_forest, groups = forest_df$Park, draw ="polygon", label = TRUE)
title("Vascular Species")
forestvasc_plot_viereck <- recordPlot()
forestvasc_plot_viereck



#lichen, but colored by viereck 3
mds_forest_lichen <- metaMDS(forest_composition_lichens, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(forest_df_lichens[["Plot"]]))
mds_forest_lichen$stress
mds_forest_lichen$iters #137
veg_colors <- c("blue4", "seagreen3")
names(veg_colors) <- unique(forest_df_lichens$Viereck.3)
dev.off()
ordiplot(mds_forest_lichen, type = "n")
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.13",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "2",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
points(scores(mds_forest_lichen, display = "sites"),
       col = veg_colors[forest_df_lichens$Viereck.3],
       pch = 19)
legend("bottomleft", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19)
ordiarrows(mds_forest_lichen, 
           groups = forest_df_lichens$PlotID, 
           levels = forest_df_lichens$Sample_Year, col = 'blue')
ordihull(mds_forest_lichen, groups = forest_df_lichens$Park, draw ="polygon", label = TRUE)
title("Lichen Species")
#plotID labels
text(mds_forest_lichen, display = "sites", labels = forest_df_lichens$PlotID, col = "black", cex = 0.7, pos = 4)
#adding only select labels 
selected_plots <- c("116", "S980")
nmds_scores <- as.data.frame(scores(mds_forest_lichen, display = "sites"))
nmds_scores$PlotID <- forest_df_lichens$PlotID
selected_scores <- nmds_scores[nmds_scores$PlotID %in% selected_plots, ]
text(selected_scores$NMDS1, selected_scores$NMDS2, labels = selected_scores$PlotID,
     col = "black", cex = 0.7, pos = 4)

forestlichen_plot_viereck <- recordPlot()

#axis2 scores
axis2_scores <- scores(mds_forest_lichen, display = "sites")[, 2]
species_cor <- apply(forest_composition_lichens, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))

#axis1 scores 
axis1_scores <- scores(mds_forest_lichen, display = "sites")[, 1]
species_cor <- apply(forest_composition_lichens, 2, function(species) cor (species, axis1_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))




#nonvascular, but colored by viereck 3
mds_forest_nonvasc <- metaMDS(forest_composition_nonvasc, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(forest_df_nonvasc[["Plot"]]))
mds_forest_nonvasc$stress
mds_forest_nonvasc$iters #131
veg_colors <- c("blue4", "seagreen3")
names(veg_colors) <- unique(forest_df_nonvasc$Viereck.3)
dev.off()
ordiplot(mds_forest_nonvasc, type = "n")
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.14",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "3",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
points(scores(mds_forest_nonvasc, display = "sites"),
       col = veg_colors[forest_df_nonvasc$Viereck.3],
       pch = 19)
legend("bottomleft", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19)
ordiarrows(mds_forest_nonvasc, 
           groups = forest_df_nonvasc$PlotID, 
           levels = forest_df_nonvasc$Sample_Year, col = 'blue')
ordihull(mds_forest_nonvasc, groups = forest_df_nonvasc$Park, draw ="polygon", label = TRUE)
title("Nonvascular Species")

#plotID labels
text(mds_forest_nonvasc, display = "sites", labels = forest_df_nonvasc$PlotID, col = "black", cex = 0.7, pos = 4)
#adding only select labels 
selected_plots <- c("116", "S980")
nmds_scores <- as.data.frame(scores(mds_forest_nonvasc, display = "sites"))
nmds_scores$PlotID <- forest_df_nonvasc$PlotID
selected_scores <- nmds_scores[nmds_scores$PlotID %in% selected_plots, ]
text(selected_scores$NMDS1, selected_scores$NMDS2, labels = selected_scores$PlotID,
     col = "black", cex = 0.7, pos = 4)


forestnonvasc_plot_viereck <- recordPlot()


#axis2 scores
axis2_scores <- scores(mds_forest_nonvasc, display = "sites")[, 2]
species_cor <- apply(forest_composition_nonvasc, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))













#quick load of prepared dataframes 
needle_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/needle_df.xlsx")
needle_df_lichens <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/needle_df_lichen.xlsx")
needle_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/needle_df_nonvasc.xlsx")


#turn into matrices for NMDS 
needle_composition <- needle_df[,c(7:280)]
needle_composition <- as.matrix(needle_composition) 

needle_composition_lichens <- needle_df_lichens[,c(13:179)]
needle_composition_lichens <- as.matrix(needle_composition_lichens) 

needle_composition_nonvasc <- needle_df_nonvasc[,c(13:219)]
needle_composition_nonvasc <- as.matrix(needle_composition_nonvasc) 


#vascualar
mds_needle <- metaMDS(needle_composition, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(needle_df[["Plot"]])) #25 
mds_needle$stress 
mds_needle$iters #92
veg_colors <- c("blue4", "seagreen3")
names(veg_colors) <- unique(needle_df$Viereck.3)
dev.off()
ordiplot(mds_needle, type = "n")
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.10",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "1",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
points(scores(mds_needle, display = "sites"),
       col = veg_colors[needle_df$Viereck.3],
       pch = 19)
legend("bottomleft", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19)
ordiarrows(mds_needle, 
           groups = needle_df$PlotID, 
           levels = needle_df$Sample_Year, col = 'blue')
ordihull(mds_needle, groups = needle_df$Park, draw ="polygon", label = TRUE)
title("Vascular Species")

#plotID labels
text(mds_needle, display = "sites", labels = needle_df$PlotID, col = "black", cex = 0.7, pos = 4)
#adding only select labels 
selected_plots <- c("116", "S980")
nmds_scores <- as.data.frame(scores(mds_needle, display = "sites"))
nmds_scores$PlotID <- needle_df$PlotID
selected_scores <- nmds_scores[nmds_scores$PlotID %in% selected_plots, ]
text(selected_scores$NMDS1, selected_scores$NMDS2, labels = selected_scores$PlotID,
     col = "black", cex = 0.7, pos = 4)





needlevasc_plot_viereck <- recordPlot()
needlevasc_plot_viereck


#lichens
mds_needle_lichens <- metaMDS(needle_composition_lichens, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(needle_df_lichens[["Plot"]])) #25 
mds_needle_lichens$stress 
mds_needle_lichens$iters #92
veg_colors <- c("blue4", "seagreen3")
names(veg_colors) <- unique(needle_df_lichens$Viereck.3)
dev.off()
ordiplot(mds_needle_lichens, type = "n")
xlim <- c(-1.5, 1)
ylim <- c(-1, 1.5)
plot(mds_needle_lichens, type = "n", xlim = xlim, ylim = ylim)
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.14",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "1",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
points(scores(mds_needle_lichens, display = "sites"),
       col = veg_colors[needle_df_lichens$Viereck.3],
       pch = 19)
legend("bottomleft", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19)
ordiarrows(mds_needle_lichens, 
           groups = needle_df_lichens$Plot, 
           levels = needle_df_lichens$Sample_Year, col = 'blue')
ordihull(mds_needle_lichens, groups = needle_df_lichens$Park, draw ="polygon", label = TRUE)
ordihull(mds_needle_lichens, groups = needle_df_lichens$Elevation_Band, draw ="polygon", label = TRUE)
legend("bottomright", title = "Elevation Band",
       legend=c("01 (<450 m)", "02 (450-900 m)"),
       ncol=1)
#legend("bottomleft", title = "Elevation Band",
#       legend=c("01 (<450 m)", "02 (450-900 m)", "03 (>900 m)"),
#       ncol=1)
title("Lichen Species")

#plotID labels
text(mds_needle_lichens, display = "sites", labels = needle_df_lichens$PlotID, col = "black", cex = 0.7, pos = 4)
#adding only select labels 
selected_plots <- c("116", "S980")
nmds_scores <- as.data.frame(scores(mds_needle_lichens, display = "sites"))
nmds_scores$PlotID <- needle_df_lichens$PlotID
selected_scores <- nmds_scores[nmds_scores$PlotID %in% selected_plots, ]
text(selected_scores$NMDS1, selected_scores$NMDS2, labels = selected_scores$PlotID,
     col = "black", cex = 0.7, pos = 4)

needlelichen_plot_viereck <- recordPlot()
needlelichen_plot_viereck


#axis2 scores
axis2_scores <- scores(mds_needle_lichens, display = "sites")[, 2]
species_cor <- apply(needle_composition_lichens, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))


#axis1 scores 
axis1_scores <- scores(mds_needle_lichens, display = "sites")[, 1]
species_cor <- apply(needle_composition_lichens, 2, function(species) cor (species, axis1_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))






#nonvasc
mds_needle_nonvasc <- metaMDS(needle_composition_nonvasc, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(needle_df_nonvasc[["Plot"]])) #25 
mds_needle_nonvasc$stress 
mds_needle_nonvasc$iters #92
veg_colors <- c("blue4", "seagreen3")
names(veg_colors) <- unique(needle_df_nonvasc$Viereck.3)
dev.off()
ordiplot(mds_needle_nonvasc, type = "n")
xlim <- c(-1, 1)
ylim <- c(-1, 1.5)
plot(mds_needle_nonvasc, type = "n", xlim = xlim, ylim = ylim)
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.13",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "1",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
points(scores(mds_needle_nonvasc, display = "sites"),
       col = veg_colors[needle_df_nonvasc$Viereck.3],
       pch = 19)
legend("bottomleft", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19)
ordiarrows(mds_needle_nonvasc, 
           groups = needle_df_nonvasc$PlotID, 
           levels = needle_df_nonvasc$Sample_Year, col = 'blue')
ordihull(mds_needle_nonvasc, groups = needle_df_nonvasc$Park, draw ="polygon", label = TRUE)
title("Nonvascular Species")
needlelichen_plot_viereck <- recordPlot()
needlelichen_plot_viereck



#axis2 scores
axis2_scores <- scores(mds_needle_nonvasc, display = "sites")[, 2]
species_cor <- apply(needle_composition_nonvasc, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))













#beetle kill spruce 
beetle_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/beetle_df.xlsx")
beetle_lichen_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/beetle_lichen_df.xlsx")
beetle_nonvasc_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/beetle_nonvasc_df.xlsx")

beetle_composition <- beetle_df[,c(7:280)]
beetle_composition <- as.matrix(beetle_composition) 

beetle_lichen_composition <- beetle_lichen_df[,c(13:179)]
beetle_lichen_composition <- as.matrix(beetle_lichen_composition) 

beetle_nonvasc_composition <- beetle_nonvasc_df[,c(13:219)]
beetle_nonvasc_composition <- as.matrix(beetle_nonvasc_composition) 

#beetle_df <- beetle_df %>%
#  left_join(viereck %>% select(Plot_Year, Viereck.2, Viereck.3),
#            by = "Plot_Year")


beetle_vasc_abundance_df <- vasc_abundance_df %>%
  filter(Plot_Year %in% beetle_df$Plot_Year)

beetle_vasc_abundance_matrix <- beetle_vasc_abundance_df[,c(8:281)]
beetle_vasc_abundance_matrix <- as.matrix(beetle_vasc_abundance_matrix) 


beetle_lichen_abundance_df <- lichen_abundance_df %>%
  filter(Plot_Year %in% beetle_lichen_df$Plot_Year)

beetle_lichen_abundance_matrix <- beetle_lichen_abundance_df[,c(12:178)]
beetle_lichen_abundance_matrix <- as.matrix(beetle_lichen_abundance_matrix) 


beetle_nonvasc_abundance_df <- nonvasc_abundance_df %>%
  filter(Plot_Year %in% beetle_nonvasc_df$Plot_Year)

beetle_nonvasc_abundance_matrix <- beetle_nonvasc_abundance_df[,c(12:218)]
beetle_nonvasc_abundance_matrix <- as.matrix(beetle_nonvasc_abundance_matrix) 





#color by canopy cover gradient 
canopy_cover <- read_csv("T:/Users/KPace/SWAN-Internship-New/Data/Unmodified/Canopy_Cover_PtInt.csv")
canopy_cover <- canopy_cover %>%
  mutate(Plot_Year = paste(Plot, Sample_Year, sep = "_")) %>%
  group_by(Plot_Year) %>%
  mutate(Percent_Cover = (sum(hits) / 177) * 100) %>% 
  ungroup()


beetle_vasc_abundance_df <- beetle_vasc_abundance_df %>%
  left_join(canopy_cover %>% select(Plot_Year, Percent_Cover), by = c("Plot_Year"))
beetle_vasc_abundance_df <- beetle_vasc_abundance_df %>% distinct()

beetle_vasc_abundance_matrix <- beetle_vasc_abundance_df[,c(8:281)]
beetle_vasc_abundance_matrix <- as.matrix(beetle_vasc_abundance_matrix) 


xlim <- c(-1.5, 1.5)
ylim <- c(-1.5, 1.5)
mds_beetle_ab_CC <- metaMDS(beetle_vasc_abundance_matrix, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
nmds_scores <- scores(mds_beetle_ab_CC, display = "sites")
nmds_scores <- as.data.frame(nmds_scores)
nmds_scores$Plot_Year <- beetle_vasc_abundance_df$Plot_Year
nmds_scores$Percent_Cover <- beetle_vasc_abundance_df$Percent_Cover
color_gradient <- colorRampPalette(c("blue", "red"))(100)
point_colors <- color_gradient[cut(nmds_scores$Percent_Cover, breaks = 100)]


ordiplot(mds_beetle_ab_CC, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4, main = "Vascular Species", cex.main = 2)
points(nmds_scores$NMDS1, nmds_scores$NMDS2, col = point_colors, pch = 19, cex = 1)
legend("bottomleft", legend = round(range(nmds_scores$Percent_Cover), 2),
       fill = color_gradient[c(1, 100)], title = "Percent Cover", bty = "n")
ordiarrows(mds_beetle_ab_CC, 
           groups = beetle_vasc_abundance_df$Plot, 
           levels = beetle_vasc_abundance_df$Sample_Year, col = 'blue')
ordihull(mds_beetle_ab_CC, groups = beetle_vasc_abundance_df$Park, draw ="polygon", label = TRUE)
ordihull(mds_beetle_ab_CC, groups = beetle_vasc_abundance_df$Viereck.4, draw ="polygon", label = TRUE)

text(mds_beetle_ab_CC, display = "sites", labels = beetle_vasc_abundance_df$Plot, col = "black", cex = 0.7, pos = 4)

ordiplot(mds_beetle_ab_CC, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4, main = "Vascular Species", cex.main = 2)
points(nmds_scores$NMDS1, nmds_scores$NMDS2, col =  "black", pch = 19, cex = 1)
ordisurf(mds_beetle_ab_CC, beetle_vasc_abundance_df$Percent_Cover, method = "REML", add = TRUE, col = "blue")
legend("bottomright", legend = "Point Intercept Canopy Cover Percent", col = "blue", lty = 1, bty = "n", cex = 1.5)
#ordiarrows(mds_beetle_ab_CC, 
#           groups = beetle_vasc_abundance_df$Plot, 
#           levels = beetle_vasc_abundance_df$Sample_Year, col = 'blue')
#ordihull(mds_beetle_ab_CC, groups = beetle_vasc_abundance_df$Park, draw ="polygon", label = TRUE)
mds_beetle_ab_CC$stress
length(unique(beetle_vasc_abundance_df[["Plot"]]))
mds_beetle_ab_CC_lichen$iters #158
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.07",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "1",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")




#seeing if its white vs black spruce forest but it is not 
#viereck_classes <- read_csv("T:/Users/KPace/SWAN-Internship-New/Data/Unmodified/Viereck_Classes.csv")
#beetle_vasc_abundance_df <- beetle_vasc_abundance_df %>%
#  left_join(viereck_classes %>% select(Plot, Viereck.4), by = c("Plot"))
#beetle_vasc_abundance_df <- beetle_vasc_abundance_df %>% distinct()


#lichen 
beetle_lichen_abundance_df <- beetle_lichen_abundance_df %>%
  left_join(canopy_cover %>% select(Plot_Year, Percent_Cover), by = c("Plot_Year"))
beetle_lichen_abundance_df <- beetle_lichen_abundance_df %>% distinct()

beetle_lichen_abundance_matrix <- beetle_lichen_abundance_df[,c(12:178)]
beetle_lichen_abundance_matrix <- as.matrix(beetle_lichen_abundance_matrix) 

mds_beetle_ab_CC_lichen <- metaMDS(beetle_lichen_abundance_matrix, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
nmds_scores <- scores(mds_beetle_ab_CC_lichen, display = "sites")
nmds_scores <- as.data.frame(nmds_scores)
nmds_scores$Plot_Year <- beetle_lichen_abundance_df$Plot_Year
nmds_scores$Percent_Cover <- beetle_lichen_abundance_df$Percent_Cover
color_gradient <- colorRampPalette(c("blue", "red"))(100)
point_colors <- color_gradient[cut(nmds_scores$Percent_Cover, breaks = 100)]

xlim <- c(-1.5, 1.5)
ylim <- c(-1.5, 1.5)
ordiplot(mds_beetle_ab_CC_lichen, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4, main = "Lichen Species", cex.main = 2)
points(nmds_scores$NMDS1, nmds_scores$NMDS2, col = point_colors, pch = 19, cex = 1)
legend("bottomright", legend = round(range(nmds_scores$Percent_Cover), 2),
       fill = color_gradient[c(1, 100)], title = "Percent Cover", bty = "n")
ordiarrows(mds_beetle_ab_CC_lichen, 
           groups = beetle_lichen_abundance_df$Plot, 
           levels = beetle_lichen_abundance_df$Sample_Year, col = 'blue')
ordihull(mds_beetle_ab_CC_lichen, groups = beetle_lichen_abundance_df$Park, draw ="polygon", label = TRUE)
ordihull(mds_beetle_ab_CC_lichen, groups = beetle_lichen_abundance_df$Viereck.4, draw ="polygon", label = TRUE)

text(mds_beetle_ab_CC_lichen, display = "sites", labels = beetle_lichen_abundance_df$Plot, col = "black", cex = 0.7, pos = 4)

ordiplot(mds_beetle_ab_CC_lichen, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4, main = "Lichen Species", cex.main = 2)
points(nmds_scores$NMDS1, nmds_scores$NMDS2, col =  "black", pch = 19, cex = 1)
ordisurf(mds_beetle_ab_CC_lichen, beetle_lichen_abundance_df$Percent_Cover, method = "REML", add = TRUE, col = "blue")
#ordiarrows(mds_beetle_ab_CC_lichen, 
#           groups = beetle_lichen_abundance_df$Plot, 
#           levels = beetle_lichen_abundance_df$Sample_Year, col = 'blue')
#ordihull(mds_beetle_ab_CC_lichen, groups = beetle_lichen_abundance_df$Park, draw ="polygon", label = TRUE)
legend("bottomright", legend = "Point Intercept Canopy Cover Percent", col = "blue", lty = 1, bty = "n", cex = 1.5)
mds_beetle_ab_CC_lichen$stress
length(unique(beetle_lichen_abundance_df[["Plot"]]))
mds_beetle_ab_CC_lichen$iters #126
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.13",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "2",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")



#nonvascular
beetle_nonvasc_abundance_df <- beetle_nonvasc_abundance_df %>%
  left_join(canopy_cover %>% select(Plot_Year, Percent_Cover), by = c("Plot_Year"))
beetle_nonvasc_abundance_df <- beetle_nonvasc_abundance_df %>% distinct()

beetle_nonvasc_abundance_matrix <- beetle_nonvasc_abundance_df[,c(12:218)]
beetle_nonvasc_abundance_matrix <- as.matrix(beetle_nonvasc_abundance_matrix) 

mds_beetle_ab_CC_nonvasc <- metaMDS(beetle_nonvasc_abundance_matrix, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
nmds_scores <- scores(mds_beetle_ab_CC_nonvasc, display = "sites")
nmds_scores <- as.data.frame(nmds_scores)
nmds_scores$Plot_Year <- beetle_nonvasc_abundance_df$Plot_Year
nmds_scores$Percent_Cover <- beetle_nonvasc_abundance_df$Percent_Cover
color_gradient <- colorRampPalette(c("blue", "red"))(100)
point_colors <- color_gradient[cut(nmds_scores$Percent_Cover, breaks = 100)]

xlim <- c(-1.5, 1.5)
ylim <- c(-1.5, 1.5)
ordiplot(mds_beetle_ab_CC_nonvasc, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4, main = "Nonvascular Species", cex.main = 2)
points(nmds_scores$NMDS1, nmds_scores$NMDS2, col = point_colors, pch = 19, cex = 1)
legend("bottomright", legend = round(range(nmds_scores$Percent_Cover), 2),
       fill = color_gradient[c(1, 100)], title = "Percent Cover", bty = "n")
ordiarrows(mds_beetle_ab_CC_nonvasc, 
           groups = beetle_nonvasc_abundance_df$Plot, 
           levels = beetle_nonvasc_abundance_df$Sample_Year, col = 'blue')
ordihull(mds_beetle_ab_CC_nonvasc, groups = beetle_nonvasc_abundance_df$Park, draw ="polygon", label = TRUE)

text(mds_beetle_ab_CC_nonvasc, display = "sites", labels = beetle_nonvasc_abundance_df$Plot, col = "black", cex = 0.7, pos = 4)

ordiplot(mds_beetle_ab_CC_nonvasc, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4, main = "Nonvascular Species", cex.main = 2)
points(nmds_scores$NMDS1, nmds_scores$NMDS2, col =  "black", pch = 19, cex = 1)
ordisurf(mds_beetle_ab_CC_nonvasc, beetle_nonvasc_abundance_df$Percent_Cover, method = "REML", add = TRUE, col = "blue")
#legend("topright", legend = "Canopy Cover Percent", col = "blue", lty = 1, bty = "n")
#ordiarrows(mds_beetle_ab_CC_nonvasc, 
#           groups = beetle_nonvasc_abundance_df$Plot, 
#           levels = beetle_nonvasc_abundance_df$Sample_Year, col = 'blue')
#ordihull(mds_beetle_ab_CC_nonvasc, groups = beetle_nonvasc_abundance_df$Park, draw ="polygon", label = TRUE)

legend("bottomright", legend = "Point Intercept Canopy Cover Percent", col = "blue", lty = 1, bty = "n", cex = 1.5)
mds_beetle_ab_CC_nonvasc$stress
mds_beetle_ab_CC_nonvasc$iters #142
length(unique(beetle_nonvasc_abundance_df[["Plot"]]))
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.11",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "3",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")













#vascualar
mds_beetle_ab <- metaMDS(beetle_vasc_abundance_matrix, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(beetle_vasc_abundance_df[["Plot"]])) #15 
mds_beetle_ab$stress 
mds_beetle_ab$iters #107
veg_colors <- c("blue4", "seagreen3")
names(veg_colors) <- unique(beetle_vasc_abundance_df$Viereck.3)
dev.off()

xlim <- c(-1.5, 1.5)
ylim <- c(-1, 1)
ordiplot(mds_beetle_ab, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4, main = "Vascular Species", cex.main = 2)
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.07",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "2",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
points(scores(mds_beetle_ab, display = "sites"),
       col = veg_colors[beetle_vasc_abundance_df$Viereck.3],
       pch = 19)
legend("bottomright", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19,
       cex = 1.3)
ordiarrows(mds_beetle_ab, 
           groups = beetle_vasc_abundance_df$Plot, 
           levels = beetle_vasc_abundance_df$Sample_Year, col = 'blue')
ordihull(mds_beetle_ab, groups = beetle_vasc_abundance_df$Park, draw ="polygon", label = TRUE)
#title("Vascular Species")

#plotID labels
text(mds_beetle_ab, display = "sites", labels = beetle_vasc_abundance_df$PlotID, col = "black", cex = 0.7, pos = 4)
#adding only select labels 
selected_plots <- c("116", "S980")
nmds_scores <- as.data.frame(scores(mds_beetle_ab, display = "sites"))
nmds_scores$PlotID <- beetle_vasc_abundance_df$PlotID
selected_scores <- nmds_scores[nmds_scores$PlotID %in% selected_plots, ]
text(selected_scores$NMDS1, selected_scores$NMDS2, labels = selected_scores$PlotID,
     col = "black", cex = 0.7, pos = 4)

beetlevasc_plot_viereck <- recordPlot()
beetlevasc_plot_viereck


#axis2 scores
axis2_scores <- scores(mds_beetle_ab, display = "sites")[, 2]
species_cor <- apply(beetle_vasc_abundance_matrix, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))


#axis1 scores 
axis1_scores <- scores(mds_beetle_ab, display = "sites")[, 1]
species_cor <- apply(beetle_vasc_abundance_matrix, 2, function(species) cor (species, axis1_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))









#lichen
mds_beetle_lichen_ab <- metaMDS(beetle_lichen_abundance_matrix, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(beetle_lichen_abundance_df[["Plot"]])) #14
mds_beetle_lichen_ab$stress 
mds_beetle_lichen_ab$iters #117
veg_colors <- c("blue4", "seagreen3")
names(veg_colors) <- unique(beetle_lichen_abundance_df$Viereck.3)
dev.off()
xlim <- c(-1.5, 1.5)
ylim <- c(-1, 1)
ordiplot(mds_beetle_lichen_ab, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4, main = "Lichen Species", cex.main = 2)
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.13",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "2",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
points(scores(mds_beetle_lichen_ab, display = "sites"),
       col = veg_colors[beetle_lichen_abundance_df$Viereck.3],
       pch = 19)
legend("bottomright", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19, 
       cex = 1.3)
ordiarrows(mds_beetle_lichen_ab, 
           groups = beetle_lichen_abundance_df$Plot, 
           levels = beetle_lichen_abundance_df$Sample_Year, col = 'blue')
ordihull(mds_beetle_lichen_ab, groups = beetle_lichen_abundance_df$Park, draw ="polygon", label = TRUE)
title("Lichen Species")
#plotID labels
text(mds_beetle_lichen_ab, display = "sites", labels = beetle_lichen_abundance_df$PlotID, col = "black", cex = 0.7, pos = 4)
#adding only select labels 
selected_plots <- c("116", "S980")
nmds_scores <- as.data.frame(scores(mds_beetle_lichen_ab, display = "sites"))
nmds_scores$PlotID <- beetle_lichen_abundance_df$PlotID
selected_scores <- nmds_scores[nmds_scores$PlotID %in% selected_plots, ]
text(selected_scores$NMDS1, selected_scores$NMDS2, labels = selected_scores$PlotID,
     col = "black", cex = 0.7, pos = 4)

beetle_lichen_plot_viereck <- recordPlot()
beetle_lichen_plot_viereck

#axis2 scores
axis2_scores <- scores(mds_beetle_lichen_ab, display = "sites")[, 2]
species_cor <- apply(beetle_lichen_abundance_matrix, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))



#nonvasc
mds_beetle_nonvasc_ab <- metaMDS(beetle_nonvasc_abundance_matrix, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(beetle_nonvasc_abundance_df[["Plot"]])) #14
mds_beetle_nonvasc_ab$stress 
mds_beetle_nonvasc_ab$iters #173
veg_colors <- c("blue4", "seagreen3")
names(veg_colors) <- unique(beetle_nonvasc_abundance_df$Viereck.3)
dev.off()
xlim <- c(-1.5, 1.5)
ylim <- c(-1, 1)
ordiplot(mds_beetle_nonvasc_ab, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4, main = "Nonvascular Species", cex.main = 2)
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.11",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "2",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
points(scores(mds_beetle_nonvasc_ab, display = "sites"),
       col = veg_colors[beetle_nonvasc_abundance_df$Viereck.3],
       pch = 19)
legend("bottomright", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19,
       cex = 1.3)
ordiarrows(mds_beetle_nonvasc_ab, 
           groups = beetle_nonvasc_abundance_df$Plot, 
           levels = beetle_nonvasc_abundance_df$Sample_Year, col = 'blue')
ordihull(mds_beetle_nonvasc_ab, groups = beetle_nonvasc_abundance_df$Park, draw ="polygon", label = TRUE)
title("Nonvascular Species")
#plotID labels
text(mds_beetle_nonvasc_ab, display = "sites", labels = beetle_nonvasc_abundance_df$PlotID, col = "black", cex = 0.7, pos = 4)
#adding only select labels 
selected_plots <- c("116", "S980")
nmds_scores <- as.data.frame(scores(mds_beetle_nonvasc_ab, display = "sites"))
nmds_scores$PlotID <- beetle_nonvasc_abundance_df$PlotID
selected_scores <- nmds_scores[nmds_scores$PlotID %in% selected_plots, ]
text(selected_scores$NMDS1, selected_scores$NMDS2, labels = selected_scores$PlotID,
     col = "black", cex = 0.7, pos = 4)

beetle_nonvasc_plot_viereck <- recordPlot()
beetle_nonvasc_plot_viereck


#axis2 scores
axis2_scores <- scores(mds_beetle_nonvasc_ab, display = "sites")[, 2]
species_cor <- apply(beetle_nonvasc_abundance_matrix, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))













#open low shrub 

openlow_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_df.xlsx")
openlow_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_df_lichen.xlsx")
openlow_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_df_nonvasc.xlsx")

openlow_composition <- openlow_df[,c(7:280)]
openlow_composition <- as.matrix(openlow_composition) 

openlow_lichen_composition <- openlow_df_lichen[,c(13:179)]
openlow_lichen_composition <- as.matrix(openlow_lichen_composition) 

openlow_nonvasc_composition <- openlow_df_nonvasc[,c(13:219)]
openlow_nonvasc_composition <- as.matrix(openlow_nonvasc_composition) 


#vascualar
mds_openlow <- metaMDS(openlow_composition, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(openlow_df[["Plot"]])) #25 
mds_openlow$stress 
mds_openlow$iters #92
veg_colors <- c("seagreen3", "blue4")
names(veg_colors) <- unique(openlow_df$Viereck.3)
dev.off()
ordiplot(mds_openlow, type = "n")
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.14",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "1",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
points(scores(mds_openlow, display = "sites"),
       col = veg_colors[openlow_df$Viereck.3],
       pch = 19)
ordiarrows(mds_openlow, 
           groups = openlow_df$Plot, 
           levels = openlow_df$Sample_Year, col = 'blue')
ordihull(mds_openlow, groups = openlow_df$Park, draw ="polygon", label = TRUE)
title("Vascular Species")

#plotID labels
text(mds_openlow, display = "sites", labels = openlow_df$PlotID, col = "black", cex = 0.7, pos = 4)
#adding only select labels 
selected_plots <- c("116", "S980")
nmds_scores <- as.data.frame(scores(mds_openlow, display = "sites"))
nmds_scores$PlotID <- openlow_df$PlotID
selected_scores <- nmds_scores[nmds_scores$PlotID %in% selected_plots, ]
text(selected_scores$NMDS1, selected_scores$NMDS2, labels = selected_scores$PlotID,
     col = "black", cex = 0.7, pos = 4)

openlowvasc_plot_viereck <- recordPlot()
openlowvasc_plot_viereck


#axis1 scores 
axis1_scores <- scores(mds_openlow, display = "sites")[, 1]
species_cor <- apply(openlow_composition, 2, function(species) cor (species, axis1_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code") 
species_cor_sorted <-species_cor_sorted %>% distinct()
summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))






#lichen
mds_openlow_lichen <- metaMDS(openlow_lichen_composition, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(openlow_df_lichen[["Plot"]])) #25 
mds_openlow_lichen$stress 
mds_openlow_lichen$iters #92
veg_colors <- c("seagreen3", "blue4")
names(veg_colors) <- unique(openlow_df_lichen$Viereck.3)
dev.off()

xlim <- c(-1, 1)
ylim <- c(-0.5, 1)
ordiplot(mds_openlow_lichen, type = "n", xlim = xlim, ylim = ylim)
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.15",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "1",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
points(scores(mds_openlow_lichen, display = "sites"),
       col = veg_colors[openlow_df_lichen$Viereck.3],
       pch = 19)
#legend("bottomleft", title = "Site Classification",
#       legend=names(veg_colors),
#       ncol=1,
#       col = veg_colors, pch = 19)
ordiarrows(mds_openlow_lichen, 
           groups = openlow_df_lichen$Plot, 
           levels = openlow_df_lichen$Sample_Year, col = 'blue')
ordihull(mds_openlow_lichen, groups = openlow_df_lichen$Park, draw ="polygon", label = TRUE)
title("Lichen Species")

#plotID labels
text(mds_openlow_lichen, display = "sites", labels = openlow_df_lichen$PlotID, col = "black", cex = 0.7, pos = 4)
#adding only select labels 
selected_plots <- c("116", "S980")
nmds_scores <- as.data.frame(scores(mds_openlow_lichen, display = "sites"))
nmds_scores$PlotID <- openlow_df_lichen$PlotID
selected_scores <- nmds_scores[nmds_scores$PlotID %in% selected_plots, ]
text(selected_scores$NMDS1, selected_scores$NMDS2, labels = selected_scores$PlotID,
     col = "black", cex = 0.7, pos = 4)

openlow_lichen_plot_viereck <- recordPlot()
openlow_lichen_plot_viereck



species_fit <- envfit(mds_openlow_lichen, openlow_lichen_composition, perm = 999)
species_scores <- as.data.frame(species_fit$vectors$arrows)
species_scores$Species_Code <- rownames(species_scores)

arrows(0,0,species_scores$NMDS1, species_scores$NMDS2, length = 0.1, col = "red")
text(species_scores$NMDS1, species_scores$NMDS2, labels = species_scores$Species_Code, 
     col = "red", pos = c(3, 1, 4), offset = 0.5)

selected_species <- c("PEBR21", "NEBE60", "PEME60", "PENE12", "CLAR6", "MELO60", "LOHA60")
filtered_scores <- species_scores %>% filter(Species_Code %in% selected_species)
arrows(0, 0, filtered_scores$NMDS1, filtered_scores$NMDS2, length = 0.1, col = "red")
text(filtered_scores$NMDS1, filtered_scores$NMDS2, labels = filtered_scores$Species_Code, 
     col = "red", pos = c(3), offset = 0.5)




#axis2 scores
axis2_scores <- scores(mds_openlow_lichen, display = "sites")[, 2]
species_cor <- apply(openlow_lichen_composition, 2, function(species) cor (species, axis2_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))


#axis1 scores 
axis1_scores <- scores(mds_openlow_lichen, display = "sites")[, 1]
species_cor <- apply(openlow_lichen_composition, 2, function(species) cor (species, axis1_scores, method = "spearman"))
species_cor_sorted <- sort(species_cor, decreasing = TRUE)
head(species_cor_sorted, 10)
tail(species_cor_sorted, 10)

species_cor_sorted <- as.data.frame(species_cor_sorted)
species_cor_sorted <- species_cor_sorted %>%
  rownames_to_column(var = "Species_Code")
species_cor_sorted <- species_cor_sorted %>%
  rename(Loadings = species_cor_sorted)
str(species_cor_sorted)
species_cor_sorted$Species_Code <- factor(species_cor_sorted$Species_Code)
fulldata$Species_Code <- factor(fulldata$Species_Code)
species_cor_sorted <- species_cor_sorted %>%
  left_join(fulldata %>% select(Species_Name, Species_Code), by = "Species_Code")
species_cor_sorted <-species_cor_sorted %>% distinct()

summary_df <- species_cor_sorted %>%
  arrange(Loadings) %>%
  slice(c(1:10, (n()-9):n()))


#nonvasc
mds_openlow_nonvasc <- metaMDS(openlow_nonvasc_composition, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
length(unique(openlow_df_nonvasc[["Plot"]])) #25 
mds_openlow_nonvasc$stress 
mds_openlow_nonvasc$iters #92
veg_colors <- c("blue4", "seagreen3")
names(veg_colors) <- unique(openlow_df_nonvasc$Viereck.3)
dev.off()
ordiplot(mds_openlow_nonvasc, type = "n")
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.14",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "1",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.2, #size 
     col = "black")
points(scores(mds_openlow_nonvasc, display = "sites"),
       col = veg_colors[openlow_df_nonvasc$Viereck.3],
       pch = 19)
#legend("bottomleft", title = "Site Classification",
#       legend=names(veg_colors),
#       ncol=1,
#       col = veg_colors, pch = 19)
ordiarrows(mds_openlow_nonvasc, 
           groups = openlow_df_nonvasc$Plot, 
           levels = openlow_df_nonvasc$Sample_Year, col = 'blue')
ordihull(mds_openlow_nonvasc, groups = openlow_df_nonvasc$Park, draw ="polygon", label = TRUE)

#plotID labels
text(mds_openlow_nonvasc, display = "sites", labels = openlow_df_nonvasc$PlotID, col = "black", cex = 0.7, pos = 4)
#adding only select labels 
selected_plots <- c("116", "S980")
nmds_scores <- as.data.frame(scores(mds_openlow_nonvasc, display = "sites"))
nmds_scores$PlotID <- openlow_df_nonvasc$PlotID
selected_scores <- nmds_scores[nmds_scores$PlotID %in% selected_plots, ]
text(selected_scores$NMDS1, selected_scores$NMDS2, labels = selected_scores$PlotID,
     col = "black", cex = 0.7, pos = 4)

title("Nonvascular Species")
beetle_nonvasc_plot_viereck <- recordPlot()
beetle_nonvasc_plot_viereck