library(vegan)
library(permute)
library(lattice)
library(dplyr)
library(readxl)
library(writexl)
library(readr)
library(dplyr)
library(tibble)
 
#load necessary data 
plot_fire <- (here("Data/Unmodified/plot_fire.rdata"))
viereck <- read_xlsx(here("Data/Modified/Viereck_env.xlsx"))
lichen_abundance_df <- read_xlsx(here("Data/Modified/quad_abundance_df_lichen.xlsx"))
nonvasc_abundance_df <- read_xlsx(here("Data/Modified/quad_abundance_df_nonvasc.xlsx"))
vasc_abundance_df <- read_xlsx(here("Data/Modified/quad_abundance_df_vascular.xlsx"))

#add in viereck classes for grouping 
plot_fire <- plot_fire %>%
  left_join(viereck %>% select(Plot, Vegetation_Class, Viereck.2, Viereck.3), by = c("Plot"))
#remove duplicates because the join is being weird 
plot_fire <- plot_fire %>% distinct()

#see which plots have evidence of fire and make a separate dataframe of them 
plot_fire_true <- plot_fire %>% filter(fire_evid == TRUE)

#check to see if plots in these datafranes are included in the fire plots list - loaded them and check 
openlow_df <- read_xlsx(here("Data/Modified/openlow_df.xlsx"))
needle_df <- read_xlsx(here("Data/Modified/needle_df.xlsx"))
beetle_df <- read_xlsx(here("Data/Modified/dwarfscrub_df.xlsx"))

    #see what plots are burned in these subsets 
        common_plots_ls <- intersect(openlow_df$Plot, plot_fire_true$Plot)
        length(common_plots_ls)

        common_plots_sw <- intersect(needle_df$Plot, plot_fire_true$Plot)
        length(common_plots_sw)

        
#Permanova analysis to see if burned plots are different than non-burned plots 
        
#add fire evidence column to main dataframe 
needle_df <- needle_df %>%
  left_join(plot_fire %>% select(Plot, fire_evid), by = "Plot")
#why are duplicates being so weird?? 
needle_df <- needle_df %>% distinct()

#select only balanced sample years that were used in the official permanova analysis 
needle_vasc_abundance_balanced <- vasc_abundance_df %>%
  filter(Plot_Year %in% needle_df$Plot_Year)
needle_df <- needle_vasc_abundance_balanced 

#prepare dataframes for adonis2
needle_composition <- needle_df[,c(8:281)]
needle_composition <- as.matrix(needle_composition) 
needle_env <- needle_df[,c(1:7, 282)]
#create visit column 
needle_env <- needle_env %>%
  arrange(Plot, Sample_Year) %>%
  group_by(Plot) %>%
  mutate(Visit = paste0("visit_", row_number())) %>%
  ungroup()
#create permutation restruction design 
perm_design_needle_time = how(
  plots = Plots(strata = needle_env$Plot, type = c("free")),
  within = Within(type = "series", mirror = FALSE),
  nperm = 999)

#run adonis2 
needle_vascular_perm <- adonis2(needle_composition ~ fire_evid + Park + Plot + Visit + fire_evid*Visit, 
                                data = needle_env, method = "bray", 
                                permutations = perm_design_needle_time, 
                                by = "terms")
print(needle_vascular_perm)


#open low shrub grouping 

#remove fire plots that made it into the low shrub grouping: create a list of them to remove from the loaded data frames 
openlow_plots_to_remove <- c("LACL_2010_01_105",
                              "LACL_2015_01_013",
                              "LACL_2015_01_104",
                              "LACL_2017_01_005",
                              "LACL_2017_01_161",
                              "LACL_2017_01_169")

#load data 
openlow_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_df.xlsx")
openlow_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_df_lichen.xlsx")
openlow_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_df_nonvasc.xlsx")

openlow_env <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_env.xlsx")
openlow_env_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_env_lichen.xlsx")
openlow_env_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_env_nonvasc.xlsx")

openlow_lichen_abundance_balanced <- lichen_abundance_df %>%
  filter(Plot_Year %in% openlow_df_lichen$Plot_Year)
openlow_df_lichen <- openlow_lichen_abundance_balanced 

openlow_df_lichen <- openlow_df_lichen %>%
  filter(!Plot %in% openlow_plots_to_remove)

          openlow_composition_lichen <- openlow_df_lichen[,c(12:178)]
          openlow_composition_lichen <- as.matrix(openlow_composition_lichen) 
          
          openlow_env_lichen <- openlow_df_lichen[,c(1:11)]
          
          openlow_env_lichen <- openlow_env_lichen %>%
            arrange(Plot, Sample_Year) %>%
            group_by(Plot) %>%
            mutate(Visit = paste0("visit_", row_number())) %>%
            ungroup()

openlow_nonvasc_abundance_balanced <- nonvasc_abundance_df %>%
  filter(Plot_Year %in% openlow_df_nonvasc$Plot_Year)
openlow_df_nonvasc <- openlow_nonvasc_abundance_balanced

openlow_df_nonvasc <- openlow_df_nonvasc %>%
  filter(!Plot %in% openlow_plots_to_remove)

          openlow_composition_nonvasc <- openlow_df_nonvasc[,c(12:218)]
          openlow_composition_nonvasc <- as.matrix(openlow_composition_nonvasc) 
          
          openlow_env_nonvasc <- openlow_df_nonvasc[,c(1:11)]
          
          openlow_env_nonvasc <- openlow_env_nonvasc %>%
            arrange(Plot, Sample_Year) %>%
            group_by(Plot) %>%
            mutate(Visit = paste0("visit_", row_number())) %>%
            ungroup()

openlow_vasc_abundance_balanced <- vasc_abundance_df %>%
  filter(Plot_Year %in% openlow_df$Plot_Year)
openlow_df <- openlow_vasc_abundance_balanced 

openlow_df <- openlow_df %>%
  filter(!Plot %in% openlow_plots_to_remove)

          openlow_composition <- openlow_df[,c(8:281)]
          openlow_composition <- as.matrix(openlow_composition) 
          
          openlow_env <- openlow_df[,c(1:7)]
          
          openlow_env <- openlow_env %>%
            arrange(Plot, Sample_Year) %>%
            group_by(Plot) %>%
            mutate(Visit = paste0("visit_", row_number())) %>%
            ungroup()

write_xlsx(openlow_df, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_df.xlsx")
write_xlsx(openlow_df_lichen, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_df_lichen.xlsx")
write_xlsx(openlow_df_nonvasc, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_df_nonvasc.xlsx")

write_xlsx(openlow_env, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_env.xlsx")
write_xlsx(openlow_env_lichen, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_env_lichen.xlsx")
write_xlsx(openlow_env_nonvasc, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_env_nonvasc.xlsx")
          
          
#needleleaf forest 

woodlands_plots_to_remove <- c("LACL_2008_02_014",
                                "LACL_2010_01_030",
                                "LACL_2010_01_105",
                                "LACL_2012_01_162",
                                "LACL_2015_01_053",
                                "LACL_2015_01_S970",
                                "LACL_2015_01_S972")

#spruce woodlands 
needle_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/needle_df.xlsx")
needle_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/needle_df_lichen.xlsx")
needle_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/needle_df_nonvasc.xlsx")

needle_lichen_abundance_balanced <- lichen_abundance_df %>%
  filter(Plot_Year %in% needle_df_lichen$Plot_Year)
needle_lichen_df <- needle_lichen_abundance_balanced 

needle_lichen_df <- needle_lichen_df %>%
  filter(!Plot %in% woodlands_plots_to_remove)

          needle_composition_lichen <- needle_df_lichen[,c(13:178)]
          needle_composition_lichen <- as.matrix(needle_composition_lichen) 
          
          #create env_file 
          needle_env_lichen <- needle_df_lichen[,c(1:12)]
          
          needle_env_lichen <- needle_env_lichen %>%
            arrange(Plot, Sample_Year) %>%
            group_by(Plot) %>%
            mutate(Visit = paste0("visit_", row_number())) %>%
            ungroup()
          
          needle_env_lichen <- needle_env_lichen %>%
            left_join(lichen_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
          


needle_nonvasc_abundance_balanced <- nonvasc_abundance_df %>%
  filter(Plot_Year %in% needle_df_nonvasc$Plot_Year)
needle_nonvasc_df <- needle_nonvasc_abundance_balanced

needle_nonvasc_df <- needle_nonvasc_df %>%
  filter(!Plot %in% woodlands_plots_to_remove)

          needle_composition_nonvasc <- needle_df_nonvasc[,c(13:218)]
          needle_composition_nonvasc <- as.matrix(needle_composition_nonvasc) 
          
          #create env_file 
          needle_env_nonvasc <- needle_df_nonvasc[,c(1:13)]
          
          needle_env_nonvasc <- needle_env_nonvasc %>%
            arrange(Plot, Sample_Year) %>%
            group_by(Plot) %>%
            mutate(Visit = paste0("visit_", row_number())) %>%
            ungroup()
          
          needle_env_nonvasc <- needle_env_nonvasc %>%
            left_join(nonvasc_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))

needle_vasc_abundance_balanced <- vasc_abundance_df %>%
  filter(Plot_Year %in% needle_df$Plot_Year)
needle_df <- needle_vasc_abundance_balanced 

needle_df <- needle_df %>%
  filter(!Plot %in% woodlands_plots_to_remove)

          needle_composition <- needle_df[,c(8:281)]
          needle_composition <- as.matrix(needle_composition) 
          
          needle_env <- needle_df[,c(1:7)]
          
          needle_env <- needle_env %>%
            arrange(Plot, Sample_Year) %>%
            group_by(Plot) %>%
            mutate(Visit = paste0("visit_", row_number())) %>%
            ungroup()
          
          needle_env <- needle_env %>%
            left_join(vasc_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
          
          
          
write_xlsx(needle_df, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/needle_df.xlsx")
write_xlsx(needle_env, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/needle_env.xlsx")




















#Fire Plots Analysis 
load("T:\\Users\\KPace\\Quadrat_Freq_Analyses\\Data\\plot_fire.rdata")
plot_fire <- as.data.frame(plot_fire)
viereck <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Viereck_env.xlsx")
lichen_abundance_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/quad_abundance_df_lichen.xlsx")
nonvasc_abundance_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/quad_abundance_df_nonvasc.xlsx")
vasc_abundance_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/quad_abundance_df_vascular.xlsx")


fire_vasc_abundance_df <- vasc_abundance_df %>%
  filter(Plot %in% plot_fire_true$Plot)
write_xlsx(fire_vasc_abundance_df, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/fire_vasc_abundance_df.xlsx")
fire_vasc_abundance_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/fire_vasc_abundance_df.xlsx")



fire_lichen_abundance_df <- lichen_abundance_df %>%
  filter(Plot %in% plot_fire_true$Plot)
write_xlsx(fire_lichen_abundance_df, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/fire_lichen_abundance_df.xlsx")
fire_lichen_abundance_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/fire_lichen_abundance_df.xlsx")


fire_nonvasc_abundance_df <- nonvasc_abundance_df %>%
  filter(Plot %in% plot_fire_true$Plot)
write_xlsx(fire_nonvasc_abundance_df, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/fire_nonvasc_abundance_df.xlsx")
fire_nonvasc_abundance_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/fire_nonvasc_abundance_df.xlsx")


#vascular 

#ask differences through time and among viereck classes 
fire_vasc_abundance_matrix <- fire_vasc_abundance_df[,c(8:281)]
fire_vasc_abundance_matrix <- as.matrix(fire_vasc_abundance_matrix) 

mds_fire_vasc_ab <- metaMDS(fire_vasc_abundance_matrix, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
#ordination information for caption (#plots, #iterations, stress value, if it converged)
length(unique(fire_vasc_abundance_df[["Plot"]]))
mds_fire_vasc_ab$stress
mds_fire_vasc_ab$iters
mds_fire_vasc_ab$converged

veg_colors <- c("darkorange", "blue4", "seagreen3", "firebrick3", "skyblue2", "orchid3")
names(veg_colors) <- unique(fire_vasc_abundance_df$Viereck.3)
dev.off()
xlim <- c(-1, 1.5)
ylim <- c(-1.5, 1)
plot(mds_fire_vasc_ab, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4, main = "Vascular Species", cex.main = 2)
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.08",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
text(x = par("usr")[2],
     y = par("usr")[4] - 0.1,
     labels = "1",
     pos = 2, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
points(scores(mds_fire_vasc_ab, display = "sites"),
       col = veg_colors[fire_vasc_abundance_df$Viereck.3],
       pch = 19)
legend("bottomright", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19, cex = 1.2)
ordiarrows(mds_fire_vasc_ab, groups = fire_vasc_abundance_df$Plot, levels = fire_vasc_abundance_df$Sample_Year, col = 'blue')
ordihull(mds_fire_vasc_ab, groups = fire_vasc_abundance_df$General_Location, draw ="polygon", label = TRUE)

text(mds_fire_vasc_ab, display = "sites", labels = fire_vasc_abundance_df$Plot, col = "black", cex = 0.7, pos = 4)

#axis1 scores 
axis1_scores <- scores(mds_fire_vasc_ab, display = "sites")[, 1]
species_cor <- apply(fire_vasc_abundance_matrix, 2, function(species) cor (species, axis1_scores, method = "spearman"))
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




plot_location_data

fire_vasc_abundance_df <- fire_vasc_abundance_df %>%
  left_join(plot_location_data %>% select(ID, General_Location), by = c("Plot_Year"))

fire_vasc_abundance_df <- left_join(fire_vasc_abundance_df, plot_location_data, by = c("Plot" = "ID"))







#lichens 
fire_lichen_abundance_matrix <- fire_lichen_abundance_df[,c(12:178)]
fire_lichen_abundance_matrix <- as.matrix(fire_lichen_abundance_matrix) 

mds_fire_lichen_ab <- metaMDS(fire_lichen_abundance_matrix, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
#ordination information for caption (#plots, #iterations, stress value, if it converged)
length(unique(fire_lichen_abundance_df[["Plot"]]))
mds_fire_lichen_ab$stress
mds_fire_lichen_ab$iters
mds_fire_lichen_ab$converged

veg_colors <- c("darkorange", "blue4", "seagreen3", "firebrick3")
names(veg_colors) <- unique(fire_lichen_abundance_df$Viereck.3)
dev.off()
xlim <- c(-1, 1.5)
ylim <- c(-2, 1)
plot(mds_fire_lichen_ab, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4, main = "Lichen Species", cex.main = 2)
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.05",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
#text(x = par("usr")[2],
#     y = par("usr")[4] - 0.1,
#     labels = "1",
#     pos = 2, #1: below, 2 left, 3 above, 4 right 
#     cex = 1.3, #size 
#    col = "black")
points(scores(mds_fire_lichen_ab, display = "sites"),
       col = veg_colors[fire_lichen_abundance_df$Viereck.3],
       pch = 19)
legend("bottomright", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19, cex = 1.2)
ordiarrows(mds_fire_lichen_ab, groups = fire_lichen_abundance_df$Plot, levels = fire_lichen_abundance_df$Sample_Year, col = 'blue')

fire_lichen_abundance_df <- left_join(fire_lichen_abundance_df, plot_location_data, by = c("Plot" = "ID"))
ordihull(mds_fire_lichen_ab, groups = fire_lichen_abundance_df$General_Location, draw ="polygon", label = TRUE)




#nonvasc
fire_nonvasc_abundance_matrix <- fire_nonvasc_abundance_df[,c(12:218)]
fire_nonvasc_abundance_matrix <- as.matrix(fire_nonvasc_abundance_matrix) 

mds_fire_nonvasc_ab <- metaMDS(fire_nonvasc_abundance_matrix, distance = "bray", k = 3, autotransform = TRUE, trymax = 200)
#ordination information for caption (#plots, #iterations, stress value, if it converged)
length(unique(fire_nonvasc_abundance_df[["Plot"]]))
mds_fire_nonvasc_ab$stress
mds_fire_nonvasc_ab$iters
mds_fire_nonvasc_ab$converged

veg_colors <- c("darkorange", "blue4", "seagreen3", "firebrick3", "skyblue2")
names(veg_colors) <- unique(fire_nonvasc_abundance_df$Viereck.3)
dev.off()
xlim <- c(-1, 1.5)
ylim <- c(-2, 1)
plot(mds_fire_nonvasc_ab, type = "n", xlim = xlim, ylim = ylim, cex.axis = 1.5, cex.lab = 1.4, main = "Nonvascular Species", cex.main = 2)
text(x = par("usr")[1],
     y = par("usr")[4] - 0.1,
     labels = "Stress = 0.09",
     pos = 4.1, #1: below, 2 left, 3 above, 4 right 
     cex = 1.3, #size 
     col = "black")
#text(x = par("usr")[2],
#     y = par("usr")[4] - 0.1,
#     labels = "1",
#     pos = 2, #1: below, 2 left, 3 above, 4 right 
#     cex = 1.3, #size 
#    col = "black")
points(scores(mds_fire_nonvasc_ab, display = "sites"),
       col = veg_colors[fire_nonvasc_abundance_df$Viereck.3],
       pch = 19)
legend("bottomright", title = "Site Classification",
       legend=names(veg_colors),
       ncol=1,
       col = veg_colors, pch = 19, cex = 1.2)
ordiarrows(mds_fire_nonvasc_ab, groups = fire_nonvasc_abundance_df$Plot, levels = fire_nonvasc_abundance_df$Sample_Year, col = 'blue')

fire_nonvasc_abundance_df <- left_join(fire_nonvasc_abundance_df, plot_location_data, by = c("Plot" = "ID"))
ordihull(mds_fire_nonvasc_ab, groups = fire_nonvasc_abundance_df$General_Location, draw ="polygon", label = TRUE)
