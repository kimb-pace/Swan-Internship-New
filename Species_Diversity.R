setwd("T:/Users/KPace/SWAN Internship")
fulldata <- read.csv("T:/Users/KPace/SWAN Internship/Quadrat_Frequency.csv")
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggpubr) 

#Dataframe Creation:
sp_richness <- fulldata %>%
  group_by(Park, Elevation_Band, Plot, Sample_Year) %>% 
  summarize(Species_Richness = n_distinct(Species_Code), .groups = "keep") #keep - same grouping structure as data
sp_richness$Elevation_Band <- factor(sp_richness$Elevation_Band, levels = c("1", "2", "3"))
se <- function(x) {sd(x) / sqrt(length(x))}

#Summary of species richness per park:
species_richness_park <- sp_richness %>%
  group_by(Park) %>%
  summarise(Average_Species_Richness = mean(Species_Richness),
            Species_Richness_SE = se(Species_Richness),
            .groups = 'drop')

#Summary of species richness per elevation band:
species_richness_elevation_park <- sp_richness %>%
  group_by(Park, Elevation_Band) %>%
  summarise(Average_Species_Richness = mean(Species_Richness),
            Species_Richness_SE = se(Species_Richness),
            .groups = 'drop')

#Figures
rich_bar <- ggplot(species_richness_elevation_park, aes(x = Elevation_Band, y = Average_Species_Richness, fill = Park)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Average_Species_Richness - Species_Richness_SE, 
                    ymax = Average_Species_Richness + Species_Richness_SE), 
                position = position_dodge(0.9), width = 0.2) + 
  labs(title = "Species Richness by Elevation Band within Parks", x = "Elevation Band", y = "Species Richness") +
  theme_minimal()

plot_scatter <- ggplot(sp_richness, aes(x = Sample_Year, y = Species_Richness, color = Park, shape = Elevation_Band)) +
  geom_point(size = 3) +
  labs(title = "Species Richness Over Time by Plot", x = "Sample_Year", y = "Species Richness") +
  theme_minimal()

scatterplot_low <- ggplot(sp_richness %>% filter(Elevation_Band == "1"), 
                          aes(x = Sample_Year, y = Species_Richness, color = Park)) + geom_point(size = 1) +
  labs(title = "Low Elevation", x = "Year", y = "Avg. Richness") +
  theme_minimal()
scatterplot_mid <- ggplot(sp_richness %>% filter(Elevation_Band == "2"), 
                          aes(x = Sample_Year, y = Species_Richness, color = Park)) + geom_point(size = 1) +
  labs(title = "Mid Elevation", x = "Year", y = "Avg. Richness") +
  theme_minimal()
scatterplot_high <- ggplot(sp_richness %>% filter(Elevation_Band == "3"), 
                           aes(x = Sample_Year, y = Species_Richness, color = Park)) +
  geom_point(size = 1) +
  labs(title = "High Elevation", x = "Year", y = "Avg. Richness") +
  theme_minimal()
scatter_by_elevation <- ggarrange(scatterplot_low, scatterplot_mid, scatterplot_high, ncol = 1, nrow = 3)

scatterplot_KATM <- ggplot(sp_richness %>% filter(Park == "KATM"), 
                           aes(x = Sample_Year, y = Species_Richness, color = Elevation_Band)) + geom_point(size = 1) +
  labs(title = "Katmai National Park", x = "Year", y = "Avg. Richness") + 
  theme_minimal()
scatterplot_LACL <- ggplot(sp_richness %>% filter(Park == "LACL"), 
                           aes(x = Sample_Year, y = Species_Richness, color = Elevation_Band)) + geom_point(size = 1) +
  labs(title = "Lake Clark National Park", x = "Year", y = "Avg. Richness") + 
  theme_minimal()
scatter_by_park <- ggarrange(scatterplot_KATM, scatterplot_LACL, ncol = 1, nrow = 2)

rich_bar
plot_scatter
scatter_by_elevation 
scatter_by_park

#Group by classification 
lichens_only <- fulldata %>% 
  filter(Vascular_Code %in% c("Lichen"))
vascular_only <- fulldata %>%
  filter(Vascular_Code %in% c("Vascular"))
nonvasc_only <- fulldata %>%
  filter(Vascular_Code %in% c("Nonvascular"))

#calculate species richness by classification 
lichen_sp_richness <- lichens_only %>%
  group_by(Park, Elevation_Band, Plot, Sample_Year) %>% 
  summarize(Species_Richness = n_distinct(Species_Code), .groups = "keep") #keep - same grouping structure as data
lichen_sp_richness$Elevation_Band <- factor(lichen_sp_richness$Elevation_Band, levels = c("1", "2", "3"))
lichen_sp_richness

vasc_sp_richness <- vascular_only %>%
  group_by(Park, Elevation_Band, Plot, Sample_Year) %>% 
  summarize(Species_Richness = n_distinct(Species_Code), .groups = "keep") #keep - same grouping structure as data
vasc_sp_richness$Elevation_Band <- factor(vasc_sp_richness$Elevation_Band, levels = c("1", "2", "3"))
vasc_sp_richness

nonvasc_sp_richness <- nonvasc_only %>%
  group_by(Park, Elevation_Band, Plot, Sample_Year) %>% 
  summarize(Species_Richness = n_distinct(Species_Code), .groups = "keep") #keep - same grouping structure as data
nonvasc_sp_richness$Elevation_Band <- factor(nonvasc_sp_richness$Elevation_Band, levels = c("1", "2", "3"))
nonvasc_sp_richness


lichen_line <- ggplot(lichen_sp_richness, aes(x = Sample_Year, y = Species_Richness, group = Plot, color = Plot)) + 
  geom_line()+ 
  geom_point()+
  facet_grid(Park ~ Elevation_Band) + 
  labs(title = "Elevation Band", x = "Sample Year", y = "Lichen Species Richness")+ 
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
lichen_line

vasc_line <- ggplot(vasc_sp_richness, aes(x = Sample_Year, y = Species_Richness, group = Plot, color = Plot)) + 
  geom_line()+ 
  geom_point()+
  facet_grid(Park ~ Elevation_Band) + 
  labs(title = "Elevation Band", x = "Sample Year", y = "Vascular Species Richness")+ 
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
vasc_line

nonvasc_line <- ggplot(nonvasc_sp_richness, aes(x = Sample_Year, y = Species_Richness, group = Plot, color = Plot)) + 
  geom_line()+ 
  geom_point()+
  facet_grid(Park ~ Elevation_Band) + 
  labs(title = "Elevation Band", x = "Sample Year", y = "Non-Vascular Species Richness")+ 
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
nonvasc_line

#create new summary tables of species richness by elevation band and such using subset by classification 

lichen_richness_elevation_park <- lichen_sp_richness %>%
  group_by(Park, Elevation_Band) %>%
  summarise(Average_Species_Richness = mean(Species_Richness),
            Species_Richness_SE = se(Species_Richness),
            .groups = 'drop')
vasc_richness_elevation_park <- vasc_sp_richness %>%
  group_by(Park, Elevation_Band) %>%
  summarise(Average_Species_Richness = mean(Species_Richness),
            Species_Richness_SE = se(Species_Richness),
            .groups = 'drop')
nonvasc_richness_elevation_park <- nonvasc_sp_richness %>%
  group_by(Park, Elevation_Band) %>%
  summarise(Average_Species_Richness = mean(Species_Richness),
            Species_Richness_SE = se(Species_Richness),
            .groups = 'drop')
lichen_richness_elevation_park
vasc_richness_elevation_park
nonvasc_richness_elevation_park

#Bar graphs
lichen_bar <- ggplot(lichen_richness_elevation_park, aes(x = Elevation_Band, y = Average_Species_Richness, fill = Park)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Average_Species_Richness - Species_Richness_SE, 
                    ymax = Average_Species_Richness + Species_Richness_SE), 
                position = position_dodge(0.9), width = 0.2) + 
  labs(title = "Lichen", x = "Elevation Band", y = "Avg. Species Richness") +
  theme_minimal()
vasc_bar <- ggplot(vasc_richness_elevation_park, aes(x = Elevation_Band, y = Average_Species_Richness, fill = Park)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Average_Species_Richness - Species_Richness_SE, 
                    ymax = Average_Species_Richness + Species_Richness_SE), 
                position = position_dodge(0.9), width = 0.2) + 
  labs(title = "Vascular", x = "Elevation Band", y = "Avg. Species Richness") +
  theme_minimal()
nonvasc_bar <- ggplot(nonvasc_richness_elevation_park, aes(x = Elevation_Band, y = Average_Species_Richness, fill = Park)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Average_Species_Richness - Species_Richness_SE, 
                    ymax = Average_Species_Richness + Species_Richness_SE), 
                position = position_dodge(0.9), width = 0.2) + 
  labs(title = "Nonvascular", x = "Elevation Band", y = "Avg. Species Richness") +
  theme_minimal()

lichen_bar
vasc_bar
nonvasc_bar
#need to make them have the same axis, only one title on Y axis, and legend on the right. 

classification_bar <- ggarrange(lichen_bar, vasc_bar, nonvasc_bar, ncol = 1, nrow = 3, 
                                common.legend = TRUE)
classification_bar



lichen_bar2 <- ggplot(lichen_richness_elevation_park, aes(x = Park, y = Average_Species_Richness, fill = Elevation_Band)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Average_Species_Richness - Species_Richness_SE, 
                    ymax = Average_Species_Richness + Species_Richness_SE), 
                position = position_dodge(0.9), width = 0.2) + 
  labs(title = "Lichen", x = "Park", y = "Avg. Species Richness") +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 50), breaks = c(0, 10, 20, 30, 40, 50))+
  theme(axis.title.x=element_blank())



vasc_bar2 <- ggplot(vasc_richness_elevation_park, aes(x = Park, y = Average_Species_Richness, fill = Elevation_Band)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Average_Species_Richness - Species_Richness_SE, 
                    ymax = Average_Species_Richness + Species_Richness_SE), 
                position = position_dodge(0.9), width = 0.2) + 
  labs(title = "Vascular", x = "Park", y = "Avg. Species Richness") +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 50), breaks = c(0, 10, 20, 30, 40, 50))+
  theme(axis.title.x=element_blank())

nonvasc_bar2 <- ggplot(nonvasc_richness_elevation_park, aes(x = Park, y = Average_Species_Richness, fill = Elevation_Band)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Average_Species_Richness - Species_Richness_SE, 
                    ymax = Average_Species_Richness + Species_Richness_SE), 
                position = position_dodge(0.9), width = 0.2) + 
  labs(title = "Nonvascular", x = "Park", y= "Avg. Species Richness") +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 50), breaks = c(0, 10, 20, 30, 40, 50))+
  theme(axis.title.x=element_blank())


classification_bar2 <- ggarrange(lichen_bar2, vasc_bar2, nonvasc_bar2, ncol = 1, nrow = 3, 
                                 common.legend = TRUE)
classification_bar2

lichen_bar2
vasc_bar2
nonvasc_bar2


#_______________________________________________________________________________________________________________________


#Calculation of indexes 

#need a dataframe that has plot, species richness, and quad_frequency, also plot_year 
fulldata <- read.csv("T:/Users/KPace/SWAN Internship/Quadrat_Frequency.csv")
vascular_only <- fulldata %>% filter(Vascular_Code %in% c("Vascular"))

vascular_only <- vascular_only %>%
  mutate(Plot_Year = paste(Plot, Sample_Year, sep = "_"))
vascular_only <- vascular_only %>%
  mutate(Quad_Num = paste(Transect, Quadrat, sep = "_"))

#Calculate Species_Richness
species_richness_df <- vascular_only %>%
  group_by(Plot_Year) %>%
  summarize(Species_Richness = n_distinct(Species_Code))

#Calculate Quad_Freq
quad_freq_df <- vascular_only %>%
  group_by(Plot_Year, Species_Code) %>%
  summarize(Quad_Freq = n_distinct(Quad_Num))
result_df <- vascular_only %>%
  left_join(species_richness_df, by = "Plot_Year") %>%
  left_join(quad_freq_df, by = c("Plot_Year", "Species_Code"))
print(result_df)
result_df <- result_df[, c(14,18)]

#use a for statement like for every Species_Code, calculate ____ and then sum it together 
#try taking a subset of only one single plot year and making a for statement to calculate the shannon and simpson index for JUST THAT PLOT
#and then make a loop that says repeat this function for every unique plot and put the results into a new dataframe 
single_plot <- result_df %>% filter(Plot_Year %in% c("KATM_2009_01_030_2009"))
single_plot
single_plot <- single_plot[, c(1)]







library(ggplot2)
library(tidyr)
library(dplyr)
fulldata <- read.csv("T:/Users/KPace/SWAN-Internship/Quadrat_Frequency.csv")
vascular_only <- fulldata %>% filter(Vascular_Code %in% c("Vascular"))

vascular_only <- vascular_only %>%
  mutate(Plot_Year = paste(Plot, Sample_Year, sep = "_"))
vascular_only <- vascular_only %>%
  mutate(Quad_Num = paste(Transect, Quadrat, sep = "_"))

species_freq <- vascular_only %>%
  group_by(Plot_Year, Species_Code) %>%
  summarize(Frequency = n(), .groups = "drop")
species_relative_abundance <- species_freq %>%
  group_by(Plot_Year) %>%
  mutate(Rel_Abundance = Frequency / sum(Frequency))
calculate_shannon <- function(rel_abundance) {
  -sum(rel_abundance * log(rel_abundance))
}
shannon_df <- species_relative_abundance %>%
  group_by(Plot_Year) %>%
  summarize(Shannon_Index = calculate_shannon(Rel_Abundance))

print(shannon_df)

#plot this as a line graph 
#extract years from Plot_Year 
shannon_df$Survey_Year <- sub(".*_(\\d+)$", "\\1", shannon_df$Plot_Year)
shannon_df <- shannon_df %>%
  separate(Plot_Year, into = c("Park", "EstYear", "Elevation_Band", "PlotID", "Sample_Year"), sep = "_", remove = FALSE)
KATM_Shannon <- shannon_df %>% filter(Park %in% c("KATM"))
LACL_Shannon <- shannon_df %>% filter(Park %in% c("LACL"))
LACL_Shannon_03 <- LACL_Shannon %>% filter(Elevation_Band %in% c("03S"))
LACL_Shannon_03


shannon_line <- ggplot(LACL_Shannon, aes(x = Sample_Year, y = Shannon_Index, group = PlotID, color = Elevation_Band)) + 
  geom_line()+ 
  geom_point()+
  facet_grid(Park ~ Elevation_Band) + 
  labs(title = "Elevation Band", x = "Sample Year", y = "Vascular Species Shannon Index")+ 
  theme_minimal()+
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))
shannon_line

calculate_simpson <- function(rel_abundance) {
  sum(rel_abundance^2)
}
simpson_df <- species_relative_abundance %>%
  group_by(Plot_Year) %>%
  summarize(Simpson_Index = calculate_simpson(Rel_Abundance)) %>%
  mutate(Simpson_Diversity = 1 - Simpson_Index)
print(simpson_df)








library(writexl)
library(readxl)
library(dplyr)


write_xlsx(Viereck_env, "T:/Users/KPace/SWAN-Internship/Viereck_env.xlsx")
transition_plots <- read_xlsx("T:/Users/KPace/SWAN-Internship/Transition_Plots.xlsx")

summary_table_transition <- Viereck_env %>%
  group_by(Plot) %>%
  summarise(
    Initial_Class = Viereck.3[which.min(Sample_Year)],
    New_Class = Viereck.3[which.max(Sample_Year)]
  )

transition_plots <- transition_plots %>%
  left_join(summary_table_transition, by = "Plot")

write_xlsx(transition_plots, "T:/Users/KPace/SWAN-Internship/transition_plots_viereck.xlsx")


twoband_plot_viereck








fulldata <- read.csv("T:/Users/KPace/SWAN Internship/Quadrat_Frequency.csv")
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggpubr)

# Basic Dataframe Summaries
n_plots <- fulldata %>% 
  distinct(Plot) %>%
  nrow()

n_species <- fulldata %>%
  distinct(Species_Code) %>%
  nrow()

n_nest <- fulldata %>%
  distinct(Frame_Size_sqm) %>%
  nrow()
n_nest

n_nest1 <- fulldata %>%
  distinct(Frame_Text) %>%
  count(Frame_Text)
n_nest1

elev_plot <- fulldata %>%
  distinct(Park, Elevation_Band, Plot) %>%
  count(Park, Elevation_Band)
elev_plot

#73 plots at Katmai 
#110 plots at lake clark 

#Next I created a basic graph reflecting this data:
elev_graph <- ggplot(elev_plot, aes(x = Elevation_Band, y=n, fill = Park)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Number of Plots Per Elevation Band", x = "Elevation Band", y = "Number of Plots") +
  theme_minimal()
elev_graph

visits_per_plot <- fulldata %>%
  distinct(Plot, Sample_Year) %>%
  count(Plot, name = "Times_Visited")

#multiple visits per year 
multiple_visits <- fulldata %>%
  group_by(Plot, Sample_Year) %>%
  filter(n_distinct(Sample_Date) > 1) %>%
  summarize(Sample_Dates = toString(unique(Sample_Date)))
multiple_visits

#seeing which plots have  quadrats and which have four
quadrat_added <- fulldata %>%
  group_by(Plot) %>%
  summarize(Frame_Codes = list(sort(unique(Frame_Code)))) %>%
  mutate(Category = case_when(
    all(c(1, 2, 3) %in% Frame_Codes) & !(0 %in% Frame_Codes) ~ "Contains 1, 2, 3", 
    all(c(0, 1, 2, 3) %in% Frame_Codes) ~ "Contains 0, 1, 2, 3", 
    TRUE ~ "Other"
  )) %>%
  arrange(Frame_Codes)
quadrat_added

#Transects per Plot
transects_per_plot <- fulldata %>% 
  distinct(Plot, Transect) %>%
  count(Plot, name = "Transects per Plot")

#Quadrats per plot:

fulldata <- fulldata %>% 
  mutate(Quad_Num = paste(Transect, Quadrat, sep = "_"))
quadrats_per_plot <- fulldata %>% 
  distinct(Plot, Quad_Num) %>%
  count(Plot, name = "Quadrats_per_Plot")

#Next, I sorted this and created a histogram to better visualize it.
quad_sorted <- quadrats_per_plot %>% 
  arrange(`Quadrats_per_Plot`)
quad_hist <- ggplot(quadrats_per_plot, aes(x = Quadrats_per_Plot)) + 
  geom_bar() + 
  labs(title = "Histogram of Quadrats per Plot", x = "Number of Quadrats", y = "Frequency") +
  theme_minimal()
quad_hist
high_quad <- quadrats_per_plot %>%
  filter(Quadrats_per_Plot > 15)
high_quad


#How many plots were visited each year:
visits_per_year <- fulldata %>%
  distinct(Sample_Year, Plot) %>%
  count(Sample_Year, name = "Plots_Visited_PerYear")

# Most Common Species
top_species <- fulldata %>%
  group_by(Park, Elevation_Band, Vascular_Code, Species_Code) %>%
  summarise(Species_Count = n(), .groups = 'drop') %>%
  arrange(Park, Elevation_Band, Vascular_Code, desc(Species_Count)) %>%
  group_by(Park, Elevation_Band, Vascular_Code) %>%
  slice_max(Species_Count, n=3)
top_species <- top_species[, c(1:4)]

species_code_info <- fulldata %>%
  select(Species_Code, Species_With_Authority) %>%
  distinct()
top_species$Species_Code <- as.character(top_species$Species_Code)
species_code_info$Species_Code <- as.character(species_code_info$Species_Code)
top_species_with_names <- top_species %>%
  left_join(species_code_info, by = "Species_Code")
top_species_with_names <- top_species_with_names[, c(1:5)]

#What the top species were in each elevation band regardless of classification.

top_species2 <- fulldata %>%
  group_by(Park, Elevation_Band, Species_Code) %>%
  summarise(Species_Count = n(), .groups = 'drop') %>%
  arrange(Park, Elevation_Band, desc(Species_Count)) %>%
  group_by(Park, Elevation_Band) %>%
  slice_max(Species_Count, n=3)
top_species2 <- top_species2[, c(1:3)]

top_species2$Species_Code <- as.character(top_species2$Species_Code)
species_code_info$Species_Code <- as.character(species_code_info$Species_Code)
species_code_info <- fulldata %>%
  select(Species_Code, Vascular_Code, Species_With_Authority) %>%
  distinct()
top_species_with_names2 <- top_species2 %>%
  left_join(species_code_info, by = c("Species_Code"))
top_species_with_names2 <- top_species_with_names2[, c(1:5)]


load("T:\\Users\\KPace\\Quadrat_Freq_Analyses\\Data\\plot_metadata.rdata")
file.choose() #to get pathway
library(mapview)
library(sf)
library(dplyr)

out$plot_loc_summary 
out$plot_sample

plot_elevation_data <- out$plot_loc_summary[c(1,2,3)]
plot_elevation_data

#dataframe creation 
veg_class_df <- out$plot_sample[c(1,3,4)]
veg_class_df <- veg_class_df %>%
  separate(Plot, into = c("Park", "EstYear", "Elevation_Band", "PlotID"), sep = "_", remove = FALSE)
veg_class_df <- veg_class_df[, c(1,2,4,6,7)]
veg_class_df
#make a plot year column so you can join it 
veg_class_df <- veg_class_df %>% 
  mutate(Plot_Year = paste(Plot, Sample_Year, sep="_"))

#exploration: how many veg classes and what are they? 
#which ones are in which elevation class? 
#are there any that are only in one park? 

Classes <- veg_class_df %>%
  distinct(Vegetation_Class) %>%
  count(Vegetation_Class)
Classes

#maps
plot(out$plot_loc_summary)
mapview(out$plot_loc_summary)

elev_class <- veg_class_df %>%
  distinct(Park, Elevation_Band, Vegetation_Class) %>%
  count(Park, Elevation_Band)

elev_class_named <- veg_class_df %>%
  distinct(Park, Elevation_Band, Vegetation_Class) %>%
  group_by(Park, Elevation_Band) %>%
  summarize(
    n = n(),
    Vegetation_Classes = paste(unique(Vegetation_Class), collapse = ", "))
elev_class_named




#making presence absence dataframes 
library(vegan)
library(ggplot2)
library(dplyr)
library(permute)
library(lattice)
library(tidyverse)
library(readxl)

fulldata <- read.csv("T:/Users/KPace/SWAN-Internship/Quadrat_Frequency.csv")
taxa <- read.csv("T:/Users/KPace/SWAN-Internship/taxa.csv")
load("T:\\Users\\KPace\\Quadrat_Freq_Analyses\\Data\\plot_metadata.rdata")

fulldata <- fulldata %>% mutate(Quad_Num = paste(Transect, Quadrat, sep = "_"))
fulldata <- fulldata %>% mutate(Plot_Year = paste(Plot, Sample_Year, sep="_")) %>%
  group_by(Plot) %>%
  mutate(Quad_Total = n_distinct(Quad_Num))

veg_class_df <- out$plot_sample[c(1,3,4)]
veg_class_df <- veg_class_df %>%
  separate(Plot, into = c("Park", "EstYear", "Elevation_Band", "PlotID"), sep = "_", remove = FALSE)
veg_class_df <- veg_class_df[, c(1,2,4,6,7)]
veg_class_df <- veg_class_df %>% mutate(Plot_Year = paste(Plot, Sample_Year, sep="_"))

#Group by classification 
lichens_only <- fulldata %>% filter(Vascular_Code %in% c("Lichen"))
vascular_only <- fulldata %>% filter(Vascular_Code %in% c("Vascular"))
nonvasc_only <- fulldata %>% filter(Vascular_Code %in% c("Nonvascular"))

#create quadrat frequency dataframe 
vasc_df <- vascular_only %>% 
  group_by(Plot_Year, Species_Code) %>% 
  summarise(Species_Quad_Count =n_distinct(Quad_Num)) %>% 
  ungroup() 
quad_abundance_df <- vasc_df %>%
  pivot_wider(names_from = Species_Code, values_from = Species_Quad_Count, values_fill = 0)
quad_freq_df <- quad_abundance_df %>%
  mutate(across(where(is.numeric), ~ ifelse(. >0, . /15, .)))
#long_form <- quad_freq_df %>%
#pivot_longer(cols = -c(Plot_Year), 
#names_to = "Species_Code",
#values_to = "Quad_Frequency")

#create presence absence DF
presence_absence_df <- quad_abundance_df %>%
  mutate(across(-Plot_Year, ~ifelse(. > 1, 1, .)))

presence_absence_df <- presence_absence_df %>%
  separate(Plot_Year, into = c("Park", "EstYear", "Elevation_Band", "PlotID", "Sample_Year"), sep = "_", remove = FALSE)

#add veg class
presence_absence_df <- presence_absence_df %>%
  left_join(veg_class_df %>% select(Plot_Year, Vegetation_Class),
            by = "Plot_Year")
presence_absence_df$Elevation_Band <- gsub("03[SN]", "03", presence_absence_df$Elevation_Band)
#maybe just use the longform data as the input for taxon ellipses dont add to overlay df? 


#create quadrat frequency dataframe for lichens 
lichens_df <- lichens_only %>% 
  group_by(Plot_Year, Species_Code) %>% 
  summarise(Species_Quad_Count =n_distinct(Quad_Num)) %>% 
  ungroup() 
lichensquad_abundance_df <- lichens_df %>%
  pivot_wider(names_from = Species_Code, values_from = Species_Quad_Count, values_fill = 0)
lichensquad_freq_df <- quad_abundance_df %>%
  mutate(across(where(is.numeric), ~ ifelse(. >0, . /15, .)))
#long_form <- quad_freq_df %>%
#pivot_longer(cols = -c(Plot_Year), 
#names_to = "Species_Code",
#values_to = "Quad_Frequency")

#create presence absence DF
lichenspresence_absence_df <- lichensquad_abundance_df %>%
  mutate(across(-Plot_Year, ~ifelse(. > 1, 1, .)))

lichenspresence_absence_df <- lichenspresence_absence_df %>%
  separate(Plot_Year, into = c("Park", "EstYear", "Elevation_Band", "PlotID", "Sample_Year"), sep = "_", remove = FALSE)

#add veg class
lichenspresence_absence_df <- lichenspresence_absence_df %>%
  left_join(Viereck_env %>% select(Plot_Year, Vegetation_Class, Viereck.1, Viereck.2, Viereck.3, Viereck.4),
            by = "Plot_Year")
lichenspresence_absence_df$Elevation_Band <- gsub("03[SN]", "03", lichenspresence_absence_df$Elevation_Band)
lichenspresence_absence_df <- lichenspresence_absence_df %>%
  left_join(Viereck_env %>% select(Plot_Year, Plot),
            by = "Plot_Year")


write_xlsx(lichenspresence_absence_df, "T:/Users/KPace/SWAN-Internship/lichenspresence_absence_df.xlsx")


#create quadrat frequency dataframe for nonvasc 
nonvasc_df <- nonvasc_only %>% 
  group_by(Plot_Year, Species_Code) %>% 
  summarise(Species_Quad_Count =n_distinct(Quad_Num)) %>% 
  ungroup() 
nonvascquad_abundance_df <- nonvasc_df %>%
  pivot_wider(names_from = Species_Code, values_from = Species_Quad_Count, values_fill = 0)
nonvascquad_freq_df <- nonvascquad_abundance_df %>%
  mutate(across(where(is.numeric), ~ ifelse(. >0, . /15, .)))
#long_form <- quad_freq_df %>%
#pivot_longer(cols = -c(Plot_Year), 
#names_to = "Species_Code",
#values_to = "Quad_Frequency")

#create presence absence DF
nonvascpresence_absence_df <- nonvascquad_abundance_df %>%
  mutate(across(-Plot_Year, ~ifelse(. > 1, 1, .)))

nonvascpresence_absence_df <- nonvascpresence_absence_df %>%
  separate(Plot_Year, into = c("Park", "EstYear", "Elevation_Band", "PlotID", "Sample_Year"), sep = "_", remove = FALSE)

#add veg class
nonvascpresence_absence_df <- nonvascpresence_absence_df %>%
  left_join(Viereck_env %>% select(Plot_Year, Vegetation_Class, Viereck.1, Viereck.2, Viereck.3, Viereck.4),
            by = "Plot_Year")
nonvascpresence_absence_df$Elevation_Band <- gsub("03[SN]", "03", nonvascpresence_absence_df$Elevation_Band)

nonvascpresence_absence_df <- nonvascpresence_absence_df %>%
  left_join(Viereck_env %>% select(Plot_Year, Plot),
            by = "Plot_Year")

write_xlsx(nonvascpresence_absence_df, "T:/Users/KPace/SWAN-Internship/nonvascpresence_absence_df.xlsx")



 