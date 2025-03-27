taxa <- read.csv("T:/Users/KPace/SWAN-Internship-New/Data/Unmodified/taxa.csv")
quad_freq <- read.csv("T:/Users/KPace/SWAN-Internship-New/Data/Unmodified/Quadrat_Frequency.csv")

taxa_filtered <- subset(taxa, select = c(Species_Code, Is_Name_Current, Vascular_Code, Genus, Species, Subspecies, Variety))
library(dplyr)
duplicates <- taxa_filtered %>%
  filter(Vascular_Code != "" & !is.na(Vascular_Code)) %>%
  group_by(Genus, Species) %>%
  filter(n() > 1) %>%
  ungroup()
duplicates

duplicates_true <- duplicates %>% filter(Is_Name_Current %in% c("TRUE"))
duplicates_false <- duplicates %>% filter(Is_Name_Current %in% c("FALSE"))
write_xlsx(duplicates_true, "T:/Users/KPace/duplicates_true.xlsx")


library(writexl)
write_xlsx(duplicates, "T:/Users/KPace/duplicates.xlsx")

genus_only <- taxa_filtered %>%
  filter(Genus != "" & !is.na(Genus) & Species == "" | is.na(Species)) %>%
  select(Species_Code, Vascular_Code, Genus, Species)
genus_only
write_xlsx(genus_only, "T:/Users/KPace/genus_only.xlsx")



#order by subspecies 
group_by(Genus, Species)
summarise(Code= First(Code))
might have to create a genus_speies merge like plot year 

update taxa table, join with old taxa table and make a column 
join to quad freq by original species code. add to new table the new species code as a column which use to make wide format 


simple_taxa <- taxa %>%
  group_by(Genus, Species) %>%
  summarise(Code1 = first(Species_Code),
            Genus = first(Genus),
            Species = first(Species))

#make a genus_species column on both taxa and simple_taxa 
taxa <- taxa %>% mutate(Genus_Species = paste(Genus, Species, sep="_"))
simple_taxa <- simple_taxa %>% mutate(Genus_Species = paste(Genus, Species, sep="_"))

#add the Code1 column to taxa 
taxa <- taxa %>%
  left_join(simple_taxa %>% select(Genus_Species, Code1),
            by = "Genus_Species")

#join code1 to quad_freq using old code 
quad_freq <- quad_freq %>%
  left_join(taxa %>% select(Species_Code, Code1),
            by = "Species_Code")

quad_freq <- quad_freq %>% mutate(Quad_Num = paste(Transect, Quadrat, sep = "_"))
quad_freq <- quad_freq %>% mutate(Plot_Year = paste(Plot, Sample_Year, sep="_")) %>%
  group_by(Plot) %>%
  mutate(Quad_Total = n_distinct(Quad_Num))

#Group by classification 
vascular_only <- quad_freq %>% filter(Vascular_Code %in% c("Vascular"))

#create quadrat frequency dataframe 
vasc_df <- vascular_only %>% 
  group_by(Plot_Year, Code1) %>% 
  summarise(Species_Quad_Count =n_distinct(Quad_Num)) %>% 
  ungroup() 
quad_abundance_df <- vasc_df %>%
  pivot_wider(names_from = Code1, values_from = Species_Quad_Count, values_fill = 0)

#check vasc df for duplicate entries in quadrats 
duplicate_quads <- vascular_only %>%
  group_by(Plot_Year, Quad_Num, Code1) %>%
  filter(n() > 1) 



#taxa <- taxa[,c(1:48,50)]


lichens_only <- quad_freq %>% filter(Vascular_Code %in% c("Lichen"))
nonvasc_only <- quad_freq %>% filter(Vascular_Code %in% c("Nonvascular"))

%>%
  summarise(Plot_Years = paste(unique(Plot_Year), collapse = ", "), .groups = "drop") %>%
  ungroup()
print(duplicate_quads)
duplicate_quads <- duplicate_quads %>% distinct()
write_xlsx(duplicate_quads, "T:/Users/KPace/duplicate_quads.xlsx")
length(unique(duplicate_quads[["Plot_Year"]]))


library(tidyr)
katm_2009_02_032 <- vascular_only %>% filter(Plot %in% c("KATM_2009_02_032"))
duplicate_quads2 <- katm_2009_02_032 %>%
  group_by(Quad_Num, Code1) %>%
  filter(n() > 1) 

lacl_2007_02_002 <- vascular_only %>% filter(Plot %in% c("LACL_2007_02_002"))
duplicate_quads2 <- lacl_2007_02_002 %>%
  group_by(Quad_Num, Code1) %>%
  filter(n() > 1) 






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
