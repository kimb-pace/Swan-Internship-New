library(vegan)
library(permute)
library(lattice)
library(dplyr)
library(readxl)
library(readr)
library(writexl)
library(here)


taxa <- read.csv("T:/Users/KPace/SWAN-Internship-New/Data/Unmodified/taxa.csv")
quad_freq <- read.csv("T:/Users/KPace/SWAN-Internship-New/Data/Unmodified/Quadrat_Frequency.csv")

taxa_filtered <- subset(taxa, select = c(Species_Code, Is_Name_Current, Vascular_Code, Genus, Species, Subspecies, Variety))










simple_taxa <- taxa %>%
  group_by(Genus, Species) %>%
  summarise(Code1 = first(Species_Code),
            Genus = first(Genus),
            Species = first(Species)) %>%
  ungroup()




#join code1 to quad_freq using old code 
quad_freq <- quad_freq %>%
  left_join(taxa %>% select(Species_Code, Code1),
            by = "Species_Code")

quad_freq <- quad_freq %>% mutate(Quad_Num = paste(Transect, Quadrat, sep = "_"))
quad_freq <- quad_freq %>% mutate(Plot_Year = paste(Plot, Sample_Year, sep="_")) %>%
  group_by(Plot) %>%
  mutate(Quad_Total = n_distinct(Quad_Num))

#Group by classification 
vascular_only <- quad_freq_filtered %>% filter(Vascular_Code %in% c("Vascular"))

#create quadrat frequency dataframe 
vasc_df <- vascular_only %>% 
  group_by(Plot_Year, Code1) %>% 
  summarise(Species_Quad_Count =n_distinct(Quad_Num)) %>% 
  ungroup() 
quad_abundance_df <- vasc_df %>%
  pivot_wider(names_from = Code1, values_from = Species_Quad_Count, values_fill = 0)

#check vasc df for duplicate entries in quadrats 
duplicate_quads <- quad_freq_filtered %>%
  group_by(Plot_Year, Quad_Num, Code1) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  select(Vascular_Code, Plot, Sample_Year, Sample_Date, Quad_Num, Code1, Species_Code)


#taxa <- taxa[,c(1:48,50)]


lichens_only <- quad_freq %>% filter(Vascular_Code %in% c("Lichen"))
nonvasc_only <- quad_freq %>% filter(Vascular_Code %in% c("Nonvascular"))


print(duplicate_quads)
duplicate_quads <- duplicate_quads %>% distinct()
write_xlsx(duplicate_quads, "T:/Users/KPace/duplicate_quads.xlsx")
length(unique(duplicate_quads[["Plot_Year"]]))
library(writexl)

library(tidyr)
katm_2009_02_032 <- vascular_only %>% filter(Plot %in% c("KATM_2009_02_032"))
duplicate_quads2 <- katm_2009_02_032 %>%
  group_by(Quad_Num, Code1) %>%
  filter(n() > 1) 

lacl_2007_02_002 <- vascular_only %>% filter(Plot %in% c("LACL_2007_02_002"))
duplicate_quads2 <- lacl_2007_02_002 %>%
  group_by(Quad_Num, Code1) %>%
  filter(n() > 1) 



#see what plots have multiple visits within the same year 
multiple_visits <- quad_freq %>%
  group_by(Vascular_Code, Plot, Sample_Year) %>%
  filter(n_distinct(Sample_Date) > 1)  %>%
  ungroup() %>%
  select(Vascular_Code, Plot, Sample_Year, Sample_Date)
multiple_visits <- multiple_visits %>% distinct()

#remove duplicate sampling events 
quad_freq_filtered <- quad_freq %>%
  filter(!(Plot == "LACL_2007_02_002" & Sample_Date == "2022-07-17") &
           !(Plot == "LACL_2007_02_006" & Sample_Date == "2007-08-13"))

#make vasc_only filtered and lichen and nonvasc and export them to modified folder 

vascular_only_filtered <- quad_freq_filtered %>% filter(Vascular_Code %in% c("Vascular"))
lichen_only_filtered <- quad_freq_filtered %>% filter(Vascular_Code %in% c("Lichen"))
nonvasc_only_filtered <- quad_freq_filtered %>% filter(Vascular_Code %in% c("Nonvascular"))

#remake abundance dfs 
  #vascular 
      vasc_df_filtered <- vascular_only_filtered %>% 
        group_by(Plot_Year, Code1) %>% 
        summarise(Species_Quad_Count =n_distinct(Quad_Num)) %>% 
        ungroup() 
      quad_abundance_df_vascular_filtered <- vasc_df_filtered %>%
        pivot_wider(names_from = Code1, values_from = Species_Quad_Count, values_fill = 0)

  #lichen
      lichen_df_filtered <- lichen_only_filtered %>% 
        group_by(Plot_Year, Code1) %>% 
        summarise(Species_Quad_Count =n_distinct(Quad_Num)) %>% 
        ungroup() 
      quad_abundance_df_lichen_filtered <- lichen_df_filtered %>%
        pivot_wider(names_from = Code1, values_from = Species_Quad_Count, values_fill = 0)
      
  #nonvascular
      nonvasc_df_filtered <- nonvasc_only_filtered %>% 
        group_by(Plot_Year, Code1) %>% 
        summarise(Species_Quad_Count =n_distinct(Quad_Num)) %>% 
        ungroup() 
      quad_abundance_df_nonvasc_filtered <- nonvasc_df_filtered %>%
        pivot_wider(names_from = Code1, values_from = Species_Quad_Count, values_fill = 0)


write_xlsx(quad_abundance_df_vascular_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/quad_abundance_df_vascular_filtered.xlsx")
write_xlsx(quad_abundance_df_lichen_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/quad_abundance_df_lichen_filtered.xlsx")
write_xlsx(quad_abundance_df_nonvasc_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/quad_abundance_df_nonvasc_filtered.xlsx")


write_xlsx(taxa_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/taxa_filtered.xlsx")
write_xlsx(quad_freq_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/quad_freq_filtered.xlsx")
write_xlsx(vascular_only_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/vascular_only_filtered.xlsx")
write_xlsx(lichen_only_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/lichen_only_filtered.xlsx")
write_xlsx(nonvasc_only_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/nonvasc_only_filtered.xlsx")


#remake species richness dataframes 
#calculate species richness by classification 
lichen_sp_richness <- lichen_only_filtered %>%
  group_by(Plot, Sample_Year) %>% 
  summarize(Species_Richness = n_distinct(Code1), .groups = "keep") 
lichen_sp_richness <- lichen_sp_richness %>% mutate(Plot_Year = paste(Plot, Sample_Year, sep="_"))
lichen_sp_richness

vasc_sp_richness <- vascular_only_filtered %>%
  group_by(Plot, Sample_Year) %>% 
  summarize(Species_Richness = n_distinct(Code1), .groups = "keep") 
vasc_sp_richness <- vasc_sp_richness %>% mutate(Plot_Year = paste(Plot, Sample_Year, sep="_"))
vasc_sp_richness

nonvasc_sp_richness <- nonvasc_only_filtered %>%
  group_by(Plot, Sample_Year) %>% 
  summarize(Species_Richness = n_distinct(Code1), .groups = "keep") 
nonvasc_sp_richness <- nonvasc_sp_richness %>% mutate(Plot_Year = paste(Plot, Sample_Year, sep="_"))
nonvasc_sp_richness

write_xlsx(lichen_sp_richness, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/lichen_sp_richness_filtered.xlsx")
write_xlsx(vasc_sp_richness, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/vascular_sp_richness_filtered.xlsx")
write_xlsx(nonvasc_sp_richness, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/nonvasc_sp_richness_filtered.xlsx")




#pull in old dfs - INCORRECT!!!!!!!!!!!!!!!!
alpine_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/alpine_df_vasc.xlsx")
alpine_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/alpine_df_lichen.xlsx")
alpine_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/alpine_df_nonvasc.xlsx")

beetle_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/beetle_df_vasc.xlsx")
beetle_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/beetle_df_lichen.xlsx")
beetle_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/beetle_df_nonvasc.xlsx")

needle_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/needle_df_vasc.xlsx")
needle_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/needle_df_lichen.xlsx")
needle_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/needle_df_nonvasc.xlsx")

openlow_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/openlow_df_vasc.xlsx")
openlow_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/openlow_df_lichen.xlsx")
openlow_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/openlow_df_nonvasc.xlsx")

dwarfscrub_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/dwarfscrub_df_vasc.xlsx")
dwarfscrub_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/dwarfscrub_df_lichen.xlsx")
dwarfscrub_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/dwarfscrub_df_nonvasc.xlsx")  

forest_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/forest_df_vasc.xlsx")
forest_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/forest_df_lichen.xlsx")
forest_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/forest_df_nonvasc.xlsx")




#filter abundance_df_corrected so that only plot_years that are in X_df are present, join extra info, and export to file folder 
  #Alpine 
        alpine_df_filtered <- quad_abundance_df_vascular_filtered %>%
          filter(Plot_Year %in% alpine_df$Plot_Year)
        alpine_df_lichen_filtered <- quad_abundance_df_lichen_filtered %>%
          filter(Plot_Year %in% alpine_df_lichen$Plot_Year)
        alpine_df_nonvasc_filtered <- quad_abundance_df_nonvasc_filtered %>%
          filter(Plot_Year %in% alpine_df_nonvasc$Plot_Year)
        
        str(alpine_df)
        alpine_df_filtered <- alpine_df_filtered %>%
          left_join(alpine_df %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.2, Viereck.3),
                    by = "Plot_Year")
        alpine_df_filtered <- alpine_df_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.2, Viereck.3, everything())

        
        str(alpine_df_lichen)
        alpine_df_lichen_filtered <- alpine_df_lichen_filtered %>%
          left_join(alpine_df_lichen %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4),
                    by = "Plot_Year")  
        alpine_df_lichen_filtered <- alpine_df_lichen_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4, everything())

        
        str(alpine_df_nonvasc)
        alpine_df_nonvasc_filtered <- alpine_df_nonvasc_filtered %>%
          left_join(alpine_df_nonvasc %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4),
                    by = "Plot_Year")         
        alpine_df_nonvasc_filtered <- alpine_df_nonvasc_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4, everything())

        
        write_xlsx(alpine_df_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_vasc_filtered.xlsx")
        write_xlsx(alpine_df_lichen_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_lichen_filtered.xlsx")
        write_xlsx(alpine_df_nonvasc_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_nonvasc_filtered.xlsx")
        
  #Beetle 
        beetle_df_filtered <- quad_abundance_df_vascular_filtered %>%
          filter(Plot_Year %in% beetle_df$Plot_Year)
        beetle_df_lichen_filtered <- quad_abundance_df_lichen_filtered %>%
          filter(Plot_Year %in% beetle_df_lichen$Plot_Year)
        beetle_df_nonvasc_filtered <- quad_abundance_df_nonvasc_filtered %>%
          filter(Plot_Year %in% beetle_df_nonvasc$Plot_Year)
        
        beetle_df_filtered <- beetle_df_filtered %>%
          left_join(beetle_df %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.2, Viereck.3),
                    by = "Plot_Year")
        beetle_df_filtered <- beetle_df_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.2, Viereck.3, everything())

        
        beetle_df_lichen_filtered <- beetle_df_lichen_filtered %>%
          left_join(beetle_df_lichen %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4),
                    by = "Plot_Year")  
        beetle_df_lichen_filtered <- beetle_df_lichen_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4, everything())

        
        beetle_df_nonvasc_filtered <- beetle_df_nonvasc_filtered %>%
          left_join(beetle_df_nonvasc %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4),
                    by = "Plot_Year")         
        beetle_df_nonvasc_filtered <- beetle_df_nonvasc_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4, everything())

        
        write_xlsx(beetle_df_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_vasc_filtered.xlsx")
        write_xlsx(beetle_df_lichen_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_lichen_filtered.xlsx")
        write_xlsx(beetle_df_nonvasc_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_nonvasc_filtered.xlsx")
        
  #spruce woodland
        needle_df_filtered <- quad_abundance_df_vascular_filtered %>%
          filter(Plot_Year %in% needle_df$Plot_Year)
        needle_df_lichen_filtered <- quad_abundance_df_lichen_filtered %>%
          filter(Plot_Year %in% needle_df_lichen$Plot_Year)
        needle_df_nonvasc_filtered <- quad_abundance_df_nonvasc_filtered %>%
          filter(Plot_Year %in% needle_df_nonvasc$Plot_Year)
        
        needle_df_filtered <- needle_df_filtered %>%
          left_join(needle_df %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.2, Viereck.3),
                    by = "Plot_Year")
        needle_df_filtered <- needle_df_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.2, Viereck.3, everything())

        
        needle_df_lichen_filtered <- needle_df_lichen_filtered %>%
          left_join(needle_df_lichen %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4),
                    by = "Plot_Year")  
        needle_df_lichen_filtered <- needle_df_lichen_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4, everything())

        
        needle_df_nonvasc_filtered <- needle_df_nonvasc_filtered %>%
          left_join(needle_df_nonvasc %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4),
                    by = "Plot_Year")         
        needle_df_nonvasc_filtered <- needle_df_nonvasc_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4, everything())

        
        write_xlsx(needle_df_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_vasc_filtered.xlsx")
        write_xlsx(needle_df_lichen_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_lichen_filtered.xlsx")
        write_xlsx(needle_df_nonvasc_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_nonvasc_filtered.xlsx")
        
  #Openlow 
        openlow_df_filtered <- quad_abundance_df_vascular_filtered %>%
          filter(Plot_Year %in% openlow_df$Plot_Year)
        openlow_df_lichen_filtered <- quad_abundance_df_lichen_filtered %>%
          filter(Plot_Year %in% openlow_df_lichen$Plot_Year)
        openlow_df_nonvasc_filtered <- quad_abundance_df_nonvasc_filtered %>%
          filter(Plot_Year %in% openlow_df_nonvasc$Plot_Year)
        
        openlow_df_filtered <- openlow_df_filtered %>%
          left_join(openlow_df %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.2, Viereck.3),
                    by = "Plot_Year")
        openlow_df_filtered <- openlow_df_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.2, Viereck.3, everything())

        
        openlow_df_lichen_filtered <- openlow_df_lichen_filtered %>%
          left_join(openlow_df_lichen %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4),
                    by = "Plot_Year")  
        openlow_df_lichen_filtered <- openlow_df_lichen_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4, everything())

        
        openlow_df_nonvasc_filtered <- openlow_df_nonvasc_filtered %>%
          left_join(openlow_df_nonvasc %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4),
                    by = "Plot_Year")         
        openlow_df_nonvasc_filtered <- openlow_df_nonvasc_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4, everything())

        
        write_xlsx(openlow_df_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_vasc_filtered.xlsx")
        write_xlsx(openlow_df_lichen_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_lichen_filtered.xlsx")
        write_xlsx(openlow_df_nonvasc_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_nonvasc_filtered.xlsx")
        
    #Dwarfscrub 
        dwarfscrub_df_filtered <- quad_abundance_df_vascular_filtered %>%
          filter(Plot_Year %in% dwarfscrub_df$Plot_Year)
        dwarfscrub_df_lichen_filtered <- quad_abundance_df_lichen_filtered %>%
          filter(Plot_Year %in% dwarfscrub_df_lichen$Plot_Year)
        dwarfscrub_df_nonvasc_filtered <- quad_abundance_df_nonvasc_filtered %>%
          filter(Plot_Year %in% dwarfscrub_df_nonvasc$Plot_Year)
        
        dwarfscrub_df_filtered <- dwarfscrub_df_filtered %>%
          left_join(dwarfscrub_df %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.2, Viereck.3),
                    by = "Plot_Year")
        dwarfscrub_df_filtered <- dwarfscrub_df_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.2, Viereck.3, everything())

        
        dwarfscrub_df_lichen_filtered <- dwarfscrub_df_lichen_filtered %>%
          left_join(dwarfscrub_df_lichen %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4),
                    by = "Plot_Year")  
        dwarfscrub_df_lichen_filtered <- dwarfscrub_df_lichen_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4, everything())

        
        dwarfscrub_df_nonvasc_filtered <- dwarfscrub_df_nonvasc_filtered %>%
          left_join(dwarfscrub_df_nonvasc %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4),
                    by = "Plot_Year")         
        dwarfscrub_df_nonvasc_filtered <- dwarfscrub_df_nonvasc_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4, everything())

        
        write_xlsx(dwarfscrub_df_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_vasc_filtered.xlsx")
        write_xlsx(dwarfscrub_df_lichen_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_lichen_filtered.xlsx")
        write_xlsx(dwarfscrub_df_nonvasc_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_nonvasc_filtered.xlsx")
        
    #Forest
        forest_df_filtered <- quad_abundance_df_vascular_filtered %>%
          filter(Plot_Year %in% forest_df$Plot_Year)
        forest_df_lichen_filtered <- quad_abundance_df_lichen_filtered %>%
          filter(Plot_Year %in% forest_df_lichen$Plot_Year)
        forest_df_nonvasc_filtered <- quad_abundance_df_nonvasc_filtered %>%
          filter(Plot_Year %in% forest_df_nonvasc$Plot_Year)
        
        forest_df_filtered <- forest_df_filtered %>%
          left_join(forest_df %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.2, Viereck.3),
                    by = "Plot_Year")
        forest_df_filtered <- forest_df_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.2, Viereck.3, everything())

        
        forest_df_lichen_filtered <- forest_df_lichen_filtered %>%
          left_join(forest_df_lichen %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4),
                    by = "Plot_Year")  
        forest_df_lichen_filtered <- forest_df_lichen_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4, everything())

        
        forest_df_nonvasc_filtered <- forest_df_nonvasc_filtered %>%
          left_join(forest_df_nonvasc %>% select(Plot_Year, Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4),
                    by = "Plot_Year")         
        forest_df_nonvasc_filtered <- forest_df_nonvasc_filtered %>% select(Plot, Vegetation_Class, Sample_Year, Viereck.1, Viereck.2, Viereck.3, Viereck.4, everything())

        
        write_xlsx(forest_df_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_vasc_filtered.xlsx")
        write_xlsx(forest_df_lichen_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_lichen_filtered.xlsx")
        write_xlsx(forest_df_nonvasc_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_nonvasc_filtered.xlsx")
        
#remake env_files 
vasc_sp_richness <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/vascular_sp_richness_filtered.xlsx")
lichen_sp_richness <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/lichen_sp_richness_filtered.xlsx")
nonvasc_sp_richness <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/nonvasc_sp_richness_filtered.xlsx")
        
beetle_df_filtered <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_filtered.xlsx")
beetle_df_lichen_filtered <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_lichen_filtered.xlsx")
beetle_df_nonvasc_filtered <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_nonvasc_filtered.xlsx")

    #Beetle 
        #Vascular
        beetle_env_filtered <- beetle_df_filtered[,c(1:6)]
        
        #adding visit 
        beetle_env_filtered <- beetle_env_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        #adding canopy cover 
        beetle_env_filtered <- beetle_env_filtered %>%
          left_join(canopy_cover %>% select(Plot_Year, Percent_Cover), by = c("Plot_Year"))
        beetle_env_filtered <- beetle_env_filtered %>% distinct()
        #adding species richness 
        beetle_env_filtered <- beetle_env_filtered %>%
          left_join(vasc_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
        
        #Lichen
        beetle_env_lichen_filtered <- beetle_df_lichen_filtered[,c(1:8)]
        
        #add visit column
        beetle_env_lichen_filtered <- beetle_env_lichen_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        #add canopy cover column 
        beetle_env_lichen_filtered <- beetle_env_lichen_filtered %>%
          left_join(canopy_cover %>% select(Plot_Year, Percent_Cover), by = c("Plot_Year"))
        beetle_env_lichen_filtered <- beetle_env_lichen_filtered %>% distinct()
        #add species richness column 
        beetle_env_lichen_filtered <- beetle_env_lichen_filtered %>%
          left_join(lichen_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
        
        #Nonvascular
        beetle_env_nonvasc_filtered <- beetle_df_nonvasc_filtered[,c(1:8)]
        
        #adding visit column 
        beetle_env_nonvasc_filtered <- beetle_env_nonvasc_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        #adding canopy cover column 
        beetle_env_nonvasc_filtered <- beetle_env_nonvasc_filtered %>%
          left_join(canopy_cover %>% select(Plot_Year, Percent_Cover), by = c("Plot_Year"))
        beetle_env_nonvasc_filtered <- beetle_env_nonvasc_filtered %>% distinct()
        #add species richness column 
        beetle_env_nonvasc_filtered <- beetle_env_nonvasc_filtered %>%
          left_join(nonvasc_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))

        
        
    #spruce woodland 
        #Vascular
        needle_env_filtered <- needle_df_filtered[,c(1:6)]
        
        #add visit column 
        needle_env_filtered <- needle_env_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        #add species richness column
        needle_env_filtered <- needle_env_filtered %>%
          left_join(vasc_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
        
        #lichen
        
        needle_env_lichen_filtered <- needle_df_lichen_filtered[,c(1:8)]
        
        #add visit column
        needle_env_lichen_filtered <- needle_env_lichen_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        #add species richness 
        needle_env_lichen_filtered <- needle_env_lichen_filtered %>%
          left_join(lichen_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
        
        
        #nonvascular 
        
        needle_env_nonvasc_filtered <- needle_df_nonvasc_filtered[,c(1:8)]
        
        #add visit column
        needle_env_nonvasc_filtered <- needle_env_nonvasc_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        #Add species richness column 
        needle_env_nonvasc_filtered <- needle_env_nonvasc_filtered %>%
          left_join(nonvasc_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))

#Open low shrub 
        #Vascular 
        openlow_env_filtered <- openlow_df_filtered[,c(1:6)]
        
        #create visit column 
        openlow_env_filtered <- openlow_env_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        #add species richness column 
        openlow_env_filtered <- openlow_env_filtered %>%
          left_join(vasc_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
        
        #Lichen 
        openlow_env_lichen_filtered <- openlow_df_lichen_filtered[,c(1:8)]
        
        #create visit column 
        openlow_env_lichen_filtered <- openlow_env_lichen_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()   
        #add species richness column 
        openlow_env_lichen_filtered <- openlow_env_lichen_filtered %>%
          left_join(lichen_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
        
        #Nonvascular 
        openlow_env_nonvasc_filtered <- openlow_df_nonvasc_filtered[,c(1:8)]
        
        #create visit column 
        openlow_env_nonvasc_filtered <- openlow_env_nonvasc_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        #add species richness column 
        openlow_env_nonvasc_filtered <- openlow_env_nonvasc_filtered %>%
          left_join(nonvasc_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))


#Dwarf Shrub 
        #vascular 
        dwarfscrub_env_filtered <- dwarfscrub_df_filtered[,c(1:6)]
        
        #Adding visit column 
        dwarfscrub_env_filtered <- dwarfscrub_env_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        #adding species richness column 
        dwarfscrub_env_filtered <- dwarfscrub_env_filtered %>%
          left_join(vasc_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
        
        #lichen 
        dwarfscrub_env_lichen_filtered <- dwarfscrub_df_lichen_filtered[,c(1:8)]
        
        #add visit column
        dwarfscrub_env_lichen_filtered <- dwarfscrub_env_lichen_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        #Add species richness column
        dwarfscrub_env_lichen_filtered <- dwarfscrub_env_lichen_filtered %>%
          left_join(lichen_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
        
        #nonvascular 
        dwarfscrub_env_nonvasc_filtered <- dwarfscrub_df_nonvasc_filtered[,c(1:8)]
        
        #Add visit column
        dwarfscrub_env_nonvasc_filtered <- dwarfscrub_env_nonvasc_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        #add species richness column
        dwarfscrub_env_nonvasc_filtered <- dwarfscrub_env_nonvasc_filtered %>%
          left_join(nonvasc_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))

  #Spruce forest 
        #Vascular 
        forest_env_filtered <- forest_df_filtered[,c(1:6)]
        
        #add visit column
        forest_env_filtered <- forest_env_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup() 
        #add species richness column
        forest_env_filtered <- forest_env_filtered %>%
          left_join(vasc_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
        #adding canopy cover 
        forest_env_filtered <- forest_env_filtered %>%
          left_join(canopy_cover %>% select(Plot_Year, Percent_Cover), by = c("Plot_Year"))
        forest_env_filtered <- forest_env_filtered %>% distinct()
        
        #lichen 
        forest_env_lichen_filtered <- forest_df_lichen_filtered[,c(1:8)]
        
        #add visit column
        forest_env_lichen_filtered <- forest_env_lichen_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        #add species richness column
        forest_env_lichen_filtered <- forest_env_lichen_filtered %>%
          left_join(lichen_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
        #adding canopy cover 
        forest_env_lichen_filtered <- forest_env_lichen_filtered %>%
          left_join(canopy_cover %>% select(Plot_Year, Percent_Cover), by = c("Plot_Year"))
        forest_env_lichen_filtered <- forest_env_lichen_filtered %>% distinct()
        
        #nonvascular 
        forest_env_nonvasc_filtered <- forest_df_nonvasc_filtered[,c(1:8)]
        
        #Add visit column
        forest_env_nonvasc_filtered <- forest_env_nonvasc_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()   
        #add species richness column
        forest_env_nonvasc_filtered <- forest_env_nonvasc_filtered %>%
          left_join(nonvasc_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
        #adding canopy cover 
        forest_env_nonvasc_filtered <- forest_env_nonvasc_filtered %>%
          left_join(canopy_cover %>% select(Plot_Year, Percent_Cover), by = c("Plot_Year"))
        forest_env_nonvasc_filtered <- forest_env_nonvasc_filtered %>% distinct()


#alpine 
        #Vascular 
        alpine_env_filtered <- alpine_df_filtered[,c(1:6)]
        
        #Add species richness column 
        alpine_env_filtered <- alpine_env_filtered %>%
          left_join(vasc_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
        
        #Add visit column for beta diversity 
        alpine_env_filtered <- alpine_env_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        
        #Lichen 
        alpine_env_lichen_filtered <- alpine_df_lichen_filtered[,c(1:8)]
        
        #Add species richness column 
        alpine_env_lichen_filtered <- alpine_env_lichen_filtered %>%
          left_join(lichen_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))
        
        #Add visit column for beta diversity 
        alpine_env_lichen_filtered <- alpine_env_lichen_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        
        
        #Nonvascular 
        
        alpine_env_nonvasc_filtered <- alpine_df_nonvasc_filtered[,c(1:8)]
        
        #Add visit column for beta diversity 
        alpine_env_nonvasc_filtered <- alpine_env_nonvasc_filtered %>%
          arrange(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          mutate(Visit = paste0("visit_", row_number())) %>%
          ungroup()
        
        #Add species richness column 
        alpine_env_nonvasc_filtered <- alpine_env_nonvasc_filtered %>%
          left_join(nonvasc_sp_richness %>% select(Plot_Year, Species_Richness), by = c("Plot_Year"))

#export them to the folder for quickload 
        write_xlsx(forest_env_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_env_vasc_filtered.xlsx")
        write_xlsx(forest_env_lichen_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_env_lichen_filtered.xlsx")
        write_xlsx(forest_env_nonvasc_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_env_nonvasc_filtered.xlsx")

        write_xlsx(alpine_env_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_env_vasc_filtered.xlsx")
        write_xlsx(alpine_env_lichen_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_env_lichen_filtered.xlsx")
        write_xlsx(alpine_env_nonvasc_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_env_nonvasc_filtered.xlsx")
        
        write_xlsx(beetle_env_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_env_vasc_filtered.xlsx")
        write_xlsx(beetle_env_lichen_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_env_lichen_filtered.xlsx")
        write_xlsx(beetle_env_nonvasc_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_env_nonvasc_filtered.xlsx")
        
        write_xlsx(needle_env_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_env_vasc_filtered.xlsx")
        write_xlsx(needle_env_lichen_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_env_lichen_filtered.xlsx")
        write_xlsx(needle_env_nonvasc_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_env_nonvasc_filtered.xlsx")
        
        write_xlsx(openlow_env_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_env_vasc_filtered.xlsx")
        write_xlsx(openlow_env_lichen_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_env_lichen_filtered.xlsx")
        write_xlsx(openlow_env_nonvasc_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_env_nonvasc_filtered.xlsx")
        
        write_xlsx(dwarfscrub_env_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_env_vasc_filtered.xlsx")
        write_xlsx(dwarfscrub_env_lichen_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_env_lichen_filtered.xlsx")
        write_xlsx(dwarfscrub_env_nonvasc_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_env_nonvasc_filtered.xlsx")



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
