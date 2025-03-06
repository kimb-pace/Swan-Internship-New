#fixed permanova with species abundance instead of richness 

library(readxl)
library(writexl)
library(readr)
library(tibble)
library(dplyr)
library(vegan)
library(permute)
library(lattice)

#quick load of prepared dataframes
lichen_abundance_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/quad_abundance_df_lichen.xlsx")
nonvasc_abundance_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/quad_abundance_df_nonvasc.xlsx")
vasc_abundance_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/quad_abundance_df_vascular.xlsx")

viereck <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Viereck_env.xlsx")

lichen_abundance_df <- lichen_abundance_df %>%
  left_join(viereck %>% select(Plot_Year, Sample_Year), by = c("Plot_Year"))
nonvasc_abundance_df <- nonvasc_abundance_df %>%
  left_join(viereck %>% select(Plot_Year, Sample_Year), by = c("Plot_Year"))
lichen_abundance_df <- lichen_abundance_df %>% select(Sample_Year, everything())
nonvasc_abundance_df <- nonvasc_abundance_df %>% select(Sample_Year, everything())

lichen_abundance_df <- lichen_abundance_df %>% distinct()
nonvasc_abundance_df <- nonvasc_abundance_df %>% distinct()

write_xlsx(lichen_abundance_df, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/quad_abundance_df_lichen.xlsx")
write_xlsx(nonvasc_abundance_df, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/quad_abundance_df_nonvasc.xlsx")

vasc_abundance_df <- vasc_abundance_df %>%
  left_join(viereck %>% select(Plot_Year, Plot), by = c("Plot_Year"))
vasc_abundance_df <- vasc_abundance_df %>% distinct()
vasc_abundance_df <- vasc_abundance_df %>% select(Plot, everything())
write_xlsx(vasc_abundance_df, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/quad_abundance_df_vascular.xlsx")

#alpine 
          alpine_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/alpine_df.xlsx")
          alpine_lichen_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/alpine_lichen_df.xlsx")
          alpine_nonvasc_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/alpine_nonvasc_df.xlsx")
          
          alpine_lichen_abundance_balanced <- lichen_abundance_df %>%
            filter(Plot_Year %in% alpine_lichen_df$Plot_Year)
          alpine_lichen_df <- alpine_lichen_abundance_balanced 
          
                  #convert dataframe to a matrix so you can run permanova successfully 
                  alpine_lichen_composition <- alpine_lichen_df[,c(12:178)]
                  alpine_lichen_composition <- as.matrix(alpine_lichen_composition) 
                  
                  #create env_file including viereck classes 
                  lichens_env_alpine <- alpine_lichen_df[,c(1:11)]
                  
                  #recode time as visit 
                  lichens_env_alpine <- lichens_env_alpine %>%
                    arrange(Plot, Sample_Year) %>%
                    group_by(Plot) %>%
                    mutate(Visit = paste0("visit_", row_number())) %>%
                    ungroup()
          
          alpine_nonvasc_abundance_balanced <- nonvasc_abundance_df %>%
            filter(Plot_Year %in% alpine_nonvasc_df$Plot_Year)
          alpine_nonvasc_df <- alpine_nonvasc_abundance_balanced
          
                    #convert dataframe to a matrix so you can run permanova successfully 
                    alpine_nonvasc_composition <- alpine_nonvasc_df[,c(12:218)]
                    alpine_nonvasc_composition <- as.matrix(alpine_nonvasc_composition) 
                    
                    #create env_file 
                    nonvasc_env_alpine <- alpine_nonvasc_df[,c(1:11)]
                    
                    #recode time as visit 
                    nonvasc_env_alpine <- nonvasc_env_alpine %>%
                      arrange(Plot, Sample_Year) %>%
                      group_by(Plot) %>%
                      mutate(Visit = paste0("visit_", row_number())) %>%
                      ungroup()
          
          alpine_vasc_abundance_balanced <- vasc_abundance_df %>%
            filter(Plot_Year %in% alpine_df$Plot_Year)
          alpine_df <- alpine_vasc_abundance_balanced 

                  #create the matrix for PERMANOVA 
                  alpine_composition <- alpine_df[,c(8:281)]
                  alpine_composition <- as.matrix(alpine_composition) 
                  
                  #code for building env file 
                  alpine_env <- alpine_df[,c(1:7)]
                  #create visit column 
                  alpine_env <- alpine_env %>%
                    arrange(Plot, Sample_Year) %>%
                    group_by(Plot) %>%
                    mutate(Visit = paste0("visit_", row_number())) %>%
                    ungroup()


#low shrub 
          openlow_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_df.xlsx")
          openlow_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_df_lichen.xlsx")
          openlow_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/openlow_df_nonvasc.xlsx")
          
          
          openlow_lichen_abundance_balanced <- lichen_abundance_df %>%
            filter(Plot_Year %in% openlow_df_lichen$Plot_Year)
          openlow_df_lichen <- openlow_lichen_abundance_balanced 
          
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

                      openlow_composition <- openlow_df[,c(8:281)]
                      openlow_composition <- as.matrix(openlow_composition) 
                      
                      openlow_env <- openlow_df[,c(1:7)]
                      
                      openlow_env <- openlow_env %>%
                        arrange(Plot, Sample_Year) %>%
                        group_by(Plot) %>%
                        mutate(Visit = paste0("visit_", row_number())) %>%
                        ungroup()


#dwarf shrub 
          dwarfscrub_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/dwarfscrub_df.xlsx")
          dwarfscrub_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/dwarfscrub_df_lichens.xlsx")
          dwarfscrub_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/dwarfscrub_df_nonvasc.xlsx")
          
          dwarfscrub_lichen_abundance_balanced <- lichen_abundance_df %>%
            filter(Plot_Year %in% dwarfscrub_df_lichen$Plot_Year)
          dwarfscrub_df_lichen <- dwarfscrub_lichen_abundance_balanced 
          
                  dwarfscrub_composition_lichens <- dwarfscrub_df_lichen[,c(12:178)]
                  dwarfscrub_composition_lichens <- as.matrix(dwarfscrub_composition_lichens) 
                  
                  dwarfscrub_env_lichens <- dwarfscrub_df_lichen[,c(1:11)]
                  
                  dwarfscrub_env_lichen <- dwarfscrub_env_lichen %>%
                    arrange(Plot, Sample_Year) %>%
                    group_by(Plot) %>%
                    mutate(Visit = paste0("visit_", row_number())) %>%
                    ungroup()
          
          dwarfscrub_nonvasc_abundance_balanced <- nonvasc_abundance_df %>%
            filter(Plot_Year %in% dwarfscrub_df_nonvasc$Plot_Year)
          dwarfscrub_df_nonvasc <- dwarfscrub_nonvasc_abundance_balanced
          
                  dwarfscrub_composition_nonvasc <- dwarfscrub_df_nonvasc[,c(12:218)]
                  dwarfscrub_composition_nonvasc <- as.matrix(dwarfscrub_composition_nonvasc) 
                  
                  dwarfscrub_env_nonvasc <- dwarfscrub_df_nonvasc[,c(1:11)]
                  
                  dwarfscrub_env_nonvasc <- dwarfscrub_env_nonvasc %>%
                    arrange(Plot, Sample_Year) %>%
                    group_by(Plot) %>%
                    mutate(Visit = paste0("visit_", row_number())) %>%
                    ungroup()
          
          dwarfscrub_vasc_abundance_balanced <- vasc_abundance_df %>%
            filter(Plot_Year %in% dwarfscrub_df$Plot_Year)
          dwarfscrub_df <- dwarfscrub_vasc_abundance_balanced 

                  dwarfscrub_composition <- dwarfscrub_df[,c(8:281)]
                  dwarfscrub_composition <- as.matrix(dwarfscrub_composition) 
                  
                  dwarfscrub_env <- dwarfscrub_df[,c(1:7)]
                  
                  dwarfscrub_env <- dwarfscrub_env %>%
                    arrange(Plot, Sample_Year) %>%
                    group_by(Plot) %>%
                    mutate(Visit = paste0("visit_", row_number())) %>%
                    ungroup()


#beetle kill 
          beetle_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/beetle_df.xlsx")
          beetle_lichen_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/beetle_lichen_df.xlsx")
          beetle_nonvasc_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/beetle_nonvasc_df.xlsx")
          
          
          beetle_lichen_abundance_balanced <- lichen_abundance_df %>%
            filter(Plot_Year %in% beetle_lichen_df$Plot_Year)
          beetle_lichen_df <- beetle_lichen_abundance_balanced 
          
                      beetle_lichen_composition <- beetle_lichen_df[,c(12:178)]
                      beetle_lichen_composition <- as.matrix(beetle_lichen_composition) 
                      
                      lichens_env_beetle <- beetle_lichen_df[,c(1:11)]

                      lichens_env_beetle <- lichens_env_beetle %>%
                        arrange(Plot, Sample_Year) %>%
                        group_by(Plot) %>%
                        mutate(Visit = paste0("visit_", row_number())) %>%
                        ungroup()
                      
                      canopy_cover <- canopy_cover %>% 
                        mutate(Plot_Year = paste(Plot, Sample_Year, sep="_"))
                      canopy_cover <- canopy_cover %>% distinct()
                      
                      lichens_env_beetle <- lichens_env_beetle %>%
                        left_join(canopy_cover %>% select(Plot_Year, Percent_Cover), by = c("Plot_Year"))
                      lichens_env_beetle <- lichens_env_beetle %>% distinct()
          
          beetle_nonvasc_abundance_balanced <- nonvasc_abundance_df %>%
            filter(Plot_Year %in% beetle_nonvasc_df$Plot_Year)
          beetle_nonvasc_df <- beetle_nonvasc_abundance_balanced
          
                      beetle_nonvasc_composition <- beetle_nonvasc_df[,c(12:218)]
                      beetle_nonvasc_composition <- as.matrix(beetle_nonvasc_composition) 
                      
                      nonvasc_env_beetle <- beetle_nonvasc_df[,c(1:11)]
                      
                      nonvasc_env_beetle <- nonvasc_env_beetle %>%
                        arrange(Plot, Sample_Year) %>%
                        group_by(Plot) %>%
                        mutate(Visit = paste0("visit_", row_number())) %>%
                        ungroup()
          
                      nonvasc_env_beetle <- nonvasc_env_beetle %>%
                        left_join(canopy_cover %>% select(Plot_Year, Percent_Cover), by = c("Plot_Year"))
                      nonvasc_env_beetle <- nonvasc_env_beetle %>% distinct()
          
          beetle_vasc_abundance_balanced <- vasc_abundance_df %>%
            filter(Plot_Year %in% beetle_df$Plot_Year)
          beetle_df <- beetle_vasc_abundance_balanced 
          
                      beetle_composition <- beetle_df[,c(8:281)]
                      beetle_composition <- as.matrix(beetle_composition) 
                      
                      beetle_env <- beetle_df[,c(1:7)]
            
                      beetle_env <- beetle_env %>%
                        arrange(Plot, Sample_Year) %>%
                        group_by(Plot) %>%
                        mutate(Visit = paste0("visit_", row_number())) %>%
                        ungroup()
                      
                      beetle_env <- beetle_env %>%
                        left_join(canopy_cover %>% select(Plot_Year, Percent_Cover), by = c("Plot_Year"))
                      beetle_env <- beetle_env %>% distinct()

#spruce woodlands 
          needle_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/needle_df.xlsx")
          needle_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/needle_df_lichen.xlsx")
          needle_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/needle_df_nonvasc.xlsx")
          
          needle_lichen_abundance_balanced <- lichen_abundance_df %>%
            filter(Plot_Year %in% needle_df_lichen$Plot_Year)
          needle_lichen_df <- needle_lichen_abundance_balanced 
          
                    needle_composition_lichen <- needle_df_lichen[,c(13:178)]
                    needle_composition_lichen <- as.matrix(needle_composition_lichen) 
                    
                    #create env_file 
                    needle_env_lichen <- needle_df_lichen[,c(1:12)]
                    
                    needle_env_lichen <- needle_env_lichen %>%
                      arrange(Plot, Sample_Year) %>%
                      group_by(Plot) %>%
                      mutate(Visit = paste0("visit_", row_number())) %>%
                      ungroup()
          
          needle_nonvasc_abundance_balanced <- nonvasc_abundance_df %>%
            filter(Plot_Year %in% needle_df_nonvasc$Plot_Year)
          needle_nonvasc_df <- needle_nonvasc_abundance_balanced
          
                      needle_composition_nonvasc <- needle_df_nonvasc[,c(13:218)]
                      needle_composition_nonvasc <- as.matrix(needle_composition_nonvasc) 
                      
                      #create env_file 
                      needle_env_nonvasc <- needle_df_nonvasc[,c(1:13)]
                      
                      needle_env_nonvasc <- needle_env_nonvasc %>%
                        arrange(Plot, Sample_Year) %>%
                        group_by(Plot) %>%
                        mutate(Visit = paste0("visit_", row_number())) %>%
                        ungroup()
          
          needle_vasc_abundance_balanced <- vasc_abundance_df %>%
            filter(Plot_Year %in% needle_df$Plot_Year)
          needle_df <- needle_vasc_abundance_balanced 
          
                  needle_composition <- needle_df[,c(8:281)]
                  needle_composition <- as.matrix(needle_composition) 
        
                  needle_env <- needle_df[,c(1:7)]
                  
                  needle_env <- needle_env %>%
                    arrange(Plot, Sample_Year) %>%
                    group_by(Plot) %>%
                    mutate(Visit = paste0("visit_", row_number())) %>%
                    ungroup()


#spruce forest 
          forest_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/forest_df.xlsx")
          forest_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/forest_df_lichens.xlsx")
          forest_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/forest_df_nonvasc.xlsx")
          
          forest_lichen_abundance_balanced <- lichen_abundance_df %>%
            filter(Plot_Year %in% forest_df_lichen$Plot_Year)
          forest_df_lichens <- forest_lichen_abundance_balanced 
          
                    forest_composition_lichens <- forest_df_lichens[,c(12:178)]
                    forest_composition_lichens <- as.matrix(forest_composition_lichens) 

                    forest_env_lichens <- forest_df_lichens[,c(1:11)]
                    
                    forest_env_lichens <- forest_env_lichens %>%
                      arrange(Plot, Sample_Year) %>%
                      group_by(Plot) %>%
                      mutate(Visit = paste0("visit_", row_number())) %>%
                      ungroup()
          
          forest_nonvasc_abundance_balanced <- nonvasc_abundance_df %>%
            filter(Plot_Year %in% forest_df_nonvasc$Plot_Year)
          forest_nonvasc_df <- forest_nonvasc_abundance_balanced
          
                    forest_composition_nonvasc <- forest_df_nonvasc[,c(13:218)]
                    forest_composition_nonvasc <- as.matrix(forest_composition_nonvasc) 

                    forest_env_nonvasc <- forest_df_nonvasc[,c(1:12)]
                    
                    forest_env_nonvasc <- forest_env_nonvasc %>%
                      arrange(Plot, Sample_Year) %>%
                      group_by(Plot) %>%
                      mutate(Visit = paste0("visit_", row_number())) %>%
                      ungroup()
          
          forest_vasc_abundance_balanced <- vasc_abundance_df %>%
            filter(Plot_Year %in% forest_df$Plot_Year)
          forest_df <- forest_vasc_abundance_balanced 

                  forest_composition <- forest_df[,c(8:281)]
                  forest_composition <- as.matrix(forest_composition) 
                  
                  forest_env <- forest_df[,c(1:7)]
                  
                  forest_env <- forest_env %>%
                    arrange(Plot, Sample_Year) %>%
                    group_by(Plot) %>%
                    mutate(Visit = paste0("visit_", row_number())) %>%
                    ungroup()
