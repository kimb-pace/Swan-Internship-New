library(tidyr)
library(codyn)
library(dplyr)
library(writexl)
library(readxl)

taxa_filtered <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/taxa_filtered.xlsx")

#official turnover code 

#Vascular 
        needle_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_vasc_filtered.xlsx")
        forest_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_vasc_filtered.xlsx")
        beetle_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_vasc_filtered.xlsx")
        dwarfscrub_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_vasc_filtered.xlsx")
        openlow_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_vasc_filtered.xlsx")
        alpine_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_vasc_filtered.xlsx")
        
        
        #get the first and most recent sample year for each plot (excluding plots visited only once)
        first_last_years <- openlow_df %>%                                                           #DF CHANGE
          distinct(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          summarise(
            first_year = min(Sample_Year),
            last_year = max(Sample_Year)) %>%
          filter(first_year != last_year)
        first_data <- openlow_df %>%                                                                #DF CHANGE
          inner_join(first_last_years %>% select(Plot, first_year), 
                     by = c("Plot" = "Plot", "Sample_Year" = "first_year"))
        last_data <- openlow_df %>%                                                                 #DF CHANGE
          inner_join(first_last_years %>% select(Plot, last_year), 
                     by = c("Plot" = "Plot", "Sample_Year" = "last_year"))
        
        #combine 
        combined_data <- bind_rows(
          first_data %>% mutate(Visit = "First"),  
          last_data %>% mutate(Visit = "Last"))
        
        str(combined_data)
        combined_data <- combined_data %>% select(Visit, everything())
        combined_data <- subset(combined_data, select = -c(Park, Vegetation_Class, Viereck.2, Viereck.3, Plot_Year))
        
        str(combined_data) #check that only visit, sample_year, and plot remain 
        
        #pivot to long format so it can fit into the function, use values_drop_na to get only rows with species present
        long_data <- combined_data %>%
          pivot_longer(cols = -c(Plot, Visit, Sample_Year), 
                       names_to = "Code1",                 
                       values_to = "Presence",              
                       values_drop_na = TRUE)                    
        
        #make sure year is numeric 
        long_data$Sample_Year <- as.numeric(as.character(long_data$Sample_Year))
        long_data <- long_data %>% distinct()
        
        #calculate the values for total turnover, losses, and gains 
        turnover_total <- turnover(df = long_data, 
                                   time.var = "Sample_Year", 
                                   species.var = "Code1", 
                                   abundance.var = "Presence", 
                                   replicate.var = "Plot", 
                                   metric = "total")
        #proportion of species that differ between the two time points 
        
        turnover_disappearance <- turnover(
          df = long_data,
          time.var = "Sample_Year",    
          species.var = "Code1",  
          abundance.var = "Presence",  
          replicate.var = "Plot",      
          metric = "disappearance")
        #proportion of species that appeared relative to the total number of species observed in both time points 
        
        turnover_appearance <- turnover(
          df = long_data,
          time.var = "Sample_Year",    
          species.var = "Code1",  
          abundance.var = "Presence",  
          replicate.var = "Plot",      
          metric = "appearance")
        #proportion of species that disappeared relative to the total number of species observed in both time points 
        
        turnover_total
        turnover_disappearance
        turnover_appearance
        
        turnover_joined <- left_join(turnover_total, turnover_appearance, by = "Plot")
        turnover_joined <- left_join(turnover_joined, turnover_disappearance, by = "Plot")
        
        #make the dataframe look better and get all the columns you want 
        turnover_joined <- turnover_joined[,c(1,3,4,6)]
        turnover_joined <- turnover_joined %>% select(Plot, everything())
        
        turnover_joined <- turnover_joined %>% rename(Total_Vasc_Prop = total)
        turnover_joined <- turnover_joined %>% rename(Disapp_Vasc_Prop = disappearance)
        turnover_joined <- turnover_joined %>% rename(App_Vasc_Prop = appearance)
        turnover_joined

#getting species list 
        long_data <- long_data %>%
          mutate(visit = factor(Visit, levels = c("First", "Last")))
        
        long_data <- long_data %>%
          mutate(across(5, ~ ifelse(. >0, 1, 0)))
        
        #corrected: 
        disappeared_species_summary <- long_data %>%
          group_by(Code1, Plot) %>%
          summarize(First = max(Presence[visit == "First"], na.rm = TRUE),
                    Last = max(Presence[visit == "Last"], na.rm = TRUE), .groups = "drop") %>%
          filter(First > 0 & Last == 0) %>%
          group_by(Code1) %>%
          summarize(Plots_Disappeared = n(), .groups = "drop")
        
        appeared_species_summary <- long_data %>%
          group_by(Code1, Plot) %>%
          summarize(First = max(Presence[visit == "First"], na.rm = TRUE),
                    Last = max(Presence[visit == "Last"], na.rm = TRUE), .groups = "drop") %>%
          filter(First == 0 & Last > 0) %>%
          group_by(Code1) %>%
          summarize(Plots_Appeared = n(), .groups = "drop")
        
        df_changes_summary <- full_join(disappeared_species_summary, appeared_species_summary, by = "Code1") %>%
          replace_na(list(plots_disappeared = 0, plots_appeared = 0))
        print(df_changes_summary)
        
        df_changes_summary <- df_changes_summary %>%
          left_join(taxa_filtered %>% select(Genus, Species, Code1), by = "Code1")
        df_changes_summary <-df_changes_summary %>% distinct()
        df_changes_summary <- df_changes_summary %>% select(Genus, Species, everything())
          
        df_changes_summary
        
         
                                                                                                            #DF CHANGE BELOW
        
        write_xlsx(df_changes_summary, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_vascular_specieslist.xlsx")
        write_xlsx(turnover_joined, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_vascular_turnover.xlsx")


#Lichen 

        needle_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_lichen_filtered.xlsx")
        forest_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_lichen_filtered.xlsx")
        beetle_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_lichen_filtered.xlsx")
        dwarfscrub_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_lichen_filtered.xlsx")
        openlow_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_lichen_filtered.xlsx")
        alpine_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_lichen_filtered.xlsx")        
        
        #get the first and most recent sample year for each plot (excluding plots visited only once)
        first_last_years <- openlow_df_lichen %>%                                                       #DF CHANGE
          distinct(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          summarise(
            first_year = min(Sample_Year),
            last_year = max(Sample_Year)) %>%
          filter(first_year != last_year)
        first_data <- openlow_df_lichen %>%                                                            #DF CHANGE
          inner_join(first_last_years %>% select(Plot, first_year), 
                     by = c("Plot" = "Plot", "Sample_Year" = "first_year"))
        last_data <- openlow_df_lichen %>%                                                             #DF CHANGE
          inner_join(first_last_years %>% select(Plot, last_year), 
                     by = c("Plot" = "Plot", "Sample_Year" = "last_year"))
        
        #combine 
        combined_data <- bind_rows(
          first_data %>% mutate(Visit = "First"),  
          last_data %>% mutate(Visit = "Last"))
        
        combined_data <- combined_data %>% select(Visit, everything())
        combined_data <- subset(combined_data, select = -c(Park, Vegetation_Class, Plot_Year, Viereck.1, Viereck.4,
                                                           Viereck.2, Viereck.3))
        
        str(combined_data)
        
        #pivot to long format so it can fit into the function, use values_drop_na to get only rows with species present
        long_data <- combined_data %>%
          pivot_longer(cols = -c(Plot, Visit, Sample_Year), 
                       names_to = "Code1",                 
                       values_to = "Presence",              
                       values_drop_na = TRUE)                    
        
        #make sure year is numeric 
        long_data$Sample_Year <- as.numeric(as.character(long_data$Sample_Year))
        long_data <- long_data %>% distinct()
        
        #calculate the values for total turnover, losses, and gains 
        turnover_total <- turnover(df = long_data, 
                                   time.var = "Sample_Year", 
                                   species.var = "Code1", 
                                   abundance.var = "Presence", 
                                   replicate.var = "Plot", 
                                   metric = "total")
        #proportion of species that differ between the two time points 
        
        turnover_disappearance <- turnover(
          df = long_data,
          time.var = "Sample_Year",    
          species.var = "Code1",  
          abundance.var = "Presence",  
          replicate.var = "Plot",      
          metric = "disappearance")
        #proportion of species that appeared relative to the total number of species observed in both time points 
        
        turnover_appearance <- turnover(
          df = long_data,
          time.var = "Sample_Year",    
          species.var = "Code1",  
          abundance.var = "Presence",  
          replicate.var = "Plot",      
          metric = "appearance")
        #proportion of species that disappeared relative to the total number of species observed in both time points 
        
        turnover_total
        turnover_disappearance
        turnover_appearance
        
        turnover_joined <- left_join(turnover_total, turnover_appearance, by = "Plot")
        turnover_joined <- left_join(turnover_joined, turnover_disappearance, by = "Plot")
        
        #make the dataframe look better and get all the columns you want 
        turnover_joined <- turnover_joined[,c(1,3,4,6)]
        turnover_joined <- turnover_joined %>% select(Plot, everything())
        
        turnover_joined <- turnover_joined %>% rename(Total_Lichen_Prop = total)
        turnover_joined <- turnover_joined %>% rename(Disapp_Lichen_Prop = disappearance)
        turnover_joined <- turnover_joined %>% rename(App_Lichen_Prop = appearance)
        turnover_joined

#getting species list 
        long_data <- long_data %>%
          mutate(visit = factor(Visit, levels = c("First", "Last")))
        
        long_data <- long_data %>%
          mutate(across(5, ~ ifelse(. >0, 1, 0)))
        
        #corrected: 
        disappeared_species_summary <- long_data %>%
          group_by(Code1, Plot) %>%
          summarize(First = max(Presence[visit == "First"], na.rm = TRUE),
                    Last = max(Presence[visit == "Last"], na.rm = TRUE), .groups = "drop") %>%
          filter(First > 0 & Last == 0) %>%
          group_by(Code1) %>%
          summarize(Plots_Disappeared = n(), .groups = "drop")
        
        appeared_species_summary <- long_data %>%
          group_by(Code1, Plot) %>%
          summarize(First = max(Presence[visit == "First"], na.rm = TRUE),
                    Last = max(Presence[visit == "Last"], na.rm = TRUE), .groups = "drop") %>%
          filter(First == 0 & Last > 0) %>%
          group_by(Code1) %>%
          summarize(Plots_Appeared = n(), .groups = "drop")
        
        df_changes_summary <- full_join(disappeared_species_summary, appeared_species_summary, by = "Code1") %>%
          replace_na(list(plots_disappeared = 0, plots_appeared = 0))
        print(df_changes_summary)
        
        df_changes_summary <- df_changes_summary %>%
          left_join(taxa_filtered %>% select(Genus, Species, Code1), by = "Code1")
        df_changes_summary <-df_changes_summary %>% distinct()
        df_changes_summary <- df_changes_summary %>% select(Genus, Species, everything())
        df_changes_summary
                                                                                                          #DF CHANGE
        write_xlsx(df_changes_summary, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_lichen_specieslist.xlsx")
        write_xlsx(turnover_joined, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_lichen_turnover.xlsx")



#Nonvascular 

          needle_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_nonvasc_filtered.xlsx")
          forest_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_nonvasc_filtered.xlsx")
          beetle_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_nonvasc_filtered.xlsx")
          dwarfscrub_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_nonvasc_filtered.xlsx")
          openlow_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_nonvasc_filtered.xlsx")
          alpine_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_nonvasc_filtered.xlsx")
          
          
          #get the first and most recent sample year for each plot (excluding plots visited only once)
          first_last_years <- openlow_df_nonvasc %>%
            distinct(Plot, Sample_Year) %>%
            group_by(Plot) %>%
            summarise(
              first_year = min(Sample_Year),
              last_year = max(Sample_Year)) %>%
            filter(first_year != last_year)
          first_data <- openlow_df_nonvasc %>%
            inner_join(first_last_years %>% select(Plot, first_year), 
                       by = c("Plot" = "Plot", "Sample_Year" = "first_year"))
          last_data <- openlow_df_nonvasc %>%
            inner_join(first_last_years %>% select(Plot, last_year), 
                       by = c("Plot" = "Plot", "Sample_Year" = "last_year"))
          
          #combine 
          combined_data <- bind_rows(
            first_data %>% mutate(Visit = "First"),  
            last_data %>% mutate(Visit = "Last"))
          
          combined_data <- combined_data %>% select(Visit, everything())
          
          str(combined_data)
          combined_data <- subset(combined_data, select = -c(Park, Plot_Year, Vegetation_Class, 
                                                             Viereck.1, Viereck.4,
                                                             Viereck.2, Viereck.3))
          
          str(combined_data)
          
          #pivot to long format so it can fit into the function, use values_drop_na to get only rows with species present
          long_data <- combined_data %>%
            pivot_longer(cols = -c(Plot, Visit, Sample_Year), 
                         names_to = "Code1",                 
                         values_to = "Presence",              
                         values_drop_na = TRUE)                    
          
          #make sure year is numeric 
          long_data$Sample_Year <- as.numeric(as.character(long_data$Sample_Year))
          long_data <- long_data %>% distinct()
          
          #calculate the values for total turnover, losses, and gains 
          turnover_total <- turnover(df = long_data, 
                                     time.var = "Sample_Year", 
                                     species.var = "Code1", 
                                     abundance.var = "Presence", 
                                     replicate.var = "Plot", 
                                     metric = "total")
          #proportion of species that differ between the two time points 
          
          turnover_disappearance <- turnover(
            df = long_data,
            time.var = "Sample_Year",    
            species.var = "Code1",  
            abundance.var = "Presence",  
            replicate.var = "Plot",      
            metric = "disappearance")
          #proportion of species that appeared relative to the total number of species observed in both time points 
          
          turnover_appearance <- turnover(
            df = long_data,
            time.var = "Sample_Year",    
            species.var = "Code1",  
            abundance.var = "Presence",  
            replicate.var = "Plot",      
            metric = "appearance")
          #proportion of species that disappeared relative to the total number of species observed in both time points 
          
          turnover_total
          turnover_disappearance
          turnover_appearance
          
          turnover_joined <- left_join(turnover_total, turnover_appearance, by = "Plot")
          turnover_joined <- left_join(turnover_joined, turnover_disappearance, by = "Plot")
          
          #make the dataframe look better and get all the columns you want 
          turnover_joined <- turnover_joined[,c(1,3,4,6)]
          turnover_joined <- turnover_joined %>% select(Plot, everything())
          
          turnover_joined <- turnover_joined %>% rename(Total_Nonvasc_Prop = total)
          turnover_joined <- turnover_joined %>% rename(Disapp_Nonvasc_Prop = disappearance)
          turnover_joined <- turnover_joined %>% rename(App_Nonvasc_Prop = appearance)
          turnover_joined
          
  #getting species list 
          long_data <- long_data %>%
            mutate(visit = factor(Visit, levels = c("First", "Last")))
          
          long_data <- long_data %>%
            mutate(across(5, ~ ifelse(. >0, 1, 0)))
          
          #corrected: 
          disappeared_species_summary <- long_data %>%
            group_by(Code1, Plot) %>%
            summarize(First = max(Presence[visit == "First"], na.rm = TRUE),
                      Last = max(Presence[visit == "Last"], na.rm = TRUE), .groups = "drop") %>%
            filter(First > 0 & Last == 0) %>%
            group_by(Code1) %>%
            summarize(Plots_Disappeared = n(), .groups = "drop")
          
          appeared_species_summary <- long_data %>%
            group_by(Code1, Plot) %>%
            summarize(First = max(Presence[visit == "First"], na.rm = TRUE),
                      Last = max(Presence[visit == "Last"], na.rm = TRUE), .groups = "drop") %>%
            filter(First == 0 & Last > 0) %>%
            group_by(Code1) %>%
            summarize(Plots_Appeared = n(), .groups = "drop")
          
          df_changes_summary <- full_join(disappeared_species_summary, appeared_species_summary, by = "Code1") %>%
            replace_na(list(plots_disappeared = 0, plots_appeared = 0))
          print(df_changes_summary)
          
          df_changes_summary <- df_changes_summary %>%
            left_join(taxa_filtered %>% select(Genus, Species, Code1), by = "Code1")
          df_changes_summary <-df_changes_summary %>% distinct()
          df_changes_summary <- df_changes_summary %>% select(Genus, Species, everything())
          df_changes_summary
          
          
          write_xlsx(df_changes_summary, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_nonvasc_specieslist.xlsx")
          write_xlsx(turnover_joined, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_nonvasc_turnover.xlsx")





          
          
          

          
#calculating average turnover and standard error for each veg class

#Alpine 
alpine_vascular_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/alpine_vascular_turnover.xlsx")
alpine_lichen_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/alpine_lichen_turnover.xlsx")
alpine_nonvasc_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/alpine_nonvasc_turnover.xlsx")
          
          alpine_vascular_turnover %>%
            summarise(
              total = mean(Total_Vasc_Prop, na.rm = TRUE),
              se_total = sd(Total_Vasc_Prop, na.rm = TRUE)/sqrt(n()),
              disapp = mean(Disapp_Vasc_Prop, na.rm = TRUE), 
              se_disapp = sd(Disapp_Vasc_Prop, na.rm = TRUE)/sqrt(n()),
              app = mean(App_Vasc_Prop, na.rm = TRUE),
              se_app = sd(App_Vasc_Prop, na.rm = TRUE)/sqrt(n()))
          
          alpine_lichen_turnover %>%
            summarise(
              total = mean(Total_Lichen_Prop, na.rm = TRUE),
              se_total = sd(Total_Lichen_Prop, na.rm = TRUE)/sqrt(n()),
              disapp = mean(Disapp_Lichen_Prop, na.rm = TRUE), 
              se_disapp = sd(Disapp_Lichen_Prop, na.rm = TRUE)/sqrt(n()),
              app = mean(App_Lichen_Prop, na.rm = TRUE),
              se_app = sd(App_Lichen_Prop, na.rm = TRUE)/sqrt(n()))
          
          alpine_nonvasc_turnover %>%
            summarise(
              total = mean(Total_Nonvasc_Prop, na.rm = TRUE),
              se_total = sd(Total_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()),
              disapp = mean(Disapp_Nonvasc_Prop, na.rm = TRUE), 
              se_disapp = sd(Disapp_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()),
              app = mean(App_Nonvasc_Prop, na.rm = TRUE),
              se_app = sd(App_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()))
          
          
openlow_vascular_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_vascular_turnover.xlsx")
openlow_lichen_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_lichen_turnover.xlsx")
openlow_nonvasc_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_nonvasc_turnover.xlsx")

openlow_vascular_turnover %>%
  summarise(
    total = mean(Total_Vasc_Prop, na.rm = TRUE),
    se_total = sd(Total_Vasc_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Vasc_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Vasc_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Vasc_Prop, na.rm = TRUE),
    se_app = sd(App_Vasc_Prop, na.rm = TRUE)/sqrt(n()))

openlow_lichen_turnover %>%
  summarise(
    total = mean(Total_Lichen_Prop, na.rm = TRUE),
    se_total = sd(Total_Lichen_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Lichen_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Lichen_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Lichen_Prop, na.rm = TRUE),
    se_app = sd(App_Lichen_Prop, na.rm = TRUE)/sqrt(n()))

openlow_nonvasc_turnover %>%
  summarise(
    total = mean(Total_Nonvasc_Prop, na.rm = TRUE),
    se_total = sd(Total_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Nonvasc_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Nonvasc_Prop, na.rm = TRUE),
    se_app = sd(App_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()))

needle_vascular_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/needle_vascular_turnover.xlsx")
needle_lichen_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/needle_lichen_turnover.xlsx")
needle_nonvasc_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/needle_nonvasc_turnover.xlsx")

needle_vascular_turnover %>%
  summarise(
    total = mean(Total_Vasc_Prop, na.rm = TRUE),
    se_total = sd(Total_Vasc_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Vasc_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Vasc_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Vasc_Prop, na.rm = TRUE),
    se_app = sd(App_Vasc_Prop, na.rm = TRUE)/sqrt(n()))

needle_lichen_turnover %>%
  summarise(
    total = mean(Total_Lichen_Prop, na.rm = TRUE),
    se_total = sd(Total_Lichen_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Lichen_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Lichen_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Lichen_Prop, na.rm = TRUE),
    se_app = sd(App_Lichen_Prop, na.rm = TRUE)/sqrt(n()))

needle_nonvasc_turnover %>%
  summarise(
    total = mean(Total_Nonvasc_Prop, na.rm = TRUE),
    se_total = sd(Total_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Nonvasc_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Nonvasc_Prop, na.rm = TRUE),
    se_app = sd(App_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()))


beetle_vascular_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/beetle_vascular_turnover.xlsx")
beetle_lichen_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/beetle_lichen_turnover.xlsx")
beetle_nonvasc_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/beetle_nonvasc_turnover.xlsx")

beetle_vascular_turnover %>%
  summarise(
    total = mean(Total_Vasc_Prop, na.rm = TRUE),
    se_total = sd(Total_Vasc_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Vasc_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Vasc_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Vasc_Prop, na.rm = TRUE),
    se_app = sd(App_Vasc_Prop, na.rm = TRUE)/sqrt(n()))

beetle_lichen_turnover %>%
  summarise(
    total = mean(Total_Lichen_Prop, na.rm = TRUE),
    se_total = sd(Total_Lichen_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Lichen_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Lichen_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Lichen_Prop, na.rm = TRUE),
    se_app = sd(App_Lichen_Prop, na.rm = TRUE)/sqrt(n()))

beetle_nonvasc_turnover %>%
  summarise(
    total = mean(Total_Nonvasc_Prop, na.rm = TRUE),
    se_total = sd(Total_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Nonvasc_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Nonvasc_Prop, na.rm = TRUE),
    se_app = sd(App_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()))




dwarfscrub_vascular_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/dwarfscrub_vascular_turnover.xlsx")
dwarfscrub_lichen_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/dwarfscrub_lichen_turnover.xlsx")
dwarfscrub_nonvasc_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/dwarfscrub_nonvasc_turnover.xlsx") 

dwarfscrub_vascular_turnover %>%
  summarise(
    total = mean(Total_Vasc_Prop, na.rm = TRUE),
    se_total = sd(Total_Vasc_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Vasc_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Vasc_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Vasc_Prop, na.rm = TRUE),
    se_app = sd(App_Vasc_Prop, na.rm = TRUE)/sqrt(n()))

dwarfscrub_lichen_turnover %>%
  summarise(
    total = mean(Total_Lichen_Prop, na.rm = TRUE),
    se_total = sd(Total_Lichen_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Lichen_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Lichen_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Lichen_Prop, na.rm = TRUE),
    se_app = sd(App_Lichen_Prop, na.rm = TRUE)/sqrt(n()))

dwarfscrub_nonvasc_turnover %>%
  summarise(
    total = mean(Total_Nonvasc_Prop, na.rm = TRUE),
    se_total = sd(Total_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Nonvasc_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Nonvasc_Prop, na.rm = TRUE),
    se_app = sd(App_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()))



forest_vascular_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/forest_vascular_turnover.xlsx")
forest_lichen_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/forest_lichen_turnover.xlsx")
forest_nonvasc_turnover <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/forest_nonvasc_turnover.xlsx") 

forest_vascular_turnover %>%
  summarise(
    total = mean(Total_Vasc_Prop, na.rm = TRUE),
    se_total = sd(Total_Vasc_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Vasc_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Vasc_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Vasc_Prop, na.rm = TRUE),
    se_app = sd(App_Vasc_Prop, na.rm = TRUE)/sqrt(n()))

forest_lichen_turnover %>%
  summarise(
    total = mean(Total_Lichen_Prop, na.rm = TRUE),
    se_total = sd(Total_Lichen_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Lichen_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Lichen_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Lichen_Prop, na.rm = TRUE),
    se_app = sd(App_Lichen_Prop, na.rm = TRUE)/sqrt(n()))

forest_nonvasc_turnover %>%
  summarise(
    total = mean(Total_Nonvasc_Prop, na.rm = TRUE),
    se_total = sd(Total_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()),
    disapp = mean(Disapp_Nonvasc_Prop, na.rm = TRUE), 
    se_disapp = sd(Disapp_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()),
    app = mean(App_Nonvasc_Prop, na.rm = TRUE),
    se_app = sd(App_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()))
