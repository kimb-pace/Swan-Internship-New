library(tidyr)
library(codyn)
library(dplyr)
library(writexl)
library(readxl)

taxa_filtered <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/taxa_filtered.xlsx")

#Official turnover code 

#Vascular ------------------------------------------------------------------------------------------------------------------------------------------------------------
        
      #Load data frames from PERMANOVA and NMDS
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
      #combine them 
        combined_data <- bind_rows(
          first_data %>% mutate(Visit = "First"),  
          last_data %>% mutate(Visit = "Last"))
      #check what columns are there 
        str(combined_data)
      #move visit to front 
        combined_data <- combined_data %>% select(Visit, everything())
      #remove unnecessary columns 
        combined_data <- subset(combined_data, select = -c(Park, Vegetation_Class, Viereck.2, Viereck.3, Plot_Year))
      #check that only visit, sample_year, and plot remain 
        str(combined_data) 
        
      #pivot to long format so it can fit into the function, use values_drop_na to get only rows with species present
        long_data <- combined_data %>%
          pivot_longer(cols = -c(Plot, Visit, Sample_Year), 
                       names_to = "Code1",                 
                       values_to = "Presence",              
                       values_drop_na = TRUE)                    
        
      #make sure year is numeric 
        long_data$Sample_Year <- as.numeric(as.character(long_data$Sample_Year))
        long_data <- long_data %>% distinct() #remove duplicate rows 
        
      #calculate the values for total turnover (losses + gains)
        turnover_total <- turnover(df = long_data, 
                                   time.var = "Sample_Year", 
                                   species.var = "Code1", 
                                   abundance.var = "Presence", 
                                   replicate.var = "Plot", 
                                   metric = "total")
        #this is the proportion of species that differ between the two time points either by addition or subtraction  
        
      #Calculate the values for proportion of species that appeared in the second time point 
        turnover_disappearance <- turnover(
          df = long_data,
          time.var = "Sample_Year",    
          species.var = "Code1",  
          abundance.var = "Presence",  
          replicate.var = "Plot",      
          metric = "disappearance")
        #proportion of species that appeared relative to the total number of species observed across both time points 
        
      #Calculate teh values for proportion of species that disappeared in the seconf time point 
        turnover_appearance <- turnover(
          df = long_data,
          time.var = "Sample_Year",    
          species.var = "Code1",  
          abundance.var = "Presence",  
          replicate.var = "Plot",      
          metric = "appearance")
        #proportion of species that disappeared relative to the total number of species observed across both time points 
        
      #View results 
        turnover_total
        turnover_disappearance
        turnover_appearance
      
      #Combine results into one data frame 
        turnover_joined <- left_join(turnover_total, turnover_appearance, by = "Plot")
        turnover_joined <- left_join(turnover_joined, turnover_disappearance, by = "Plot")
        
      #make the dataframe look better and clean it up 
        turnover_joined <- turnover_joined[,c(1,3,4,6)]
        turnover_joined <- turnover_joined %>% select(Plot, everything())
        
      #Rename the columns 
        turnover_joined <- turnover_joined %>% rename(Total_Vasc_Prop = total)
        turnover_joined <- turnover_joined %>% rename(Disapp_Vasc_Prop = disappearance)
        turnover_joined <- turnover_joined %>% rename(App_Vasc_Prop = appearance)
      
      #Verify the changes you made 
        turnover_joined

#getting species list (number of plots species disappeared and appeared at)
      #Make a visit column 
        long_data <- long_data %>%
          mutate(visit = factor(Visit, levels = c("First", "Last")))
      #Swap abundance to presence-absence 
        long_data <- long_data %>%
          mutate(across(5, ~ ifelse(. >0, 1, 0)))
        
      #Calculate number of plots disappeared 
        disappeared_species_summary <- long_data %>%
          group_by(Code1, Plot) %>%
          summarize(First = max(Presence[visit == "First"], na.rm = TRUE),
                    Last = max(Presence[visit == "Last"], na.rm = TRUE), .groups = "drop") %>%
          filter(First > 0 & Last == 0) %>%
          group_by(Code1) %>%
          summarize(Plots_Disappeared = n(), .groups = "drop")
      
      #Calculate number of plots appeared 
        appeared_species_summary <- long_data %>%
          group_by(Code1, Plot) %>%
          summarize(First = max(Presence[visit == "First"], na.rm = TRUE),
                    Last = max(Presence[visit == "Last"], na.rm = TRUE), .groups = "drop") %>%
          filter(First == 0 & Last > 0) %>%
          group_by(Code1) %>%
          summarize(Plots_Appeared = n(), .groups = "drop")
      
      #join them together 
        df_changes_summary <- full_join(disappeared_species_summary, appeared_species_summary, by = "Code1") %>%
          replace_na(list(plots_disappeared = 0, plots_appeared = 0))
        
      #Clean up and add species names 
        df_changes_summary <- df_changes_summary %>%
          left_join(taxa_filtered %>% select(Genus, Species, Code1), by = "Code1")
        df_changes_summary <-df_changes_summary %>% distinct()
        df_changes_summary <- df_changes_summary %>% select(Genus, Species, everything())
      #Export                                                                                              #DF CHANGE BELOW
        write_xlsx(df_changes_summary, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_vascular_specieslist.xlsx")
        write_xlsx(turnover_joined, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_vascular_turnover.xlsx")


#Lichen ------------------------------------------------------------------------------------------------------------------------------------------------------------------

    #Load data frames from PERMANOVA and NMDS 
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
        
      #combine data frames 
        combined_data <- bind_rows(
          first_data %>% mutate(Visit = "First"),  
          last_data %>% mutate(Visit = "Last"))
        
      #Clean up 
        combined_data <- combined_data %>% select(Visit, everything())
        combined_data <- subset(combined_data, select = -c(Park, Vegetation_Class, Plot_Year, Viereck.1, Viereck.4,
                                                           Viereck.2, Viereck.3))
      #Check to make sure only plot, year, and visit are there   
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
        
      #calculate the values for total turnover
        turnover_total <- turnover(df = long_data, 
                                   time.var = "Sample_Year", 
                                   species.var = "Code1", 
                                   abundance.var = "Presence", 
                                   replicate.var = "Plot", 
                                   metric = "total")
        #proportion of species that differ between the two time points 
        
      #Calculate total disappearance proportions 
        turnover_disappearance <- turnover(
          df = long_data,
          time.var = "Sample_Year",    
          species.var = "Code1",  
          abundance.var = "Presence",  
          replicate.var = "Plot",      
          metric = "disappearance")
        #proportion of species that appeared relative to the total number of species observed across both time points 
        
      #Calculate total appearance proportions 
        turnover_appearance <- turnover(
          df = long_data,
          time.var = "Sample_Year",    
          species.var = "Code1",  
          abundance.var = "Presence",  
          replicate.var = "Plot",      
          metric = "appearance")
        #proportion of species that disappeared relative to the total number of species observed across both time points 
        
      #View results 
        turnover_total
        turnover_disappearance
        turnover_appearance
        
      #Combine results 
        turnover_joined <- left_join(turnover_total, turnover_appearance, by = "Plot")
        turnover_joined <- left_join(turnover_joined, turnover_disappearance, by = "Plot")
        
      #Clean up dataframe 
        turnover_joined <- turnover_joined[,c(1,3,4,6)]
        turnover_joined <- turnover_joined %>% select(Plot, everything())
        
      #Rename columns 
        turnover_joined <- turnover_joined %>% rename(Total_Lichen_Prop = total)
        turnover_joined <- turnover_joined %>% rename(Disapp_Lichen_Prop = disappearance)
        turnover_joined <- turnover_joined %>% rename(App_Lichen_Prop = appearance)
      
      #View results 
        turnover_joined

  #getting species list (number of plots species disappeared and appeared at)
      #Make visit column 
        long_data <- long_data %>%
          mutate(visit = factor(Visit, levels = c("First", "Last")))
      #convert abundance to presence-absence 
        long_data <- long_data %>%
          mutate(across(5, ~ ifelse(. >0, 1, 0)))
        
      #Calculate number of plots disappeared
        disappeared_species_summary <- long_data %>%
          group_by(Code1, Plot) %>%
          summarize(First = max(Presence[visit == "First"], na.rm = TRUE),
                    Last = max(Presence[visit == "Last"], na.rm = TRUE), .groups = "drop") %>%
          filter(First > 0 & Last == 0) %>%
          group_by(Code1) %>%
          summarize(Plots_Disappeared = n(), .groups = "drop")
      
      #Calculate number of plots appeared  
        appeared_species_summary <- long_data %>%
          group_by(Code1, Plot) %>%
          summarize(First = max(Presence[visit == "First"], na.rm = TRUE),
                    Last = max(Presence[visit == "Last"], na.rm = TRUE), .groups = "drop") %>%
          filter(First == 0 & Last > 0) %>%
          group_by(Code1) %>%
          summarize(Plots_Appeared = n(), .groups = "drop")
     
       #Combine 
        df_changes_summary <- full_join(disappeared_species_summary, appeared_species_summary, by = "Code1") %>%
          replace_na(list(plots_disappeared = 0, plots_appeared = 0))
        print(df_changes_summary)
        
      #add species names and clean up 
        df_changes_summary <- df_changes_summary %>%
          left_join(taxa_filtered %>% select(Genus, Species, Code1), by = "Code1")
        df_changes_summary <-df_changes_summary %>% distinct()
        df_changes_summary <- df_changes_summary %>% select(Genus, Species, everything())
      
      #Export                                                                                                     #DF CHANGE
        write_xlsx(df_changes_summary, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_lichen_specieslist.xlsx")
        write_xlsx(turnover_joined, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_lichen_turnover.xlsx")



#Nonvascular ----------------------------------------------------------------------------------------------------------------------------------------------------------------

        #Import data from PERMANOVA and NMDS 
          needle_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_nonvasc_filtered.xlsx")
          forest_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_nonvasc_filtered.xlsx")
          beetle_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_nonvasc_filtered.xlsx")
          dwarfscrub_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_nonvasc_filtered.xlsx")
          openlow_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_nonvasc_filtered.xlsx")
          alpine_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_nonvasc_filtered.xlsx")
          
          
      #get the first and most recent sample year for each plot (excluding plots visited only once)
          first_last_years <- openlow_df_nonvasc %>%                                                              #DF CHANGE
            distinct(Plot, Sample_Year) %>%
            group_by(Plot) %>%
            summarise(
              first_year = min(Sample_Year),
              last_year = max(Sample_Year)) %>%
            filter(first_year != last_year)
          first_data <- openlow_df_nonvasc %>%                                                                    #DF CHANGE
            inner_join(first_last_years %>% select(Plot, first_year), 
                       by = c("Plot" = "Plot", "Sample_Year" = "first_year"))
          last_data <- openlow_df_nonvasc %>%                                                                     #DF CHANGE
            inner_join(first_last_years %>% select(Plot, last_year), 
                       by = c("Plot" = "Plot", "Sample_Year" = "last_year"))
          
        #combine 
          combined_data <- bind_rows(
            first_data %>% mutate(Visit = "First"),  
            last_data %>% mutate(Visit = "Last"))
          
        #Clean up 
          combined_data <- combined_data %>% select(Visit, everything())
        #See what columns are there 
          str(combined_data)
        #Remove everything that's not plot, visit, and sample year (excluding data)
          combined_data <- subset(combined_data, select = -c(Park, Plot_Year, Vegetation_Class, 
                                                             Viereck.1, Viereck.4,
                                                             Viereck.2, Viereck.3))
        #verify those that remain 
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
          
        #calculate the values for total turnover
          turnover_total <- turnover(df = long_data, 
                                     time.var = "Sample_Year", 
                                     species.var = "Code1", 
                                     abundance.var = "Presence", 
                                     replicate.var = "Plot", 
                                     metric = "total")
          
        #calculate the values for disappearance 
          turnover_disappearance <- turnover(
            df = long_data,
            time.var = "Sample_Year",    
            species.var = "Code1",  
            abundance.var = "Presence",  
            replicate.var = "Plot",      
            metric = "disappearance")
          
        #calculate the values for total appearance
          turnover_appearance <- turnover(
            df = long_data,
            time.var = "Sample_Year",    
            species.var = "Code1",  
            abundance.var = "Presence",  
            replicate.var = "Plot",      
            metric = "appearance")
          
        #View results 
          turnover_total
          turnover_disappearance
          turnover_appearance
          
        #Combine results 
          turnover_joined <- left_join(turnover_total, turnover_appearance, by = "Plot")
          turnover_joined <- left_join(turnover_joined, turnover_disappearance, by = "Plot")
          
        #Clean up dataframe 
          turnover_joined <- turnover_joined[,c(1,3,4,6)]
          turnover_joined <- turnover_joined %>% select(Plot, everything())
          
        #rename columns 
          turnover_joined <- turnover_joined %>% rename(Total_Nonvasc_Prop = total)
          turnover_joined <- turnover_joined %>% rename(Disapp_Nonvasc_Prop = disappearance)
          turnover_joined <- turnover_joined %>% rename(App_Nonvasc_Prop = appearance)
          
    #getting species list (number of plots species disappeared and appeared at)
        #Create visit column 
          long_data <- long_data %>%
            mutate(visit = factor(Visit, levels = c("First", "Last")))
        #transform abundance to presence-absence 
          long_data <- long_data %>%
            mutate(across(5, ~ ifelse(. >0, 1, 0)))
          
        #Calculate number of plots disappeared 
          disappeared_species_summary <- long_data %>%
            group_by(Code1, Plot) %>%
            summarize(First = max(Presence[visit == "First"], na.rm = TRUE),
                      Last = max(Presence[visit == "Last"], na.rm = TRUE), .groups = "drop") %>%
            filter(First > 0 & Last == 0) %>%
            group_by(Code1) %>%
            summarize(Plots_Disappeared = n(), .groups = "drop")
          
        #Calculate number of plots appeared
          appeared_species_summary <- long_data %>%
            group_by(Code1, Plot) %>%
            summarize(First = max(Presence[visit == "First"], na.rm = TRUE),
                      Last = max(Presence[visit == "Last"], na.rm = TRUE), .groups = "drop") %>%
            filter(First == 0 & Last > 0) %>%
            group_by(Code1) %>%
            summarize(Plots_Appeared = n(), .groups = "drop")
          
        #Combine 
          df_changes_summary <- full_join(disappeared_species_summary, appeared_species_summary, by = "Code1") %>%
            replace_na(list(plots_disappeared = 0, plots_appeared = 0))
          
        #Add species names and clean up 
          df_changes_summary <- df_changes_summary %>%
            left_join(taxa_filtered %>% select(Genus, Species, Code1), by = "Code1")
          df_changes_summary <-df_changes_summary %>% distinct()
          df_changes_summary <- df_changes_summary %>% select(Genus, Species, everything())
          
      #Export                                                                                                 #DF CHANGE 
          write_xlsx(df_changes_summary, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_nonvasc_specieslist.xlsx")
          write_xlsx(turnover_joined, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Turnover_Analysis/openlow_nonvasc_turnover.xlsx")

---------------------------------------------------------------------------------------------------------------------------------------------------------
#Calculating average turnover and standard error for each veg class

            
#Alpine 

  #Load data 
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
   
#Low Shrub        
         
  #Load Data 
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

#Spruce Woodland 
        
    #Load Data 
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


#Spruce Beetle Disturbed 
          
    #Load Data 
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



#Dwarf Shrub 
  
   #Load Data 
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


#Spruce Forest 
          
   #Load Data 
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

----------------------------------------------------------------------------------------------------------------------------------------------------------------

#Calculating species richness for each vegetation class (Total, and by park)

#Load Data from permanova folder 
    beetle_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_vasc_filtered.xlsx")
    beetle_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_lichen_filtered.xlsx")
    beetle_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_nonvasc_filtered.xlsx")
    
    needle_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_vasc_filtered.xlsx")
    needle_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_lichen_filtered.xlsx")
    needle_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_nonvasc_filtered.xlsx")
    
    openlow_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_vasc_filtered.xlsx")
    openlow_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_lichen_filtered.xlsx")
    openlow_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_nonvasc_filtered.xlsx")
    
    dwarfscrub_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_vasc_filtered.xlsx")
    dwarfscrub_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_lichen_filtered.xlsx")
    dwarfscrub_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_nonvasc_filtered.xlsx") 
    
    forest_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_vasc_filtered.xlsx")
    forest_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_lichen_filtered.xlsx")
    forest_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_nonvasc_filtered.xlsx") 
    
    alpine_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_vasc_filtered.xlsx")
    alpine_df_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_lichen_filtered.xlsx")
    alpine_df_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_nonvasc_filtered.xlsx")

#Filter by park 
      KATM_needle_vasc <- needle_df %>%
        filter(Park == "KATM")
      LACL_needle_vasc <- needle_df %>%
        filter(Park == "LACL")
      
      KATM_needle_lichen <- needle_df_lichen %>%
        filter(Park == "KATM")
      LACL_needle_lichen <- needle_df_lichen %>%
        filter(Park == "LACL")
      
      KATM_needle_nonvasc <- needle_df_nonvasc %>%
        filter(Park == "KATM")
      LACL_needle_nonvasc <- needle_df_nonvasc %>%
        filter(Park == "LACL")

#TOTAL -------------------------------------------------------------------------
      summary_vasc <- needle_df %>%
        select(8:261) %>%
        summarise(
          nonzero_cols = sum(colSums(. != 0) > 0),
          all_zero_cols = sum(colSums(. != 0) == 0)
        )
      print(summary_vasc)
      
      summary_lichen <- needle_df_lichen %>%
        select(10:167) %>%
        summarise(
          nonzero_cols = sum(colSums(. != 0) > 0),
          all_zero_cols = sum(colSums(. != 0) == 0)
        )
      print(summary_lichen)
      
      summary_nonvasc <- needle_df_nonvasc %>%
        select(10:210) %>%
        summarise(
          nonzero_cols = sum(colSums(. != 0) > 0),
          all_zero_cols = sum(colSums(. != 0) == 0)
        )
      print(summary_nonvasc)

#LACL --------------------------------------------------------------------------
      summary_vasc <- LACL_needle_vasc %>%
        select(8:261) %>%
        summarise(
          nonzero_cols = sum(colSums(. != 0) > 0),
          all_zero_cols = sum(colSums(. != 0) == 0)
        )
      print(summary_vasc)
      
      summary_lichen <- LACL_needle_lichen %>%
        select(10:167) %>%
        summarise(
          nonzero_cols = sum(colSums(. != 0) > 0),
          all_zero_cols = sum(colSums(. != 0) == 0)
        )
      print(summary_lichen)
      
      summary_nonvasc <- LACL_needle_nonvasc %>%
        select(10:210) %>%
        summarise(
          nonzero_cols = sum(colSums(. != 0) > 0),
          all_zero_cols = sum(colSums(. != 0) == 0)
        )
      print(summary_nonvasc)

#KATM --------------------------------------------------------------------------
      summary_vasc <- KATM_needle_vasc %>%
        select(8:261) %>%
        summarise(
          nonzero_cols = sum(colSums(. != 0) > 0),
          all_zero_cols = sum(colSums(. != 0) == 0)
        )
      print(summary_vasc) 
      
      summary_lichen <- KATM_needle_lichen %>%
        select(10:167) %>%
        summarise(
          nonzero_cols = sum(colSums(. != 0) > 0),
          all_zero_cols = sum(colSums(. != 0) == 0)
        )
      print(summary_lichen)
      
      summary_nonvasc <- KATM_needle_nonvasc %>%
        select(10:210) %>%
        summarise(
          nonzero_cols = sum(colSums(. != 0) > 0),
          all_zero_cols = sum(colSums(. != 0) == 0)
        )
      print(summary_nonvasc)

------------------------------------------------------------------------------------------------------------------------------------------------------------
#Creation of species richness dataframes 

        #make a genus_species column on both taxa and simple_taxa 
        taxa <- taxa %>% mutate(Genus_Species = paste(Genus, Species, sep="_"))
      taxa_filtered <- taxa_filtered %>% mutate(Genus_Species = paste(Genus, Species, sep="_"))
      simple_taxa <- simple_taxa %>% mutate(Genus_Species = paste(Genus, Species, sep="_"))
      
      #add the Code1 column to taxa 
      taxa_filtered <- taxa_filtered %>%
        left_join(simple_taxa %>% select(Genus_Species, Code1),
                  by = "Genus_Species")
      
      taxa_filtered <- taxa_filtered %>% rename(new_column_name = old_column_name)
      write_xlsx(taxa_filtered, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/taxa_filtered.xlsx")
      
      
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
        
      lichens_only <- quad_freq %>% filter(Vascular_Code %in% c("Lichen"))
      nonvasc_only <- quad_freq %>% filter(Vascular_Code %in% c("Nonvascular"))
        
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
        

      
    #Calculate species richness 
      #Lichen 
        lichen_sp_richness <- lichen_only_filtered %>%
        group_by(Plot, Sample_Year) %>% 
        summarize(Species_Richness = n_distinct(Code1), .groups = "keep") 
        lichen_sp_richness <- lichen_sp_richness %>% mutate(Plot_Year = paste(Plot, Sample_Year, sep="_"))
      
      #Vascular 
      vasc_sp_richness <- vascular_only_filtered %>%
        group_by(Plot, Sample_Year) %>% 
        summarize(Species_Richness = n_distinct(Code1), .groups = "keep") 
        vasc_sp_richness <- vasc_sp_richness %>% mutate(Plot_Year = paste(Plot, Sample_Year, sep="_"))
      
      #Nonvascular 
      nonvasc_sp_richness <- nonvasc_only_filtered %>%
        group_by(Plot, Sample_Year) %>% 
        summarize(Species_Richness = n_distinct(Code1), .groups = "keep") 
        nonvasc_sp_richness <- nonvasc_sp_richness %>% mutate(Plot_Year = paste(Plot, Sample_Year, sep="_"))
      
      #Export 
        write_xlsx(lichen_sp_richness, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/lichen_sp_richness_filtered.xlsx")
        write_xlsx(vasc_sp_richness, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/vascular_sp_richness_filtered.xlsx")
        write_xlsx(nonvasc_sp_richness, "T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/nonvasc_sp_richness_filtered.xlsx")
      
        
        
        
    #overall species richness spreadsheet
        # Renaming column 'A' to 'X'
        vasc_sp_richness <- vasc_sp_richness %>% rename(Vasc_Species_Richness = Species_Richness)
        lichen_sp_richness <- lichen_sp_richness %>% rename(Lichen_Species_Richness = Species_Richness)
        nonvasc_sp_richness <- nonvasc_sp_richness %>% rename(Nonvasc_Species_Richness = Species_Richness)
        
        combined_df <- vasc_sp_richness %>%
          left_join(lichen_sp_richness, by = c("Plot", "Sample_Year"))
        
        
        
        combined_df <- combined_df %>%
          left_join(nonvasc_sp_richness, by = c("Plot", "Sample_Year"))
        combined_df <- combined_df %>%
          left_join(viereck %>% select(Plot, Sample_Year, Vegetation_Class, Viereck.3), by = c("Plot", "Sample_Year"))
        str(viereck)
        viereck$Sample_Year <- as.numeric(viereck$Sample_Year)
        str(combined_df)  
        
        
        combined_df <- combined_df[, -4]
        combined_df <- combined_df[, -5]
        combined_df <- combined_df[, -6]
        
        
        species_richness_collapsed_codes <- read.csv("T:/Users/KPace/Species_Richness_Collapsed_Codes.csv")
        viereck <- read.csv("T:/Users/KPace/Swan-Internship-New/Data/Unmodified/Viereck_Classes.csv")
        species_richness_collapsed_codes <- species_richness_collapsed_codes %>%
          left_join(viereck %>% select(Plot, Sample_Year, Sample_Date), by = c("Plot", "Sample_Year"))
        write.csv(species_richness_collapsed_codes, "T:/Users/KPace/Species_Richness_Collapsed_Codes.csv")     
        

-------------------------------------------------------------------------------------------------------------------------------------------------------------
        
#Generalized Linear Mixed Model Analysis - Species Richness 
     
#Setup    
  library(lme4)
  library(Matrix)
  library(glmmTMB)
  library(MASS)
  library(ggplot2)
  library(readxl)
  library(DHARMa)      
        
---------------------------------------------------------------   
#Open Low Shrub 
        openlow_env <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_env_vasc_filtered.xlsx")
        
    #Vascular 
        #scale sample year to remove warnings 
        openlow_env$Sample_Year <- as.numeric(openlow_env$Sample_Year)
        openlow_env$Sample_Year_Scaled <- scale(openlow_env$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_vasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                 data = openlow_env, family = poisson)
        summary(model_pois_vasc)
        
        pearson_overdisp <- sum(residuals(model_pois_vasc, type = "pearson")^2) / df.residual(model_pois_vasc) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_vasc) / df.residual(model_pois_vasc) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_vasc@optinfo$conv #0 = converged, 1 = not converged
        
        confint(model_pois_vasc)
        #negative interval
        exp(-0.06847906/sd(openlow_env$Sample_Year))
        (0.9829035-1)*100 #-1.71
        #positive interval 
        exp(0.0562481/sd(openlow_env$Sample_Year))
        (1.014265-1)*100 #1.01
        
        original_slope <- fixef(model_pois_vasc)["Sample_Year_Scaled"]/sd(openlow_env$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.00158) -1)*100 #=percent change 
        
    #Lichen 
        openlow_env_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_env_lichen_filtered.xlsx")
        #scale sample year to remove warnings 
        openlow_env_lichen$Sample_Year <- as.numeric(openlow_env_lichen$Sample_Year)
        openlow_env_lichen$Sample_Year_Scaled <- scale(openlow_env_lichen$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_lichen <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                   data = openlow_env_lichen, family = poisson)
        summary(model_pois_lichen)
        
        pearson_overdisp <- sum(residuals(model_pois_lichen, type = "pearson")^2) / df.residual(model_pois_lichen) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_lichen) / df.residual(model_pois_lichen) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_lichen@optinfo$conv #0 = converged, 1 = not converged
        
        confint(model_pois_lichen)
        #negative interval
        exp(-0.12536944/sd(openlow_env_lichen$Sample_Year))
        (0.9730882-1)*100 #-2.69
        #positive interval 
        exp(-0.01917884/sd(openlow_env_lichen$Sample_Year))
        (0.9958354-1)*100 #-0.42
        
        original_slope <- fixef(model_pois_lichen)["Sample_Year_Scaled"]/sd(openlow_env_lichen$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.0158) -1)*100 #=percent change 
        
    #Nonvascular 
        openlow_env_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_env_nonvasc_filtered.xlsx")
        #scale sample year to remove warnings 
        openlow_env_nonvasc$Sample_Year <- as.numeric(openlow_env_nonvasc$Sample_Year)
        openlow_env_nonvasc$Sample_Year_Scaled <- scale(openlow_env_nonvasc$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_nonvasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                    data = openlow_env_nonvasc, family = poisson)
        summary(model_pois_nonvasc)
        
        pearson_overdisp <- sum(residuals(model_pois_nonvasc, type = "pearson")^2) / df.residual(model_pois_nonvasc) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_nonvasc) / df.residual(model_pois_nonvasc) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_nonvasc@optinfo$conv #0 = converged, 1 = not converged
        
        confint(model_pois_nonvasc)
        #negative interval
        exp(-0.1426947/sd(openlow_env_nonvasc$Sample_Year))
        (0.9694265-1)*100 #-3.06
        #positive interval 
        exp(0.005000223/sd(openlow_env_nonvasc$Sample_Year))
        (1.001089-1)*100 #0.11
        
        original_slope <- fixef(model_pois_nonvasc)["Sample_Year_Scaled"]/sd(openlow_env_nonvasc$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.0151) -1)*100 #=percent change 
        
        
---------------------------------------------------------------        
        
#Spruce Woodland 
        needle_env <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_env_vasc_filtered.xlsx")
        
    #Vascular 
        #scale sample year to remove warnings 
        needle_env$Sample_Year <- as.numeric(needle_env$Sample_Year)
        needle_env$Sample_Year_Scaled <- scale(needle_env$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_vasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                 data = needle_env, family = poisson)
        summary(model_pois_vasc)
        
        pearson_overdisp <- sum(residuals(model_pois_vasc, type = "pearson")^2) / df.residual(model_pois_vasc) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_vasc) / df.residual(model_pois_vasc) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_vasc@optinfo$conv #0 = converged, 1 = not converged
        
        confint(model_pois_vasc)
        #negative interval
        exp(-0.07878733/sd(needle_env$Sample_Year))
        (0.9771621-1)*100 #-2.28
        #positive interval 
        exp(0.05138275/sd(needle_env$Sample_Year))
        (1.015181-1)*100 #1.52
        
        original_slope <- fixef(model_pois_vasc)["Sample_Year_Scaled"]/sd(needle_env$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.00411)-1)*100 #=percent change 
        
    #Lichen 
        needle_env_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_env_lichen_filtered.xlsx")
        #scale sample year to remove warnings 
        needle_env_lichen$Sample_Year <- as.numeric(needle_env_lichen$Sample_Year)
        needle_env_lichen$Sample_Year_Scaled <- scale(needle_env_lichen$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_lichen <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                   data = needle_env_lichen, family = poisson)
        summary(model_pois_lichen)
        
        pearson_overdisp <- sum(residuals(model_pois_lichen, type = "pearson")^2) / df.residual(model_pois_lichen) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_lichen) / df.residual(model_pois_lichen) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_lichen@optinfo$conv #0 = converged, 1 = not converged
        
        confint(model_pois_lichen)
        #negative interval
        exp(-0.1427322/sd(needle_env_lichen$Sample_Year))
        (0.96139-1)*100 #-3.86
        ((exp(-0.1427322/sd(needle_env_lichen$Sample_Year)))-1)*100
        #positive interval 
        exp(-0.0376746/sd(needle_env_lichen$Sample_Year))
        (0.9896606-1)*100 #-1.03
        
        original_slope <- fixef(model_pois_lichen)["Sample_Year_Scaled"]/sd(needle_env_lichen$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.0249) -1)*100 #=percent change 
        
    #Nonvascular 
        needle_env_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_env_nonvasc_filtered.xlsx")
        #scale sample year to remove warnings 
        needle_env_nonvasc$Sample_Year <- as.numeric(needle_env_nonvasc$Sample_Year)
        needle_env_nonvasc$Sample_Year_Scaled <- scale(needle_env_nonvasc$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_nonvasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                    data = needle_env_nonvasc, family = poisson)
        summary(model_pois_nonvasc)
        
        pearson_overdisp <- sum(residuals(model_pois_nonvasc, type = "pearson")^2) / df.residual(model_pois_nonvasc) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_nonvasc) / df.residual(model_pois_nonvasc) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_nonvasc@optinfo$conv #0 = converged, 1 = not converged
        
        original_slope <- fixef(model_pois_nonvasc)["Sample_Year_Scaled"]/sd(needle_env_nonvasc$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.0214) -1)*100 #=percent change  
        
        confint(model_pois_nonvasc)
        #negative interval
        ((exp(-0.1427495/sd(needle_env_nonvasc$Sample_Year)))-1)*100
        #positive interval 
        ((exp(-0.009377086/sd(needle_env_nonvasc$Sample_Year)))-1)*100
        
        
        
---------------------------------------------------------------   
#Spruce Forest 
        forest_env <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_env_vasc_filtered.xlsx")
    
    #Vascular 
        #scale sample year to remove warnings 
        forest_env$Sample_Year <- as.numeric(forest_env$Sample_Year)
        forest_env$Sample_Year_Scaled <- scale(forest_env$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_vasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                 data = forest_env, family = poisson)
        summary(model_pois_vasc)
        
        pearson_overdisp <- sum(residuals(model_pois_vasc, type = "pearson")^2) / df.residual(model_pois_vasc) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_vasc) / df.residual(model_pois_vasc) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_vasc@optinfo$conv #0 = converged, 1 = not converged
        
        original_slope <- fixef(model_pois_vasc)["Sample_Year_Scaled"]/sd(forest_env$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.0154) -1)*100 #=percent change 
        
        confint(model_pois_vasc)
        #negative interval
        ((exp(-0.1015123/sd(forest_env$Sample_Year)))-1)*100
        #positive interval 
        ((exp(0.0176431/sd(forest_env$Sample_Year)))-1)*100
        
        
    #Lichen 
        forest_env_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_env_lichen_filtered.xlsx")
        #scale sample year to remove warnings 
        forest_env_lichen$Sample_Year <- as.numeric(forest_env_lichen$Sample_Year)
        forest_env_lichen$Sample_Year_Scaled <- scale(forest_env_lichen$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_lichen <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                   data = forest_env_lichen, family = poisson)
        summary(model_pois_lichen)
        
        pearson_overdisp <- sum(residuals(model_pois_lichen, type = "pearson")^2) / df.residual(model_pois_lichen) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_lichen) / df.residual(model_pois_lichen) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_lichen@optinfo$conv #0 = converged, 1 = not converged
        
        original_slope <- fixef(model_pois_lichen)["Sample_Year_Scaled"]/sd(forest_env_lichen$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.0367) -1)*100 #=percent change 
        
        confint(model_pois_lichen)
        #negative interval
        ((exp(-0.1806259/sd(forest_env_lichen$Sample_Year)))-1)*100
        #positive interval 
        ((exp(-0.01977007/sd(forest_env_lichen$Sample_Year)))-1)*100
        
        
    #Nonvascular 
        forest_env_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_env_nonvasc_filtered.xlsx") 
        #scale sample year to remove warnings 
        forest_env_nonvasc$Sample_Year <- as.numeric(forest_env_nonvasc$Sample_Year)
        forest_env_nonvasc$Sample_Year_Scaled <- scale(forest_env_nonvasc$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_nonvasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                    data = forest_env_nonvasc, family = poisson)
        summary(model_pois_nonvasc)
        
        pearson_overdisp <- sum(residuals(model_pois_nonvasc, type = "pearson")^2) / df.residual(model_pois_nonvasc) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_nonvasc) / df.residual(model_pois_nonvasc) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_nonvasc@optinfo$conv #0 = converged, 1 = not converged
        
        original_slope <- fixef(model_pois_nonvasc)["Sample_Year_Scaled"]/sd(forest_env_nonvasc$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.0437) -1)*100 #=percent change 
        
        confint(model_pois_nonvasc)
        #negative interval
        ((exp(-0.20026648/sd(forest_env_nonvasc$Sample_Year)))-1)*100
        #positive interval 
        ((exp(-0.03815634/sd(forest_env_nonvasc$Sample_Year)))-1)*100
        
---------------------------------------------------------------           
#Beetle 
        beetle_env <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_env_vasc_filtered.xlsx")
        
      #Vascular 
        #scale sample year to remove warnings 
        beetle_env$Sample_Year <- as.numeric(beetle_env$Sample_Year)
        beetle_env$Sample_Year_Scaled <- scale(beetle_env$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_vasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                 data = beetle_env, family = poisson)
        summary(model_pois_vasc)
        
        pearson_overdisp <- sum(residuals(model_pois_vasc, type = "pearson")^2) / df.residual(model_pois_vasc) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_vasc) / df.residual(model_pois_vasc) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_vasc@optinfo$conv #0 = converged, 1 = not converged
        
        original_slope <- fixef(model_pois_vasc)["Sample_Year_Scaled"]/sd(beetle_env$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(0.00727) -1)*100 #=percent change 
        
        confint(model_pois_vasc)
        #negative interval
        ((exp(-0.05080881/sd(beetle_env$Sample_Year)))-1)*100
        #positive interval 
        ((exp(0.09308241/sd(beetle_env$Sample_Year)))-1)*100

    #Lichen
        beetle_env_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_env_lichen_filtered.xlsx")
        #scale sample year to remove warnings 
        beetle_env_lichen$Sample_Year <- as.numeric(beetle_env_lichen$Sample_Year)
        beetle_env_lichen$Sample_Year_Scaled <- scale(beetle_env_lichen$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_lichen <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                   data = beetle_env_lichen, family = poisson)
        summary(model_pois_lichen)
        
        pearson_overdisp <- sum(residuals(model_pois_lichen, type = "pearson")^2) / df.residual(model_pois_lichen) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_lichen) / df.residual(model_pois_lichen) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_lichen@optinfo$conv #0 = converged, 1 = not converged
        
        original_slope <- fixef(model_pois_lichen)["Sample_Year_Scaled"]/sd(beetle_env_lichen$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.0772) -1)*100 #=percent change 
        
        confint(model_pois_lichen)
        #negative interval
        ((exp(-0.3377355/sd(beetle_env_lichen$Sample_Year)))-1)*100
        #positive interval 
        ((exp(-0.1239696/sd(beetle_env_lichen$Sample_Year)))-1)*100

    #Nonvascular 
        beetle_env_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_env_nonvasc_filtered.xlsx")
        #scale sample year to remove warnings 
        beetle_env_nonvasc$Sample_Year <- as.numeric(beetle_env_nonvasc$Sample_Year)
        beetle_env_nonvasc$Sample_Year_Scaled <- scale(beetle_env_nonvasc$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_nonvasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                    data = beetle_env_nonvasc, family = poisson)
        summary(model_pois_nonvasc)
        
        pearson_overdisp <- sum(residuals(model_pois_nonvasc, type = "pearson")^2) / df.residual(model_pois_nonvasc) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_nonvasc) / df.residual(model_pois_nonvasc) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_nonvasc@optinfo$conv #0 = converged, 1 = not converged
        
        original_slope <- fixef(model_pois_nonvasc)["Sample_Year_Scaled"]/sd(beetle_env_nonvasc$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.0215) -1)*100 #=percent change 
        
        confint(model_pois_nonvasc)
        #negative interval
        ((exp(-0.147474/sd(beetle_env_nonvasc$Sample_Year)))-1)*100
        #positive interval 
        ((exp(0.01905762/sd(beetle_env_nonvasc$Sample_Year)))-1)*100

        
        
---------------------------------------------------------------           
#dwarfscrub
        dwarfscrub_env <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_env_vasc_filtered.xlsx")
        
      #Vascular 
        #scale sample year to remove warnings 
        dwarfscrub_env$Sample_Year <- as.numeric(dwarfscrub_env$Sample_Year)
        dwarfscrub_env$Sample_Year_Scaled <- scale(dwarfscrub_env$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_vasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                 data = dwarfscrub_env, family = poisson)
        summary(model_pois_vasc)
        
        pearson_overdisp <- sum(residuals(model_pois_vasc, type = "pearson")^2) / df.residual(model_pois_vasc) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_vasc) / df.residual(model_pois_vasc) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_vasc@optinfo$conv #0 = converged, 1 = not converged
        
        original_slope <- fixef(model_pois_vasc)["Sample_Year_Scaled"]/sd(dwarfscrub_env$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(0.0022) -1)*100 #=percent change 
        
        confint(model_pois_vasc)
        #negative interval
        ((exp(-0.03527206/sd(dwarfscrub_env$Sample_Year)))-1)*100
        #positive interval 
        ((exp(0.05808153/sd(dwarfscrub_env$Sample_Year)))-1)*100

    #Lichen 
        dwarfscrub_env_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_env_lichen_filtered.xlsx")
        #scale sample year to remove warnings 
        dwarfscrub_env_lichen$Sample_Year <- as.numeric(dwarfscrub_env_lichen$Sample_Year)
        dwarfscrub_env_lichen$Sample_Year_Scaled <- scale(dwarfscrub_env_lichen$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_lichen <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                   data = dwarfscrub_env_lichen, family = poisson)
        summary(model_pois_lichen)
        
        pearson_overdisp <- sum(residuals(model_pois_lichen, type = "pearson")^2) / df.residual(model_pois_lichen) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_lichen) / df.residual(model_pois_lichen) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_lichen@optinfo$conv #0 = converged, 1 = not converged
        
        original_slope <- fixef(model_pois_lichen)["Sample_Year_Scaled"]/sd(dwarfscrub_env_lichen$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.0156) -1)*100 #=percent change
        
        confint(model_pois_lichen)
        #negative interval
        ((exp(-0.1034390/sd(dwarfscrub_env_lichen$Sample_Year)))-1)*100
        #positive interval 
        ((exp(0.01536733/sd(dwarfscrub_env_lichen$Sample_Year)))-1)*100
        
    #Nonvascular 
        dwarfscrub_env_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_env_nonvasc_filtered.xlsx") 
        #scale sample year to remove warnings 
        dwarfscrub_env_nonvasc$Sample_Year <- as.numeric(dwarfscrub_env_nonvasc$Sample_Year)
        dwarfscrub_env_nonvasc$Sample_Year_Scaled <- scale(dwarfscrub_env_nonvasc$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_nonvasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                    data = dwarfscrub_env_nonvasc, family = poisson)
        summary(model_pois_nonvasc)
        
        pearson_overdisp <- sum(residuals(model_pois_nonvasc, type = "pearson")^2) / df.residual(model_pois_nonvasc) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_nonvasc) / df.residual(model_pois_nonvasc) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_nonvasc@optinfo$conv #0 = converged, 1 = not converged
        
        original_slope <- fixef(model_pois_nonvasc)["Sample_Year_Scaled"]/sd(dwarfscrub_env_nonvasc$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.00211) -1)*100 #=percent change 
        
        confint(model_pois_nonvasc)
        #negative interval
        ((exp(-0.08833292/sd(dwarfscrub_env_nonvasc$Sample_Year)))-1)*100
        #positive interval 
        ((exp(0.07659689/sd(dwarfscrub_env_nonvasc$Sample_Year)))-1)*100
        
---------------------------------------------------------------   
#alpine 
        alpine_env <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_env_vasc_filtered.xlsx")
        
      #Vascular 
        #scale sample year to remove warnings 
        alpine_env$Sample_Year <- as.numeric(alpine_env$Sample_Year)
        alpine_env$Sample_Year_Scaled <- scale(alpine_env$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_vasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                 data = alpine_env, family = poisson)
        summary(model_pois_vasc)
        
        pearson_overdisp <- sum(residuals(model_pois_vasc, type = "pearson")^2) / df.residual(model_pois_vasc) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_vasc) / df.residual(model_pois_vasc) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_vasc@optinfo$conv #0 = converged, 1 = not converged
        
        original_slope <- fixef(model_pois_vasc)["Sample_Year_Scaled"]/sd(alpine_env$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(0.00818) -1)*100 #=percent change 
        
        confint(model_pois_vasc)
        #negative interval
        ((exp(-0.01222514/sd(alpine_env$Sample_Year)))-1)*100
        #positive interval 
        ((exp(0.09459859/sd(alpine_env$Sample_Year)))-1)*100

    #Lichen 
        alpine_env_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_env_lichen_filtered.xlsx")
        #scale sample year to remove warnings 
        alpine_env_lichen$Sample_Year <- as.numeric(alpine_env_lichen$Sample_Year)
        alpine_env_lichen$Sample_Year_Scaled <- scale(alpine_env_lichen$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_lichen <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                   data = alpine_env_lichen, family = poisson)
        summary(model_pois_lichen)
        
        pearson_overdisp <- sum(residuals(model_pois_lichen, type = "pearson")^2) / df.residual(model_pois_lichen) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_lichen) / df.residual(model_pois_lichen) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_lichen@optinfo$conv #0 = converged, 1 = not converged
        
        original_slope <- fixef(model_pois_lichen)["Sample_Year_Scaled"]/sd(alpine_env_lichen$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.00951) -1)*100 #=percent change 
        
        confint(model_pois_lichen)
        #negative interval
        ((exp(-0.08255785/sd(alpine_env_lichen$Sample_Year)))-1)*100
        #positive interval 
        ((exp(0.03348602/sd(alpine_env_lichen$Sample_Year)))-1)*100

    #Nonvascular 
        alpine_env_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_env_nonvasc_filtered.xlsx")
        #scale sample year to remove warnings 
        alpine_env_nonvasc$Sample_Year <- as.numeric(alpine_env_nonvasc$Sample_Year)
        alpine_env_nonvasc$Sample_Year_Scaled <- scale(alpine_env_nonvasc$Sample_Year)
        
        #Poisson, assumes mean = variance 
        model_pois_nonvasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                                    data = alpine_env_nonvasc, family = poisson)
        summary(model_pois_nonvasc)
        
        pearson_overdisp <- sum(residuals(model_pois_nonvasc, type = "pearson")^2) / df.residual(model_pois_nonvasc) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois_nonvasc) / df.residual(model_pois_nonvasc) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        
        model_pois_nonvasc@optinfo$conv #0 = converged, 1 = not converged
        
        original_slope <- fixef(model_pois_nonvasc)["Sample_Year_Scaled"]/sd(alpine_env_nonvasc$Sample_Year)
        #calculate percent change per year using corrected coefficient 
        (exp(-0.0106) -1)*100 #=percent change
        
        confint(model_pois_nonvasc)
        #negative interval
        ((exp(-0.112528/sd(alpine_env_nonvasc$Sample_Year)))-1)*100
        #positive interval 
        ((exp(0.05784369/sd(alpine_env_nonvasc$Sample_Year)))-1)*100


        
------------------------------------------------------------------------------------
#MODEL COMPARISON 
       
  #checking the overdispersion statistics 
      #Poisson 
        pearson_overdisp <- sum(residuals(model_pois, type = "pearson")^2) / df.residual(model_pois) #pearson = how much each observation deviates from expected value
        deviance_overdisp <- deviance(model_pois) / df.residual(model_pois) #how well the model fits the data 
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        summary(model_pois)
        #if ratio is higher than 1, data is likely overdispersed and should use a negative binomial model
        
      #negative binomial 
        pearson_overdisp <- sum(residuals(model_nb1, type = "pearson")^2) / df.residual(model_nb1)
        deviance_overdisp <- deviance(model_nb1) / df.residual(model_nb1)
        cat("Pearson Overdispersion:", pearson_overdisp, "\n")
        cat("Deviance Overdispersion:", deviance_overdisp, "\n")
        summary(model_nb1)
        
        AIC(model_pois, model_nb1) #lower = better
        BIC(model_pois, model_nb1) #lower = better 
        
  #PLOTS 
      #species richness vs. time ggplot
        dev.off()
        plot.new()
        ggplot(openlow_env_lichen, aes(x = Sample_Year, y = Species_Richness)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "glm", method.args = list(family = poisson), se = TRUE, color = "blue") +
          theme_minimal() +
          labs(title = "Species Richness Over Time (Low Shrub, Lichen Species)",
               x = "Sample Year",
               y = "Species Richness")
        
      #mean vs. variance 
        mean_var_data <- openlow_env_lichen %>%
          group_by(Sample_Year) %>%
          summarise(mean_richness = mean(Species_Richness, na.rm = TRUE),
                    var_richness = var(Species_Richness, na.rm = TRUE))
        ggplot(mean_var_data, aes(x=mean_richness, y = var_richness)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, color = "blue") + #trend line 
          geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") + #poisson distribution expectation 
          theme_minimal()+
          labs(title = "Mean vs. Variance of Species Richness", 
               x = "Mean Species Richness",
               y = "Variance of Species Richness")
        
        sim_res <- simulateResiduals(model_pois)
        plot(sim_res)
        #QQ plot has perfect fit line and points along it. 
        #random scatter = good, upward/downward trend = bad indicates nonlinearlity, 
        #increasing spread at higher predictions = variance changes with fitted values
        
      #plots to check normality 
        ggplot(openlow_env_lichen, aes(x=Species_Richness)) +
          geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
          geom_density(color = "red", size = 1) +
          theme_minimal() +
          labs(title = "Distribution of Species Richness",
               x = "Species Richness",
               y = "Density")
        qqnorm(openlow_env_lichen$Species_Richness)
        qqline(openlow_env_lichen$Species_Richness, col = "red")
        
      #poisson vs. observed counts 
        sim_pois <- rpois(n = nrow(openlow_env_lichen), lambda = mean(openlow_env_lichen$Species_Richness))
        ggplot()+
          geom_histogram(aes(x= openlow_env_lichen$Species_Richness, y = ..density..),
                         bins = 30, fill = "red", alpha = 0.05) +
          geom_histogram(aes(x=sim_pois, y = ..density..),
                         bins = 30, fill = "blue", alpha = 0.5) +
          theme_minimal()+
          labs(title = "Observed vs. Poisson-Simulated Species Richness",
               x = "Species Richness", 
               y = "Density")
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        

