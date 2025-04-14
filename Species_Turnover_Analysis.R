library(tidyr)
library(codyn)
library(dplyr)
library(writexl)
library(readxl)
library(here)

taxa_filtered <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/taxa_filtered.xlsx"))

#Official turnover code 

      #Load data frames from PERMANOVA and NMDS
        needle_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_vasc_filtered.xlsx"))
        forest_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_vasc_filtered.xlsx"))
        beetle_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_vasc_filtered.xlsx"))
        dwarfscrub_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_vasc_filtered.xlsx"))
        openlow_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_vasc_filtered.xlsx"))
        alpine_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_vasc_filtered.xlsx"))
        
        needle_df_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_lichen_filtered.xlsx"))
        forest_df_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_lichen_filtered.xlsx"))
        beetle_df_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_lichen_filtered.xlsx"))
        dwarfscrub_df_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_lichen_filtered.xlsx"))
        openlow_df_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_lichen_filtered.xlsx"))
        alpine_df_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_lichen_filtered.xlsx"))    
        
        needle_df_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_nonvasc_filtered.xlsx"))
        forest_df_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_nonvasc_filtered.xlsx"))
        beetle_df_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_nonvasc_filtered.xlsx"))
        dwarfscrub_df_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_nonvasc_filtered.xlsx"))
        openlow_df_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_nonvasc_filtered.xlsx"))
        alpine_df_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_nonvasc_filtered.xlsx"))
        
        
    #get the first and most recent sample year for each plot (excluding plots visited only once)
    
      #assign current working DF
        current_df <- openlow_df
        
      #code it to where the early sample year is called first year and the most recent sample year is called last year 
        first_last_years <- current_df %>%                                                           
          distinct(Plot, Sample_Year) %>%
          group_by(Plot) %>%
          summarise(
            first_year = min(Sample_Year),
            last_year = max(Sample_Year)) %>%
          filter(first_year != last_year)
      #join everything together 
        first_data <- current_df %>%                                                                
          inner_join(first_last_years %>% select(Plot, first_year), 
                     by = c("Plot" = "Plot", "Sample_Year" = "first_year"))
        last_data <- current_df %>%                                                               
          inner_join(first_last_years %>% select(Plot, last_year), 
                     by = c("Plot" = "Plot", "Sample_Year" = "last_year"))
        combined_data <- bind_rows(
          first_data %>% mutate(Visit = "First"),  
          last_data %>% mutate(Visit = "Last"))
      #check what columns are there to see what you need to remove  
        str(combined_data)
      #move visit to front for improved clarity 
        combined_data <- combined_data %>% select(Visit, everything())
      
        
    #remove extra columns, use one of the following based on what species group dataframe you're using     
      #Clean up - USE FOR VASCULAR 
        combined_data <- subset(combined_data, select = -c(Park, Vegetation_Class, Viereck.2, Viereck.3, Plot_Year))
        
      #Clean up - USE FOR LICHEN
        combined_data <- subset(combined_data, select = -c(Park, Vegetation_Class, Plot_Year, Viereck.1, Viereck.4,
                                                           Viereck.2, Viereck.3))
      
      #Clean up - USE FOR NONVASCULAR 
        combined_data <- subset(combined_data, select = -c(Park, Plot_Year, Vegetation_Class, 
                                                           Viereck.1, Viereck.4,
                                                           Viereck.2, Viereck.3))
        
      
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
        #use replicate.var = plot to calculate turnover proportion based on plot as the unit instead of across the vegetation class 
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
        
      #Calculate the values for proportion of species that disappeared in the second time point 
        turnover_appearance <- turnover(
          df = long_data,
          time.var = "Sample_Year",    
          species.var = "Code1",  
          abundance.var = "Presence",  
          replicate.var = "Plot",      
          metric = "appearance")
        #proportion of species that disappeared relative to the total number of species observed across both time points 
        
      #View results if desired 
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
        
        #for vascular turnover use this: 
        turnover_joined <- turnover_joined %>% rename(Total_Vasc_Prop = total)
        turnover_joined <- turnover_joined %>% rename(Disapp_Vasc_Prop = disappearance)
        turnover_joined <- turnover_joined %>% rename(App_Vasc_Prop = appearance)
        
        #for lichen turnover use this: 
        turnover_joined <- turnover_joined %>% rename(Total_Lichen_Prop = total)
        turnover_joined <- turnover_joined %>% rename(Disapp_Lichen_Prop = disappearance)
        turnover_joined <- turnover_joined %>% rename(App_Lichen_Prop = appearance)
        
        #for nonvascular turnover use this: 
        turnover_joined <- turnover_joined %>% rename(Total_Nonvasc_Prop = total)
        turnover_joined <- turnover_joined %>% rename(Disapp_Nonvasc_Prop = disappearance)
        turnover_joined <- turnover_joined %>% rename(App_Nonvasc_Prop = appearance)
      
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


          
          
          
          
          
#Calculating average turnover and standard error for each veg class

            
#Alpine 

  #Load data 
    alpine_vascular_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/alpine_vascular_turnover.xlsx"))
    alpine_lichen_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/alpine_lichen_turnover.xlsx"))
    alpine_nonvasc_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/alpine_nonvasc_turnover.xlsx"))
    openlow_vascular_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/openlow_vascular_turnover.xlsx"))
    openlow_lichen_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/openlow_lichen_turnover.xlsx"))
    openlow_nonvasc_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/openlow_nonvasc_turnover.xlsx"))      
    needle_vascular_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/needle_vascular_turnover.xlsx")) #spruce woodlands
    needle_lichen_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/needle_lichen_turnover.xlsx")) #spruce woodlands
    needle_nonvasc_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/needle_nonvasc_turnover.xlsx")) #spruce woodlands
    beetle_vascular_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/beetle_vascular_turnover.xlsx"))
    beetle_lichen_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/beetle_lichen_turnover.xlsx"))
    beetle_nonvasc_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/beetle_nonvasc_turnover.xlsx"))
    dwarfscrub_vascular_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/dwarfscrub_vascular_turnover.xlsx"))
    dwarfscrub_lichen_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/dwarfscrub_lichen_turnover.xlsx"))
    dwarfscrub_nonvasc_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/dwarfscrub_nonvasc_turnover.xlsx")) 
    forest_vascular_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/forest_vascular_turnover.xlsx"))
    forest_lichen_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/forest_lichen_turnover.xlsx"))
    forest_nonvasc_turnover <- read_xlsx(here("Data/Modified/Turnover_Analysis/forest_nonvasc_turnover.xlsx")) 
    
    
    current_vasc_df <- alpine_vascular_turnover
    current_lichen_df <- alpine_lichen_turnover
    current_nonvasc_df <- alpine_nonvasc_turnover
    
          current_vasc_df %>%
            summarise(
              total = mean(Total_Vasc_Prop, na.rm = TRUE),
              se_total = sd(Total_Vasc_Prop, na.rm = TRUE)/sqrt(n()),
              disapp = mean(Disapp_Vasc_Prop, na.rm = TRUE), 
              se_disapp = sd(Disapp_Vasc_Prop, na.rm = TRUE)/sqrt(n()),
              app = mean(App_Vasc_Prop, na.rm = TRUE),
              se_app = sd(App_Vasc_Prop, na.rm = TRUE)/sqrt(n()))
          
          current_lichen_df %>%
            summarise(
              total = mean(Total_Lichen_Prop, na.rm = TRUE),
              se_total = sd(Total_Lichen_Prop, na.rm = TRUE)/sqrt(n()),
              disapp = mean(Disapp_Lichen_Prop, na.rm = TRUE), 
              se_disapp = sd(Disapp_Lichen_Prop, na.rm = TRUE)/sqrt(n()),
              app = mean(App_Lichen_Prop, na.rm = TRUE),
              se_app = sd(App_Lichen_Prop, na.rm = TRUE)/sqrt(n()))
          
          current_nonvasc_df %>%
            summarise(
              total = mean(Total_Nonvasc_Prop, na.rm = TRUE),
              se_total = sd(Total_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()),
              disapp = mean(Disapp_Nonvasc_Prop, na.rm = TRUE), 
              se_disapp = sd(Disapp_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()),
              app = mean(App_Nonvasc_Prop, na.rm = TRUE),
              se_app = sd(App_Nonvasc_Prop, na.rm = TRUE)/sqrt(n()))

          

#Calculating species richness for each vegetation class (Total, and by park)

#Load Data from permanova folder 
    beetle_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_vasc_filtered.xlsx"))
    beetle_df_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_lichen_filtered.xlsx"))
    beetle_df_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_nonvasc_filtered.xlsx"))
    
    needle_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_vasc_filtered.xlsx"))
    needle_df_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_lichen_filtered.xlsx"))
    needle_df_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_df_nonvasc_filtered.xlsx"))
    
    openlow_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_vasc_filtered.xlsx"))
    openlow_df_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_lichen_filtered.xlsx"))
    openlow_df_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_df_nonvasc_filtered.xlsx"))
    
    dwarfscrub_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_vasc_filtered.xlsx"))
    dwarfscrub_df_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_lichen_filtered.xlsx"))
    dwarfscrub_df_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_df_nonvasc_filtered.xlsx"))
    
    forest_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_vasc_filtered.xlsx"))
    forest_df_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_lichen_filtered.xlsx"))
    forest_df_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_df_nonvasc_filtered.xlsx"))
    
    alpine_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_vasc_filtered.xlsx"))
    alpine_df_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_lichen_filtered.xlsx"))
    alpine_df_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_df_nonvasc_filtered.xlsx"))

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

#calculate species richness across plots 
      
    #assign current df either full one or park specific 
      current_df_vasc <- forest_df
      current_df_lichen <- forest_df_lichen
      current_df_nonvasc <- alpine_df_nonvasc
      
    #vascular richness 
      summary_vasc <- current_df_vasc %>%
        select(8:261) %>%
        summarise(
          nonzero_cols = sum(colSums(. != 0) > 0),
          all_zero_cols = sum(colSums(. != 0) == 0)
        )
      print(summary_vasc)
      
    #lichen richness 
      summary_lichen <- current_df_lichen %>%
        select(10:167) %>%
        summarise(
          nonzero_cols = sum(colSums(. != 0) > 0),
          all_zero_cols = sum(colSums(. != 0) == 0)
        )
      print(summary_lichen)
      
    #nonvascular richness 
      summary_nonvasc <- current_df_nonvasc %>%
        select(10:210) %>%
        summarise(
          nonzero_cols = sum(colSums(. != 0) > 0),
          all_zero_cols = sum(colSums(. != 0) == 0)
        )
      print(summary_nonvasc)




      
      
      
      
      
      
      



        
        
        
        
        
        
        
        
        
        
        
        
        
        
#Generalized Linear Mixed Model Analysis - Species Richness 
     
#Setup - load packages. some are used for the model testing/exploratory plots not all are required for running the base analysis   
  #for base glm 
  library(lme4)
  library(Matrix)
  library(readxl)
  library(here) 
  library(MASS)
      
  #for exploration 
  library(glmmTMB)
  library(ggplot2)
  library(DHARMa)

        
  
#load data 
      needle_env <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_env_vasc_filtered.xlsx"))
      forest_env <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_env_vasc_filtered.xlsx"))
      beetle_env <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_env_vasc_filtered.xlsx"))
      dwarfscrub_env <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_env_vasc_filtered.xlsx"))
      openlow_env <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_env_vasc_filtered.xlsx"))
      alpine_env <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_env_vasc_filtered.xlsx"))
      
      needle_env_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_env_lichen_filtered.xlsx"))
      forest_env_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_env_lichen_filtered.xlsx"))
      beetle_env_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_env_lichen_filtered.xlsx"))
      dwarfscrub_env_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_env_lichen_filtered.xlsx"))
      openlow_env_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_env_lichen_filtered.xlsx"))
      alpine_env_lichen <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_env_lichen_filtered.xlsx"))    
      
      needle_env_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_env_nonvasc_filtered.xlsx"))
      forest_env_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/forest_env_nonvasc_filtered.xlsx"))
      beetle_env_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_env_nonvasc_filtered.xlsx"))
      dwarfscrub_env_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/dwarfscrub_env_nonvasc_filtered.xlsx"))
      openlow_env_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/openlow_env_nonvasc_filtered.xlsx"))
      alpine_env_nonvasc <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/alpine_env_nonvasc_filtered.xlsx"))      
      
      
  #assign current env file 
      current_env <- needle_env_lichen

      #scale sample year to remove warnings 
      current_env$Sample_Year <- as.numeric(current_env$Sample_Year)
      current_env$Sample_Year_Scaled <- scale(current_env$Sample_Year)
      
      #glm using poisson distribution 
      model_pois <- glmer(Species_Richness ~ Sample_Year_Scaled + (Sample_Year_Scaled + (1 | Plot)),
                               data = current_env, family = poisson)
      summary(model_pois)
      
      #check for overdispersion 
      pearson_overdisp <- sum(residuals(model_pois, type = "pearson")^2) / df.residual(model_pois) #pearson = how much each observation deviates from expected value
      deviance_overdisp <- deviance(model_pois) / df.residual(model_pois) #how well the model fits the data 
      cat("Pearson Overdispersion:", pearson_overdisp, "\n")
      cat("Deviance Overdispersion:", deviance_overdisp, "\n")
      
      #check if model converged 
      model_pois@optinfo$conv #0 = converged, 1 = not converged
      
      #calculate 95% confidence intervals 
      ci <- confint(model_pois)
      ci_lower <- ci["Sample_Year_Scaled","2.5 %"]
      ci_upper <- ci["Sample_Year_Scaled","97.5 %"]
      
      #adjust for CI interpretation because it's using scaled year 
          #negative interval
          ((exp(ci_lower/sd(current_env$Sample_Year)))-1)*100
          #positive interval 
          ((exp(ci_upper/sd(current_env$Sample_Year)))-1)*100
      
      #calculate the original unscaled slope 
      original_slope <- fixef(model_pois)["Sample_Year_Scaled"]/sd(current_env$Sample_Year)
      
      #calculate percent change per year using corrected slope coefficient 
      (exp(original_slope) -1)*100 #=percent change     
      
  
      
      
      
      
      
      
      
#MODEL COMPARISON - exploration for helping decide which distribution to use 
       
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
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        

