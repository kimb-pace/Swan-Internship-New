#GLM Analysis (Species Richness)


library(lme4)
library(Matrix)
library(glmmTMB)
library(MASS)
library(ggplot2)
library(readxl)
library(DHARMa)

viereck <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Viereck_env.xlsx")
lichen_sp_richness <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/lichen_sp_richness_filtered.xlsx")
nonvasc_sp_richness <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/nonvasc_sp_richness_filtered.xlsx")
vasc_sp_richness <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/vascular_sp_richness_filtered.xlsx")

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

write.csv(combined_df, "T:/Users/KPace/Species_Richness_Collapsed_For_Mike.csv")


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
          
          
          #negative interval
          #exp(-0.1427495/sd(forest_env$Sample_Year))
          #(0.9613854-1)*100 #-3.86
          #positive interval 
          #exp(-0.009377086/sd(forest_env$Sample_Year))
          #(0.9974165-1)*100 #-0.26
          
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
          
          
          #negative interval
          #exp(-0.1427495/sd(forest_env_lichen$Sample_Year))
          #(0.9613854-1)*100 #-3.86
          #positive interval 
          #exp(-0.009377086/sd(forest_env_lichen$Sample_Year))
          #(0.9974165-1)*100 #-0.26
          
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
          
          
              #negative interval
              #exp(-0.1427495/sd(forest_env_nonvasc$Sample_Year))
              #(0.9613854-1)*100 #-3.86
              #positive interval 
              #exp(-0.009377086/sd(forest_env_nonvasc$Sample_Year))
              #(0.9974165-1)*100 #-0.26
              
          confint(model_pois_nonvasc)
              #negative interval
              ((exp(-0.20026648/sd(forest_env_nonvasc$Sample_Year)))-1)*100
              #positive interval 
              ((exp(-0.03815634/sd(forest_env_nonvasc$Sample_Year)))-1)*100


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
           
          #negative interval
          #exp(-0.1427495/sd(beetle_env$Sample_Year))
          #(0.9613854-1)*100 #-3.86
          #positive interval 
          #exp(-0.009377086/sd(beetle_env$Sample_Year))
          #(0.9974165-1)*100 #-0.26
          
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
          
          #negative interval
          #exp(-0.1427495/sd(beetle_env_lichen$Sample_Year))
          #(0.9613854-1)*100 #-3.86
          #positive interval 
          #exp(-0.009377086/sd(beetle_env_lichen$Sample_Year))
          #(0.9974165-1)*100 #-0.26
          

          
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
          
          
          #negative interval
          #exp(-0.1427495/sd(beetle_env_nonvasc$Sample_Year))
          #(0.9613854-1)*100 #-3.86
          #positive interval 
          #exp(-0.009377086/sd(beetle_env_nonvasc$Sample_Year))
          #(0.9974165-1)*100 #-0.26
          


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
          
          #negative interval
          #exp(-0.1427495/sd(dwarfscrub_env$Sample_Year))
          #(0.9613854-1)*100 #-3.86
          #positive interval 
          #exp(-0.009377086/sd(dwarfscrub_env$Sample_Year))
          #(0.9974165-1)*100 #-0.26
          


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
          
          
          #negative interval
          #exp(-0.1427495/sd(dwarfscrub_env_lichen$Sample_Year))
          #(0.9613854-1)*100 #-3.86
          #positive interval 
          #exp(-0.009377086/sd(dwarfscrub_env_lichen$Sample_Year))
          #(0.9974165-1)*100 #-0.26
          
 
          
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
          
          
          #negative interval
          #exp(-0.1427495/sd(dwarfscrub_env_nonvasc$Sample_Year))
          #(0.9613854-1)*100 #-3.86
          #positive interval 
          #exp(-0.009377086/sd(dwarfscrub_env_nonvasc$Sample_Year))
          #(0.9974165-1)*100 #-0.26
          



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
          
          
          #negative interval
          #exp(-0.1427495/sd(alpine_env$Sample_Year))
          #(0.9613854-1)*100 #-3.86
          #positive interval 
          #exp(-0.009377086/sd(alpine_env$Sample_Year))
          #(0.9974165-1)*100 #-0.26
          

          
          
          
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
          
          #negative interval
          #exp(-0.1427495/sd(alpine_env_lichen$Sample_Year))
          #(0.9613854-1)*100 #-3.86
          #positive interval 
          #exp(-0.009377086/sd(alpine_env_lichen$Sample_Year))
          #(0.9974165-1)*100 #-0.26
          

          
          
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
          
          #negative interval
          #exp(-0.1427495/sd(alpine_env_nonvasc$Sample_Year))
          #(0.9613854-1)*100 #-3.86
          #positive interval 
          #exp(-0.009377086/sd(alpine_env_nonvasc$Sample_Year))
          #(0.9974165-1)*100 #-0.26
          
 
























































str(openlow_env_lichen)
openlow_env_lichen$Sample_Year <- as.numeric(openlow_env_lichen$Sample_Year)

openlow_env_lichen$Year_Centered <- openlow_env_lichen$Sample_Year - 2009
lmer_model <- lmer(Species_Richness ~ Year_Centered + (1 | Plot), data = openlow_env_lichen)

summary(lmer_model)

str(openlow_env_lichen)

openlow_env_lichen$Sample_Year <- as.numeric(openlow_env_lichen$Sample_Year)
openlow_env_lichen$Park <- as.factor(openlow_env_lichen$Park)
openlow_env_lichen$Year_Centered <- as.numeric(openlow_env_lichen$Year_Centered)


https://cran.r-project.org/web/packages/GlmSimulatoR/vignettes/count_data_and_overdispersion.html

ggplot(simdata, aes(Y)) +
  geom_histogram(bins = 200)













use GLMM if you have count data, data is grouped or heirarchical (plots in sites), non-normal distributed data, and you need to control for random effects 
to avoid pseudoreplication 


#scale sample year to remove warnings 
openlow_env_lichen$Sample_Year <- as.numeric(openlow_env_lichen$Sample_Year)
openlow_env_lichen$Sample_Year_Scaled <- scale(openlow_env_lichen$Sample_Year)



#Poisson, assumes mean = variance 
    model_pois <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                        data = openlow_env_lichen, family = poisson)
    summary(model_pois)
    model_pois@optinfo$conv #0 = converged, 1 = not converged
    confint(model_pois)
    
#generalized poisson model 
    gen_pois <- vglm(count ~ fixed_var + random_var, family = poissonff, data = data)

#Negative Binomial = use when variance > mean (over dispersed)
    model_nb1 <- glmmTMB(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                         data = openlow_env_lichen, family = nbinom2)
    summary(model_nb1)
    model_nb1$fit$convergence #0 = converged, 1 = not converged
    confint(model_nb1)

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

    
#Interpretation of slope coefficient when using scaled years: 
  #need to divide by SD(Sample_Year)
      #For poisson 
      original_slope <- fixef(model_pois)["Sample_Year_Scaled"]/sd(openlow_env_lichen$Sample_Year)
  #for NB models:
      original_slope <- fixef(model_pois)$cond["Sample_Year_Scaled"]/sd(openlow_env_lichen$Sample_Year)
      
  #calculate percent change per year using corrected coefficient 
      exp(-0.07559)
      1-0.9271963
      0.0728037*100
      
      
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




ranef(model_nb1)$cond
#large differences = significant site to site variation 








model_quasi <- glm(Species_Richness ~ Sample_Year, family = quasipoisson, data = openlow_env_lichen)

res <- resid(model_pois)
plot(fitted(model_pois), res)
qqnorm(res)
qqline(res)
plot(density(res))



#calculate overdispersion statistic 
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "deviance")
  pearson_chisq <- sum(rp^2)
  ratio <- pearson_chisq / rdf 
  return(ratio)
}
overdisp_fun(model_pois)
#if ratio is higher than 1, data is likely overdispersed and should use a negative binomial model 










library(lmtest)
#likelihood ratio test 
lrtest(model_pois, model_nb1) 
#P > 0.05 use poisson, P < 0.05 use negative binomial 





Reporting 
Methods 
-justification of using GLMM
its a good model choice for count data because its non-negative. also a gaussian distribution is inappropriate for count data so we can use a poisson
or negative binomial distribution instead. it can also handle unequal sampliong effort/unbalanced data 



-choosing poisson or negative binomial 
-how over dispersion was checked, what tests you used 
-model used and what the fixed and random effects are
We fit the model using R version X with the lme4 package (glmer()) for poisson GLMM and the glmmTMB package for negative binomial gllmm
include the link function (log link for count data), estimateion method (maximum liklihood), random effect structure 

Reporting results 
-coefficient (shows the effect size)
-standard error (measure of uncertainty)
-z value (test stat for signinicance)
-p value statistical sig 

sample year had a significnat negative effect on species richness (B = -0.016, SE = 0.005, z = -3.12, P = 0.002) indicating a 1.63% decline in richness per year. 
report model fit statistics too ie AIC and BIC, log liklihood (indicator of model fit), and deviance (if relevant)
ie the negative binomial GLMM provided a better fit than teh poisson GLMM (DELTA-AIC = 23.4, LRT: X2 = 27.1, P<0.001)
report variance/stdev of the random effects. ie site level variability accounted for 12% of the total variance (SD = 0.35)
include a plot of species richness over time
include confidence intervals- the estimated decline was -1.63% per year (95% CI:-2.5%, -0.8%)


#fit poisson model first 
model_pois <- glmer(Species_Richness ~ Sample_Year + (1 | Plot),
                    data = openlow_env_lichen, family = poisson)
model_pois

#fit the negative binomial 
#use glmmTMB package 
model_nb1 <- glmmTMB(Species_Richness ~ Sample_Year + (1 | Plot),
                     data = openlow_env_lichen, family = nbinom2)
summary(model_nb1)

#use scaled year
# OPen Low vascular 

#scale sample year to remove warnings 
openlow_env$Sample_Year <- as.numeric(openlow_env$Sample_Year)
openlow_env$Sample_Year_Scaled <- scale(openlow_env$Sample_Year)

#Poisson, assumes mean = variance 
model_pois_vasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                         data = openlow_env, family = poisson)
summary(model_pois_vasc)
model_pois_vasc@optinfo$conv #0 = converged, 1 = not converged
confint(model_pois_vasc)
#negative interval
exp(-0.06847/sd(openlow_env_lichen$Sample_Year))
(0.9852-1)*100 #-1.48
#positive interval 
exp(0.05624/sd(openlow_env_lichen$Sample_Year))
(1.01231-1)*100 #1.23

#Negative Binomial = use when variance > mean (over dispersed)
#model_nb1_vasc <- glmmTMB(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
#                          data = openlow_env_lichen, family = nbinom2)
#summary(model_nb1_vasc)
#model_nb1_vasc$fit$convergence #0 = converged, 1 = not converged
#confint(model_nb1_vasc)

#MODEL COMPARISON 
#checking the overdispersion statistics 
#Poisson 
pearson_overdisp <- sum(residuals(model_pois_vasc, type = "pearson")^2) / df.residual(model_pois_vasc) #pearson = how much each observation deviates from expected value
deviance_overdisp <- deviance(model_pois_vasc) / df.residual(model_pois_vasc) #how well the model fits the data 
cat("Pearson Overdispersion:", pearson_overdisp, "\n")
cat("Deviance Overdispersion:", deviance_overdisp, "\n")
summary(model_pois_vasc)
#if ratio is higher than 1, data is likely overdispersed and should use a negative binomial model

#negative binomial 
pearson_overdisp <- sum(residuals(model_nb1_vasc, type = "pearson")^2) / df.residual(model_nb1_vasc)
deviance_overdisp <- deviance(model_nb1_vasc) / df.residual(model_nb1_vasc)
cat("Pearson Overdispersion:", pearson_overdisp, "\n")
cat("Deviance Overdispersion:", deviance_overdisp, "\n")
summary(model_nb1_vasc)

AIC(model_pois_vasc, model_nb1_vasc) #lower = better
BIC(model_pois_vasc, model_nb1_vasc) #lower = better 


#Interpretation of slope coefficient when using scaled years: 
#need to divide by SD(Sample_Year)
#For poisson 
original_slope <- fixef(model_pois_vasc)["Sample_Year_Scaled"]/sd(openlow_env_lichen$Sample_Year)
#for NB models:
original_slope <- fixef(model_pois_vasc)$cond["Sample_Year_Scaled"]/sd(openlow_env_lichen$Sample_Year)

#calculate percent change per year using corrected coefficient 
(exp(-0.00137) -1)*100 #=percent change 
#exp(slope) > 1 species richness is increasing per year, if < 1 its decreasing per year 

# Open Low Lichen 

#scale sample year to remove warnings 
openlow_env_lichen$Sample_Year <- as.numeric(openlow_env_lichen$Sample_Year)
openlow_env_lichen$Sample_Year_Scaled <- scale(openlow_env_lichen$Sample_Year)

#Poisson, assumes mean = variance 
model_pois_lichen <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                           data = openlow_env_lichen, family = poisson)
summary(model_pois_lichen)
model_pois_lichen@optinfo$conv #0 = converged, 1 = not converged
confint(model_pois_lichen)
#negative interval
exp(-0.12536944/sd(openlow_env_lichen$Sample_Year))
(0.9730882-1)*100 #-2.69
#positive interval 
exp(-0.01917884/sd(openlow_env_lichen$Sample_Year))
(0.9958354-1)*100 #-0.42

#generalized poisson model 
#gen_pois <- vglm(count ~ fixed_var + random_var, family = poissonff, data = data)

#Negative Binomial = use when variance > mean (over dispersed)
model_nb1_lichen <- glmmTMB(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                            data = openlow_env_lichen, family = nbinom2)
summary(model_nb1_lichen)
model_nb1_lichen$fit$convergence #0 = converged, 1 = not converged
confint(model_nb1_lichen)

#MODEL COMPARISON 
#checking the overdispersion statistics 
#Poisson 
pearson_overdisp <- sum(residuals(model_pois_lichen, type = "pearson")^2) / df.residual(model_pois_lichen) #pearson = how much each observation deviates from expected value
deviance_overdisp <- deviance(model_pois_lichen) / df.residual(model_pois_lichen) #how well the model fits the data 
cat("Pearson Overdispersion:", pearson_overdisp, "\n")
cat("Deviance Overdispersion:", deviance_overdisp, "\n")
summary(model_pois_lichen)
#if ratio is higher than 1, data is likely overdispersed and should use a negative binomial model

#negative binomial 
pearson_overdisp <- sum(residuals(model_nb1_lichen, type = "pearson")^2) / df.residual(model_nb1_lichen)
deviance_overdisp <- deviance(model_nb1_lichen) / df.residual(model_nb1_lichen)
cat("Pearson Overdispersion:", pearson_overdisp, "\n")
cat("Deviance Overdispersion:", deviance_overdisp, "\n")
summary(model_nb1_lichen)

AIC(model_pois_lichen, model_nb1_lichen) #lower = better
BIC(model_pois_lichen, model_nb1_lichen) #lower = better 


#Interpretation of slope coefficient when using scaled years: 
#need to divide by SD(Sample_Year)
#For poisson 
original_slope <- fixef(model_pois_lichen)["Sample_Year_Scaled"]/sd(openlow_env_lichen$Sample_Year)
#for NB models:
original_slope <- fixef(model_pois_lichen)$cond["Sample_Year_Scaled"]/sd(openlow_env_lichen$Sample_Year)

#calculate percent change per year using corrected coefficient 
(exp(-0.0158) -1)*100 #=percent change 
#exp(slope) > 1 species richness is increasing per year, if < 1 its decreasing per year 

# Open Low nonvascular 

#scale sample year to remove warnings 
openlow_env_nonvasc$Sample_Year <- as.numeric(openlow_env_nonvasc$Sample_Year)
openlow_env_nonvasc$Sample_Year_Scaled <- scale(openlow_env_nonvasc$Sample_Year)

#Poisson, assumes mean = variance 
model_pois_nonvasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                            data = openlow_env_nonvasc, family = poisson)
summary(model_pois_nonvasc)
model_pois_nonvasc@optinfo$conv #0 = converged, 1 = not converged
confint(model_pois_nonvasc)
#negative interval
exp(-0.1426947/sd(openlow_env_lichen$Sample_Year))
(0.9694265-1)*100 #-3.06
#positive interval 
exp(0.005000223/sd(openlow_env_lichen$Sample_Year))
(1.001089-1)*100 #0.11

#generalized poisson model 
#gen_pois <- vglm(count ~ fixed_var + random_var, family = poissonff, data = data)

#Negative Binomial = use when variance > mean (over dispersed)
model_nb1_nonvasc <- glmmTMB(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                             data = openlow_env_nonvasc, family = nbinom2)
summary(model_nb1_nonvasc)
model_nb1_nonvasc$fit$convergence #0 = converged, 1 = not converged
confint(model_nb1_nonvasc)

#MODEL COMPARISON 
#checking the overdispersion statistics 
#Poisson 
pearson_overdisp <- sum(residuals(model_pois_nonvasc, type = "pearson")^2) / df.residual(model_pois_nonvasc) #pearson = how much each observation deviates from expected value
deviance_overdisp <- deviance(model_pois_nonvasc) / df.residual(model_pois_nonvasc) #how well the model fits the data 
cat("Pearson Overdispersion:", pearson_overdisp, "\n")
cat("Deviance Overdispersion:", deviance_overdisp, "\n")
summary(model_pois_nonvasc)
#if ratio is higher than 1, data is likely overdispersed and should use a negative binomial model

#negative binomial 
pearson_overdisp <- sum(residuals(model_nb1_nonvasc, type = "pearson")^2) / df.residual(model_nb1_nonvasc)
deviance_overdisp <- deviance(model_nb1_nonvasc) / df.residual(model_nb1_nonvasc)
cat("Pearson Overdispersion:", pearson_overdisp, "\n")
cat("Deviance Overdispersion:", deviance_overdisp, "\n")
summary(model_nb1_lichen)

AIC(model_pois_nonvasc, model_nb1_nonvasc) #lower = better
BIC(model_pois_nonvasc, model_nb1_nonvasc) #lower = better 


#Interpretation of slope coefficient when using scaled years: 
#need to divide by SD(Sample_Year)
#For poisson 
original_slope <- fixef(model_pois_nonvasc)["Sample_Year_Scaled"]/sd(openlow_env_nonvasc$Sample_Year)
#for NB models:
original_slope <- fixef(model_pois_nonvasc)$cond["Sample_Year_Scaled"]/sd(openlow_env_nonvasc$Sample_Year)

#calculate percent change per year using corrected coefficient 
(exp(-0.0151) -1)*100 #=percent change 
#exp(slope) > 1 species richness is increasing per year, if < 1 its decreasing per year 


# spruce woodland vascular 
needle_env <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_env_vasc_filtered.xlsx")
needle_env_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_env_lichen_filtered.xlsx")
needle_env_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/needle_env_nonvasc_filtered.xlsx")

#scale sample year to remove warnings 
needle_env$Sample_Year <- as.numeric(needle_env$Sample_Year)
needle_env$Sample_Year_Scaled <- scale(needle_env$Sample_Year)

#Poisson, assumes mean = variance 
model_pois_vasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                         data = needle_env, family = poisson)
summary(model_pois_vasc)
model_pois_vasc@optinfo$conv #0 = converged, 1 = not converged
confint(model_pois_vasc)
#negative interval
exp(-0.07878733/sd(needle_env$Sample_Year))
(0.9784997-1)*100 #-2.15
#positive interval 
exp(0.05138275/sd(needle_env$Sample_Year))
(1.014276-1)*100 #1.43

#Negative Binomial = use when variance > mean (over dispersed)
model_nb1_vasc <- glmmTMB(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                          data = needle_env_lichen, family = nbinom2)
summary(model_nb1_vasc)
model_nb1_vasc$fit$convergence #0 = converged, 1 = not converged
confint(model_nb1_vasc)

#MODEL COMPARISON 
#checking the overdispersion statistics 
#Poisson 
pearson_overdisp <- sum(residuals(model_pois_vasc, type = "pearson")^2) / df.residual(model_pois_vasc) #pearson = how much each observation deviates from expected value
deviance_overdisp <- deviance(model_pois_vasc) / df.residual(model_pois_vasc) #how well the model fits the data 
cat("Pearson Overdispersion:", pearson_overdisp, "\n")
cat("Deviance Overdispersion:", deviance_overdisp, "\n")
summary(model_pois_vasc)
#if ratio is higher than 1, data is likely overdispersed and should use a negative binomial model

#negative binomial 
pearson_overdisp <- sum(residuals(model_nb1_vasc, type = "pearson")^2) / df.residual(model_nb1_vasc)
deviance_overdisp <- deviance(model_nb1_vasc) / df.residual(model_nb1_vasc)
cat("Pearson Overdispersion:", pearson_overdisp, "\n")
cat("Deviance Overdispersion:", deviance_overdisp, "\n")
summary(model_nb1_vasc)

AIC(model_pois_vasc, model_nb1_vasc) #lower = better
BIC(model_pois_vasc, model_nb1_vasc) #lower = better 


#Interpretation of slope coefficient when using scaled years: 
#need to divide by SD(Sample_Year)
#For poisson 
original_slope <- fixef(model_pois_vasc)["Sample_Year_Scaled"]/sd(needle_env$Sample_Year)

#calculate percent change per year using corrected coefficient 
(exp(-0.00386) -1)*100 #=percent change 
#exp(slope) > 1 species richness is increasing per year, if < 1 its decreasing per year 

# spruce woodland Lichen 

#scale sample year to remove warnings 
needle_env_lichen$Sample_Year <- as.numeric(needle_env_lichen$Sample_Year)
needle_env_lichen$Sample_Year_Scaled <- scale(needle_env_lichen$Sample_Year)

#Poisson, assumes mean = variance 
model_pois_lichen <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                           data = needle_env_lichen, family = poisson)
summary(model_pois_lichen)
model_pois_lichen@optinfo$conv #0 = converged, 1 = not converged
confint(model_pois_lichen)
#negative interval
exp(-0.1427322/sd(needle_env_lichen$Sample_Year))
(0.96139-1)*100 #-3.86
#positive interval 
exp(-0.0376746/sd(needle_env_lichen$Sample_Year))
(0.9896606-1)*100 #-1.03

#generalized poisson model 
#gen_pois <- vglm(count ~ fixed_var + random_var, family = poissonff, data = data)

#Negative Binomial = use when variance > mean (over dispersed)
model_nb1_lichen <- glmmTMB(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                            data = needle_env_lichen, family = nbinom2)
summary(model_nb1_lichen)
model_nb1_lichen$fit$convergence #0 = converged, 1 = not converged
confint(model_nb1_lichen)

#MODEL COMPARISON 
#checking the overdispersion statistics 
#Poisson 
pearson_overdisp <- sum(residuals(model_pois_lichen, type = "pearson")^2) / df.residual(model_pois_lichen) #pearson = how much each observation deviates from expected value
deviance_overdisp <- deviance(model_pois_lichen) / df.residual(model_pois_lichen) #how well the model fits the data 
cat("Pearson Overdispersion:", pearson_overdisp, "\n")
cat("Deviance Overdispersion:", deviance_overdisp, "\n")
summary(model_pois_lichen)
#if ratio is higher than 1, data is likely overdispersed and should use a negative binomial model

#negative binomial 
pearson_overdisp <- sum(residuals(model_nb1_lichen, type = "pearson")^2) / df.residual(model_nb1_lichen)
deviance_overdisp <- deviance(model_nb1_lichen) / df.residual(model_nb1_lichen)
cat("Pearson Overdispersion:", pearson_overdisp, "\n")
cat("Deviance Overdispersion:", deviance_overdisp, "\n")
summary(model_nb1_lichen)

AIC(model_pois_lichen, model_nb1_lichen) #lower = better
BIC(model_pois_lichen, model_nb1_lichen) #lower = better 


#Interpretation of slope coefficient when using scaled years: 
#need to divide by SD(Sample_Year)
#For poisson 
original_slope <- fixef(model_pois_lichen)["Sample_Year_Scaled"]/sd(needle_env_lichen$Sample_Year)
#for NB models:
original_slope <- fixef(model_pois_lichen)$cond["Sample_Year_Scaled"]/sd(needle_env_lichen$Sample_Year)

#calculate percent change per year using corrected coefficient 
(exp(-0.0249) -1)*100 #=percent change 
#exp(slope) > 1 species richness is increasing per year, if < 1 its decreasing per year 


#spruce woodland nonvascular 

#scale sample year to remove warnings 
needle_env_nonvasc$Sample_Year <- as.numeric(needle_env_nonvasc$Sample_Year)
needle_env_nonvasc$Sample_Year_Scaled <- scale(needle_env_nonvasc$Sample_Year)

#Poisson, assumes mean = variance 
model_pois_nonvasc <- glmer(Species_Richness ~ Sample_Year_Scaled + (1 | Plot),
                            data = needle_env_nonvasc, family = poisson)
summary(model_pois_nonvasc)
model_pois_nonvasc@optinfo$conv #0 = converged, 1 = not converged
confint(model_pois_nonvasc)
#negative interval
exp(-0.1427495/sd(needle_env_nonvasc$Sample_Year))
(0.9613854-1)*100 #-3.86
#positive interval 
exp(-0.009377086/sd(needle_env_nonvasc$Sample_Year))
(0.9974165-1)*100 #-0.26

#generalized poisson model 
#gen_pois <- vglm(count ~ fixed_var + random_var, family = poissonff, data = data)


#MODEL COMPARISON 
#checking the overdispersion statistics 
#Poisson 
pearson_overdisp <- sum(residuals(model_pois_nonvasc, type = "pearson")^2) / df.residual(model_pois_nonvasc) #pearson = how much each observation deviates from expected value
deviance_overdisp <- deviance(model_pois_nonvasc) / df.residual(model_pois_nonvasc) #how well the model fits the data 
cat("Pearson Overdispersion:", pearson_overdisp, "\n")
cat("Deviance Overdispersion:", deviance_overdisp, "\n")
summary(model_pois_nonvasc)
#if ratio is higher than 1, data is likely overdispersed and should use a negative binomial model

#Interpretation of slope coefficient when using scaled years: 
#need to divide by SD(Sample_Year)
#For poisson 
original_slope <- fixef(model_pois_nonvasc)["Sample_Year_Scaled"]/sd(needle_env_nonvasc$Sample_Year)

#calculate percent change per year using corrected coefficient 
(exp(-0.0214) -1)*100 #=percent change 
#exp(slope) > 1 species richness is increasing per year, if < 1 its decreasing per year 
