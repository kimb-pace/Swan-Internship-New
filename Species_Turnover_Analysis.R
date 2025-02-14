library(tidyr)
library(codyn)
library(dplyr)
library(writexl)
library(readxl)

vascpresence_absence_df <- read_xlsx("T:/Users/KPace/SWAN-Internship/Verified_Spreadsheets/presence_absence_df_full.xlsx")
lichenspresence_absence_df <- read_xlsx("T:/Users/KPace/SWAN-Internship/Verified_Spreadsheets/lichenspresence_absence_df_full.xlsx")
nonvascpresence_absence_df <- read_xlsx("T:/Users/KPace/SWAN-Internship/Verified_Spreadsheets/nonvascpresence_absence_df_full.xlsx")
viereck <- read_xlsx("T:/Users/KPace/SWAN-Internship/Verified_Spreadsheets/Viereck_env.xlsx")


#dwarfvasc_turn <- dwarfscrub_df[ ,c(7:280)]
#dwarflichens_turn <- dwarfscrub_df_lichens[ ,c(13:179)]
#dwarfnonvasc_turn <- dwarfscrub_df_nonvasc[ ,c(13:219)]


#get the first and most recent sample year for each plot (excluding plots visited only once)
first_last_years <- nonvascpresence_absence_df %>%
  distinct(Plot, Sample_Year) %>%
  group_by(Plot) %>%
  summarise(
    first_year = min(Sample_Year),
    last_year = max(Sample_Year)) %>%
  filter(first_year != last_year)
first_data <- nonvascpresence_absence_df %>%
  inner_join(first_last_years %>% select(Plot, first_year), 
             by = c("Plot" = "Plot", "Sample_Year" = "first_year"))
last_data <- nonvascpresence_absence_df %>%
  inner_join(first_last_years %>% select(Plot, last_year), 
             by = c("Plot" = "Plot", "Sample_Year" = "last_year"))

#combine 
combined_data <- bind_rows(
  first_data %>% mutate(Visit = "First"),  
  last_data %>% mutate(Visit = "Last"))

#clean up/organize again 
combined_data <- combined_data %>% select(Visit, everything())
#combined_data <- combined_data %>% select(Vegetation_Class, everything())
#combined_data <- combined_data %>% select(Plot, everything())

combined_data <- subset(combined_data, select = -c(Park, Vegetation_Class, Plot_Year, EstYear, Elevation_Band, PlotID, Viereck.1, Viereck.2, Viereck.3, Viereck.4))

#pivot to long format so it can fit into the function, use values_drop_na to get only rows with species present
long_data <- combined_data %>%
  pivot_longer(cols = -c(Plot, Visit, Sample_Year), 
               names_to = "Species_Code",                 
               values_to = "Presence",              
               values_drop_na = TRUE)                    

#make sure year is numeric 
long_data$Sample_Year <- as.numeric(as.character(long_data$Sample_Year))

long_data <- long_data %>% distinct()

#calculate the values for total turnover, losses, and gains 
turnover_total <- turnover(df = long_data, 
                           time.var = "Sample_Year", 
                           species.var = "Species_Code", 
                           abundance.var = "Presence", 
                           replicate.var = "Plot", 
                           metric = "total")
#proportion of species that differ between the two time points 

turnover_disappearance <- turnover(
  df = long_data,
  time.var = "Sample_Year",    
  species.var = "Species_Code",  
  abundance.var = "Presence",  
  replicate.var = "Plot",      
  metric = "disappearance")
#proportion of species that appeared relative to the total number of species observed in both time points 

turnover_appearance <- turnover(
  df = long_data,
  time.var = "Sample_Year",    
  species.var = "Species_Code",  
  abundance.var = "Presence",  
  replicate.var = "Plot",      
  metric = "appearance")
#proportion of species that disappeared relative to the total number of species observed in both time points 

#convert the turnover output into a dataframe and add veg class and stuff so you can group things and look for trends 
turnover_total <- as.data.frame(turnover_total)
turnover_appearance <- as.data.frame(turnover_appearance)
turnover_disappearance <- as.data.frame(turnover_disappearance)



turnover_joined <- left_join(turnover_total, turnover_appearance, by = "Plot")
turnover_joined <- left_join(turnover_joined, turnover_disappearance, by = "Plot")

#make the dataframe look better and get all the columns you want 
turnover_joined <- turnover_joined[,c(1,3,4,6)]
#colnames(turnover_joined)[colnames(turnover_joined) == 'Sample_Year.y'] <- 'Sample_Year'
turnover_joined <- turnover_joined %>% select(Plot, everything())

turnover_joined <- turnover_joined %>%
  left_join(viereck %>%
              select(Plot, Viereck.2, Viereck.3, Vegetation_Class), 
            by = "Plot")
turnover_joined <- turnover_joined %>% distinct()

#this calculates the proportion of species that appear or disappear between timepoints. 

#export this to excel so you don't have to build it again every time 

write_xlsx(turnover_joined, "C:/Users/kmpace/Desktop/turnover_joined.xlsx")
turnover_joined <- read_xlsx("C:/Users/kmpace/Desktop/turnover_joined.xlsx")



#building master turnover spreadsheet 
vasc_turnover <- read_xlsx("C:/Users/kmpace/Desktop/Plot_Turnover_Vascular.xlsx")
lichen_turnover <- read_xlsx("C:/Users/kmpace/Desktop/Plot_Turnover_Lichen.xlsx")
nonvasc_turnover <- read_xlsx("C:/Users/kmpace/Desktop/Plot_Turnover_Nonvascular.xlsx")


turnover_all <- vasc_turnover %>%
  left_join(lichen_turnover %>%
              select(Plot, total_lichen, appearance_lichen, disappearance_lichen), 
            by = "Plot")
turnover_all <- turnover_all %>%
  left_join(nonvasc_turnover %>%
              select(Plot, total_nonvasc, appearance_nonvasc, disappearance_nonvasc), 
            by = "Plot")

vasc_summary <- vasc_sp_richness %>%
  group_by(Plot) %>%
  summarise(
    First_vasc = Species_Richness[which.min(Sample_Year)],
    Last_vasc = Species_Richness[which.max(Sample_Year)])
lichen_summary <- lichen_sp_richness %>%
  group_by(Plot) %>%
  summarise(
    First_lichen = Species_Richness[which.min(Sample_Year)],
    Last_lichen = Species_Richness[which.max(Sample_Year)])
nonvasc_summary <- nonvasc_sp_richness %>%
  group_by(Plot) %>%
  summarise(
    First_nonvasc = Species_Richness[which.min(Sample_Year)],
    Last_nonvasc = Species_Richness[which.max(Sample_Year)])

turnover_all <- turnover_all %>%
  left_join(vasc_summary %>%
              select(Plot, First_vasc, Last_vasc), 
            by = "Plot")
turnover_all <- turnover_all %>%
  left_join(lichen_summary %>%
              select(Plot, First_lichen, Last_lichen), 
            by = "Plot")
turnover_all <- turnover_all %>%
  left_join(nonvasc_summary %>%
              select(Plot, First_nonvasc, Last_nonvasc), 
            by = "Plot")

write_xlsx(turnover_all, "C:/Users/kmpace/Desktop/Plot_Turnover_All.xlsx")
