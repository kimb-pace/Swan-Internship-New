#GLM Analysis (Species Richness)


library(lme4)
library(Matrix)
library(glmmTMB)
library(MASS)
library(ggplot2)
library(readxl)


openlow_env <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/openlow_env_vasc.xlsx")
openlow_env_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/openlow_env_lichen.xlsx")
openlow_env_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/openlow_env_nonvasc.xlsx")


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





#fit poisson model 
model_pois <- glmer(Species_Richness ~ Sample_Year + (1 | Plot),
                    data = openlow_env_lichen, family = poisson)

#calculate overdispersion statistic 
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  pearson_chisq <- sum(rp^2)
  ratio <- pearson_chisq / rdf 
  return(ratio)
}
overdisp_fun(model_pois)

#if ratio is higher than 1, data is likely overdispersed and should use a negative binomial model 

#fit the negative binomial 
model_nb1 <- glmmTMB(Species_Richness ~ Sample_Year + (1 | Plot),
                     data = openlow_env_lichen, family = nbinom2)
print(model_nb1)
#OR

model_nb2 <- glmmPQL(Species_Richness ~ Sample_Year, random = ~1 | Plot,
                     family = negative.binomial(1), data = openlow_env_lichen)
print(model_nb2)

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
                 bins = 30, fill = "blue", alpha = 0.05) +
  geom_histogram(aes(x=sim_pois, y = ..density..),
                 bins = 30, fill = "red", alpha = 0.5) +
  theme_minimal()+
  labs(title = "Observed vs. Poisson-Simulated Species Richness",
       x = "Species Richness", 
       y = "Density")
