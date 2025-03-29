#GLM Analysis (Species Richness)


library(lme4)
library(Matrix)
library(glmmTMB)
library(MASS)
library(ggplot2)
library(readxl)
library(DHARMa)


openlow_env <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/openlow_env_vasc.xlsx")
openlow_env_lichen <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/openlow_env_lichen.xlsx")
openlow_env_nonvasc <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/PERMANOVA_DF_QuickLoad/openlow_env_nonvasc.xlsx")
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
