library(readxl)
library(Matrix)
library(permute)
library(lattice)
library(dplyr)
library(vegan)
library(here)
 
adjusted_permanova <- function(data, 
                               matrix, 
                               base_permutations, 
                               adjusted_permutations, 
                               corrected_F_equation,
                               terms, 
                               by = "terms",
                               method = "bray") {
  #build formula 
  formula_str <- paste("matrix ~", terms)
  formula <- as.formula(formula_str)
  #run base model 
  base_model <- adonis2(formula,
                        data = data,
                        method = method,
                        permutations = base_permutations,
                        by = by)
  #prepare loop permutations 
  perms <- rbind(1:nrow(matrix),
                 shuffleSet(n = nrow(matrix), control = adjusted_permutations, nset = 999))
  term_names <- rownames(base_model)
  n_terms <- length(term_names)
  results <- matrix(nrow = nrow(perms), ncol = n_terms)
  colnames(results) <- term_names
  
  for (i in 1:nrow(perms)) {
    temp.data <- data[perms[i, ], ]
    temp_model <- adonis2(formula,
                          data = temp.data,
                          method = method,
                          permutations = 0,
                          by = by)
    results[i, ] <- temp_model$SumOfSqs
  }
  results_df <- as.data.frame(results)
  
  #parse corrected F equation 
  terms_split <- strsplit(corrected_F_equation, "/")[[1]]
  num_term <- trimws(terms_split[1])
  denom_term <- trimws(terms_split[2])
  
  #extract DF for corrected F 
  num_DF <- base_model[num_term, "Df"]
  denom_DF <- base_model[denom_term, "Df"]
  
  #calculate F values across permuted SS 
  results_df$Corrected_F <- (results_df[[num_term]] / num_DF) / (results_df[[denom_term]]/denom_DF)
  
  #calculate P value
  corrected_F_value <- results_df$Corrected_F[1]
  p_value <- sum(results_df$Corrected_F >= corrected_F_value) / nrow(results_df)
  
  
  #update base model with corrected F and P values for recalculated term 
  if ("F" %in% colnames(base_model) && "Pr(>F)" %in% colnames(base_model)) {
    base_model[num_term, "F"] <- corrected_F_value
    base_model[num_term, "Pr(>F)"] <- p_value
  }
  cat("Base PERMANOVA model (with corrected F and P values):\n")
  print(base_model)
  cat("\nCorrected F =", corrected_F_value, "\n")
  cat("Corrected P =", p_value, "\n")
  return(list(base_model = base_model,
              corrected_F = corrected_F_value,
              corrected_P = p_value))
}





#test call 
#Load data 
    beetle_df <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_df_vasc_filtered.xlsx"))
    beetle_env <- read_xlsx(here("Data/Modified/Collapsed_Species_Code_DFs/PERMANOVA_DF_QuickLoad/beetle_env_vasc_filtered.xlsx"))
    
    #format matrix 
    beetle_composition <- beetle_df[,c(8:261)]
    beetle_composition <- as.matrix(beetle_composition) 
    
    #design permutation structure 
    #time 
    perm_design_beetle_time = how(
      plots = Plots(strata = beetle_env$Plot, type = c("free")),
      within = Within(type = "series", mirror = FALSE),
      nperm = 999)
    #no time 
    perm_design_beetle = how(
      plots = Plots(strata = beetle_env$Plot, type = c("free")),
      within = Within(type = "none"),
      nperm = 999)

    #recalculate viereck term 
    adjusted_beetle_result_viereck <- adjusted_permanova(
      data = beetle_env,
      matrix = beetle_composition,
      base_permutations = perm_design_beetle_time,
      adjusted_permutations = perm_design_beetle,
      corrected_F_equation = "Viereck.3/Plot",
      terms = "Viereck.3 + Park + Plot + Sample_Year + Viereck.3*Sample_Year",
      by = "terms",
      method = "bray"
    )
    
    #recalculate park term 
    adjusted_beetle_result_park <- adjusted_permanova(
      data = beetle_env,
      matrix = beetle_composition,
      base_permutations = perm_design_beetle_time,
      adjusted_permutations = perm_design_beetle,
      corrected_F_equation = "Park/Plot",
      terms = "Viereck.3 + Park + Plot + Sample_Year + Viereck.3*Sample_Year",
      by = "terms",
      method = "bray"
    )



