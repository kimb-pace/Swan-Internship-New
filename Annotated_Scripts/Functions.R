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
                               corrected_F_equations = list(
                                 "Term1/Term4" = permutation_structure_1,
                                 "Term2/Term4" = permutation_structure_2),
                               terms, 
                               by = "terms",
                               method = "bray") {
  
  #build formula using input from call 
  formula_str <- paste("matrix ~", terms)
  formula <- as.formula(formula_str)
  
  #run base model first 
  base_model <- adonis2(formula,
                        data = data,
                        method = method,
                        permutations = base_permutations,
                        by = by)
  
  term_names <- rownames(base_model)
  n_terms <- length(term_names)
  
  #create storage for multiple corrected values 
  corrected_F_values <- list()
  p_values <- list()
  
  #loop through corrected calculations for each specified F equation, as many as you want 
  #parse corrected F equation from your input equations 
  
  for (eqn in names(corrected_F_equations)) {
    terms_split <- strsplit(eqn, "/")[[1]]
    num_term <- trimws(terms_split[1])
    denom_term <- trimws(terms_split[2])
    
  #prepare objects for looped permutations 
  #first the permutation object that determines how your data will be shuffled and use the associated perms structure
    perms_structure <- corrected_F_equations[[eqn]]
    perms <- rbind(1:nrow(matrix),
                 shuffleSet(n = nrow(matrix), control = perms_structure, nset = 999))

  #object that will store sums of squares for each permutation, loop deposits them here 
  results <- matrix(nrow = nrow(perms), ncol = n_terms)
  #name the columns after your specified terms from your base model 
  colnames(results) <- term_names
  
  #run through each row of your permutation object, running an adonis model on each shuffle of your data 
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
  
  #extract DF for corrected F 
  num_DF <- base_model[num_term, "Df"]
  denom_DF <- base_model[denom_term, "Df"]
  
  #calculate F values across permuted SS 
  #add specific name for clarity for corrected equations in output 
  col_name <- paste0("Corrected_F_", num_term, "_over_", denom_term)
  #recalc F for each col_name 
  results_df[[col_name]] <- (results_df[[num_term]] / num_DF) / (results_df[[denom_term]]/denom_DF)
  
  #calculate P values for each equation 
  corrected_F <- results_df[[col_name]][1]
  p_value <- mean(results_df[[col_name]] >= corrected_F)
  
  #store results of each corrected F test in named lists 
  corrected_F_values[[col_name]] <- corrected_F
  p_values[[col_name]] <- p_value 
  
  #update base model with corrected F and P values for recalculated term 
  if ("F" %in% colnames(base_model) && "Pr(>F)" %in% colnames(base_model)) {
    base_model[num_term, "F"] <- corrected_F
    base_model[num_term, "Pr(>F)"] <- p_value
      }
  }
  
  cat("Base PERMANOVA model (with corrected F and P values):\n")
  print(base_model)
  cat("\nCorrected F and P values:\n")
  #loop through all of the corrected values you generated 
  for (name in names(corrected_F_values)) {
    cat(name, " = ", corrected_F_values[[name]], ", P =", p_values[[name]], "\n")
  }
  
  return(list(base_model = base_model,
              corrected_F_values = corrected_F_values,
              corrected_P_values = p_values))
  }





#Test Calls: 

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


#First hand calculating the values to test the function call against: 
      perms02 <- rbind(1:nrow(beetle_composition),
                       shuffleSet(n = nrow(beetle_composition), control = perm_design_beetle, nset = 999))
      results02 <- matrix(nrow = nrow(perms02), ncol = 7)
      colnames(results02) <- c("Viereck.3", "Park", "Plot", "Sample_Year", 
                               "Viereck.3*Sample_Year", "Residual", "Total")
      for (i in 1:nrow(perms02)) {
        temp.data <- beetle_env[perms02[i, ], ]
        temp <- adonis2(beetle_composition ~ Viereck.3 + Park + 
                          Plot + Sample_Year + Viereck.3*Sample_Year,
                        data = temp.data,
                        method = "bray",
                        by = "terms",
                        permutations = 0)
        results02[i, ] <- t(temp$SumOfSqs)
      }
      
      #calculate F for park 
          results02 <- results02 |>
            data.frame() |>
            mutate(F.Park = (Park/1)/(Plot/12))
          head02 <- head(results02)
          print.data.frame(head02)
          with(results02, sum(F.Park >= F.Park[1]) / length(F.Park))
      
      #calculate F for viereck 
          results02 <- results02 |>
            data.frame() |>
            mutate(F.Viereck = (Viereck.3/1)/(Plot/12))
          head002 <- head(results02)
          print.data.frame(head002)
          #calculate P value 
          with(results02, sum(F.Viereck >= F.Viereck[1]) / length(F.Viereck))

#test calls for the function: 
          
  #recalculate viereck term ONLY 
    adjusted_beetle_result_viereck <- adjusted_permanova(
      data = beetle_env,
      matrix = beetle_composition,
      base_permutations = perm_design_beetle_time,
      adjusted_permutations = perm_design_beetle,
      corrected_F_equations = c("Viereck.3/Plot"),
      terms = "Viereck.3 + Park + Plot + Sample_Year + Viereck.3*Sample_Year",
      by = "terms",
      method = "bray"
    )
    
  #recalculate park term ONLY 
    adjusted_beetle_result_park <- adjusted_permanova(
      data = beetle_env,
      matrix = beetle_composition,
      base_permutations = perm_design_beetle_time,
      adjusted_permutations = perm_design_beetle,
      corrected_F_equations = c("Park/Plot"),
      terms = "Viereck.3 + Park + Plot + Sample_Year + Viereck.3*Sample_Year",
      by = "terms",
      method = "bray"
    )


  #testing the call using multiple corrected F equations that use the same permutation restrictions 
    multiple_term_result <- adjusted_permanova(
      data = beetle_env, 
      matrix = beetle_composition, 
      base_permutations = perm_design_beetle_time,
      adjusted_permutations = perm_design_beetle,
      corrected_F_equations = c("Viereck.3/Plot", "Park/Plot"),
      terms = "Viereck.3 + Park + Plot + Sample_Year + Viereck.3*Sample_Year",
      by = "terms",
      method = "bray"
    )
    
    
#new call with specifying permutation structure for each adjusted term 
    
    #testing the call using multiple corrected F equations that use the same permutation restrictions 
    multiple_term_result <- adjusted_permanova(
      data = beetle_env, 
      matrix = beetle_composition, 
      base_permutations = perm_design_beetle_time,
      corrected_F_equations = list(
        "Viereck.3/Plot" = perm_design_beetle,
        "Park/Plot" = perm_design_beetle),
      terms = "Viereck.3 + Park + Plot + Sample_Year + Viereck.3*Sample_Year",
      by = "terms",
      method = "bray"
    ) 
    
    

    

