library(readxl)
library(Matrix)
library(permute)
library(lattice)
library(dplyr)
library(vegan)
library(here)
 
#' #' Adjusted PERMANOVA with Custom F-Ratio Permutation Tests
#'
#' Performs a PERMANOVA using the \code{adonis2} function from the \code{vegan} package,
#' with the option to compute custom F-ratios using user-specified permutation structures.
#'
#' This function gives you the option to test hypotheses involving adjusted F-ratios,
#' which is done by applying different permutation control structures to the numerator and denominator terms
#' of the F-test and by specifying the desired numerator and denominator associated with your specified hypotheses.
#'
#' @param data A data frame containing the grouping variables (factors) associated with each unit (such as plot or visit).
#' @param matrix A raw data matrix (which will be converted to a distance matrix internally using the distance parameter you specify) that 
#' contains each species as a column and each row as a unit, with data as either presence-absence or counts.
#' @param base_permutations A \code{how()} object from the \code{permute} package used for the base PERMANOVA that determines how your non-adjusted terms will be permuted.
#' @param corrected_F_equations A named list of expressions indicating F-ratio comparisons, where names
#' are in the form \code{"Term1/Term4"} and values are the corresponding permutation structures
#' (e.g., from \code{how()}).
#' @param terms A character string specifying all of the individual terms of the model formula (e.g., \code{"A + B + C"}). Include all terms you would like to partition 
#' variance among, excluding residual and total which will be added internally. 
#' @param by A string that is passed to \code{adonis2}, indicating how to partition sums of squares; typically
#' \code{"terms"} or \code{"margin"}.
#' @param method A string specifying the distance method to use. Defaults to \code{"bray"} but \code{"jaccard"}, 
#' \code{"euclidian"}, \code{"manhattan"} or other distance measures can be used as long as they are compatable with 
#' adonis2 from the \code{vegan} package.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{\code{base_model}}{The base model PERMANOVA table with corrected F and p-values inserted where applicable.}
#'   \item{\code{corrected_F_values}}{A named list of custom-calculated F-ratios. They are also present in the base model table}
#'   \item{\code{corrected_P_values}}{A named list of p-values for each custom F-ratio test. They are also present in the base model table}
#' }
#'
#' @examples
#' \dontrun{
#' # Define permutation structures with permute::how()
#' ps1 <- how(plots = Plots(strata = variable_data$FactorB, type = c("free")), within = Within(type = "free"), nperm = 999)
#' ps2 <- how(Plots(strata = variable_data$FactorB, type = c("free")), within = Within(type = "series", mirror = FALSE), nperm = 999)
#'
#' adjusted_permanova(
#'   data = variable_data,
#'   matrix = species_matrix,
#'   base_permutations = ps1,
#'   corrected_F_equations = list("A/B" = ps1, "C/A" = ps2),
#'   terms = "A + B + C"
#'   by = "terms", 
#'   method = "bray"
#' )
#' }
#'
#' @import vegan
#' @importFrom permute shuffleSet how Within
#' @export




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
beetle_df <- read_xlsx(here("beetle_df_vasc_filtered.xlsx"))
beetle_env <- read_xlsx(here("beetle_env_vasc_filtered.xlsx"))

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
      method = "bray") 
    
    

 
    
    
    
    
    
    
    
    
    
#EMS function 
    
  #draft input 
    terms <- list(
      list(name = "Vs", label = "Viereck.3", subscripts = c("s"), type = "fixed", levels = "a"),
      list(name = "Pi", label = "Park", subscripts = c("i"), type = "fixed", levels = "b"),
      list(name = "X(si)j", label = "Plot", subscripts = c("j", "(s,i)"), type = "random", levels = "(ab)c"),
      list(name = "Tk", label = "Time", subscripts = c("k"), type = "random", levels = "d"),
      list(name = "(VT)sk", label = "Interaction", subscripts = c("s", "k"), type = "random", levels = "ad"),
      list(name = "Esijk", label = "Residual", subscripts = c("s", "i", "j", "k"), type = "random", levels = "abcd"))

