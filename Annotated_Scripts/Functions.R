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
    

    TO DO: 
    1. make the matrix script into a function 
  2. figure out how to accomodate nesting and complex stuff 
  3. make the output prettier (more better?)
  4. finish documenting with roxygen 
  5. finish loading example data for test calls 
  6. beef up roxygen documentation for all of these. especially with dependency packages, figure out which one uses what 
  7. ?????? 
    8. package 
  9. adjust the balancing dataframes function to accomodate other things. ie park or elevation band or number of plots 
  per vegetation class? add it to the call and you specify which variable you want it to balance  
  10. adjust years param in balance function to accomodate multiple visits in the same year ie visit_tag or something instead of explicitly year? 

#balancing function 
  
  #Original script - not a function 
  #hand select the years for plots with more than the decided number of visits 
  filter_criteria <- list(
    Plot1 = c(Selected_Year, Selected_Year, Selected_Year),
    Plot2 = c(Selected_Year, Selected_Year, Selected_Year),
    Plot3 = c(Selected_Year, Selected_Year, Selected_Year),
    Plot4	= c(Selected_Year, Selected_Year, Selected_Year))
  
  #next identify plots with three plot visits from your vegetation class (or whatever the number you've decided is)
  selected_plots <- summary_table_of_choice %>%
    filter(Vegetation_Class == "Desired Vegetation Class", Number_Visits == 3) %>%
    pull(Plot)
  
  #combine plots with hand selected plots from filter criteria 
  selected_plots <- union(selected_plots, names(filter_criteria))
  
  #subset main presence absence dataframe based on both conditions 
  #verify that the hand selected plots were correctly pulled 
  subset_hand_selected <- quad_abundance_dataframe_of_choice %>%
    filter(
      Plot %in% names(filter_criteria) & 
        Sample_Year %in% unlist(filter_criteria[Plot]))
  
  balanced_df <- quad_abundance_dataframe_of_choice %>%
    filter(
      (Plot %in% selected_plots) | 
        (Plot %in% names(filter_criteria) &
           Sample_Year %in% unlist(filter_criteria[Plot])))
  

#roxygen documentation 

#' Balance Plot Visits for PERMANOVA Analysis 
#'
#' Subsets a data frame to ensure a specified number of visits per plot for PERMANOVA analyses using \code(Adonis2) in the \code(Vegan) Package or the Adjusted_Permanova Package.
#' If some plots have more visits than the desired number, this function selects a subset of years 
#' that are most similar to those in plots already balanced at the target number of visits.
#'
#' Optionally has a manual override built in to specify which years to retain for particular plots of interest.
#'
#' @param df A data frame containing plot and year information.
#' @param plot_col The name of the column in `df` that identifies plots.
#' @param year_col The name of the column in `df` that identifies years.
#' @param n_visits A number specifying the number of visits (years) to retain per plot.
#' @param manual_selection Optional named list specifying plots as names and vectors of years to retain. 
#'   This overrides automatic selection for those plots.
#'
#' @return A data frame (`filtered_df`) that retains only the selected visits for plots with exactly `n_visits`.
#'
#' @examples
#' #Assume you have a data frame called 'veg_df' with columns "Plot" and "Year" and you want to retain two visits of your choice
#' balanced <- balance_visits(
#'   df = veg_df,
#'   plot_col = "Plot",
#'   year_col = "Year",
#'   n_visits = 2)
#'
#' # With manual override
#' manual_years <- list(
#'   "Plot4" = c(2012, 2015),
#'   "Plot5" = c(2013, 2016))
#'
#' balanced_manual <- balance_visits(
#'   df = veg_df,
#'   plot_col = "Plot",
#'   year_col = "Year",
#'   n_visits = 2,
#'   manual_selection = manual_years)
#'
#'@import dyplyr tidyr (i think, double check on this though)
#'
#' @export    
  
  
  
#balancing function 
balance_visits <- function(df, 
                           plot_col = "Plot", 
                           year_col = "Sample_Year", 
                           n_visits = 2,
                           manual_selection = NULL) { #use null if you aren't manually selecting plots, use manual_plots in selection if you are choosing specific ones
  #this makes a df that has one row per plot and a list of all years that plot was visited. 
  #basically the summary table from the building dataframes section, which is what I used to hand select and balance things. 
  #this way you can look at the years and specify if you want a manual override or to let it handle it auto-style 
 
  plot_years <- df %>%
    dplyr::select(.data[[plot_col]], .data[[year_col]]) %>% #selects just the two columns of interest from the dataframe, one for plotIDs and one for sample years.
    #used .data[[name]] instead of actual column name so that you can have variation in what you call the column to have flexibility in future use 
    distinct() %>%
    group_by(.data[[plot_col]]) %>%
    summarize(Years = list(sort(unique(.data[[year_col]]))), .groups = "drop")
  
  #manual override handling of selection 
  manual_df <- NULL #creates items for manual selection if used. this is the longform version of selected years if added 
  manual_summary <- NULL #this is the labeled version of the years for if you want to visualize/print it, for readability 
  manual_plots <- character() #names of plots hand selected, skips auto-selection for them as it sifts through. makes an empty vector to store names of plots taht were manually selected. 
  #use this to exclude plots from automatic processing later on 
  if (!is.null(manual_selection)) { #if null the rest will be skipped! 
    manual_df <- tibble::tibble( #turns named list of years into a tibble for ease of use 
      !!plot_col := names(manual_selection),
      Selected_Years = manual_selection
    ) %>%
      tidyr::unnest(cols = Selected_Years) %>% #expands each row into multiple rows (longform table) ie plot1 c(2018, 2020) into plot1 2018
                                                                                                              # plot1 2020 
      dplyr::rename(Selected_Year = Selected_Years) #since now its long form instead of c(year1, year2) rename column to reflect that 
    manual_summary <- manual_df %>%
      mutate(Selection_Type = "Manual") #adds manual as the selection type for designation 
    manual_plots <- unique(manual_df[[plot_col]]) #grabs list of plot names in this section for later use 
  }
  
  #remove manually selected plots from being part of the auto-selection process if there are some specified in call, so they 
  #don't get processed twice (manual and auto)
  plot_years <- plot_years %>%
    filter(!(!!rlang::sym(plot_col) %in% manual_plots)) #keep rows where the plot column is NOT in the list of manual plots; tidy eval
  #categorize plots based on how many visits they have 
  #plots woth the desired number of visits: 
  exact_plots <- plot_years %>%
    filter(lengths(Years) == n_visits)
  #plots with more than the desired number of visits (need to be decreased)
  over_plots <- plot_years %>%
    filter(lengths(Years) > n_visits)
  #plots with less than teh number of visits (will be excluded)
  under_plots <- plot_years %>%
    filter(lengths(Years) < n_visits)
  #figure out what the common year combos are among plots with the correct number of visits, to have it match in its auto-selection 
  #this will guide the auto-selection's decisions on the over_plots 
  common_combos <- exact_plots$Years %>%
    purrr::map_chr(~ paste(sort(.x), collapse = "_")) %>% #takes the vector of years and sorts them in ascending order, and then joins them as a list with underscores. 
    #ie c(2012, 2014) would become 2012_2014. used purr from tidyverse to return a character vector so it's easier to tally up with table. used laaply() originally but had to unlist and it got messy down the line and this is just easier. 
    #i also tried sapply() but this just worked better because map_chr() guarantees it returns as a character vector instead of sapply() assuming it's a character but sometimes listing it as something else and giving me an error 
    table() %>% #creates a frequewncy table for the year combinations to see what the common ones are 
    sort(decreasing = TRUE) #sort from msot common to least common 
  top_combo <- strsplit(names(common_combos)[1], "_")[[1]] %>% as.numeric() #decide what the most common one is, split it back from the string into the OG format 
  
  #originally was 
    #common_combos <- table(
      #sapply(exact_plots$Years, function(x) paste(sort(x), collapse = "_"))
      # ) %>%
      #sort(decreasing = TRUE)
  
  
  #auto select closest matching years from the over_plots list of plots 
  #basically it generates all combinations of n_visits and then picks the closest to the most similar combination using absolute year differences 
  over_plots <- over_plots %>%
    mutate(
      Selected_Years = purrr::map(Years, function(yrs) { #for each element of years, applies function to select n_visits subset and purrr:map returns a list 
        combs <- combn(yrs, n_visits, simplify = FALSE) #combn to generate all combos of n_visits years from the full list of years, simplify = false to get a list of vectors 
        combs[which.min(sapply(combs, function(x) sum(abs(sort(x) - top_combo))))][[1]]})) #sort(x) to put years and top combo in order, abs(sortx-topcombo) 
          #calculates the difference between the combos of years ie (2012, 2014) and (2012, 2015) would return something like (0,1) and then these are aded together
          #so the lower the overall sum the closer the combo of years is to top combo if that makes sense (used which.min to find the lowest)
  
  #combine the exact_plots and the auto-selected plots 
  selected_auto <- bind_rows(
    exact_plots %>% mutate(Selected_Years = Years),
    over_plots
  ) %>%
    select(!!plot_col, Selected_Years) %>%
    tidyr::unnest(cols = Selected_Years) %>% #so each year becomes its own row again 
    dplyr::rename(Selected_Year = Selected_Years) %>% #changes the year column name to reflect that 
    mutate(Selection_Type = "Auto") #adds a selection type column for tracking if you want to knwo which plots were auto selected and which were manual 
  
  #add in manual selections if specified, optional if nothing is specified in the call 
  if (!is.null(manual_summary)) {
    all_selected <- bind_rows(selected_auto, manual_summary)
  } else {
    all_selected <- selected_auto}
  
  #filter dataframe with plot criteria outlined above now that the specification has been made 
  filtered_dataframe <- df %>%
    dplyr::inner_join(all_selected,
                      by = setNames(c(plot_col, year_col), c(plot_col, "Selected_Year")))
  
  #generate summary table with selected plots 
  summary_table <- all_selected %>%
    arrange(.data[[plot_col]], Selected_Year) #use .data for flexibility 
  
  #print results with clarity in whats what
  cat("balance Summary\n")
  cat("Plots with exact/trimmed years (auto):", length(unique(selected_auto[[plot_col]])), "\n")
  if (!is.null(manual_summary)) {
    cat("Plots with manual override:", length(manual_plots), "\n")
  }
  if (nrow(under_plots) > 0) {
    cat("Dropped", nrow(under_plots), "plot(s) with fewer than", n_visits, "visits:\n")
    print(under_plots[[plot_col]])
  }
  return(list(
    filtered_dataframe = filtered_dataframe,
    summary_table = summary_table
  ))
}
}



#Example call
#first specify manual override 
manual_override <- list(
  KATM_2009_01_S996 = c(2012, 2014, 2019),
  LACL_2010_01_S995 = c(2012, 2014, 2019))
#then call for filtered DF using criteria 
filtered_df <- balance_visits(
  quad_abundance_df_vascular_filtered,
  plot_col = "Plot",
  year_col = "Sample_Year",
  n_visits = 2,
  manual_selection = manual_override)


#updated call with manual override built in instead of separate item 
result <- balance_visits(
  df = quad_abundance_df_vascular_filtered,
  plot_col = "Plot",
  year_col = "Sample_Year",
  n_visits = 2,
  manual_selection = list(
    "KATM_2009_01_S996" = c(2012, 2014),
    "LACL_2010_01_S995" = c(2014, 2019)))

#otehr call option, if manual override is not utilized 
result <- balance_visits(
  df = quad_abundance_df_vascular_filtered,
  plot_col = "Plot",
  year_col = "Sample_Year",
  n_visits = 2,
  manual_selection = NULL)

  
  
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
  

  
#EMS function - MATRIX GENERATION 
    
#EMS function 
    
  #draft input 
    terms <- list(
      list(name = "Vs", label = "Viereck.3", subscripts = c("s"), type = "fixed", levels = "a"),
      list(name = "Pi", label = "Park", subscripts = c("i"), type = "fixed", levels = "b"),
      list(name = "X(si)j", label = "Plot", subscripts = c("j", "(s,i)"), type = "random", levels = "(ab)c"),
      list(name = "Tk", label = "Time", subscripts = c("k"), type = "random", levels = "d"),
      list(name = "(VT)sk", label = "Interaction", subscripts = c("s", "k"), type = "random", levels = "ad"),
      list(name = "Esijk", label = "Residual", subscripts = c("s", "i", "j", "k"), type = "random", levels = "abcd"))

