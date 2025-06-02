library(readxl)
library(Matrix)
library(permute)
library(lattice)
library(dplyr)
library(vegan)
library(here)

#' Adjusted PERMANOVA with Custom F-Ratio Permutation Tests
#'
#' Performs a PERMANOVA using the \code{adonis2} function from the \code{vegan} package,
#' with the option to compute custom F-ratios using user-specified permutation structures.
#'
#' This function gives you the option to test alternate hypotheses involving adjusted F-ratios,
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
      base_model[num_term, "Pr(>F)"] <- p_value}}
  
  cat("Base PERMANOVA model (with corrected F and P values):\n")
  print(base_model)
  cat("\nCorrected F and P values:\n")
  #loop through all of the corrected values you generated 
  for (name in names(corrected_F_values)) {
    cat(name, " = ", corrected_F_values[[name]], ", P =", p_values[[name]], "\n")}
  
  return(list(base_model = base_model,
              corrected_F_values = corrected_F_values,
              corrected_P_values = p_values))}





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


-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
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
  
  #mike do you have any ideas to add to this if so let me know 
  
  
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
  if (!is.null(manual_selection)) { #if null the rest will be skipped!!!!
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
    cat("Plots with manual override:", length(manual_plots), "\n")}
  if (nrow(under_plots) > 0) {
    cat("Dropped", nrow(under_plots), "plot(s) with fewer than", n_visits, "visits:\n")
    print(under_plots[[plot_col]])}
  return(list(
    filtered_dataframe = filtered_dataframe,
    summary_table = summary_table))}}



#Example call
#first specify manual override if using 
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
  
  #draft input for complex model 
  terms <- list(
    list(name = "Vs", label = "Viereck.3", subscripts = c("s"), type = "fixed", levels = "a"),
    list(name = "Pi", label = "Park", subscripts = c("i"), type = "fixed", levels = "b"),
    list(name = "X(si)j", label = "Plot", subscripts = c("j", "(s,i)"), type = "random", levels = "(ab)c"),
    list(name = "Tk", label = "Time", subscripts = c("k"), type = "random", levels = "d"),
    list(name = "(VT)sk", label = "Interaction", subscripts = c("s", "k"), type = "random", levels = "ad"),
    list(name = "Esijk", label = "Residual", subscripts = c("s", "i", "j", "k"), type = "random", levels = "abcd"))
#NOT ready for this biz ^^^^ 





#' Generate Output Matrix for Deriving Expected Mean Squares    

#NEED TO ADD DOCUMENTATION FOR OUTPUT MATRIX FUNCTION. also need to make it into a function lmao 

#' Derive EMS Input Matrix 
#'
#' This function assists with constructing the matrix used to derive the Expected Mean Squares (EMS) for custom (? come up with better word choice here) models. 
#' The resulting output matrix includes the fixed or random status of the variable, its number of levels, and the subscript associated with it. 
#'
#' @param terms This is a list of term objects. Each object is a named list with the following elements: 
#'      \describe{
#'      \item{label}{Character. The label for the term ie Park = P. "Error" is the name for the error term.}
#'      \item{name}{Character. A common name for the term, ie Park}
#'      \item{type}{Character. Designate each term as "Fixed" or "Random"}
#'      \item{levels}{Character or numeric. The number of levels associated with each term, can be a number or letter}
#'      \item{subscripts}{Character. Subscripts that define the term structure}}
#'      
#' @return A character matrix where rows have metadata regarding the terms from the input data (fixed or random, levels, subscripts) 
#' followed by rows for each term with associated 0 and 1 entries based on parameters. 
#' 
#' @@details 
#' Additional details...
#' This function is the first part of a two step process to derive the expected mean variance of a custom model. It builds an input matrix that is then 
#' used by the derive_ems function from this package to complete the process. 
#'
#'
#'
#'
#'
#'





#first make the output matrix with a simple model 
#Simple model: Vs + Pi + Tk + Esik 

terms <- list(
  list(name = "Vs", label = "Viereck", subscripts = c("s"), type = "fixed", levels = "a"),
  list(name = "Pi", label = "Park", subscripts = c("i"), type = "fixed", levels = "b"),
  list(name = "Tk", label = "Time", subscripts = c("k"), type = "random", levels = "c"),
  list(name = "Esik", label = "Error", subscripts = c("s", "i", "k"), type = "random", levels = "abc"))


#goal: 
Fixed or random:      F   F   R 
Number of levels:     a   b   c
Subscript:            s   i   k 
Vs 
Pi 
Tk 
Esik


#assign the error term 
is_error <- sapply(terms, function(x) x$label == "Error")

#extract all non error subscripts for making the columns. unlist error = removes the one labeled error
#lapply function applies a function to each element of a list and returns a list 
#function(x) x$sunscripts - for each item x in the list, it extracts x$subscripts so that 
#can have as many subscripts as you put in the input function that aren't error
#unlist flattens the subscripts into a single vector after theyve been in multiple in the input i think 
#unique removes duplicates - there shouldnt be any but have this just in case as a failsafe because I do be making errors 
subscripts <- unique(unlist(lapply(terms[!is_error], function(x) x$subscripts)))

#create data rows for the header and tells you if each subscript is associated with a fixed or random term from the input 
#for each term (x, so as many as you input that arent errors), it checks if subscript (s) is associated with term x
#[[1]] only use first matching term, assumes one subscript belongs to one term. will need to possibly change this if using nesting


#identify the error term and assign it as error 
#logical for a logical vector (true or false)
is_error <- logical(length(terms))
#seq_along for looping through list to check if terms are error and if so gets "true" assigned in logical vector 
for (i in seq_along(terms)) {
  is_error[i] <- terms[[i]]$label == "Error"}

#object containing all non-error subscritps 
subscripts_list <- list()
#check to make sure error is not incldued in terms - look for true 
#is_error[i, number of terms] == true, skip, ==false, record. +1 = next available index i (i = 1, 2, 3, 4... depending on input)
#basically just loops through and adds subscripts to a list, then unlist to flatten, then make sure theyre unique and not duplicated 
for (i in seq_along(terms)) {
  if (!is_error[i]) {
    subscripts_list[[length(subscripts_list) + 1]] <- terms[[i]]$subscripts}}
#makes lsit of all unique subscripts 
subscripts <- unique(unlist(subscripts_list))

#extract fixed or random for each subscript 
#same as above but for assigning F or R based on input 
fixed_or_random <- character(length(subscripts))
names(fixed_or_random) <- subscripts
#loop through each subscript 
for (s in subscripts) {
  #for each subscript (s) check each term to see which terms are associated with it, because some terms could both have the same subscript if nested
  for (term in terms) {
    #checks if term label is not error and if the term's subscript incldues the current s subscript. if true then assigns 
    if (term$label != "Error" && s %in% term$subscripts) {
      fixed_or_random[s] <- if (term$type == "fixed") "F" else "R"
      break}}}
#levels as an object 
#same as above but for assigning levels associated with the input for each unique term 
levels_row <- character(length(subscripts))
names(levels_row) <- subscripts
for (s in subscripts) {
  for (term in terms) {
    if (term$label != "Error" && s %in% term$subscripts) {
      levels_row[s] <- term$levels
      #stop looking once term is found 
      break}}}

#subscript names as an onject 
subscripts_row <- subscripts

#get the term names for the matrix and make it into an object 
term_names <- character(length(terms))
for (i in seq_along(terms)) {
  term_names[i] <- terms[[i]]$name}

#create the framework for output matrix 
output_matrix <- matrix("", nrow = 3 + length(terms), ncol = length(subscripts),
                        dimnames = list(c("Fixed or random", "Number of levels", "Subscript", term_names), subscripts))

#fill in header rows for output matrix 
output_matrix["Fixed or random", ] <- fixed_or_random
output_matrix["Number of levels", ] <- levels_row
output_matrix["Subscript", ] <- subscripts_row

#subscript type for numeric values (0 = fixed, 1 = random)
subscript_type <- integer(length(subscripts))
names(subscript_type) <- subscripts
for (s in subscripts) {
  for (term in terms) {
    if (term$label != "Error" && s %in% term$subscripts) {
      subscript_type[s] <- if (term$type == "fixed") 0 else 1
      break}}}

#fill in the rows based on subscript 
for (term in terms) {
  for (s in term$subscripts) {
    if (s %in% subscripts) {
      output_matrix[term$name, s] <- as.character(subscript_type[s])}}}

#fill in remaining empty cells with the level of the column
for (term in term_names) {
  for (s in subscripts) {
    if (output_matrix[term, s] == "") {
      output_matrix[term, s] <- levels_row[s]}}}

#output matrix 
print(output_matrix, quote = FALSE)    



#function start 
derive_matrix <- function(terms) {
  is_error <- sapply(terms, function(x) x$label == "Error") #first identify the error term in the input 
  subscripts <- unique(unlist(lapply(terms[!is_error], function(x) x$subscripts)))
  #assign fixed or random based on input 
  
  
  #get levels 
  levels_row <- sapply(subscripts, function(s) {
    for(term in terms) {
      if(term$label != error && s %in% term$subscripts) {
        return(term$levels)}}})
  
  
  #make the matrix 
  output_matrix["Fixed or Random", ] <- fixed_or_random 
  output_matrix["Number of Levels", ] <- levels_row 
  output_matrix["subscript"] <- subscripts 
  
  subscript_type <- ifelse(fixed_or_random == "F", 0, 1)
}

#call 
terms <- list(
  list(name = "Vs", label = "Viereck", subscripts = c("s"), type = "fixed", levels = "a"),
  list(name = "Pi", label = "Park", subscripts = c("i"), type = "fixed", levels = "b"),
  list(name = "Tk", label = "Time", subscripts = c("k"), type = "random", levels = "c"),
  list(name = "Esik", label = "Error", subscripts = c("s", "i", "k"), type = "random", levels = "abc"))

output_matrix <- input_matrix(terms)

#results 

hopefully would give us something like this: (except prettier) 

s    i    k 
Fixed or Random     F    F    R 
Number of Levels    a    b    c 
Subscript           s    i    k 
Vs                  0    a   a
Pi                  a    0    a 
Tk                 a    a     1 
Esik                a   b    c 




-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------    
  
  
  #derive EMS function documentation 
  
  #' Calculate Expected Mean Squares (EMS) from Output Matrix
  #'
  #' This function derives the partitioned variance for the Expected Mean Squares (EMS)
  #' associated with each term in a model, using a matrix representation of 
  #' subscripts, nesting, and factor types that is generated using the \code{generate_ems_matrix} function. 
  #'
  #' @param output_matrix A matrix where rows represent model terms,
  #' and columns represent subscripts. The top three rows include:
  #'   - "Fixed or random": indicates whether each model term is fixed ("F") or random ("R")
  #'   - "Number of levels": the number of levels associated with each subscript, represented as a letter variable different from the subscript letter
  #'   - "Subscript": the subscript associated with each term, representing the number of groups associated with the associated term 
  #' @param terms A list where each element is a list containing:
  #'   - \code{name}: the name of the term as it appears in the output matrix
  #'   - \code{type}: either "fixed" or "random"
  #' @return A named list of EMS expressions, one for each term in the model. 
  #' @examples
  #' \dontrun{
  #' # Example output_matrix and terms
  #' output_matrix <- matrix(c(
  #'   "F", "F", "R",
  #'   "a", "b", "c",
  #'   "s", "i", "k",
  #'   "0", "b", "c",
  #'   "a", "0", "c",
  #'   "a", "b", "1",
  #'   "0", "0", "1"
  #' ), nrow = 7, byrow = TRUE)
  #' rownames(output_matrix) <- c("Fixed or random", "Number of levels", "Subscript",
  #'                              "Vs", "Pi", "Tk", "Esik")
  #' colnames(output_matrix) <- c("s", "i", "k")
  #' terms <- list(
  #'   list(name = "Vs", type = "fixed"),
  #'   list(name = "Pi", type = "fixed"),
  #'   list(name = "Tk", type = "random"),
  #'   list(name = "Esik", type = "random")
  #' )
  #'
  #' ems_list <- calculate_ems(output_matrix, terms)
  #' print(ems_list)
  #' }
  #'
  #' @export
  
  #function script 
  #input your matrix from above function when I finish it, and the terms which is a list that you specify with saying its fixed or random etc.
  calculate_ems <- function(output_matrix, terms) {
    term_names <- rownames(output_matrix)[-(1:3)]  #skip header rows (ie the number of levels and fixed or random)
    subscripts <- colnames(output_matrix) #pulls the column names and assigns them as the subscripts 
    
    #create lookup for subscript types and levels, this row stores the number of levels for each subscript 
    levels_lookup <- output_matrix["Number of levels", ]
    #this will tell whether each subscript is fixed or random based on the term it's associated with 
    subscript_type <- output_matrix["Fixed or random", ]
    
    #create a named list to store EMS expressions for each term 
    ems_list <- list()
    
    for (term_name in term_names) {
      #get the current term's subscripts in the loop, does this loop for each model term in the input matrix 
      current_subscripts <- subscripts[output_matrix[term_name, ] %in% c("0", "1")]
      #extracts which subscripts are relevant (ie marked as 0 or 1 in the matrix) to the current loop through 
      
      #identify which columns will be used for that step (ie this column is not used for this row so it will be excluded, these will be kept, etc)
      columns_to_keep <- setdiff(subscripts, current_subscripts)
      #ie which will contribute to the ems of that term 
      
      #for each row, check if it contains all current_subscripts
      contributing_terms <- c()
      #does it contribute to the current EMS if so then proceed 
      for (other_term in term_names) {
        other_subscripts <- subscripts[output_matrix[other_term, ] %in% c("0", "1")]
        if (all(current_subscripts %in% other_subscripts)) {
          #build product of values from columns not in current_subscripts (ie those not "hidden" with a pencil like on the paper)
          visible_cols <- output_matrix[other_term, columns_to_keep]
          product_factors <- visible_cols[visible_cols != ""]
          product_expr <- paste(product_factors, collapse = " * ")
          #basically for the visible subscripts (columns outside the current term) it collects their number of levels and multiplies them together
          
          #determine if fixed or random
          this_type <- terms[[which(sapply(terms, function(x) x$name == other_term))]]$type
          scale_symbol <- if (this_type == "random") paste0("sigma^2[", other_term, "]") else paste0("phi[", other_term, "]")
          #assigns sigma2 to random variance and i can't figure out how to make it assign the summation notation easily so i just said phi. i'll fix this in an update soon
          
          #checks if scaling multiplier is needed, formats teh component correctly, and adds it to a list of terms that will be summed together to form the full EMS for that term 
          #product_expr is the string that is the product of used factor levels (NOT IN THE CURRENT TERMS SUBSCRIPTS) but in the contributing term 
          #ie the number of times you're multiplying the factor. ie if the subscript has x factor levels and htat gets carried down in that step of deriving EMS 
          #pull down scale symbol to determine what gets multiplied ie sigma2 for random and phi for fixed 
          if (product_expr != "") { #ie if there are levels that get multiplied the variance component should get multiplied by this 
            full_expr <- paste(scale_symbol, "*", product_expr)
          } else {
            full_expr <- scale_symbol} #if there arent any levels to get multiplied for htat term, then you use the variance term as is depending on fixed or random
          contributing_terms <- c(contributing_terms, full_expr)}} #join all contributing terms as a string to be joined together later 
      
      #convert to bquote expression for ease of translation, combines all terms associated with that variable with a + sign to form the full expression 
      #used bquote so it looks nicer and more readable 
      if (length(contributing_terms) > 0) {
        ems_list[[term_name]] <- bquote(.(paste(contributing_terms, collapse = " + ")))}}
    
    return(ems_list) #get output }
    
    #print results 
    ems_results <- calculate_ems(output_matrix, terms)
    print(ems_results)
    
    
    
    -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------    
      
      
      #PLOT EMS RESULTS PRETTY ?????? becasue this current output is ugly AF   >:( 
      
      plot.new()
    #create PNG to save the plot to 
    png("EMS_output.png", width = 800, height = 400)
    
    #make blank plot
    par(mar = c(4, 4, 2, 2))  #adjust margins
    plot(1, type = "n", xlim = c(0, 1), ylim = c(0, length(ems_results) + 1),
         xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "", main = "Expected Mean Squares")
    
    #draw each expression with bquote formatting
    i <- length(ems_results)
    for (term in names(ems_results)) {
      text(0, i, labels = bquote(EMS[.(term)] == .(ems_results[[term]])), pos = 4, cex = 1.4)
      i <- i - 1}
    
    #define the EMS expressions using bquote() so they look better 
    ems_list <- list(
      Vs   = bquote(b * c * phi[Vs] + sigma^2[Esik]),
      Pi   = bquote(a * c * phi[Pi] + sigma^2[Esik]),
      Tk   = bquote(a * b * sigma^2[Tk] + sigma^2[Esik]),
      Esik = bquote(sigma^2[Esik])
    )
    
    #adding the fixed factor expression with the summation notation?? 
    #create new png to write on 
    png("output_with_equation.png", width = 800, height = 600)
    
    #make a blank plot 
    dev.off()
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))
    
    #add the equation 
    text(
      x = 0.5, y = 0.5,
      expression(frac(1, a - 1) * sum(alpha[i]^2, i == 1, a)),
      cex = 2
    )  
    
    
    -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      
      #Package - putting it all together (adjusted_permanova, derive_ems, calculate_matrix, balance_dataframe) 
      
      
      
      #install required packages to create a package 
      install.packages(c("usethis", "devtools", "roxygen2", "testthat"))
    
    #run this in console:  
    usethis::create_package("path/to/Adjusted_Permanova")
    
    #place all of your function files into the package directory once theyre done 
    usethis::use_r("adjusted_permanova")
    usethis::use_r("derive_ems")
    usethis::use_r("balance")
    usethis::use_r("calculate_matrix")
    
    #use roxygen style comments above each function to document them 
    ----
      
      devtools::document()
    
    #add dependencies 
    usethis::use_package("dplyr")
    usethis::use_package("vegan")
    
    #include example data 
    usethis::use_data_raw("beetle_data")
    usethis::use_data(beetle_data, overwrite = TRUE)
    
    #load and test package then test functions like normal 
    devtools::load_all()
    
    #run checks for problems 
    devtools::check()
    
    #install package locally 
    devtools::install()
    
    
    
    
    
    
    -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      
      
      #saving data as the example data for package 
      
      #load data into R 
      beetle_matrix <- beetle_distance_matrix
    beetle_env <- beetle_env
    
    #save them for package use
    usethis::use_data(beetle_matrix, beetle_env, overwrite = TRUE)
    
    #document using Roxygen 
    #' Example Distance Matrix
    #'
    #' A sample distance matrix representing dissimilarity among bark beetle disturbed sites.
    #'
    #' @format A matrix with X rows and X columns.
    #' @usage data(beetle_matrix)
    #' @examples
    #' data(beetle_matrix)
    "beetle_matrix"
    
    #' Example Environmental Metadata
    #'
    #' A sample metadata dataframe with grouping factors such as sample year, vegetation subclass, and canopy cover percent.
    #'
    #' @format A data frame with X rows and X columns.
    #' @usage data(beetle_env)
    #' @examples
    #' data(beetle_env)
    "beetle_env"
    
    #save both DFs in an .R file in the R/ directory of the package (R/data_matrix.R).
    
    #' @examples
    #' data(beetle_matrix)
    #' data(beetle_env)
    #' 
    #' base_perm <- permute::how(within = permute::Within(type = "free"), blocks = beetle_env$Block)
    #' corrected_struct <- permute::how(within = permute::Within(type = "none"), blocks = beetle_env$Plot)
    #' 
    #' do i need to add in base perm in here? like pre-loaded or does it get pulled in with the pre-load from the function... check on tjhis
    #' 
    #' adjusted_permanova(
    #'   data = beetle_env,
    #'   matrix = beetle_matrix,
    #'   base_permutations = base_perm,
    #'   corrected_F_equations = list("Group/Plot" = corrected_struct),
    #'   terms = "Group + Plot"
    #' )
    
    
    
    
    