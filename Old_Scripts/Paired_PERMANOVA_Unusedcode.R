# Paired PERMANOVA For Vegetation Classes 

```{r}
library(readxl)
library(vegan)
library(permute)
library(lattice)
library(pairwiseAdonis)
library(writexl)

vascpresence_absence_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/presence_absence_df_full.xlsx")
lichenspresence_absence_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/lichenspresence_absence_df_full.xlsx")
nonvascpresence_absence_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/nonvascpresence_absence_df_full.xlsx")

vasc_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/vasc_df_balanced.xlsx")
nonvasc_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/nonvasc_df_balanced.xlsx")
lichens_df <- read_xlsx("T:/Users/KPace/SWAN-Internship-New/Data/Modified/lichens_df_balanced.xlsx")

lichenspresence_absence_df <- read_xlsx("C:/Users/kmpace/Desktop/LICHENPRESENCEANSENCEBALANCED.xlsx")
lichens_env <- read_xlsx("C:/Users/kmpace/Desktop/LICHENENVBALANCED.xlsx")

perm_design_lichen = how(
  plots = Plots(strata = lichens_env$Plot, type = c("free")),
  within = Within(type = "none"),
  nperm = 999)

lichen_comp <- lichenspresence_absence_df[,c(13:179)]
lichen_comp <- as.matrix(lichen_comp)

lichenspresence_absence_df <- lichenspresence_absence_df[order(lichenspresence_absence_df$Plot_Year), ]
lichens_env <- lichens_env[order(lichens_env$Plot_Year), ]

lichenspresence_absence_df <- lichenspresence_absence_df[-c(19,22,65,68,123,124,125,128,145,158,161,164,167,172,175,186,193,194,197), ]
lichens_env <- lichens_env[-c(19,22,65,68,123,124,125,128,145,158,161,164,167,172,175,186,193,194,197), ]

table(lichenspresence_absence_df$Park, lichenspresence_absence_df$Plot)



str(lichens_env)
lichens_env$Park <- as.factor(lichens_env$Park)
lichens_env$Viereck.3 <- as.factor(lichens_env$Viereck.3)
lichens_env$Viereck.2 <- as.factor(lichens_env$Viereck.2)
lichens_env$Viereck.1 <- as.factor(lichens_env$Viereck.1)
lichens_env$Vegetation_Class <- as.factor(lichens_env$Vegetation_Class)



lichen_pairwise <- adonis2(lichen_comp ~ Viereck.3 + Plot,
                           data = lichens_env, method = "bray",
                           permutations = perm_design_lichen,
                           by = "terms")
lichen_pairwise

#write_xlsx(lichens_env, "/Users/kimberlynpace/Desktop/LICHENENVBALANCED.xlsx")
#write_xlsx(lichenspresence_absence_df, "/Users/kimberlynpace/Desktop/LICHENPRESENCEANSENCEBALANCED.xlsx")


dist_matrix <- vegdist(lichen_comp, method = "bray")

pairwise_results <- pairwise.adonis2(dist_matrix ~ Viereck.3, data = lichens_env, permutations = perm_design_lichen)
print(pairwise_results)

extract_results <- function(results_list) {
  df_list <- lapply(names(results_list), function(pair) {
    result <- results_list[[pair]]
    data.frame(
      Comparison = pair,
      Term = rownames(result),
      R2 = result$R2,
      F_stat = result$F,
      p_value = result$`Pr(>F)`,
      stringsAsFactors = FALSE)})
  do.call(rbind, df_list)}


dist_matrix <- vegdist(lichen_comp, method = "bray")
groups <- unique(lichens_env$Viereck.3)
pairwise_results <- list()

for (i in 1:(length(groups) - 1)) {
  for (j in (i + 1):length(groups)) {
    
    subset_idx <- lichens_env$Viereck.3 %in% c(groups[i], groups[j])
    env_subset <- lichens_env[subset_idx, , drop = FALSE]
    comp_subset <- lichen_comp[subset_idx, ]
    
    perm_subset <- how(nperm = perm_design_lichen$nperm) 
    setBlocks(perm_subset) <- perm_design_lichen$blocks[subset_idx]  
    
    pairwise_results[[paste(groups[i], "vs", groups[j])]] <- adonis2(
      vegdist(comp_subset, method = "bray") ~ Viereck.3 + Plot, 
      data = env_subset, 
      method = "bray", 
      permutations = perm_subset,  
      by = "terms"
    )
  }
}

pairwise_results
viereck3_lichen_paired <- extract_results(pairwise_results)


#viereck 2
dist_matrix <- vegdist(lichen_comp, method = "bray")
groups <- unique(lichens_env$Viereck.2)
pairwise_results <- list()

for (i in 1:(length(groups) - 1)) {
  for (j in (i + 1):length(groups)) {
    
    subset_idx <- lichens_env$Viereck.2 %in% c(groups[i], groups[j])
    env_subset <- lichens_env[subset_idx, , drop = FALSE]
    comp_subset <- lichen_comp[subset_idx, ]
    
    perm_subset <- how(nperm = perm_design_lichen$nperm) 
    setBlocks(perm_subset) <- perm_design_lichen$blocks[subset_idx]  
    
    pairwise_results[[paste(groups[i], "vs", groups[j])]] <- adonis2(
      vegdist(comp_subset, method = "bray") ~ Viereck.2 + Plot, 
      data = env_subset, 
      method = "bray", 
      permutations = perm_subset,  
      by = "terms")}}

pairwise_results
viereck2_lichen_paired <- extract_results(pairwise_results)

#veg sight assigned
dist_matrix <- vegdist(lichen_comp, method = "bray")
groups <- unique(lichens_env$Vegetation_Class)
pairwise_results <- list()

for (i in 1:(length(groups) - 1)) {
  for (j in (i + 1):length(groups)) {
    
    subset_idx <- lichens_env$Vegetation_Class %in% c(groups[i], groups[j])
    env_subset <- lichens_env[subset_idx, , drop = FALSE]
    comp_subset <- lichen_comp[subset_idx, ]
    
    perm_subset <- how(nperm = perm_design_lichen$nperm) 
    setBlocks(perm_subset) <- perm_design_lichen$blocks[subset_idx]  
    
    pairwise_results[[paste(groups[i], "vs", groups[j])]] <- adonis2(
      vegdist(comp_subset, method = "bray") ~ Vegetation_Class + Plot, 
      data = env_subset, 
      method = "bray", 
      permutations = perm_subset,  
      by = "terms")}}

pairwise_results
veg_lichen_paired <- extract_results(pairwise_results)


#bind all results together 
paired_list <- list(viereck3_lichen_paired, viereck2_lichen_paired, veg_lichen_paired)
lichen_combined_results <- bind_rows(paired_list)
write_xlsx(lichen_combined_results, "C:/Users/kmpace/Desktop/LICHEN_PairedTest_Results.xlsx")








#vascular species 

vasc_df <- read_xlsx("C:/Users/kmpace/Desktop/vasc_df_balanced.xlsx")

vasc_df <- vasc_df %>%
  group_by(Plot) %>%
  filter(!any(is.na(Viereck.3))) %>%
  ungroup()

vasc_env_balanced <- vasc_df[,c(1:10)]

vasc_composition <- vasc_df[,c(11:284)]
vasc_composition <- as.matrix(vasc_composition) 

perm_design_vasc = how(
  plots = Plots(strata = vasc_env_balanced$Plot, type = c("free")),
  within = Within(type = "none"),
  nperm = 999)

vasc_pairwise <- adonis2(vasc_composition ~ Vegetation_Class + Plot,
                         data = vasc_env_balanced, method = "bray",
                         permutations = perm_design_vasc,
                         by = "terms")
vasc_pairwise



#veg class sight assigned 

dist_matrix <- vegdist(vasc_composition, method = "bray")
groups <- unique(vasc_env_balanced$Vegetation_Class)
pairwise_results <- list()

for (i in 1:(length(groups) - 1)) {
  for (j in (i + 1):length(groups)) {
    
    subset_idx <- vasc_env_balanced$Vegetation_Class %in% c(groups[i], groups[j])
    env_subset <- vasc_env_balanced[subset_idx, , drop = FALSE]
    comp_subset <- vasc_composition[subset_idx, ]
    
    perm_subset <- how(nperm = perm_design_vasc$nperm) 
    setBlocks(perm_subset) <- perm_design_vasc$blocks[subset_idx]  
    
    pairwise_results[[paste(groups[i], "vs", groups[j])]] <- adonis2(
      vegdist(comp_subset, method = "bray") ~ Vegetation_Class + Plot, 
      data = env_subset, 
      method = "bray", 
      permutations = perm_subset,  
      by = "terms")}}

pairwise_results
veg_vasc_paired <- extract_results(pairwise_results)

#viereck 3
dist_matrix <- vegdist(vasc_composition, method = "bray")
groups <- unique(vasc_env_balanced$Viereck.3)
pairwise_results <- list()

for (i in 1:(length(groups) - 1)) {
  for (j in (i + 1):length(groups)) {
    
    subset_idx <- vasc_env_balanced$Viereck.3 %in% c(groups[i], groups[j])
    env_subset <- vasc_env_balanced[subset_idx, , drop = FALSE]
    comp_subset <- vasc_composition[subset_idx, ]
    
    perm_subset <- how(nperm = perm_design_vasc$nperm) 
    setBlocks(perm_subset) <- perm_design_vasc$blocks[subset_idx]  
    
    pairwise_results[[paste(groups[i], "vs", groups[j])]] <- adonis2(
      vegdist(comp_subset, method = "bray") ~ Viereck.3 + Plot, 
      data = env_subset, 
      method = "bray", 
      permutations = perm_subset,  
      by = "terms")}}

pairwise_results
viereck3_vasc_paired <- extract_results(pairwise_results)

#viereck 2
dist_matrix <- vegdist(vasc_composition, method = "bray")
groups <- unique(vasc_env_balanced$Viereck.2)
pairwise_results <- list()

for (i in 1:(length(groups) - 1)) {
  for (j in (i + 1):length(groups)) {
    
    subset_idx <- vasc_env_balanced$Viereck.2 %in% c(groups[i], groups[j])
    env_subset <- vasc_env_balanced[subset_idx, , drop = FALSE]
    comp_subset <- vasc_composition[subset_idx, ]
    
    perm_subset <- how(nperm = perm_design_vasc$nperm) 
    setBlocks(perm_subset) <- perm_design_vasc$blocks[subset_idx]  
    
    pairwise_results[[paste(groups[i], "vs", groups[j])]] <- adonis2(
      vegdist(comp_subset, method = "bray") ~ Viereck.2 + Plot, 
      data = env_subset, 
      method = "bray", 
      permutations = perm_subset,  
      by = "terms")}}

pairwise_results
viereck2_vasc_paired <- extract_results(pairwise_results)


#bind all results together 
paired_list <- list(viereck3_vasc_paired, viereck2_vasc_paired, veg_vasc_paired)
vasc_combined_results <- bind_rows(paired_list)
write_xlsx(vasc_combined_results, "C:/Users/kmpace/Desktop/VASC_PairedTest_Results.xlsx")










#nonvascular species 

nonvasc_df <- read_xlsx("C:/Users/kmpace/Desktop/nonvasc_df_balanced.xlsx")

nonvasc_df <- nonvasc_df %>%
  group_by(Plot) %>%
  filter(!any(is.na(Viereck.3))) %>%
  ungroup()

nonvasc_env_balanced <- nonvasc_df[,c(1:12)]
nonvasc_composition <- nonvasc_df[,c(13:219)]
nonvasc_composition <- as.matrix(nonvasc_composition) 

perm_design_nonvasc = how(
  plots = Plots(strata = nonvasc_env_balanced$Plot, type = c("free")),
  within = Within(type = "none"),
  nperm = 999)

nonvasc_pairwise <- adonis2(nonvasc_composition ~ Vegetation_Class + Plot,
                            data = nonvasc_env_balanced, method = "bray",
                            permutations = perm_design_nonvasc,
                            by = "terms")
nonvasc_pairwise







#veg class sight assigned 

dist_matrix <- vegdist(nonvasc_composition, method = "bray")
groups <- unique(nonvasc_env_balanced$Vegetation_Class)
pairwise_results <- list()

for (i in 1:(length(groups) - 1)) {
  for (j in (i + 1):length(groups)) {
    
    subset_idx <- nonvasc_env_balanced$Vegetation_Class %in% c(groups[i], groups[j])
    env_subset <- nonvasc_env_balanced[subset_idx, , drop = FALSE]
    comp_subset <- nonvasc_composition[subset_idx, ]
    
    perm_subset <- how(nperm = perm_design_nonvasc$nperm) 
    setBlocks(perm_subset) <- perm_design_nonvasc$blocks[subset_idx]  
    
    pairwise_results[[paste(groups[i], "vs", groups[j])]] <- adonis2(
      vegdist(comp_subset, method = "bray") ~ Vegetation_Class + Plot, 
      data = env_subset, 
      method = "bray", 
      permutations = perm_subset,  
      by = "terms")}}

pairwise_results
veg_nonvasc_paired <- extract_results(pairwise_results)

#viereck 3
dist_matrix <- vegdist(nonvasc_composition, method = "bray")
groups <- unique(nonvasc_env_balanced$Viereck.3)
pairwise_results <- list()

for (i in 1:(length(groups) - 1)) {
  for (j in (i + 1):length(groups)) {
    
    subset_idx <- nonvasc_env_balanced$Viereck.3 %in% c(groups[i], groups[j])
    env_subset <- nonvasc_env_balanced[subset_idx, , drop = FALSE]
    comp_subset <- nonvasc_composition[subset_idx, ]
    
    perm_subset <- how(nperm = perm_design_nonvasc$nperm) 
    setBlocks(perm_subset) <- perm_design_nonvasc$blocks[subset_idx]  
    
    pairwise_results[[paste(groups[i], "vs", groups[j])]] <- adonis2(
      vegdist(comp_subset, method = "bray") ~ Viereck.3 + Plot, 
      data = env_subset, 
      method = "bray", 
      permutations = perm_subset,  
      by = "terms")}}

pairwise_results
viereck3_nonvasc_paired <- extract_results(pairwise_results)

#viereck 2
dist_matrix <- vegdist(nonvasc_composition, method = "bray")
groups <- unique(nonvasc_env_balanced$Viereck.2)
pairwise_results <- list()

for (i in 1:(length(groups) - 1)) {
  for (j in (i + 1):length(groups)) {
    
    subset_idx <- nonvasc_env_balanced$Viereck.2 %in% c(groups[i], groups[j])
    env_subset <- nonvasc_env_balanced[subset_idx, , drop = FALSE]
    comp_subset <- nonvasc_composition[subset_idx, ]
    
    perm_subset <- how(nperm = perm_design_nonvasc$nperm) 
    setBlocks(perm_subset) <- perm_design_nonvasc$blocks[subset_idx]  
    
    pairwise_results[[paste(groups[i], "vs", groups[j])]] <- adonis2(
      vegdist(comp_subset, method = "bray") ~ Viereck.2 + Plot, 
      data = env_subset, 
      method = "bray", 
      permutations = perm_subset,  
      by = "terms")}}

pairwise_results
viereck2_nonvasc_paired <- extract_results(pairwise_results)


#bind all results together 
paired_list <- list(viereck3_nonvasc_paired, viereck2_nonvasc_paired, veg_nonvasc_paired)
nonvasc_combined_results <- bind_rows(paired_list)
write_xlsx(nonvasc_combined_results, "C:/Users/kmpace/Desktop/NONVASC_PairedTest_Results.xlsx")







```