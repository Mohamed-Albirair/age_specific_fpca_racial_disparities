abs_to_rel <- function(data, metric, group_var, cmpr_var, variance, col_indx){
      data %>% arrange(.[[1]], .[[2]]) %>% 
            tidyr::pivot_wider(id_cols = c({{group_var}}), names_from = {{cmpr_var}},
                               values_from = c({{metric}}, {{variance}})) %>%
            # mutate(Ratio = str_glue(paste0({{metric}}, "_", numerator)) / str_glue(paste0({{metric}}, "_",denominator)))
            #        var   = (1 / str_glue(paste0({{metric}}, "_", denominator)) ^ 2) * str_glue(paste0({{variance}}, "_", numerator)) +
            #              (str_glue(paste0({{metric}}, "_", numerator)) ^ 2 / str_glue(paste0({{metric}}, "_", denominator)) ^ 4 *
            #                     str_glue(paste0({{variance}}, "_", denominator))),
            #        LCL   = Ratio - 1.96 * sqrt(Var),
            #        UCL   = Ratio + 1.96 * sqrt(Var))
      
      
            # group_by({{group_var}}) %>%
            mutate(Ratio = .[[col_indx]] / .[[col_indx + 1]],
                   Var = ((1 / .[[col_indx + 1]] ^ 2) * .[[col_indx + 2]]) +
                         (.[[col_indx]] ^ 2 / .[[col_indx + 1]] ^ 4 * .[[col_indx + 3]]),
                   LCL = Ratio - 1.96 * sqrt(Var),
                   UCL = Ratio + 1.96 * sqrt(Var)) %>% 
      
      # data %>% group_by(across({{group_var}})) %>% 
            # summarize(Ratio = .[[col_indx]] / .[[col_indx + 1]],
            #           Var = ((1 / .[[col_indx + 1]] ^ 2) * .[[col_indx + 2]]) +
            #                 (.[[col_indx]] ^ 2 / .[[col_indx + 1]] ^ 4 * .[[col_indx + 3]]),
            #           LCL = Ratio - 1.96 * sqrt(Var),
            #           UCL = Ratio + 1.96 * sqrt(Var)) %>% 
            
            mutate(Age = order(ageGroups_label)[match(AgeGroup, ageGroups_label)])
}