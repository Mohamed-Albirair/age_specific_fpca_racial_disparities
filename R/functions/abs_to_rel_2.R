abs_to_rel_2 <- function(dat, metric, group_var, cmpr_var, variance, numerator, denominator){
      dat %>% group_by(across({{group_var}})) %>% 
            summarize(Ratio = {{metric}}[{{cmpr_var}} == numerator] / {{metric}}[{{cmpr_var}} == denominator],
                      Var = ((1 / {{metric}}[{{cmpr_var}} == denominator] ^ 2) * 
                                   {{variance}}[{{cmpr_var}} == numerator]) +
                            ({{metric}}[{{cmpr_var}} == numerator] ^ 2 / {{metric}}[{{cmpr_var}} == denominator] ^ 4 *
                                   {{variance}}[{{cmpr_var}} == denominator]),
                      LCL = Ratio - 1.96 * sqrt(Var),
                      UCL = Ratio + 1.96 * sqrt(Var)) %>% 
            ungroup() %>% 
            
            mutate(Age = order(ageGroups_label)[match(AgeGroup, ageGroups_label)] - 1)
}