lm_reg <- function(data){
      # model <- lm(Ratio ~ rep(1:ageCount, each = each_val) + scnd_var, weights = Var ^ (-1), data = data)
      data <- data %>% mutate(Age = order(ageGroups_label)[match(AgeGroup, ageGroups_label)])
      # model <- lm(paste("Ratio ~ Age +", paste0(scnd_var)), weights = Var ^ (-1), data = data)
      
      model <- lm(Ratio ~ Age * Period, 
                  weights = Var ^ (-1),
                  data = data)
      return(model)
}