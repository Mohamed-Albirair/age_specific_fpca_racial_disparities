kableOutput <- function(output, caption = NULL){
      output %>% 
            # pander::pander(., caption = caption, style = "rmarkdown") %>% 
            # knitr::kable(caption = caption, "latex") %>% 
            # kableExtra::kable_styling(latex_options = c("HOLD_position"), fixed_thead = T, full_width = F) %>% 
            # kableExtra::kable_minimal() %>% 
            # kableExtra::kable_paper(bootstrap_options = "striped",
            #                         latex_options     = "HOLD_position",
            #                         position          = "center",
            #                         full_width        = F) %>% 
            kableExtra::kbl(., format = "latex", caption = caption, booktabs = T, linesep = "") %>% 
            kableExtra::kable_styling(full_width = F, latex_options = "HOLD_position")
            # kableExtra::column_spec(1, bold = T)
}