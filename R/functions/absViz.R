absViz <- function(data,
                   ageCount,
                   each_val,
                   xlabel,
                   cmpr_var,
                   facet_var,
                   metric,
                   LCL,
                   UCL,
                   title = NULL,
                   colVal1 = "#112345",
                   colVal2 = "#39B6B9"){
      
      data %>% ggplot(aes(x = rep(x = 1:ageCount, each = each_val), y = {{metric}}, color = {{cmpr_var}})) +
            geom_errorbar(aes(ymin = {{LCL}}, ymax = {{UCL}}), width = 0.3, alpha = 0.7,
                          position = position_dodge(width = 0.3)) +

            # geom_point(position = position_dodge(width = 0.3)) +
            geom_line(position = position_dodge(width = 0.3)) +
            scale_color_manual(values = c(colVal1, colVal2)) +
            labs(x     = "Age group (years)", 
                 y     = NULL,
                 title = title) +
            facet_wrap(vars({{facet_var}})) +
            scale_x_continuous(breaks = seq(1, ageCount), labels = xlabel) +
            goldenScatterCAtheme +
            theme(legend.position = "bottom",
                  legend.title    = element_blank(),
                  plot.title      = element_text(hjust = 0.5),
                  axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1),
                  strip.text.y    = element_text(size  = 10),
                  axis.text.y     = element_text(color = "black", size = 10,
                                                 margin = margin(t = 0, r = -4, b = 0, l = 0)))
}