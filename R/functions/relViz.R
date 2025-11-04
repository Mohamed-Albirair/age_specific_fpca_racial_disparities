relViz <- function(data,
                   ageCount,
                   each_val,
                   xlabel,
                   color_choice,
                   se = FALSE,
                   fill_choice = NULL,
                   facet_var = NULL,
                   title = NULL){
      data %>% ggplot(aes(x = rep(x = 1:ageCount, each = each_val),
                          y = Ratio,
                          color = {{fill_choice}})) +
            geom_errorbar(aes(ymin = LCL, ymax = UCL),
                          position = position_dodge(width = 0.3),
                          color = "#8B8378",
                          width = 0.3, alpha = 0.7) +
            # geom_point(position = position_dodge(width = 0.3)) +
            geom_line(position = position_dodge(width = 0.3),
                      color = "#8B8378") +
            # geom_smooth(aes(fill = {{fill_choice}}), method = "lm", se = se) +
            geom_line(aes(y = fitted), color = noquote({{color_choice}})) +
            scale_x_continuous(breaks = seq(1, ageCount), labels = xlabel) +
            scale_color_manual(values = c({{color_choice}})) +
            scale_fill_manual(values = c({{color_choice}})) +
            facet_wrap(vars({{facet_var}})) +
            geom_hline(yintercept = 1, linetype = 2, color = "red", alpha = 0.4) +
            labs(x     = "Age group (years)",
                 y     = NULL,
                 title = title) +
            goldenScatterCAtheme +
            theme(legend.position = "bottom",
                  legend.title    = element_blank(),
                  plot.title      = element_text(hjust = 0.5),
                  axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1),
                  strip.text.y    = element_text(size  = 10),
                  axis.text.y     = element_text(color = "black", size = 10,
                                                 margin = margin(t = 0, r = -4, b = 0, l = 0)))
}