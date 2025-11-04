

relViz_modf <- function(data,
                        ageCount,
                        each_val,
                        xlabel,
                        color_choice,
                        se          = FALSE,
                        fill_choice = NULL,
                        facet_var   = NULL,
                        title       = NULL) {

      data %>%
            ggplot(aes(x     = rep(x = 1:8, each = each_val),
                       y     = Ratio)) +
            geom_errorbar(aes(ymin = LCL, ymax = UCL),
                          position = position_dodge(width = 0.3),
                          color = "#8B8378",
                          width = 0.3, alpha = 0.7) +
            geom_line(position = position_dodge(width = 0.3),
                      color = "#8B8378") +
            geom_smooth(color = noquote({{fill_choice}}),
                        fill = noquote({{fill_choice}}),
                        method = "lm",
                        se = TRUE) +

            # geom_smooth(aes(fill = {{fill_choice}}), method = "lm", se = se) +
            # geom_line(aes(y = fitted), color = noquote({{color_choice}})) +
            scale_x_continuous(breaks = seq(1, ageCount), labels = xlabel) +

            # scale_x_continuous(breaks = seq(1, 8), labels = ageGroupsVec[-1]) +
            scale_color_manual(values = c("#8B8378", "#002453", "#0471A6")) +
            scale_fill_manual(values = c("#8B8378", "#002453", "#0471A6")) +
            # facet_grid(. ~ plot) +
            geom_hline(yintercept = 1, linetype = 2, color = "red", alpha = 0.4) +

            labs(x     = "Age group (years)",
                 y     = "Black-to-all races\nRelative annaul incidence (IRR)") +
            goldenScatterCAtheme +
            theme(legend.position = "top",
                  legend.box      = "vertical",
                  legend.title    = element_blank(),
                  plot.title      = element_text(hjust = 0.5),
                  axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1),
                  strip.text.y    = element_blank(),
                  axis.text.y     = element_text(color = "black", size = 10,
                                                 margin = margin(t = 0, r = -4, b = 0, l = 0)))
}
