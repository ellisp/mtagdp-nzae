

totals_test <- mtagdp_totals %>%
    group_by(TA, Year) %>%
    summarise(GDP_real = sum(GDP_real),
              GDP = sum(GDP))

totals_test %>%
    gather(variable, value, -TA, -Year) %>%
    ggplot(aes(x = Year, y = value, colour = variable)) +
    geom_line() +
    facet_wrap(~TA, scales = "free_y")