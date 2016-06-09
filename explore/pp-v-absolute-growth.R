
totals <- mtagdp_totals %>%
    mutate(TA = gsub(" District", "", TA),
           TA = gsub(" City", "", TA)) %>%
    group_by(TA) %>%
    summarise(RealGrowthPP = CAGR(sum(GDP_real_perCapita[Year == endYear]) / 
                                sum(GDP_real_perCapita[Year == startYear]), 
                            period = endYear - startYear) / 100,
              RealGrowth = CAGR(sum(GDP_real[Year == endYear]) / 
                                    sum(GDP_real[Year == startYear]), 
                                period = endYear - startYear) / 100,
              GDP = sum(GDP_real[Year == endYear])) 


CairoPDF("figures/pp-scatter.pdf", 9, 8)
print(ggplot(totals, (aes(x = RealGrowthPP, y = RealGrowth))) +
    geom_point() +
    geom_text_repel(aes(label = TA), colour = "steelblue", family = TheFont) +
    scale_x_continuous("Real GDP growth per person", label = percent) +
    scale_y_continuous("Real GDP growth", label = percent) +
    coord_equal())
dev.off()