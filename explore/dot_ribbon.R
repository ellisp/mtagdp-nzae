

## this function is just a stub, need a more general function to deal with other geographical nuances
source("R/organise_geographies.R")

endYear <- max(mtagdp_totals$Year)
startYear <- endYear - 10

## calculate the totals & generate the plots
totals <- mtagdp_totals %>%
    mutate(TA = gsub(" District", "", TA),
           TA = gsub(" City", "", TA)) %>%
    group_by(TA) %>%
    summarise(Growth = CAGR(sum(GDP_real_perCapita[Year == endYear]) / 
                                sum(GDP_real_perCapita[Year == startYear]), 
                            period = endYear - startYear) / 100,
              GDP = sum(GDP_real[Year == endYear])) %>%
    arrange(Growth) %>%
    mutate(TA = factor(TA, levels = TA))

totals$TA <- organise_tas(totals)
totals$TA <- factor(totals$TA, levels = rev(levels(totals$TA)))  ## order needs to be reversed for this plot

p1 <- totals %>%
    ggplot(aes(x = TA, size = GDP, colour = Growth, y = Growth)) +
    coord_flip() +
    theme_minimal(10, base_family = "Calibri") +
    geom_point() +
    geom_point(colour = "black", shape = 1) +
    geom_smooth(aes(group=1), colour=alpha("grey", 0.05), show_guide=FALSE) +
    theme(legend.position = "top") +
    scale_size("Gross Domestic Product in 2015 ($m)", labels = comma, 
               range = c(2, 9), breaks = c(20000, 50000, 80000)) +
    scale_x_discrete("") +
    scale_y_continuous(paste0("Average annual real per person growth ",
                           startYear," - ", endYear), label = percent) +
    scale_colour_gradientn(colours = mbie.cols(c("Orange", "Blue")), guide = "none")

lines <- mtagdp_totals %>%
    mutate(TA = gsub(" District", "", TA),
           TA = gsub(" City", "", TA)) %>%
    group_by(TA, Year) %>%
    summarise(GDP = sum(GDP_real_perCapita)) %>%
    ungroup() %>%
    left_join(totals[, c("TA", "Growth")])%>%
    mutate(TA = factor(TA, levels = levels(totals$TA)[nrow(totals):1])) 


p2 <- lines %>%
    ggplot(aes(x = Year, y = GDP, colour = Growth)) +
    stat_index(index.ref = 1) +
    theme_nothing() +
    facet_wrap(~TA, ncol = 1) +
    scale_colour_gradientn(colours = mbie.cols(c("Orange", "Blue")), guide = "none") +
    theme(
        panel.margin = unit(-0.1, "lines"),
        strip.text = element_blank()
    ) 


CairoPDF("figures/dotplot_growth.pdf", 5.83333, 8.3333)
## setting the viewports
vp1 <- viewport(0.45, 0.5, width = 0.9, height = 1)
vp2 <- viewport(0.93, 0.4903, width = 0.1695, height = 0.8635)


print(p1, vp = vp1)
print(p2, vp = vp2)
dev.off()