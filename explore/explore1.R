

#===============cluster analysis=============
x <- TAGDP_public %>%
    filter(Year == 2013) %>%
    group_by(NGDP_industry, TA) %>%
    summarise(GDP = sum(GDP)) %>%
    spread(NGDP_industry, GDP)

TAs <- as.character(x$TA)
TAs <- gsub(" District", "", TAs)
TAs <- gsub(" City", "", TAs)

x <- x[ , -1]
dimnames(x)[[1]] <- TAs

x[is.na(x)] <- 0
x <- t(scale(t(x)))

CairoPDF("figures/ta-associations.pdf", 8, 8)
par(family = "Calibri")
plot(diana(x), which = 2, main = "", xlab = "", cex = 0.9)
dev.off()

CairoPDF("figures/industry-associations.pdf", 8, 11)
par(family = "Calibri")
plot(diana(t(x)), which.plots = 2, main = "", xlab = "", cex = 0.9)
dev.off()


#===================maps=======================

#-------------construction------------
const <- TAGDP_public %>%
    filter(RGDP_industry == "Construction") %>%
    group_by(TA) %>%
    summarise(construction_growth = sum(GDP_real[Year == 2013]) / sum(GDP_real[Year == 2003]),
              cagr = CAGR(construction_growth, 10) / 100) %>%
    select(-construction_growth) %>%
    rename(FULLNAME = TA)

tmp <- ta_simpl_gg %>%
    left_join(const) %>%
    arrange(order)

CairoPDF("figures/construction-map.pdf", 8, 8)
ggplot(tmp, aes(x = long, y = lat, group = group, fill = cagr)) +
    geom_polygon() +
    coord_map() +
    mbie::theme_nothing(base_family = "Calibri") +
    scale_fill_gradientn("Growth per year",
                         colours = brewer.pal(8, "RdYlBu"), limits = c(-.1, .1),
                         label = percent) +
    theme(legend.position = c(0.2, 0.7))
dev.off()


#-------------gdp per capita since 2008------------
load("data/mtagdp_totals.rda")
gpp <- mtagdp_totals %>%
    group_by(TA) %>%
    summarise(growth = sum(GDP_real[Year == 2015]) / sum(GDP_real[Year == 2010]),
              cagr = CAGR(growth, 5) / 100) %>%
    select(-growth) %>%
    rename(FULLNAME = TA)

tmp <- ta_simpl_gg %>%
    left_join(gpp) %>%
    arrange(order)

CairoPDF("figures/gdp-pp-map.pdf", 8, 8)
ggplot(tmp, aes(x = long, y = lat, group = group, fill = cagr)) +
    geom_polygon() +
    coord_map() +
    mbie::theme_nothing(base_family = "Calibri") +
    scale_fill_gradientn("Growth per year",
                         colours = brewer.pal(8, "RdYlBu"), limits = c(-.08, .08),
                         label = percent) +
    theme(legend.position = c(0.2, 0.7))
dev.off()


#=============Opotiki===============


opotiki <- TAGDP_public %>%
    filter(TA == "Opotiki District") %>%
    group_by(NGDP_industry, Year) %>%
    summarise(GDP_real = sum(GDP_real)) %>%
    ungroup()

big_industries <- filter(opotiki, Year == max(Year)) %>%
    filter(GDP_real > 10) %>%
    select(NGDP_industry)

big_industries <- big_industries %>%
    mutate(year_label = sample(2002:2013, nrow(big_industries), replace = TRUE))

labels <- opotiki %>%
    left_join(big_industries) %>%
    filter(Year == year_label) 


print(opotiki %>%
    filter(NGDP_industry %in% unique(big_industries$NGDP_industry)) %>%
    ggplot(aes(x = Year, y = GDP_real, colour = NGDP_industry)) +
    geom_line() +
    geom_text_repel(data = labels, 
              aes(label = str_wrap(NGDP_industry, 27)), fontface = "bold") +
    theme(legend.position = "none") +
    labs(y = "Real GDP ($m)"))



CairoPDF("figures/opotiki.pdf", 11, 11)
TAGDP_public %>%
    filter(TA == "Opotiki District") %>%
    group_by(RGDP_industry) %>%
    summarise(CAGR8 = CAGR(sum(GDP_real[Year == 2013]) / sum(GDP_real[Year == 2005]), 8) / 100,
              GDP_real2013 = sum(GDP_real[Year == 2013])) %>%
    ggplot(aes(x = GDP_real2013, y = CAGR8, label = str_wrap(RGDP_industry, 28))) +
    geom_point() +
    geom_text_repel(colour = "steelblue", family = "Calibri") +
    scale_x_log10("Real GDP in 2013 ($m, logarithmic scale)", breaks = c(1, 3.3, 10, 33)) +
    scale_y_continuous("Eight year average growth rate", label = percent)
dev.off()
