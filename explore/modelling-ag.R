

snapshot_2005 <- TAGDP_public %>%
    group_by(TA) %>%
    summarise(Ag2005 = sum(GDP[Year == 2005 & RGDP_industry == "Agriculture"])/ 
                  sum(GDP[Year == 2005]),
              GDPpp2005 = sum(GDP_perCapita[Year == 2005]))

totals <- mtagdp_totals %>%
    group_by(TA, Year) %>%
    summarise(GDP_real = sum(GDP_real))


combined <- totals %>%
    left_join(snapshot_2005)

CairoPDF("figures/ag-growth-line.pdf", 8, 5)
print(ggplot(combined, aes(x = Year, y = GDP_real, colour = Ag2005, group = TA)) +
    stat_index(index.ref = 6, geom = "point") +
    stat_index(index.ref = 6, geom = "line") +
    scale_colour_gradientn("Proportion Agriculture in 2005",
                         colours = brewer.pal(8, "RdYlBu"),
                         label = percent) +
    labs(y = "Real GDP (index: 2005 = 100)") +
    theme(legend.position = "top"))
dev.off()


the_data <- combined %>%
    ungroup() %>%
    group_by(TA) %>%
    summarise(Ag2005 = unique(Ag2005),
              CAGR10 = CAGR(GDP_real[Year == 2015] / GDP_real[Year == 2005], 10) / 100,
              GDPpp2005 = unique(GDPpp2005)) %>%
    rename(FULLNAME = TA) %>%
    left_join(distinct(ta_simpl_gg[ , c("long.centre", "lat.centre", "FULLNAME")])) %>%
    mutate(TA = gsub(" District", "", FULLNAME),
           TA = gsub(" City", "", TA),
           outlier = ifelse(CAGR10 > 0.05 | CAGR10 < -0.02, TA, ""))

CairoPDF("figures/dotcagr10.pdf", 7, 9)
print(
the_data %>%
    arrange(CAGR10) %>%
    mutate(TA = factor(TA, levels = TA)) %>%
    ggplot(aes(x = CAGR10, colour = Ag2005, y = TA) ) +
    geom_vline(xintercept = c(-0.025, 0, 0.025, 0.05, 0.075), 
               colour = "grey90", size = 0.1) +
    geom_point() +
    geom_text(aes(label = TA, x = CAGR10 + 0.003), colour = "grey50", 
              hjust = 0, vjust = 0.5, size = 2, family = TheFont) +
    scale_colour_gradientn("Proportion\nAgriculture\nin 2005",
                       colours = brewer.pal(8, "RdYlBu"),
                       label = percent) +
    scale_x_continuous("", label = percent) +
    labs(y = "") +
    theme_tufte(base_family = TheFont) +
    theme(axis.text.y = element_blank(),
          axis.line.y = element_blank())
)
dev.off()

p1 <- ggplot(the_data, aes(x = Ag2005, y = CAGR10)) +
    geom_point(aes(colour = Ag2005))  +
    geom_point(shape = 1) +
    geom_text_repel(aes(label = outlier), colour = "steelblue", 
                    family = TheFont, segment.color = NA) +
    geom_smooth(method = "rlm") +
    scale_colour_gradientn("Proportion Agriculture in 2005",
                           colours = brewer.pal(8, "RdYlBu"),
                           label = percent) +scale_x_continuous("Proportion of economy that is agriculture in 2005", 
                   label = percent) +
    scale_y_continuous("Average annual real growth since 2005", label = percent) +
        theme(legend.position = "none")


p2 <- p1 + 
    aes(x = GDPpp2005) +
    scale_x_continuous("GDP per capita in 2005", label = dollar) +
    labs(y = " ")

CairoPDF("figures/scatter-2005-v-growth.pdf", 10, 5)
    grid.arrange(p1, p2, ncol = 2)
dev.off()


# how many degrees of freedom for each variable
sp2 <- spearman2(CAGR10 ~ Ag2005 + GDPpp2005 + long.centre + lat.centre, data = the_data)

CairoPDF("figures/spearman.pdf", 8, 3.5)
par(family = TheFont, bty = "l")
plot(sp2)
dev.off()
model <- gam(CAGR10 ~ Ag2005 + GDPpp2005 + s(long.centre, lat.centre, k = 5), data = the_data)

anova(model)
summary(model)

stargazer(model, out = "tables/gam.tex", style = "aer")


sink("tables/s.table.tex")
xtable(summary(model)$s.table)
sink()


CairoPDF("figures/gam.pdf", 8, 5)
par(bty = "l")
plot(model, pages = 1, residuals = TRUE, seWithMean = TRUE, 
     shade = TRUE, shade.col = "grey90", scale = 0,
     all.terms = TRUE)
dev.off()

model_spatial <- gam(CAGR10 ~ s(long.centre, lat.centre), data = the_data)
the_data$pred <- fitted(model_spatial)

longs <- seq(min(the_data$long.centre) - 3, max(the_data$long.centre) + 1, length = 100)
lats <- seq(min(the_data$lat.centre) - 1, max(the_data$lat.centre) + 1, length = 100)
the_grid <- expand.grid(long.centre = longs, lat.centre = lats)
the_grid$pred <- predict(model_spatial, newdata = the_grid)

p_map <- ggplot(the_grid, aes(x = long.centre, y = lat.centre, z = pred * 100)) +
    geom_raster(aes(fill = pred), interpolate = TRUE) +
    geom_contour(aes(colour = ..level.. )) +
    borders("nz", colour = "grey20") +
    mbie::theme_nothing(base_family = TheFont) +
    coord_equal() +
    scale_fill_gradientn("Average annual\ngrowth", colours = brewer.pal(8, "BuPu"), label = percent)

CairoPDF("figures/growth0513.pdf", 8, 6)
print(direct.label(p_map, method="bottom.pieces"))
dev.off()

# PNG version for tweeting
png("figures/growth0513.png", 800, 600, res = 100)
print(direct.label(p_map + ggtitle("Real GDP growth by District/City 2005 - 2015, smoothed"), method="bottom.pieces"))
dev.off()


p3 <- qqNormEnv(residuals(model)) + 
    coord_equal() +
    labs(x = "Expected value of scaled residuals\nunder Normality assumption",
         y = "Actual value of scaled residuals") +
    coord_flip()


the_data$res <- residuals(model)
the_data$fit <- fitted(model)

p4 <- ggplot(the_data, aes(x = fit, y = res)) + 
    geom_hline(yintercept = 0, colour = "steelblue") +
    geom_point() +
    scale_x_continuous("Predicted ten year average economic growth", label = percent) +
    scale_y_continuous("Residual", label = percent)

CairoPDF("figures/resid-diag.pdf", 7, 9)
grid.arrange(p3, p4, ncol = 1)
dev.off()

