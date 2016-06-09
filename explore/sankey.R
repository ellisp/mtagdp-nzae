## example sankey chart
library(riverplot)
library(sqldf)
Pal1 <- brewer.pal(9, "Pastel1")
Pal2 <- brewer.pal(8, "Pastel2")

CairoPDF("figures/sankey_taranakiConstruction.pdf",11, 9)
par(family = "Calibri")

the_region <- "Taranaki"
the_ind <- "Construction"

# Get the data we need for just this particular subset of industry and region
sql <- paste0('select * from TAGDP_public where 
              RGDP_industry = "', the_ind, '" and
              Region = "', the_region, '"')

tagdp <- sqldf(sql)

tmp <- tagdp %>%
    filter(Year == max(Year)) %>%
    group_by(Region, RGDP_industry, NGDP_industry, TA) %>%
    summarise(GDP = sum(GDP)) %>%
    ungroup()

# First big node - the particular Region - Industry combination
AllTot <- paste(unique(tmp$RGDP_industry), sum(tmp$GDP) %>% FormatDollars("m")) 

# Second node - break down to TA level
TATots <- tmp %>%
    group_by(TA) %>%
    summarise(GDP = sum(GDP)) %>%
    mutate(TA_Lab = paste(TA,  GDP %>% FormatDollars("m")))

# Third node - break down to NGDP industry x TA
TAIndTots <- tmp %>%
    select(-Region, -RGDP_industry) %>%
    mutate(col = Pal1[as.numeric(as.factor(NGDP_industry))]) %>%
    mutate(TAIndLab = paste(NGDP_industry, GDP %>% FormatDollars("m", 1))) %>%
    mutate(N1 = TA %>% factor() %>% rename.levels(orig = TATots$TA, new = TATots$TA_Lab) %>% as.character()) %>%
    rename(N2 = TAIndLab,
           Value = GDP) %>%
    filter(Value > 0) %>%
    select(N1, N2, Value, col) %>%
    arrange(N1, col)

# wrapping parameter
w <- 25

# set the height of the second layer in the chart
lo <- nrow(TATots)
if(lo ==1){
    y2 <- 1.5
} else {
    y2 <- seq(from = 0.5, to = 2.5, length.out = lo)  
}

if(nrow(TAIndTots) > nrow(TATots)){
    nodes <- data.frame(ID = c(AllTot, TATots$TA_Lab, TAIndTots$N2) %>% wrap(w), 
                        x = c(1, rep(2, nrow(TATots)), rep(3, nrow(TAIndTots))), stringsAsFactors = FALSE,
                        col = c(Pal2[1],
                                Pal2[2:(nrow(TATots) + 1)],
                                TAIndTots$col),
                        y= c(1.5, y2, (1:nrow(TAIndTots)) / nrow(TAIndTots) * 3))
    
    edges <- TATots %>%
        select(TA_Lab, GDP) %>%
        mutate(N1 = AllTot) %>%
        rename(N2 = TA_Lab, Value = GDP) %>%
        select(N1, N2, Value) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        rbind(TAIndTots[ , c("N1", "N2", "Value")]) %>%
        mutate(N1 = N1 %>% wrap(w),
               N2 = N2 %>% wrap(w))
    
} else {
    
    # This segment draws the plot whenwe don't want the industries ie there's 
    # only one NGDP industry per RGDP industry.  So we don't use TAIndTots
    nodes <- data.frame(ID = c(AllTot, TATots$TA_Lab) %>% wrap(w), 
                        x = c(1, rep(2, nrow(TATots))), stringsAsFactors = FALSE,
                        col = c(Pal2[1],
                                Pal2[2:(nrow(TATots) + 1)]),
                        y= c(1.5, y2))
    
    edges <- TATots %>%
        select(TA_Lab, GDP) %>%
        mutate(N1 = AllTot) %>%
        rename(N2 = TA_Lab, Value = GDP) %>%
        select(N1, N2, Value) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        mutate(N1 = N1 %>% wrap(w),
               N2 = N2 %>% wrap(w))
    
}

r <- makeRiver( nodes, edges)

st <- default.style()
st$srt <- "0"

if (nrow(TATots) > 1 | nrow(TAIndTots) != nrow(TATots)){
    # we don't want a plot at all if there's only one node going to one node - looks silly.
    
    plot(r, default_style = st, plot_area = 0.9)
    grid.text(paste(the_region, the_ind), 0.5, 0.95,
              gp = gpar(fontfamily = "Calibri", fontface = "bold", fontsize = 17))
}



dev.off()
