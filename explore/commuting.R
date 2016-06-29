

# Re-used from original post at http://ellisp.github.io/blog/2015/12/26/commuting-network/
# download data from Statistics New Zealand

X <- read.csv("http://www.stats.govt.nz/~/media/Statistics/Census/2013%20Census/profile-and-summary-reports/commuter-view-interactive/2013-usual-residence-by-workplace-address-territorial-authority.csv",
              na.strings       = "..C",
              sep              = ",",
              stringsAsFactors = FALSE,
              check.names      = TRUE,
              header           = TRUE)

# Tidy into long form, remove clutter, make names consistent, 
# drop uninteresting data:
TravelAll <- X[1:67, ] %>% 
    rename(from = Usual.residence) %>%
    gather(to, value, -from) %>%
    mutate(from = gsub(" District", "", from),
           from = gsub(" City", "", from),
           from = gsub(" Territory", "", from),
           to = gsub(".", " ", to, fixed = TRUE),
           to = gsub(" Territory", "", to),
           to = gsub(" District", "", to),
           to = gsub(" City", "", to),
           to = gsub("Hawke s", "Hawke's", to, fixed = TRUE),
           to = gsub("Matamata Piako", "Matamata-Piako", to),
           to = gsub("Queenstown Lakes", "Queenstown-Lakes", to),
           to = gsub("Thames Coromandel", "Thames-Coromandel", to)) %>%
    filter(from != to &
               to != "Total workplace address" &
               to != "No Fixed Workplace Address" &
               to != "Area Outside Territorial Authority") %>%
    filter(!grepl("Not Further Defined", to)) %>%
    mutate(value = as.numeric(value),
           value = ifelse(is.na(value), 0, value)) 

# subset dropping the small values:
Travel <- TravelAll %>% 
    filter(value > 100)


draw_plot <- function(seed = 125){
    set.seed(seed) # there's some randomness in the graphing layout
    g <- Travel  %>%
        graph.data.frame(directed = TRUE)       
    par(family = TheFont, mai = c(.2, .2, .2, .2))  
    plot(g, edge.arrow.size = .4, layout = layout.davidson.harel,
         edge.width = sqrt(Travel$value) / 20,
         vertex.size = 0, edge.curved = TRUE,
         vertex.label.family = TheFont,
         vertex.label.cex = .8,
         main = ""
    )          
}

CairoPDF("Figures/commuting.pdf", 8.3, 8.3)
draw_plot()
dev.off()

svg("Figures/commuting.svg", 13, 7.3)
draw_plot()
dev.off()




