try(detach("package:tidyr", unload = TRUE))
try(detach("package:dplyr", unload = TRUE))
library(MASS)
library(knitr)
library(ggplot2)
library(scales)
library(ggrepel)
library(Cairo)
library(extrafont)
library(mgcv)
library(ggmap)
library(ggthemes)
library(directlabels)
library(tidyr)
library(dplyr)
library(gridExtra)
library(stargazer)
library(xtable)
library(rms)
library(cluster)
library(RColorBrewer)
library(stringr)
library(ggseas)
library(riverplot)
library(sqldf)
library(devtools)
library(igraph)
install_github("nz-mbie/mbie-r-package-public/pkg")
install_github("nz-mbie/mbiemaps-public/pkg")
library(mbie)
library(mbiemaps)
library(rmarkdown)

TheFont <- "Times New Roman"
theme_set(theme_minimal(base_family = TheFont))
update_geom_defaults("text",   list(family = TheFont))

# load in the data (only once per session)
if(!exists("TAGDP_public")){
    TAGDP_public <- read.csv("http://www.mbie.govt.nz/info-services/sectors-industries/regions-cities/research/modelled-territorial-authority-gross-domestic-product/resolveuid/98debb063cb14ecbab0f018b7681e7f4",
                   stringsAsFactors = FALSE)
    mtagdp_totals <- read.csv("http://www.mbie.govt.nz/info-services/sectors-industries/regions-cities/research/modelled-territorial-authority-gross-domestic-product/resolveuid/b811fea8196c4d269c1d5ca973933c7c",
                              stringsAsFactors = FALSE)
    data(ta_simpl_gg) # from mbiemaps; TA boundaries
}




source("explore/explore1.R")
source("explore/dot_ribbon.R")
source("explore/modelling-ag.R")
source("explore/pp-v-absolute-growth.R")
source("explore/sankey.R")
source("explore/commuting.R")

knit2pdf("ellis-mtagdp.Rnw")
render("ellisp-mtagdp-presentation.Rmd")
