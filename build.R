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
library(mbie)
library(mbiemaps)
library(RColorBrewer)
library(stringr)
library(ggseas)
library(riverplot)
library(sqldf)

theme_set(theme_minimal(base_family = "Calibri"))


load("data/TAGDP_public.rda")
load("data/mtagdp_totals.rda")



source("explore/explore1.R")

knit2pdf("ellis-mtagdp.Rnw", compiler = "XeLaTeX")

