##
##    Name:       organise_geographies.R
##
##    Objective:  For some of the graphical output, it is desireable to place the TAs or RCs
##                in some North to South (or reverse) order.  This script creates some functions
##                to do that, as well as shorten the names for plots.
##
##   Approach:    Functons are designed to take a general column of a data set (here, defined
##                as 'class_name') for a given regional classification.  The default for organise_tas
##                is the column "TA" in the MTAGDP data object, but it could be something else (e.g.
##                "Territorial_Authority").  The similar principle applies to the organise_rcs.
##
##                Internal checks are made to ensure the levels match in the class_name column.
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2015-05-29
##

##
##  1. Territorial Authorities
##  
   organise_tas <- function(x, class_name = "TA") {
  	  
  	     x <- data.frame(x)
	              
	              # create a short TA name for graphical output
                    x[ , class_name] <- gsub(" District", "", x[ , class_name])
                    x[ , class_name] <- gsub(" City",     "", x[ , class_name])               

	   # set levels
	     y <- factor(x[ , class_name], 
                        levels = c("Far North", 
                                   "Whangarei",
                                   "Kaipara",
                                   "Auckland",
                                   "Thames-Coromandel",
                                   "Hauraki",
                                   "Waikato",
                                   "Matamata-Piako",
                                   "Hamilton",
                                   "Waipa",
                                   "Otorohanga",
                                   "South Waikato",
                                   "Waitomo",
                                   "Taupo",
                                   "Western Bay of Plenty",
                                   "Tauranga",
                                   "Rotorua",
                                   "Whakatane",
                                   "Kawerau",
                                   "Opotiki",
                                   "Gisborne",
                                   "Wairoa",
                                   "Hastings",
                                   "Napier",
                                   "Central Hawke's Bay",
                                   "New Plymouth",
                                   "Stratford",
                                   "South Taranaki",
                                   "Ruapehu",
                                   "Wanganui",
                                   "Rangitikei",
                                   "Manawatu",
                                   "Palmerston North",
                                   "Tararua",
                                   "Horowhenua",
                                   "Kapiti Coast",
                                   "Porirua",
                                   "Upper Hutt",
                                   "Lower Hutt",
                                   "Wellington",
                                   "Masterton",
                                   "Carterton",
                                   "South Wairarapa",
                                   "Tasman",
                                   "Nelson",
                                   "Marlborough",
                                   "Kaikoura",
                                   "Buller",
                                   "Grey",
                                   "Westland",
                                   "Hurunui",
                                   "Waimakariri",
                                   "Christchurch",
                                   "Selwyn",
                                   "Ashburton",
                                   "Timaru",
                                   "Mackenzie",
                                   "Waimate",
                                   "Waitaki",
                                   "Central Otago",
                                   "Queenstown-Lakes",
                                   "Dunedin",
                                   "Clutha",
                                   "Southland",
                                   "Gore",
                                   "Invercargill"))	

                  if(sum(is.na(y)) > 0){
                     stop("Some TAs did not match in the sorting process or are NA")
                  }                  
                  
                  return(y)

          }

##
##  2. Regional Councils
##

   organise_rcs <- function(x, class_name = "Region") {
  	  
  	     x <- data.frame(x)
	              
	              # create a short RC name for graphical output
                    x[ , class_name] <- gsub(" Region", "", x[ , class_name])            

	   # set levels
	     y <- factor(x[ , class_name], 
                        levels = c("Northland",
                                   "Auckland",
                                   "Waikato",
                                   "Bay of Plenty",
                                   "Gisborne",
                                   "Hawke's Bay",
                                   "Taranaki",
                                   "Manawatu-Wanganui",
                                   "Wellington",
                                   "Marlborough",
                                   "Nelson",
                                   "Tasman",
                                   "West Coast",
                                   "Canterbury",
                                   "Otago",
                                   "Southland"))
                                   
                  if(sum(is.na(y)) > 0){
                     stop("Some RCs did not match in the sorting process or are NA")
                  }                  
                  
                  return(y)
                                   
             }                             
