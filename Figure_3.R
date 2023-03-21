## Data set example for mass, since rate of weight gain, and tarsus and wing growth are different.
##
# Ind | Day | Mass    |
# _____________________
#   1 |  0  |  1.63   |


## Data set example for tarsus and wing length growth.
##
# Days | Value | Trait   |
# ________________________
#   0  |  6.3  | Tarsus  |


#################################################
## Packages with we need for making the graphs ##
#################################################

  library(boot)
  library(ggplot2)
  library(plyr)
  library(grid)
  library(gridExtra)

## Adding our directory
setwd("C:/Users/jhanc/Box/Investigacion/Investigaciones/Side projects/Masius/Inves")

# Save the figure as a .png
png("nestlings_growth.png", width = 1370, height = 760, units = "px", type = "cairo")


data_day.vs.mass  <- read.csv(file = "Masius_mass_FINAL.csv", header = TRUE)
  attach(data_day.vs.mass) # We use this to attach the mass dataset to the search path

  head(data_day.vs.mass)

# First plot; Days vs Mass  
day_vs_mass <- ggplot(data = data_day.vs.mass, aes(x = Day, y = Mass)) +
    
    geom_point(shape = 1, fill = "black", color = "black", size = 8) +
    geom_smooth(method = "nls", se = T, fullrange = TRUE, color = "black") +
    annotate(family = "serif", geom = "text", x = 0.2, y = 8.5, label = "A", size = 15, hjust = 0, vjust = 0) +
    
    theme_classic()+
    theme(legend.key = element_rect(colour = "white"), 
          legend.background = element_rect(colour = "white"), 
          legend.text = element_text(family = "serif", size = 21),
          legend.justification = c("right", "top"),
          legend.title = element_blank()) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    
    theme(panel.grid.major = element_line(colour = NA),
          panel.grid.minor = element_line(colour = NA)) +
    
    theme(axis.title.y= element_text( color = "black")) +
    scale_y_continuous(breaks=seq(0, 10, 1)) +
    scale_x_continuous(breaks=seq(0, 16, 2)) +
    labs(x="Days after hatching", y = "Body mass (g)") +
    
    theme(axis.text.x = element_text(family = "serif", size = 35, lineheight = 0.9, vjust = 1)) +
    theme(axis.text.y = element_text(family = "serif",size = 35, lineheight = 0.9, vjust = 1)) +
    theme(axis.title.y = element_text(family = "serif",vjust = 1.2, size = 30)) +
    theme(axis.title.x = element_text(family = "serif",vjust = -0.5, size = 30))
  

detach(data_day.vs.mass) 
        # We used this function to remove the database from the search path


####################################################################################


data_day_vs_tarsus.wing.beak <- read.csv(file = "Masius_mass1.csv", header=TRUE, sep = ";", dec = ",")
                                          # I saved this dataset in an computer in Spanish, that why is separated by semicolons
                                          # and decimals are separated with commas
  attach(data_day_vs_tarsus.wing.beak)

  head(data_day_vs_tarsus.wing.beak)
  
  data_day_vs_tarsus.wing.beak$Carac <- factor(data_day_vs_tarsus.wing.beak$Carac, 
                                          levels=c("Wing", "Tarsus ", "Bill")) #Para arreglar el orden de los factores dentro del Plot
  
# Second plot; Days vs Tarsus/wing/beak length 
  day_vs_tarsus.wing.beak <- ggplot(data = data_day_vs_tarsus.wing.beak, aes(x = Days, y = Valor, color = Carac)) +
    
    geom_point(aes(shape = Carac, color = Carac, size = Carac), size = 8) +
    annotate(family = "serif", geom = "text", x = 0.2, y = 42.4, label = "B", size = 15, hjust = 0, vjust = 0)+
    
    scale_color_manual(values = c("black", "black", "black"), labels = c("Wing", "Tarsus ", "Bill")) +
    scale_shape_manual(values = c(0, 1, 2), labels = c("Wing", "Tarsus ", "Bill")) +
    scale_fill_manual(values = c("grey42", "grey42", "grey42"), labels = c("Wing", "Tarsus ", "Bill"))+
    scale_size_manual(values = c(5, 5, 5), labels = c("Wing", "Tarsus ", "Bill") )+
    
    theme_classic() +
    theme(legend.key = element_rect(colour = "white"), 
          legend.background = element_rect(colour = "white"), 
          legend.text=element_text(family="serif", size = 21),
          legend.position = c(0.15, .84),
          legend.title=element_blank())+
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
        
    theme(panel.grid.major = element_line(colour =NA),
          panel.grid.minor =element_line(colour = NA))+
    
    theme(axis.title.y = element_text( color = "black"))+
    scale_y_continuous(breaks = seq(0, 45, 5))+
    scale_x_continuous(breaks = seq(0, 16, 2))+
    labs(x = "Days after hatching", y = "Length (mm)")+
    
    theme(axis.text.x = element_text(family="serif", size = 35, lineheight = 0.9, vjust = 1))+
    theme(axis.text.y = element_text(family="serif",size = 35, lineheight = 0.9, vjust = 1))+
    theme(axis.title.y = element_text(family="serif",vjust = 1.2, size = 30))+
    theme(axis.title.x = element_text(family="serif",vjust = -0.5, size = 30))
  
detach(data_day_vs_tarsus.wing.beak)
  
## This use this to put the plots together in one page
  grid.arrange(day_vs_mass, day_vs_tarsus.wing.beak,  nrow = 1, ncol = 2)
  
  
dev.off() # shuts down the specified device, and save the image we created with the function, png(), at the beginning of the code 

