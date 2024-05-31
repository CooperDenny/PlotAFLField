##################################################################
# plot_afl_field.R
# R Project: PlotAFLField
##################################################################

#Load necessary libraries
library(tidyverse)  #Collection of R packages for data manipulation and visualization
library(ggplot2)    #Package for creating graphics
library(ggforce)    #Package for additional ggplot2 functionalities

#Define the function to plot the AFL field
afl_field <- function(venue_length = 160, venue_width = 140, venue_arclength = 50) {
  
  #This function, afl_field, generates a graphical representation of an Australian Football League (AFL) field.
  #The field is an oval shape with specific markings such as the goal posts, centre square, goal squares, centre circles, and 50m arcs.
  
  #The function takes three parameters:
  #- venue_length: The length of the field (default is 160 meters).
  #- venue_width: The width of the field (default is 140 meters).
  #- venue_arclength: The radius of the 50m arcs from the goal (default is 50 meters).
  
  #Section required to calculate the intersection of the 50m arc with the boundary of the oval
  size <- tibble(x = seq(venue_length, venue_length, length.out = 100), y = seq(venue_width, venue_width, length.out = 100))
  z <- (((size$x + size$y) / 4.01)^2 - 2500 + (size$x/2)^2) / (2 * (size$x/2)) * (1 - (50 - venue_arclength) / 500)
  h <- ((size$x + size$y) / 4)
  a <- sqrt(h^2 - z^2)
  angle <- acos(a / 50)
  
  oval <- ggplot(size) +
    
    #Plot the shape of the oval
    geom_ellipse(aes(x0 = 0, y0 = 0, a = (x + 10)/2, b = (y + 10)/2, angle = 0), fill = "#65AF56") +
    
    #Plot the boundary line (white line)
    geom_ellipse(aes(x0 = 0, y0 = 0, a = x/2, b = y/2, angle = 0), size = 1.2, color = "white") +
    
    #Add goal posts on the left side (black lines)
    geom_segment(aes(x = -(venue_length/2) + 1, y = -9.6, xend = -(venue_length/2) - 6, yend = -9.6), size = 1.2, color = "black") +
    geom_segment(aes(x = -(venue_length/2) + 1, y = -3.2, xend = -(venue_length/2) - 12, yend = -3.2), size = 1.2, color = "black") +
    geom_segment(aes(x = -(venue_length/2) + 1, y = 3.2, xend = -(venue_length/2) - 12, yend = 3.2), size = 1.2, color = "black") +
    geom_segment(aes(x = -(venue_length/2) + 1, y = 9.6, xend = -(venue_length/2) - 6, yend = 9.6), size = 1.2, color = "black") +
    
    #Add goal posts on the right side (black lines)
    geom_segment(aes(x = (venue_length/2) - 1, y = -9.6, xend = (venue_length/2) + 6, yend = -9.6), size = 1.2, color = "black") +
    geom_segment(aes(x = (venue_length/2) - 1, y = -3.2, xend = (venue_length/2) + 12, yend = -3.2), size = 1.2, color = "black") +
    geom_segment(aes(x = (venue_length/2) - 1, y = 3.2, xend = (venue_length/2) + 12, yend = 3.2), size = 1.2, color = "black") +
    geom_segment(aes(x = (venue_length/2) - 1, y = 9.6, xend = (venue_length/2) + 6, yend = 9.6), size = 1.2, color = "black") +
    
    #Add centre square (white lines)
    geom_segment(aes(x = -20, y = -20, xend = 20, yend = -20), size = 1.2, color = "white") +
    geom_segment(aes(x = -20, y = 20, xend = 20, yend = 20), size = 1.2, color = "white") +
    geom_segment(aes(x = -20, y = -20, xend = -20, yend = 20), size = 1.2, color = "white") +
    geom_segment(aes(x = 20, y = -20, xend = 20, yend = 20), size = 1.2, color = "white") +
    
    #Add goal square on the left side (white lines)
    geom_segment(aes(x = -(venue_length/2), y = -3.2, xend = -(venue_length/2) + 9, yend = -3.2), size = 1.2, color = "white") +
    geom_segment(aes(x = -(venue_length/2), y = 3.2, xend = -(venue_length/2) + 9, yend = 3.2), size = 1.2, color = "white") +
    geom_segment(aes(x = -(venue_length/2) + 9, y = -3.2, xend = -(venue_length/2) + 9, yend = 3.2), size = 1.2, color = "white") +
    
    #Add goal square on the right side (white lines)
    geom_segment(aes(x = (venue_length/2), y = -3.2, xend = (venue_length/2) - 9, yend = -3.2), size = 1.2, color = "white") +
    geom_segment(aes(x = (venue_length/2), y = 3.2, xend = (venue_length/2) - 9, yend = 3.2), size = 1.2, color = "white") +
    geom_segment(aes(x = (venue_length/2) - 9, y = -3.2, xend = (venue_length/2) - 9, yend = 3.2), size = 1.2, color = "white") +
    
    #Add centre circle (white lines)
    geom_circle(aes(x0 = 0, y0 = 0, r = 1.5), size = 1.2, color = "white") +  #Small center circle
    geom_circle(aes(x0 = 0, y0 = 0, r = 5), size = 1.2, color = "white") +   #Large center circle
    geom_segment(aes(x = 0, y = -5, xend = 0, yend = 5), size = 1.2, color = "white") +  #Line through center circles
    
    #Add 50m arc on left side (white line)
    geom_arc(aes(x0 = -x/2, y0 = 0, r = venue_arclength, start = angle, end = pi - angle), size = 1.2, color = "white") +
    
    #Add 50m arc on right side (white line)
    geom_arc(aes(x0 = x/2, y0 = 0, r = venue_arclength, start = -angle, end = -(pi - angle)), size = 1.2, color = "white") + 
      
    #Remove background and set aspect ratio
    theme_void() +
    coord_fixed(ratio = 1)
  
  #Return the plot object
  return(oval)
}

#Call the function to plot the AFL field
afl_field() 
