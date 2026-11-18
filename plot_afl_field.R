##################################################################
# plot_afl_field.R
# R Project: PlotAFLField
#
# Usage:
#   source("https://raw.githubusercontent.com/CooperDenny/PlotAFLField/main/plot_afl_field.R")
#   afl_field(venue_length = 160, venue_width = 140, venue_arclength = 50)
##################################################################

#Load necessary libraries
library(ggplot2)    #Package for creating graphics

#Calculate where a 50m arc meets the boundary line
afl_arc_angle <- function(venue_length, venue_width, venue_arclength) {

  #The 50m arc is a circle of radius venue_arclength centred on the middle of the
  #goal line, which sits on the boundary at (-venue_length/2, 0). The boundary is
  #the ellipse x^2/A^2 + y^2/B^2 = 1. Substituting y^2 from the ellipse into the
  #circle (x + A)^2 + y^2 = r^2 gives a quadratic in x: qa*x^2 + qb*x + qc = 0.
  A <- venue_length / 2
  B <- venue_width / 2
  r <- venue_arclength

  qa <- 1 - B^2 / A^2
  qb <- 2 * A
  qc <- A^2 + B^2 - r^2

  #A perfectly round ground makes the quadratic linear
  roots <- if (abs(qa) < 1e-12) {
    -qc / qb
  } else {
    disc <- qb^2 - 4 * qa * qc
    if (disc < 0) disc <- NA
    (-qb + c(-1, 1) * sqrt(disc)) / (2 * qa)
  }

  #Keep the crossing that lies on the ground, nearest the goal
  roots <- roots[!is.na(roots) & roots >= -A & roots <= A]
  if (length(roots) == 0) {
    stop("venue_arclength (", venue_arclength, ") does not meet the boundary of a ",
         venue_length, " x ", venue_width, " ground.", call. = FALSE)
  }
  x <- min(roots)
  y <- B * sqrt(max(0, 1 - x^2 / A^2))

  #Angle measured clockwise from 12 o'clock, as arc_path() expects
  atan2(x + A, y)
}

#Points around an ellipse centred on the origin (a circle when a == b)
ellipse_path <- function(a, b, n = 361) {
  t <- seq(0, 2 * pi, length.out = n)
  data.frame(x = a * cos(t), y = b * sin(t))
}

#Points along a circular arc centred on (x0, 0). Angles are clockwise from 12 o'clock,
#the same convention afl_arc_angle() returns.
arc_path <- function(x0, r, start, end, n = 181) {
  t <- seq(start, end, length.out = n)
  data.frame(x = x0 + r * sin(t), y = r * cos(t))
}

#Define the function to plot the AFL field
afl_field <- function(venue_length = 160, venue_width = 140, venue_arclength = 50) {

  #This function, afl_field, generates a graphical representation of an Australian Football League (AFL) field.
  #The field is an oval shape with specific markings such as the goal posts, centre square, goal squares, centre circles, and 50m arcs.

  #The function takes three parameters:
  #- venue_length: The length of the field (default is 160 metres).
  #- venue_width: The width of the field (default is 140 metres).
  #- venue_arclength: The radius of the 50m arcs from the goal (default is 50 metres).

  #It returns a ggplot object, so further layers can be added with `+`.

  stopifnot(
    is.numeric(venue_length), length(venue_length) == 1, venue_length > 0,
    is.numeric(venue_width), length(venue_width) == 1, venue_width > 0,
    is.numeric(venue_arclength), length(venue_arclength) == 1, venue_arclength > 0
  )

  #Angle at which each 50m arc meets the boundary line
  angle <- afl_arc_angle(venue_length, venue_width, venue_arclength)

  #One row holding the ground dimensions (kept so code that relied on the plot's
  #base data still finds x = length and y = width)
  field <- data.frame(x = venue_length, y = venue_width)

  line <- 1.2   #Width of the field markings

  oval <- ggplot(field) +

    #Plot the shape of the oval (grass extends 5m beyond the boundary line)
    geom_polygon(data = ellipse_path(venue_length/2 + 5, venue_width/2 + 5), aes(x, y), fill = "#65AF56") +

    #Plot the boundary line (white line)
    geom_path(data = ellipse_path(venue_length/2, venue_width/2), aes(x, y), linewidth = line, color = "white") +

    #Add goal posts on the left side (black lines)
    geom_segment(aes(x = -(venue_length/2) + 1, y = -9.6, xend = -(venue_length/2) - 6, yend = -9.6), linewidth = line, color = "black") +
    geom_segment(aes(x = -(venue_length/2) + 1, y = -3.2, xend = -(venue_length/2) - 12, yend = -3.2), linewidth = line, color = "black") +
    geom_segment(aes(x = -(venue_length/2) + 1, y = 3.2, xend = -(venue_length/2) - 12, yend = 3.2), linewidth = line, color = "black") +
    geom_segment(aes(x = -(venue_length/2) + 1, y = 9.6, xend = -(venue_length/2) - 6, yend = 9.6), linewidth = line, color = "black") +

    #Add goal posts on the right side (black lines)
    geom_segment(aes(x = (venue_length/2) - 1, y = -9.6, xend = (venue_length/2) + 6, yend = -9.6), linewidth = line, color = "black") +
    geom_segment(aes(x = (venue_length/2) - 1, y = -3.2, xend = (venue_length/2) + 12, yend = -3.2), linewidth = line, color = "black") +
    geom_segment(aes(x = (venue_length/2) - 1, y = 3.2, xend = (venue_length/2) + 12, yend = 3.2), linewidth = line, color = "black") +
    geom_segment(aes(x = (venue_length/2) - 1, y = 9.6, xend = (venue_length/2) + 6, yend = 9.6), linewidth = line, color = "black") +

    #Add centre square, 50m x 50m (white lines)
    geom_segment(aes(x = -25, y = -25, xend = 25, yend = -25), linewidth = line, color = "white") +
    geom_segment(aes(x = -25, y = 25, xend = 25, yend = 25), linewidth = line, color = "white") +
    geom_segment(aes(x = -25, y = -25, xend = -25, yend = 25), linewidth = line, color = "white") +
    geom_segment(aes(x = 25, y = -25, xend = 25, yend = 25), linewidth = line, color = "white") +

    #Add goal square on the left side (white lines)
    geom_segment(aes(x = -(venue_length/2), y = -3.2, xend = -(venue_length/2) + 9, yend = -3.2), linewidth = line, color = "white") +
    geom_segment(aes(x = -(venue_length/2), y = 3.2, xend = -(venue_length/2) + 9, yend = 3.2), linewidth = line, color = "white") +
    geom_segment(aes(x = -(venue_length/2) + 9, y = -3.2, xend = -(venue_length/2) + 9, yend = 3.2), linewidth = line, color = "white") +

    #Add goal square on the right side (white lines)
    geom_segment(aes(x = (venue_length/2), y = -3.2, xend = (venue_length/2) - 9, yend = -3.2), linewidth = line, color = "white") +
    geom_segment(aes(x = (venue_length/2), y = 3.2, xend = (venue_length/2) - 9, yend = 3.2), linewidth = line, color = "white") +
    geom_segment(aes(x = (venue_length/2) - 9, y = -3.2, xend = (venue_length/2) - 9, yend = 3.2), linewidth = line, color = "white") +

    #Add centre circle (white lines)
    geom_path(data = ellipse_path(1.5, 1.5), aes(x, y), linewidth = line, color = "white") +  #Small centre circle (3m diameter)
    geom_path(data = ellipse_path(5, 5), aes(x, y), linewidth = line, color = "white") +      #Large centre circle (10m diameter)
    geom_segment(aes(x = 0, y = -5, xend = 0, yend = 5), linewidth = line, color = "white") +  #Line through centre circles

    #Add 50m arc on left side (white line)
    geom_path(data = arc_path(-venue_length/2, venue_arclength, angle, pi - angle), aes(x, y), linewidth = line, color = "white") +

    #Add 50m arc on right side (white line)
    geom_path(data = arc_path(venue_length/2, venue_arclength, -angle, -(pi - angle)), aes(x, y), linewidth = line, color = "white") +

    #Remove background and set aspect ratio
    theme_void() +
    coord_fixed(ratio = 1)

  #Return the plot object
  return(oval)
}
