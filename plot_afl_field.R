##################################################################
# plot_afl_field.R
# R Project: PlotAFLField
#
# Usage:
#   source("https://raw.githubusercontent.com/CooperDenny/PlotAFLField/main/plot_afl_field.R")
#   afl_field(venue_length = 160, venue_width = 140, venue_arclength = 50)
#   afl_half_field(venue_length = 160, venue_width = 140, venue_arclength = 50)
#   afl_half_field() + afl_angle_contours(venue_length = 160, venue_width = 140)
#   afl_zone_map(data, venue_length = 160, venue_width = 140, venue_arclength = 50)
#   afl_heatmap(data, venue_length = 160, venue_width = 140, venue_arclength = 50)
#
# Coordinates are metres with the origin at the centre bounce and x running goal
# to goal. Data should be oriented so the team of interest attacks towards +x
# (the right-hand goal). Every plotting function returns a ggplot object.
##################################################################

#Load necessary libraries
library(ggplot2)    #Package for creating graphics


##################################################################
# Dimensions (AFL Laws of the Game)
##################################################################

afl_goal_half_width  <- 3.2    #Goal posts are 6.4m apart
afl_behind_offset    <- 9.6    #Behind posts stand 6.4m outside each goal post
afl_goal_square_len  <- 9      #Goal square is 9m deep (and 6.4m wide)
afl_centre_square    <- 25     #Half of the 50m centre square
afl_line_width       <- 1.2    #Width of the field markings
afl_grass_colour     <- "#65AF56"


##################################################################
# Geometry helpers
##################################################################

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

#Stop early with a clear message if a ground dimension is not a single positive number
afl_check_dims <- function(venue_length, venue_width, venue_arclength = 1) {
  ok <- function(v) is.numeric(v) && length(v) == 1 && !is.na(v) && v > 0
  if (!ok(venue_length) || !ok(venue_width) || !ok(venue_arclength)) {
    stop("venue_length, venue_width and venue_arclength must each be a single positive number (metres).",
         call. = FALSE)
  }
  invisible(TRUE)
}

#Every marking on the ground, as data. Straight white lines and black posts are one
#row each; curved lines are paths identified by `id`.
afl_marking_data <- function(venue_length, venue_width, venue_arclength) {
  A  <- venue_length / 2
  B  <- venue_width / 2
  g  <- afl_goal_half_width
  p  <- afl_behind_offset
  gs <- afl_goal_square_len
  cs <- afl_centre_square
  angle <- afl_arc_angle(venue_length, venue_width, venue_arclength)

  #Goal posts (12m out from the goal line) and behind posts (6m), both ends.
  #They start 1m inside the ground so they meet the goal square lines.
  posts <- data.frame(
    x    = rep(c(-A + 1, A - 1), each = 4),
    xend = c(-A - 6, -A - 12, -A - 12, -A - 6, A + 6, A + 12, A + 12, A + 6),
    y    = rep(c(-p, -g, g, p), times = 2)
  )
  posts$yend <- posts$y

  #Straight white lines: centre square, goal squares and the line through the centre circles
  lines <- data.frame(
    line = c(rep("centre square", 4), rep("left goal square", 3), rep("right goal square", 3),
             "centre line"),
    x    = c(-cs, -cs, -cs,  cs,   -A, -A, -A + gs,    A,  A,  A - gs,   0),
    xend = c( cs,  cs, -cs,  cs,   -A + gs, -A + gs, -A + gs,   A - gs, A - gs, A - gs,   0),
    y    = c(-cs,  cs, -cs, -cs,   -g,  g, -g,   -g,  g, -g,   -5),
    yend = c(-cs,  cs,  cs,  cs,   -g,  g,  g,   -g,  g,  g,    5)
  )

  #Curved white lines: centre circles (3m and 10m diameter) and the two 50m arcs
  paths <- rbind(
    cbind(id = "small centre circle", ellipse_path(1.5, 1.5)),
    cbind(id = "large centre circle", ellipse_path(5, 5)),
    cbind(id = "left 50m arc",  arc_path(-A, venue_arclength, angle, pi - angle)),
    cbind(id = "right 50m arc", arc_path( A, venue_arclength, -angle, -(pi - angle)))
  )

  list(boundary = ellipse_path(A, B), posts = posts, lines = lines, paths = paths)
}

#The grass, extending 5m beyond the boundary line
afl_surface <- function(venue_length, venue_width) {
  geom_polygon(data = ellipse_path(venue_length / 2 + 5, venue_width / 2 + 5), aes(x, y),
               fill = afl_grass_colour, inherit.aes = FALSE)
}

#Layers that draw every marking once: boundary, posts, then the white lines. Add this
#after your own fill layers (zones, heatmaps) to put the markings back on top.
afl_markings <- function(venue_length = 160, venue_width = 140, venue_arclength = 50,
                         colour = "white", linewidth = afl_line_width) {
  afl_check_dims(venue_length, venue_width, venue_arclength)
  m <- afl_marking_data(venue_length, venue_width, venue_arclength)
  list(
    geom_path(data = m$boundary, aes(x, y), linewidth = linewidth, colour = colour,
              inherit.aes = FALSE),
    geom_segment(data = m$posts, aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = linewidth, colour = "black", inherit.aes = FALSE),
    geom_segment(data = m$lines, aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = linewidth, colour = colour, inherit.aes = FALSE),
    geom_path(data = m$paths, aes(x, y, group = id), linewidth = linewidth, colour = colour,
              inherit.aes = FALSE)
  )
}


##################################################################
# Zones
##################################################################

afl_third_levels <- c("Defensive 50", "Midfield", "Forward 50")
afl_lane_levels  <- c("Left", "Corridor", "Right")

#Classify points into thirds (inside either 50m arc, or the midfield) and lanes
#(corridor, or the boundary lane on either side). Left and right are as seen by
#the team attacking towards +x, so "Left" is y > corridor_width / 2.
afl_zone_of <- function(x, y, venue_length = 160, venue_width = 140, venue_arclength = 50,
                        corridor_width = 50) {
  afl_check_dims(venue_length, venue_width, venue_arclength)
  A <- venue_length / 2
  r <- venue_arclength
  h <- corridor_width / 2

  third <- ifelse((x + A)^2 + y^2 <= r^2, "Defensive 50",
           ifelse((x - A)^2 + y^2 <= r^2, "Forward 50", "Midfield"))
  lane  <- ifelse(y > h, "Left", ifelse(y < -h, "Right", "Corridor"))

  third <- factor(third, levels = afl_third_levels)
  lane  <- factor(lane,  levels = afl_lane_levels)
  data.frame(third = third, lane = lane,
             zone = factor(paste(third, lane, sep = " - "),
                           levels = paste(rep(afl_third_levels, each = 3), afl_lane_levels, sep = " - ")))
}

#Nine zone polygons (three thirds x three lanes) clipped exactly to the boundary.
#Each zone is built from horizontal slices: for every y, the zone covers an interval
#of x bounded by the boundary ellipse and the 50m arcs.
afl_zones <- function(venue_length = 160, venue_width = 140, venue_arclength = 50,
                      corridor_width = 50, n = 200) {
  afl_check_dims(venue_length, venue_width, venue_arclength)
  A <- venue_length / 2
  B <- venue_width / 2
  r <- venue_arclength
  h <- corridor_width / 2
  if (!is.numeric(corridor_width) || corridor_width <= 0 || h >= B) {
    stop("corridor_width must be positive and narrower than the ground.", call. = FALSE)
  }

  #Height at which the arcs meet the boundary, so the polygons keep that corner sharp
  y_corner <- r * cos(afl_arc_angle(venue_length, venue_width, venue_arclength))

  clamp <- function(v, lo, hi) pmin(pmax(v, lo), hi)
  lanes <- list(Left = c(h, B), Corridor = c(-h, h), Right = c(-B, -h))

  out <- list()
  for (lane in names(lanes)) {
    lo_y <- lanes[[lane]][1]
    hi_y <- lanes[[lane]][2]
    corners <- c(-y_corner, y_corner)
    ys <- sort(unique(c(seq(lo_y, hi_y, length.out = n), corners[corners > lo_y & corners < hi_y])))
    e_left  <- -A * sqrt(pmax(0, 1 - ys^2 / B^2))
    e_right <- -e_left
    reach   <- sqrt(pmax(0, r^2 - ys^2))
    arc_l   <- ifelse(abs(ys) <= r, -A + reach, -Inf)   #Right edge of the defensive 50
    arc_r   <- ifelse(abs(ys) <= r,  A - reach,  Inf)   #Left edge of the forward 50

    d_right <- clamp(arc_l, e_left, e_right)
    f_left  <- clamp(arc_r, e_left, e_right)
    edges <- list(
      "Defensive 50" = list(lo = e_left,  hi = d_right),
      "Midfield"     = list(lo = d_right, hi = pmax(d_right, f_left)),
      "Forward 50"   = list(lo = f_left,  hi = e_right)
    )
    for (third in names(edges)) {
      lo <- edges[[third]]$lo
      hi <- edges[[third]]$hi
      if (all(hi - lo < 1e-9)) next
      out[[length(out) + 1]] <- data.frame(
        third = third, lane = lane,
        x = c(hi, rev(lo)), y = c(ys, rev(ys))
      )
    }
  }
  zones <- do.call(rbind, out)
  zones$third <- factor(zones$third, levels = afl_third_levels)
  zones$lane  <- factor(zones$lane,  levels = afl_lane_levels)
  zones$zone  <- factor(paste(zones$third, zones$lane, sep = " - "),
                        levels = paste(rep(afl_third_levels, each = 3), afl_lane_levels, sep = " - "))
  zones
}

#Area-weighted centre of a polygon (shoelace formula), for placing zone labels
polygon_centroid <- function(x, y) {
  x2 <- c(x[-1], x[1])
  y2 <- c(y[-1], y[1])
  cross <- x * y2 - x2 * y
  a <- sum(cross) / 2
  if (abs(a) < 1e-9) return(c(x = mean(x), y = mean(y)))
  c(x = sum((x + x2) * cross) / (6 * a), y = sum((y + y2) * cross) / (6 * a))
}


##################################################################
# Page layout
##################################################################

#How a ground sits on the page. Horizontal: x (goal to goal) runs across the page.
#Vertical: a quarter turn so x runs up the page with the goal at `end` at the top.
#For the +x goal that is an anticlockwise turn (+y ends up on the left); for the -x
#goal a clockwise one. Either way it is a rotation, never a mirror image, so left and
#right stay as the attacking team sees them.
#- xlim, ylim: the area to show, in metres
#- crop: TRUE shows exactly xlim/ylim; FALSE adds ggplot's usual margin
afl_view <- function(vertical, xlim, ylim, crop = FALSE, end = "right") {
  stopifnot(is.logical(vertical), length(vertical) == 1, !is.na(vertical))
  if (!vertical) {
    return(if (crop) coord_fixed(ratio = 1, xlim = xlim, ylim = ylim, expand = FALSE)
           else coord_fixed(ratio = 1))
  }
  #Coordinate limits are given in data units and pass through the scale's transform,
  #so on a reversed scale they must be given high-to-low: low-to-high limits would
  #undo the reversal and mirror the ground. coord_flip() has no fixed ratio, so
  #theme(aspect.ratio) keeps metres square; fixed limits keep it exact whatever
  #layers are added.
  aspect <- theme(aspect.ratio = diff(range(xlim)) / diff(range(ylim)))
  if (end == "right") {
    list(scale_y_reverse(), coord_flip(xlim = xlim, ylim = rev(ylim), expand = !crop), aspect)
  } else {
    list(scale_x_reverse(), coord_flip(xlim = rev(xlim), ylim = ylim, expand = !crop), aspect)
  }
}

#The whole ground: the posts reach 12m past each goal line and the grass 5m past the boundary
afl_full_view <- function(venue_length, venue_width, vertical) {
  afl_view(vertical, xlim = c(-venue_length / 2 - 12, venue_length / 2 + 12),
           ylim = c(-venue_width / 2 - 5, venue_width / 2 + 5))
}


##################################################################
# Plotting functions
##################################################################

#Define the function to plot the AFL field
afl_field <- function(venue_length = 160, venue_width = 140, venue_arclength = 50,
                      vertical = FALSE) {

  #This function, afl_field, generates a graphical representation of an Australian Football League (AFL) field.
  #The field is an oval shape with specific markings such as the goal posts, centre square, goal squares, centre circles, and 50m arcs.

  #The function takes three parameters:
  #- venue_length: The length of the field (default is 160 metres).
  #- venue_width: The width of the field (default is 140 metres).
  #- venue_arclength: The radius of the 50m arcs from the goal (default is 50 metres).
  #- vertical: TRUE turns the ground to run up the page, with the +x goal at the top.

  #It returns a ggplot object, so further layers can be added with `+`. Your data keeps
  #the same x and y either way.

  afl_check_dims(venue_length, venue_width, venue_arclength)

  #One row holding the ground dimensions (kept so code that relied on the plot's
  #base data still finds x = length and y = width)
  field <- data.frame(x = venue_length, y = venue_width)

  oval <- ggplot(field) +
    afl_surface(venue_length, venue_width) +
    afl_markings(venue_length, venue_width, venue_arclength) +

    #Remove background and set aspect ratio
    theme_void() +
    afl_full_view(venue_length, venue_width, vertical)

  #Return the plot object
  return(oval)
}


#Plot one end of the ground, for shot maps and inside 50 entries
afl_half_field <- function(venue_length = 160, venue_width = 140, venue_arclength = 50,
                           from = 0, end = c("right", "left"), vertical = FALSE) {

  #- from: Distance from the centre (metres) at which the view starts. 0 shows the
  #        whole half; venue_length/2 - venue_arclength - 3 frames the 50m arc.
  #- end:  "right" shows the goal at +x (the default attacking direction), "left" the goal at -x.
  #- vertical: TRUE turns the view so the goal is at the top. The plot becomes wide
  #        and short, which suits wide panels. Data still uses the same x and y; the
  #        view is rotated, not mirrored.

  afl_check_dims(venue_length, venue_width, venue_arclength)
  end <- match.arg(end)
  A <- venue_length / 2
  B <- venue_width / 2
  if (!is.numeric(from) || length(from) != 1 || from < 0 || from >= A) {
    stop("from must be between 0 and venue_length / 2.", call. = FALSE)
  }
  xlim <- if (end == "right") c(from, A + 13) else c(-A - 13, -from)
  ylim <- c(-B - 5, B + 5)

  field <- data.frame(x = venue_length, y = venue_width)
  ggplot(field) +
    afl_surface(venue_length, venue_width) +
    afl_markings(venue_length, venue_width, venue_arclength) +
    theme_void() +
    afl_view(vertical, xlim, ylim, crop = TRUE, end = end)
}


#Lines of equal scoring angle: every point on one line sees the 6.4m goal mouth at the
#same angle. By the inscribed angle theorem each line is an arc of a circle through
#both goal posts, with radius 3.2 / sin(angle) and centre 3.2 / tan(angle) in front
#of the goal line. Add to afl_field() or afl_half_field().
afl_angle_contours <- function(venue_length = 160, venue_width = 140,
                               angles = c(10, 15, 20, 30), end = c("right", "left", "both"),
                               labels = TRUE, colour = "white", linewidth = 0.4,
                               linetype = "dashed", label_size = 2.6) {
  afl_check_dims(venue_length, venue_width)
  end <- match.arg(end)
  if (!is.numeric(angles) || any(angles <= 0 | angles >= 180)) {
    stop("angles must be in degrees, between 0 and 180.", call. = FALSE)
  }
  A <- venue_length / 2
  B <- venue_width / 2
  g <- afl_goal_half_width

  arcs <- do.call(rbind, lapply(angles, function(a) {
    th <- a * pi / 180
    R  <- g / sin(th)
    cx <- A - g / tan(th)
    t  <- seq(0, 2 * pi, length.out = 1441)
    x  <- cx + R * cos(t)
    y  <- R * sin(t)
    #Keep the part of the circle on the ground and in front of the goal line
    x[x > A | x^2 / A^2 + y^2 / B^2 > 1] <- NA
    data.frame(angle = a, x = x, y = y)
  }))
  #Label each line where it crosses the centre line of the ground: 3.2 * cot(angle / 2) out
  labs <- data.frame(angle = angles, x = A - g / tan(angles * pi / 360), y = 0)
  labs <- labs[labs$x > -A, ]

  if (end == "left") {
    arcs$x <- -arcs$x
    labs$x <- -labs$x
  } else if (end == "both") {
    arcs <- rbind(arcs, transform(arcs, x = -x, angle = -angle))
    labs <- rbind(labs, transform(labs, x = -x))
  }

  layers <- list(
    geom_path(data = arcs, aes(x, y, group = angle), colour = colour, linewidth = linewidth,
              linetype = linetype, na.rm = TRUE, inherit.aes = FALSE)
  )
  if (labels) {
    layers <- c(layers, geom_label(data = labs, aes(x, y, label = paste0(abs(angle), "°")),
                                   size = label_size, colour = "grey15", fill = colour,
                                   label.size = 0, label.padding = unit(0.12, "lines"),
                                   inherit.aes = FALSE))
  }
  layers
}


#Shade the nine zones by how many events fall in each, or by the average of `value`
#(e.g. a logical "effective disposal" column gives an efficiency rate per zone)
afl_zone_map <- function(data, venue_length = 160, venue_width = 140, venue_arclength = 50,
                         x = "x", y = "y", value = NULL, corridor_width = 50,
                         colour = "#B2182B", labels = TRUE, min_n = 1, legend_title = NULL,
                         vertical = FALSE) {

  afl_check_dims(venue_length, venue_width, venue_arclength)
  if (!all(c(x, y, value) %in% names(data))) {
    stop("data must contain the columns ", paste(c(x, y, value), collapse = ", "), ".", call. = FALSE)
  }
  data <- data[!is.na(data[[x]]) & !is.na(data[[y]]), , drop = FALSE]
  if (nrow(data) == 0) stop("data has no rows with coordinates.", call. = FALSE)

  z <- afl_zone_of(data[[x]], data[[y]], venue_length, venue_width, venue_arclength, corridor_width)
  n <- table(z$zone)

  if (is.null(value)) {
    stat   <- as.numeric(n) / sum(n)
    lab    <- sprintf("%d\n%.0f%%", as.integer(n), 100 * stat)
    title  <- "Share of events"
    fmt    <- function(b) paste0(round(100 * b), "%")
  } else {
    v      <- as.numeric(data[[value]])
    stat   <- as.numeric(tapply(v, z$zone, mean, na.rm = TRUE))
    rate   <- all(v %in% c(0, 1, NA))
    lab    <- if (rate) sprintf("%.0f%%\nn = %d", 100 * stat, as.integer(n))
              else sprintf("%.1f\nn = %d", stat, as.integer(n))
    title  <- value
    fmt    <- if (rate) function(b) paste0(round(100 * b), "%") else waiver()
  }
  if (!is.null(legend_title)) title <- legend_title
  stats <- data.frame(zone = names(n), n = as.integer(n), stat = stat, label = lab)
  #Too few events to trust a share or rate: leave the zone unshaded and show only its count
  few <- stats$n < min_n
  stats$stat[few]  <- NA
  stats$label[few] <- sprintf("n = %d", stats$n[few])

  zones <- afl_zones(venue_length, venue_width, venue_arclength, corridor_width)
  zones$stat <- stats$stat[match(zones$zone, stats$zone)]

  centres <- do.call(rbind, lapply(split(zones, zones$zone, drop = TRUE), function(d) {
    cbind(zone = as.character(d$zone[1]), as.data.frame(t(polygon_centroid(d$x, d$y))))
  }))
  centres <- merge(centres, stats, by = "zone")
  centres <- centres[centres$n > 0, ]
  #The midfield corridor's centre is the centre bounce: move its label off the circles
  #(below them, or to their right on a vertical ground)
  mid <- centres$zone == "Midfield - Corridor"
  centres$y[mid] <- centres$y[mid] - min(12, corridor_width / 4)

  field <- data.frame(x = venue_length, y = venue_width)
  p <- ggplot(field) +
    afl_surface(venue_length, venue_width) +
    geom_polygon(data = zones, aes(x, y, group = zone, fill = stat), colour = "white",
                 linewidth = 0.3, linetype = "dotted", alpha = 0.9, inherit.aes = FALSE) +
    scale_fill_gradient(low = "#FFF1E6", high = colour, na.value = NA, name = title,
                        labels = fmt, limits = if (is.null(value) || !rate) NULL else c(0, 1)) +
    afl_markings(venue_length, venue_width, venue_arclength) +
    theme_void() +
    afl_full_view(venue_length, venue_width, vertical)

  if (labels && nrow(centres) > 0) {
    p <- p + geom_label(data = centres, aes(x, y, label = label), size = 3, lineheight = 0.9,
                        colour = "grey10", fill = "white", alpha = 0.85, label.size = 0,
                        inherit.aes = FALSE)
  }
  p
}


#Smoothed density of event locations, clipped to the boundary. A Gaussian kernel with
#standard deviation `bandwidth` metres is placed on every event; the fill shows where
#events concentrate relative to the busiest spot on the ground. The fill is opaque and
#starts at the grass colour, because semi-transparent warm colours over green turn brown.
afl_heatmap <- function(data, venue_length = 160, venue_width = 140, venue_arclength = 50,
                        x = "x", y = "y", bandwidth = 8, resolution = 0.5,
                        colours = c(afl_grass_colour, "#C8E06B", "#FFD23F", "#FD8D3C", "#BD0026"),
                        vertical = FALSE) {

  afl_check_dims(venue_length, venue_width, venue_arclength)
  if (!all(c(x, y) %in% names(data))) {
    stop("data must contain the columns ", x, " and ", y, ".", call. = FALSE)
  }
  px <- data[[x]]
  py <- data[[y]]
  keep <- !is.na(px) & !is.na(py)
  px <- px[keep]
  py <- py[keep]
  if (length(px) == 0) stop("data has no rows with coordinates.", call. = FALSE)

  A <- venue_length / 2
  B <- venue_width / 2
  gx <- seq(-A, A, by = resolution)
  gy <- seq(-B, B, by = resolution)

  #Product Gaussian kernel: density[i, j] = sum_k phi(gx_i - x_k) * phi(gy_j - y_k)
  kx <- exp(-outer(gx, px, "-")^2 / (2 * bandwidth^2))
  ky <- exp(-outer(gy, py, "-")^2 / (2 * bandwidth^2))
  dens <- kx %*% t(ky)

  grid <- expand.grid(x = gx, y = gy)
  grid$density <- as.vector(dens) / max(dens)
  grid <- grid[grid$x^2 / A^2 + grid$y^2 / B^2 <= 1, ]

  field <- data.frame(x = venue_length, y = venue_width)
  ggplot(field) +
    afl_surface(venue_length, venue_width) +
    geom_raster(data = grid, aes(x, y, fill = density), inherit.aes = FALSE) +
    scale_fill_gradientn(colours = colours, limits = c(0, 1), breaks = c(0, 1),
                         labels = c("Low", "High"), name = "Density") +
    afl_markings(venue_length, venue_width, venue_arclength) +
    theme_void() +
    afl_full_view(venue_length, venue_width, vertical)
}
