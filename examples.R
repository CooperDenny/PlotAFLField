##################################################################
# examples.R
# Regenerates every image in images/. The event data is simulated (set.seed),
# so the plots show what each function draws, not real match statistics.
#   Rscript examples.R
##################################################################

source("plot_afl_field.R")

L <- 160; W <- 140; R <- 50   #Ground used for the examples
A <- L / 2; B <- W / 2

save <- function(plot, file, width, height) {
  ggsave(file.path("images", file), plot, width = width, height = height, dpi = 150, bg = "white")
}

#Keep only simulated points that land on the ground
on_ground <- function(d) d[d$x^2 / A^2 + d$y^2 / B^2 <= 1, ]

set.seed(2026)

##################################################################
# Fields
##################################################################

save(afl_field(160, 140, 50), "afl_field_1.png", 7, 6)
save(afl_field(170, 116, 50), "afl_field_2.png", 7, 5.2)
save(afl_half_field(160, 140, 50), "afl_half_field.png", 4.6, 6.6)

##################################################################
# Scoring-angle contours and a shot map
##################################################################

save(afl_field(L, W, R) + afl_angle_contours(L, W, angles = c(10, 20, 30), end = "both"),
     "afl_angle_contours.png", 7, 6)

#Simulated shots inside the forward 50. The chance of a goal rises with the angle
#the shooter has at the goal mouth.
shots <- data.frame(x = runif(400, A - R, A - 1), y = runif(400, -R, R))
shots <- on_ground(shots[(shots$x - A)^2 + shots$y^2 <= R^2, ])[1:70, ]
angle <- (atan2(3.2 - shots$y, A - shots$x) - atan2(-3.2 - shots$y, A - shots$x)) * 180 / pi
goal  <- runif(nrow(shots)) < plogis(-1.3 + 0.085 * angle)
shots$result <- factor(ifelse(goal, "Goal", ifelse(runif(nrow(shots)) < 0.7, "Behind", "No score")),
                       levels = c("Goal", "Behind", "No score"))

shot_map <- afl_half_field(L, W, R, from = A - R - 3) +
  afl_angle_contours(L, W) +
  geom_point(data = shots, aes(x, y, fill = result), shape = 21, size = 3.2, stroke = 0.6,
             colour = "black") +
  scale_fill_manual(values = c(Goal = "#FFD23F", Behind = "#3A86FF", `No score` = "grey80"),
                    name = NULL) +
  theme(legend.position = "bottom")
save(shot_map, "afl_shot_map.png", 5.2, 6.4)

##################################################################
# Zones
##################################################################

zones <- afl_zones(L, W, R)
zone_names <- do.call(rbind, lapply(split(zones, zones$zone), function(d) {
  data.frame(label = paste(d$third[1], d$lane[1], sep = "\n"), t(polygon_centroid(d$x, d$y)))
}))
zone_names$y[zone_names$label == "Midfield\nCorridor"] <- -12
zone_outlines <- afl_field(L, W, R) +
  geom_polygon(data = zones, aes(x, y, group = zone), fill = NA, colour = "white",
               linetype = "dotted", inherit.aes = FALSE) +
  geom_label(data = zone_names, aes(x, y, label = label), size = 2.8, lineheight = 0.9,
             label.size = 0, fill = "white", alpha = 0.85, inherit.aes = FALSE)
save(zone_outlines, "afl_zones.png", 7, 6)

#Simulated possessions: busiest through the midfield corridor and forward 50
possessions <- on_ground(rbind(
  data.frame(x = rnorm(220, 10, 30), y = rnorm(220, 0, 22)),
  data.frame(x = rnorm(120, 55, 12), y = rnorm(120, 0, 18)),
  data.frame(x = rnorm(70, -10, 25), y = rnorm(70, 48, 10)),
  data.frame(x = rnorm(60, -50, 15), y = rnorm(60, -15, 20))
))
save(afl_zone_map(possessions, L, W, R), "afl_zone_map.png", 7.6, 6)

#Simulated kicks with an "effective" flag: more reliable in the corridor
kicks <- possessions
kicks$effective <- runif(nrow(kicks)) <
  ifelse(abs(kicks$y) < 25, 0.78, 0.6) - ifelse(kicks$x > A - R, 0.12, 0)
save(afl_zone_map(kicks, L, W, R, value = "effective", colour = "#1B7837", min_n = 8,
                  legend_title = "Kick efficiency"),
     "afl_zone_map_efficiency.png", 7.6, 6)

##################################################################
# Heatmap
##################################################################

save(afl_heatmap(possessions, L, W, R), "afl_heatmap.png", 7.6, 6)
