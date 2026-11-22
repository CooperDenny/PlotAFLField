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
save(afl_field(160, 140, 50, vertical = TRUE), "afl_field_vertical.png", 5.6, 7)
save(afl_half_field(160, 140, 50), "afl_half_field.png", 4.6, 6.6)

##################################################################
# Distance rings and a shot map
##################################################################

save(afl_field(L, W, R) + afl_distance_rings(L, W, end = "both"), "afl_distance_rings.png", 7, 6)

#Simulated shots inside the forward 50. The chance of a goal falls with distance from goal.
shots <- data.frame(x = runif(400, A - R, A - 1), y = runif(400, -R, R))
shots <- on_ground(shots[(shots$x - A)^2 + shots$y^2 <= R^2, ])[1:70, ]
dist  <- sqrt((A - shots$x)^2 + shots$y^2)
goal  <- runif(nrow(shots)) < plogis(1.6 - 0.06 * dist)
shots$result <- factor(ifelse(goal, "Goal", ifelse(runif(nrow(shots)) < 0.7, "Behind", "No score")),
                       levels = c("Goal", "Behind", "No score"))

shot_map <- afl_half_field(L, W, R, from = A - R - 3) +
  afl_distance_rings(L, W) +
  geom_point(data = shots, aes(x, y, fill = result), shape = 21, size = 3.2, stroke = 0.6,
             colour = "black") +
  scale_fill_manual(values = c(Goal = "#FFD23F", Behind = "#3A86FF", `No score` = "grey80"),
                    name = NULL) +
  theme(legend.position = "bottom")
save(shot_map, "afl_shot_map.png", 5.2, 6.4)

#The same shots with the ground vertical, goal at the top: a wide, short panel
shot_map_top <- afl_half_field(L, W, R, from = A - R - 3, vertical = TRUE) +
  afl_distance_rings(L, W) +
  geom_point(data = shots, aes(x, y, fill = result), shape = 21, size = 3, stroke = 0.6,
             colour = "black") +
  scale_fill_manual(values = c(Goal = "#FFD23F", Behind = "#3A86FF", `No score` = "grey80"),
                    name = NULL) +
  theme(legend.position = "bottom")
save(shot_map_top, "afl_shot_map_vertical.png", 7.4, 4.2)

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
save(afl_heatmap(possessions, L, W, R, vertical = TRUE), "afl_heatmap_vertical.png", 6.2, 7)

##################################################################
# Gallery: ground shapes and options
##################################################################

#The same functions on differently shaped grounds (dimensions in metres)
save(afl_field(185, 125, 50), "afl_shape_long.png", 7, 5.2)
save(afl_field(150, 120, 50), "afl_shape_compact.png", 7, 5.8)
save(afl_field(150, 150, 50), "afl_shape_round.png", 6.4, 6.4)
save(afl_field(185, 125, 50) + afl_distance_rings(185, 125, end = "both"), "afl_shape_long_rings.png", 7, 5.2)

#The left-hand end, and custom ring distances on a vertical half field
save(afl_half_field(L, W, R, end = "left"), "afl_half_field_left.png", 4.6, 6.6)
save(afl_half_field(L, W, R, from = A - R - 3, vertical = TRUE) +
       afl_distance_rings(L, W, distances = c(15, 25, 35, 45)), "afl_rings_custom.png", 7.4, 3.8)

#Zones with a narrower 30m corridor, and a vertical zone map
zones30 <- afl_zones(L, W, R, corridor_width = 30)
names30 <- do.call(rbind, lapply(split(zones30, zones30$zone), function(d) {
  data.frame(label = paste(d$third[1], d$lane[1], sep = "\n"), t(polygon_centroid(d$x, d$y)))
}))
names30$y[names30$label == "Midfield\nCorridor"] <- -9
save(afl_field(L, W, R) +
       geom_polygon(data = zones30, aes(x, y, group = zone), fill = NA, colour = "white",
                    linetype = "dotted", inherit.aes = FALSE) +
       geom_label(data = names30, aes(x, y, label = label), size = 2.6, lineheight = 0.9,
                  label.size = 0, fill = "white", alpha = 0.85, inherit.aes = FALSE),
     "afl_zones_corridor_30.png", 7, 6)
save(afl_zone_map(possessions, L, W, R, vertical = TRUE), "afl_zone_map_vertical.png", 6.6, 7)
save(afl_zone_map(kicks, L, W, R, value = "effective", colour = "#1B7837", min_n = 8,
                  legend_title = "Kick efficiency", corridor_width = 30),
     "afl_zone_map_efficiency_corridor_30.png", 7.6, 6)

#A tighter heatmap bandwidth shows more local detail
save(afl_heatmap(possessions, L, W, R, bandwidth = 4), "afl_heatmap_bandwidth_4.png", 7.6, 6)
