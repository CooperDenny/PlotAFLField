# PlotAFLField

R functions for plotting a customisable AFL field, and analysis views on top of it, using ggplot2.

## Functions (in `plot_afl_field.R`)

- `afl_field(venue_length, venue_width, venue_arclength)` — the whole ground. Parameters control the oval dimensions — different AFL venues have different field sizes.
- `afl_half_field(..., from, end)` — one end, for shot maps
- `afl_angle_contours(venue_length, venue_width, angles)` — layer of equal scoring-angle arcs (circles through both goal posts)
- `afl_zones()` / `afl_zone_of()` / `afl_zone_map()` — 3 thirds x 3 lanes; corridor width is a parameter
- `afl_heatmap()` — base-R Gaussian KDE clipped to the boundary, opaque ramp starting at the grass colour
- `afl_markings()` — layer of every marking, for redrawing lines over fills

`examples.R` regenerates every image in `images/` from simulated data (`Rscript examples.R`, under a UTF-8 locale so the degree sign renders).

## Usage

Source directly from GitHub (used by FootyStatsApp and other projects):

```r
source("https://raw.githubusercontent.com/CooperDenny/PlotAFLField/main/plot_afl_field.R")
```

Returns a ggplot2 object. Can be extended with additional ggplot2 layers.

Keep the signature and return type stable: FootyStatsApp sources this file from `main` at runtime and adds layers with their own `data =`. Sourcing must have no side effects (no top-level plot calls).

## Geometry

- Coordinates are metres with the origin at the centre bounce; x runs goal to goal.
- Markings follow the AFL Laws: 50 m centre square, 3 m / 10 m centre circles, 9 m x 6.4 m goal squares, posts 6.4 m apart.
- `afl_arc_angle()` solves the circle-ellipse intersection exactly, so the 50 m arcs meet the boundary for any ground shape.
- Outlines and arcs are drawn with `geom_path` (not ggforce) so line widths behave the same on every ggforce version.
- Every marking is a row in `afl_marking_data()` (posts, straight lines, curved paths) and is drawn once by `afl_markings()`. Edit the tables rather than adding `geom_segment()` calls.
- Analysis functions assume the team of interest attacks towards +x; "Left" lane is y > corridor_width / 2.

## Example

```r
afl_field(venue_length = 160, venue_width = 140, venue_arclength = 50)
```

See `images/` for example outputs.

## Packages

`ggplot2` (>= 3.4.0) only
