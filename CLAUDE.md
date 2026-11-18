# PlotAFLField

R function for plotting a customisable AFL field using ggplot2.

## Function

`afl_field(venue_length, venue_width, venue_arclength)` in `plot_afl_field.R`

Parameters control the oval dimensions — different AFL venues have different field sizes.

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

## Example

```r
afl_field(venue_length = 160, venue_width = 140, venue_arclength = 50)
```

See `images/` for example outputs.

## Packages

`ggplot2` (>= 3.4.0) only
