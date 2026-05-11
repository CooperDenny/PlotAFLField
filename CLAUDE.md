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

## Example

```r
afl_field(venue_length = 160, venue_width = 140, venue_arclength = 50)
```

See `images/` for example outputs.

## Packages

`ggplot2`, `ggforce`
