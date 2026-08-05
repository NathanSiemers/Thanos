################################################################
## Thanos visual identity: compact histogram theme + the plasma
## sel/unsel fill scale, both carried over from the original thanos.R
################################################################

theme_thanos <- theme(
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 9),
    legend.position = "none",
    plot.title   = element_text(size = 12, hjust = 0),
    axis.title   = element_text(size = 0, hjust = 0),
    legend.text  = element_text(size = 0, hjust = 0),
    legend.title = element_text(size = 0, hjust = 0)
)

## drop = FALSE keeps 'sel' anchored to the same plasma color even when a
## histogram momentarily contains only selected (or only unselected) rows
scale_fill_thanos <- function() {
    scale_fill_viridis_d(end = 0.4, option = "plasma", drop = FALSE)
}
