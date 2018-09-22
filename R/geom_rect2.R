#just to allow two diff fills in heat plot
geom_rect2 <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRect2,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            ...
        )
    )
}
#' @importFrom grid rectGrob grobTree gpar
GeomRect2 <- ggproto('GeomRect2', GeomRect,
    draw_panel = function (self, data, panel_params, coord) {
        if (!coord$is_linear()) {
            aesthetics <- setdiff(names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax"))
            polys <- plyr::alply(data, 1, function(row) {
                poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
                aes <- as.data.frame(row[aesthetics], stringsAsFactors = FALSE)[rep(1, 5), ]
                GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
            })
            ggname("bar", do.call(grobTree, polys))
        }
        else {
            coords <- coord$transform(data, panel_params)
            ggname("geom_rect", rectGrob(
                coords$xmin,
                coords$ymax,
                width = coords$xmax - coords$xmin,
                height = coords$ymax - coords$ymin,
                default.units = "native",
                just = c("left", "top"),
                gp = gpar(col = coords$fill, fill = alpha(coords$colour, coords$alpha), lwd = coords$size * .pt, lty = coords$linetype, lineend = "butt")))
        }
    }
)
rect_to_poly <- function(xmin, xmax, ymin, ymax) {
    data.frame(
        y = c(ymax, ymax, ymin, ymin, ymax),
        x = c(xmin, xmax, xmax, xmin, xmin)
    )
}
#' @importFrom grid grobName
ggname <- function (prefix, grob) {
    grob$name <- grobName(grob, prefix)
    grob
}
