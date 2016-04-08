#' @include hSet.R
NULL

#' Visualize hierarchical sets
#'
#' This is the main visualization interface to hierarchicalSet object. By
#' changing the type argument you control which types of plots are produced. See
#' datails for a walkthrough of the different plot types. All plots are based on
#' ggplot2 but heavily modified using gtable. Because of this the return value
#' is always a gtable object, so it is not possible to add additional geoms, or
#' change scales etc. on the result of \code{plot()}.
#'
#' @param x A hierarchicalSet object to plot.
#'
#' @param label logical. Should sets be labeled.
#'
#' @param type The type of plot to produce. See detail. The name of the type may
#' be abbreviated.
#'
#' @param transform A string giving the scale transformation or a
#' \code{\link[scales]{trans}} object.
#'
#' @param style A ggplot2 theme to use as basis for the plot. Defaults to
#' theme_bw().
#'
#' @param quantiles The quantiles to split outlying elements up in for
#' outlying_elements plot. If length is above one a facetted plot will be
#' produced.
#'
#' @param upperBound The upper quantile threshold to include. Defaults to 1
#' (i.e. everything is included)
#'
#' @param tension The tension used for the hierarchical edge bundles in
#' outlying_elements plot. Defaults to 0.8
#'
#' @param alpha The alpha level for the edge bundles. Defaults to 1
#'
#' @param circular Logical. Should the hierarchical edge bundles be laid out in
#' a circular layout.
#'
#' @param showHierarchy Logical. For intersectionStack plots, should a
#' dendrogram mapping union sizes be drawn above the icicle plot. For
#' outlying_elements plots should a dendrogram be plotted below (for circular) or
#' to the left (for linear) of the edge bundles.
#'
#' @param evenHierarchy Logical. Should the heights of the dendrogram used for
#' constructing the edge bundles be evened out.
#'
#' @param ... Currently ignored
#'
#' @return A gtable object invisibly. This function is mainly called for the
#' side effect of creating a plot.
#'
#' @details
#' Currently 4 different plottypes are available:
#'
#' \describe{
#'   \item{\strong{dendrogram}}{Plots a horizontal dendrogram with the x-value
#'   mapped to the intersection size divided by the union size. This plot very
#'   clearly shows the rise in heterogenity as more and more sets are joined,
#'   and clearly shows clusters of very similar sets.}
#'   \item{\strong{intersectStack}}{Plots a bottom-up icicleplot with height
#'   showing the size of the intersection. In essence this plot communicates the
#'   same type of information as a Venn-diagram, but in a scalable way and only
#'   showing the intersections along the hierarchy. Box color maps to the degree
#'   (number of sets) of the intersection making high-degree high-intersection
#'   as well as low-degree low-intersection boxes stand out.}
#'   \item{\strong{heatmap}}{Plots a traditional heatmap showing all 2-degree
#'   intersections. The sets are organized according to the hierarchy so the
#'   result should show a number of squares along the diagonal. If two very
#'   similar sets have been forced apart by the clustering, this will show up
#'   nicely as high value squares away from the diagonal.}
#'   \item{\strong{composite}}{Combines dendrogram, intersectStack and heatmap
#'   into a composite plot.}
#'   \item{\strong{outlyingElements}}{Plots intersects between two sets that are
#'   missing from the intersect of their shared top node as hierarchical edge
#'   bundles. It helps detect deviations from the global structure as defined
#'   by the hierarchcial clustering.}
#' }
#'
#' @importFrom grid grid.draw grid.newpage
#'
#' @export
#'
plot.HierarchicalSet <- function(x, label = TRUE, type = 'dendrogram',
                                 transform=NULL, style=theme_bw(),
                                 quantiles = 0, upperBound = 1, tension = 0.8,
                                 alpha = 1, circular = TRUE,
                                 showHierarchy = !circular,
                                 evenHierarchy = circular, ...) {
    types <- c(
        'dendrogram',
        'intersectStack',
        'heatmap',
        'composite',
        'outlyingElements'
    )
    type <- match.arg(type, types)
    if (inherits(transform, 'trans') || length(transform) < 2) {
        transform <- lapply(c(types, 'bar'), function(x) transform)
        names(transform) <- c(types, 'bar')
    } else {
        if (is.null(names(transform))) {
            stop('Tranforms must be named when passing multiple values')
        }
        transform <- as.list(transform)
        names(transform) <- sapply(names(transform),
                                   match.arg, choices = c(types, 'bar'))
    }
    if (type %in% c('dendrogram', 'composite')) {
        denData <- createDendroData(clusters(x))
    }
    if (type %in% c('intersectStack', 'composite')) {
        iceData <- createIcicleData(clusters(x))
    }
    if (type %in% c('heatmap', 'composite')) {
        heatData <- createHeatData(x)
    }
    if (type %in% c('composite')) {
        barData <- createBarData(sets(x)[,match(denData$labels$label,
                                                set_names(x))])
    }
    if (type %in% c('outlyingElements')) {
        outData <- createOutlierData(x, quantiles = quantiles,
                                     tension = tension, circular = circular,
                                     evenHierarchy = evenHierarchy,
                                     upperBound = upperBound)
    }
    p <- switch(
        type,
        dendrogram = createDenTable(denData, style = style, label = label,
                                    transform = transform$dendrogram),
        intersectStack = createIceTable(iceData, style = style, label = label,
                                        transform = transform$intersectStack,
                                        showHierarchy = showHierarchy),
        heatmap = createHeatTable(heatData, style = style, label = label,
                                  transform = transform$heatmap),
        composite = createCompositeTable(
            denData, iceData, heatData, barData,
            style = style, label = label, transform = transform
        ),
        outlyingElements = createOutlierTable(outData, style, label, circular,
                                              showHierarchy, alpha),
        stop('Unknown plot type')
    )
    grid.newpage()
    grid.draw(p)
    invisible(p)
}
#' Plot the outlying elements of a HierarchicalSet object
#'
#' This function creates a scatter plot showing each outlying element as a
#' function of the number of sets it is present in and the number of times it
#' is outlying.
#'
#' @param x A HierarchicalSet object
#'
#' @param alpha The transparancy of the dots
#'
#' @return This function is called for its side effects
#'
#' @seealso \code{\link{outlying_elements}} for extracting outlying element
#' information from a HierarchicalSet object
#'
#' @importFrom Matrix rowSums
#'
#' @export
#'
#' @examples
#' data('twitter')
#'
#' twitSet <- create_hierarchy(twitter)
#' plot_outlier_distribution(twitSet)
#'
plot_outlier_distribution <- function(x, alpha = 0.3) {
    if (!inherits(x, 'HierarchicalSet')) {
        stop('plotOutDist only supports HierarchicalSet objects')
    }
    out <- table(unlist(outlying_elements(x, FALSE)$outliers))
    out <- data.frame(
        element = as.integer(names(out)),
        nOutlier = as.integer(out),
        nSets = rowSums(sets(x))[as.integer(names(out))]
    )
    ggplot() +
        geom_point(aes_(x = ~nSets, y = ~nOutlier), data = out, alpha = alpha) +
        ggtitle(paste0(nrow(out), ' outlying elements out of ', n_elements(x))) +
        xlab('# of sets with element') +
        ylab('# of times element is outlier') +
        theme_bw()
}
#' Extract the outlying elements from each set pair
#'
#' This function detects the outlying elements of each pair of sets in a
#' HierarchicalSet object. An outlying element is defined as an element in the
#' intersection of the two sets, but not in the intersection of their nearest
#' common set family in the hierarchy.
#'
#' @param x A HierarchicalSet object
#'
#' @param counts Should number of elements rather than the actual elements be
#' returned. Defaults to \code{TRUE}
#'
#' @return A data.frame containing information on the outlying elements of each
#' set pair. Only pairs with outlying elements are returned. The 'setX' coloumn
#' contains the index of the first set in the pair and the 'setY' column
#' contains the index of the second set in the pair. If \code{counts = TRUE}
#' then the 'nOutliers' column contains the number of outlying elements for each
#' pair. If \code{counts = FALSE} the the 'outlier' column contains the index of
#' the outlying elements for each pair
#'
#' @seealso \code{\link{plot_outlier_distribution}} for plotting the
#' distribution  of outlying elements in a HierarchicalSet object
#'
#' @importFrom stats is.leaf
#'
#' @export
#'
#' @examples
#' data('twitter')
#'
#' twitSet <- create_hierarchy(twitter)
#'
#' # Just get the counts
#' countOut <- outlying_elements(twitSet)
#' head(countOut)
#'
#' # Or the actual elements
#' elemOut <- outlying_elements(twitSet, FALSE)
#' head(elemOut)
#'
outlying_elements <- function(x, counts = TRUE) {
    newClusters <- lapply(clusters(x), dendrapply, function(x) {
        if (is.leaf(x)) {
            leafAttr <- attributes(x)
            x <- as.list(x)
            attributes(x) <- leafAttr
        }
        x
    })
    outliers <- getOutliers(newClusters, x$sets@p, x$sets@i, counts)
    if (counts) {
        data.frame(
            setX = outliers$from,
            setY = outliers$to,
            nOutliers = unlist(outliers$outliers)
        )
    } else {
        data.frame(
            setX = outliers$from,
            setY = outliers$to,
            outliers = I(outliers$outliers)
        )
    }
}
## HELPERS
#' Based on createDenData create a gtable
#'
#' @param data Data as returned by \code{\link{createDenData}}
#'
#' @param style A complete ggplot theme
#'
#' @param label Logical should the sets be labelled
#'
#' @param transform A trans object or the name of one from scales
#'
#' @return A gtable object ready to draw
#'
#' @importFrom grid nullGrob
#'
#' @noRd
#'
createDenTable <- function(data, style, label = TRUE, transform = NULL) {
    p <- ggplot()
    p <- p + style
    p <- p + theme(
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust = 0)
    )
    if (!label) {
        p <- p + theme(
            axis.text.y = element_blank()
        )
    }
    if (is.null(transform)) {
        transform <- 'identity'
    }
    p <- p + xlab(expression(paste(lambda, "'") ~~ italic("heterogeneity")))
    p <- p + scale_x_continuous(trans = transReverser(transform),
                                expand = c(0, 0))
    p <- p + expand_limits(x = c(0, max(data$segments$y)*1.025))
    p <- p + scale_y_continuous(breaks = data$labels$x,
                                labels = data$labels$label, expand = c(0,0.5),
                                limits = c(1, max(data$labels$x)))
    p <- p + geom_segment(aes_(y = ~x, x = ~y, yend = ~xend, xend = ~yend),
                          data = data$segments, lineend = 'round')
    pBuild <- ggplot_build(p)
    yaxisGrob <- ggplot2:::guide_axis(at = pBuild$panel$ranges[[1]]$y.major,
                                      data$labels$label, 'right', p$theme)
    p <- ggplot_gtable(pBuild)

    p$grobs[[2]] <- if (label) yaxisGrob else nullGrob()
    p$widths <- p$widths[c(1, 2, 4, 3, 5)]
    p$layout$l[p$layout$name == 'axis-l'] <- 4
    p$layout$r[p$layout$name == 'axis-l'] <- 4
    p$layout$l[p$layout$name %in% c('panel', 'axis-b', 'xlab')] <- 3
    p$layout$r[p$layout$name %in% c('panel', 'axis-b', 'xlab')] <- 3
    p
}
#' Based on createIceData create a gtable
#'
#' @param data Data as returned by \code{\link{createIceData}}
#'
#' @param style A complete ggplot theme
#'
#' @param label Logical should the sets be labelled
#'
#' @param yaxis The position of the y-axis
#'
#' @param transform A trans object or the name of one from scales
#'
#' @param showHierarchy Should a dendrogram be plotted on top
#'
#' @return A gtable object ready to draw
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices axisTicks
#'
#' @noRd
#'
createIceTable <- function(data, style, label = TRUE, yaxis = 'left',
                           transform = NULL, showHierarchy = FALSE) {
    data$segments$type <- factor('Union', levels = c('Union', 'Intersection'))
    data$rectangles$type <- factor('Intersection', levels = c('Union', 'Intersection'))
    p <- ggplot() + style + theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(
            angle = if (yaxis == 'left') 45 else 315,
            hjust = if (yaxis == 'left') 1 else 0,
            vjust = 1
        ),
        axis.line.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(angle = if (showHierarchy) 90 else 0),
        axis.text.y = element_text(hjust = if (yaxis == 'left') 1 else 0)
    )
    if (!label) {
        p <- p + theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
        )
    }
    if (is.null(transform)) {
        transform <- 'identity'
    }
    tops <- data$rectangles[data$rectangles$degree == 1, ]
    xLoc <- (tops$xmin + tops$xmax) / 2
    tops <- data.frame(x = xLoc, xend = xLoc, y = tops$ymax,
                       yend = max(tops$ymax),
                       type = factor('Intersection', levels = c(
                           'Union', 'Intersection'
                       )))

    if (showHierarchy) {
        p <- p + geom_segment(aes_(x = ~x, xend = ~xend, y = ~y,
                                   yend = ~yend),
                              data = data$segments, lineend = 'round')
        p <- p + facet_grid(type~., scales = 'free_y')
    }
    p <- p + geom_segment(aes_(x = ~x, xend = ~xend, y = ~y,
                               yend = ~yend),
                          data = tops, color = 'lightgrey')
    p <- p + geom_rect(aes_(xmin = ~xmin, xmax = ~xmax, ymin = ~ymin,
                            ymax = ~ymax, fill = ~degree),
                       data = data$rectangles, color = I('white'))
    p <- p + scale_y_continuous(trans = transform, expand = c(0, 0))
    p <- p + scale_x_continuous(expand = c(0, 0), breaks = data$labels$x,
                                labels = data$labels$label)
    p <- p + scale_fill_gradientn(
        'Set family\nsize',
        colours = rev(brewer.pal(9, 'Greens')[-(1:2)]),
        trans = 'log10',
        breaks = axisTicks(log10(range(data$rectangles$degree)), log = TRUE),
        guide = guide_colorbar(override.aes = list(size = 0))
    )
    if (showHierarchy) {
        p <- p + ylab('Size')
    } else {
        p <- p + ylab(expression(group('|', intersect(), '|')))
    }

    pBuild <- ggplot_build(p)
    yaxisGrob <- ggplot2:::guide_axis(at = pBuild$panel$ranges[[1]]$y.major,
                                      pBuild$panel$ranges[[1]]$y.labels,
                                      'right', p$theme)
    p <- ggplot_gtable(pBuild)
    if (yaxis == 'right') {
        p$grobs[[2]] <- yaxisGrob
        p$widths <- p$widths[c(1, 4, 3, 2, 5, 6)]
        p$layout$l[p$layout$name == 'axis-l'] <- 3
        p$layout$r[p$layout$name == 'axis-l'] <- 3
        p$layout$l[p$layout$name == 'ylab'] <- 4
        p$layout$r[p$layout$name == 'ylab'] <- 4
        p$layout$l[p$layout$name %in% c('panel', 'axis-b', 'xlab')] <- 2
        p$layout$r[p$layout$name %in% c('panel', 'axis-b', 'xlab')] <- 2
    }
    p
}
#' @importFrom gtable gtable_width
createHeatTable <- function(data, style, label = TRUE, transform = NULL) {
    p <- ggplot() + style + theme(
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.line = element_blank(),
        panel.border = element_blank()
    )
    if (!label) {
        p <- p + theme(
            axis.text = element_blank(),
            axis.ticks = element_blank()
        )
    }
    if (is.null(transform)) {
        transform <- 'identity'
    }
    intersectScale <- scale_color_distiller('Intersect\nsize',
                                            trans = transform,
                                            palette = 'Blues',
                                            guide = 'colorbar',
                                            direction = 1)
    val <- unique(c(data$path$intersect, data$circles$intersect))
    intersectScale$range$train(range(val))
    pal <- intersectScale$palette(intersectScale$rescaler(val))
    data$path$intersectCol <- pal[match(data$path$intersect, val)]
    data$circles$intersectCol <- pal[match(data$circles$intersect, val)]
    unionScale <- scale_fill_distiller('Union\nsize', trans = transform,
                                       palette = 'Purples', guide = 'colorbar',
                                       direction = 1)
    val <- unique(data$circles$union)
    unionScale$range$train(range(val))
    pal <- unionScale$palette(unionScale$rescaler(val))
    data$circles$unionCol <- pal[match(data$circles$union, val)]

    p <- p + geom_polygon(aes_(x = ~x, y = ~y, group = ~group),
                          data = data$path, fill = I(data$path$intersectCol))
    p <- p + geom_rect(aes_(xmin = ~x - 0.25, xmax = ~x + 0.25,
                           ymin = ~y - 0.25, ymax = ~y + 0.25),
                       data = data$circles, fill = I(data$circles$intersectCol))
    p <- p + geom_rect(aes_(xmin = ~y - 0.5, xmax = ~y + 0.5, ymin = ~x - 0.5,
                           ymax = ~x + 0.5),
                       data = data$circles, fill = I(data$circles$unionCol))
    p <- p + geom_path(aes_(x = ~y, y = ~x, group = ~group), data = data$path,
                       color = I('white'))
    p <- p + scale_x_continuous(expand = c(0, 0), breaks = data$labels$x,
                                labels = data$labels$label)
    p <- p + scale_y_continuous(expand = c(0, 0), breaks = data$labels$x,
                                labels = data$labels$label)

    customScales <- ggplot2:::scales_list()
    customScales$add(intersectScale)
    customScales$add(unionScale)
    guides <- ggplot2:::build_guides(
        customScales,p$layers, p$mapping, 'right', p$theme, p$guides, p$labels
    )
    guideWidth <- gtable_width(guides) + p$theme$legend.margin

    p <- ggplotGrob(p)
    p <- gtable_add_cols(p, guideWidth, 4)
    p <- gtable_add_grob(p, guides, t = 3, l = 5, clip = 'off',
                         name = 'guide-box')
    p
}
createBarTable <- function(data, style, label = TRUE, yaxis = 'left',
                           transform = NULL) {
    p <- ggplot() + style + theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(
            angle = if (yaxis == 'left') 45 else 315,
            hjust = if (yaxis == 'left') 1 else 0,
            vjust = 1
        ),
        axis.line.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(angle = if (yaxis == 'left') 90 else 270),
        axis.text.y = element_text(hjust = if (yaxis == 'left') 1 else 0)
    )
    if (!label) {
        p <- p + theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
        )
    }
    if (is.null(transform)) {
        transform <- 'identity'
    }
    p <- p + geom_bar(aes_(x = ~x, y = ~y), data = data, stat = 'identity')
    p <- p + ylab('# unique')
    p <- p + scale_y_continuous(trans = transform, expand = c(0, 0))
    p <- p + scale_x_continuous(expand = c(0, 0), breaks = data$x,
                                labels = data$label)
    pBuild <- ggplot_build(p)
    yaxisGrob <- ggplot2:::guide_axis(at = pBuild$panel$ranges[[1]]$y.major,
                                      pBuild$panel$ranges[[1]]$y.labels,
                                      'right', p$theme)
    p <- ggplot_gtable(pBuild)

    if (yaxis == 'right') {
        p$grobs[[2]] <- yaxisGrob
        p$widths <- p$widths[c(1, 4, 3, 2, 5)]
        p$layout$l[p$layout$name == 'axis-l'] <- 3
        p$layout$r[p$layout$name == 'axis-l'] <- 3
        p$layout$l[p$layout$name == 'ylab'] <- 4
        p$layout$r[p$layout$name == 'ylab'] <- 4
        p$layout$l[p$layout$name %in% c('panel', 'axis-b', 'xlab')] <- 2
        p$layout$r[p$layout$name %in% c('panel', 'axis-b', 'xlab')] <- 2
    }

    p
}
#' @importFrom gtable gtable gtable_add_grob
#' @importFrom grid unit.c nullGrob
createCompositeTable <- function(den, ice, heat, bar, style, label, transform) {
    den <- createDenTable(den, style = style, label = label,
                          transform = transform$dendrogram)
    ice <- createIceTable(ice, style = style, label = label, yaxis = 'right',
                          transform = transform$intersectStack)
    heat <- createHeatTable(heat, style = style, label = FALSE,
                            transform = transform$heatmap)
    bar <- createBarTable(bar, style = style, label = FALSE, yaxis = 'right',
                          transform = transform$bar)

    legends <- gtable(
        widths = unit.c(unit(1, 'null'), heat$widths[5], heat$widths[5],
                        ice$widths[5], unit(1, 'null')),
        heights = unit.c(den$heights[4:5], unit(1, 'null'))
    )
    legends <- gtable_add_grob(
        legends,
        grobs = list(
            den$grobs[[5]],
            den$grobs[[6]],
            heat$grobs[[9]]$grobs[[1]],
            heat$grobs[[9]]$grobs[[2]],
            ice$grobs[[8]]$grobs[[1]]
        ),
        t = c(1, 2, 3, 3, 3),
        l = c(1, 1, 2, 3, 4),
        r = c(5 ,5, 2, 3, 4),
        clip = 'off'
    )
    widths <- unit.c(
        unit(0.5, 'lines'),
        unit(1, 'null'),
        unit(1, 'null'),
        max(ice$widths[3], bar$widths[3]),
        max(ice$widths[4], bar$widths[4]),
        if (label) den$widths[4] else unit(1, 'lines')
    )
    heights <- unit.c(
        unit(1, 'lines'),
        unit(0.25, 'null'),
        unit(0.25, 'lines'),
        unit(1, 'null'),
        unit(1, 'null'),
        if (label) ice$heights[4] else unit(0, 'lines'),
        unit(1, 'lines')
    )
    p <- gtable(widths, heights)
    p <- gtable_add_grob(
        p,
        grobs = list(
            den$grobs[[1]],
            den$grobs[[4]],
            bar$grobs[[4]],
            heat$grobs[[4]],
            ice$grobs[[4]]
        ),
        t = c(1, 4, 2, 4, 5),
        l = c(1, 2, 3, 3, 3),
        b = c(length(heights), 4, 2, 4, 5),
        r = c(length(widths), 2, 3, 3, 3)
    )
    gtable_add_grob(
        p,
        grobs = list(
            legends,
            bar$grobs[[2]],
            bar$grobs[[7]],
            if (label) den$grobs[[2]] else nullGrob(),
            ice$grobs[[2]],
            ice$grobs[[7]],
            if (label) ice$grobs[[5]] else nullGrob()
        ),
        t = c(5, 2, 2, 4, 5, 5, 6),
        l = c(2, 4, 5, 4, 4, 5, 3),
        clip = 'off'
    )
}
#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @importFrom RColorBrewer brewer.pal
createOutlierTable <- function(data, style, label, circular, showHierarchy, alpha) {
    if (circular) {
        nPanels <- length(levels(data$bundles$group))
        p <- ggplot() + style + theme(
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank()
        )
        p <- p + geom_path(aes_(x = ~x, y = ~y, group = ~id,
                                color = ~nOutliers),
                           alpha = alpha, data = data$bundles)
        if (label) {
            data$labels$hjust <- ifelse(data$labels$x > 0, 0, 1)
            data$labels$vjust <- 0.5
            data$labels$angle <- ifelse(
                sign(data$labels$x) == 1,
                atan2(data$labels$y, data$labels$x)*180/pi,
                atan2(data$labels$y, data$labels$x)*180/pi - 180)
        }
        if (nPanels > 1) {
            nColumns <-  if (label || showHierarchy) {
                getCol(nPanels)
            } else {
                NULL
            }
            p <- p + facet_wrap(~group, ncol=nColumns)
        } else if (label) {
            nColumns <- 1
            p  <- p + geom_text(aes_(x = ~x*1.05, y = ~y*1.05, hjust = ~hjust,
                                    vjust = ~vjust, angle = ~angle,
                                    label = ~label),
                                data = data$labels,
                                size = getAxisTextSize(style)*5/14)
            maxLength <- nchar(as.character(data$labels$label))
            newLim <- c(-1, 1) * maxLength*0.12
            p <- p + expand_limits(x = newLim, y = newLim)
            p <- p + theme(panel.border = element_blank())
        }
        p <- p + coord_fixed()
        p <- p + scale_color_gradientn(
            '# Outlying\nelements',
            colours = brewer.pal(9, 'YlOrRd')[-(1:2)]
        )
        p <- ggplotGrob(p)
        if ((label && nPanels > 1) || showHierarchy) {
            denP <- ggplot() + style + theme(
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(),
                panel.grid = element_blank(),
                panel.border = element_blank()
            )
            denP <- denP + geom_segment(aes_(x = ~x, y = ~y, xend = ~xend,
                                            yend = ~yend),
                                        data = data$segments, lineend = 'round')
            if (label) {
                denP <- denP + geom_text(aes_(x = ~x*1.05, y = ~y*1.05,
                                             hjust = ~hjust, vjust = ~vjust,
                                             angle = ~angle, label = ~label),
                                         data = data$labels,
                                         size = getAxisTextSize(style)*5/14)
                maxLength <- nchar(as.character(data$labels$label))
                newLim <- c(-1, 1) * maxLength*0.12
                denP <- denP + expand_limits(x = newLim, y = newLim)
            }
            denP <- ggplotGrob(denP)
            insertAt <- length(p$heights) - 2
            colSpan <- range(p$layout$l[grep('panel', p$layout$name)])
            p <- gtable_add_rows(p, unit(nColumns, 'null'), insertAt)
            p <- gtable_add_grob(p, denP$grobs[[4]], t = insertAt + 1,
                                 l = colSpan[1], r = colSpan[2])
        }
    } else {
        p <- ggplot() + style + theme(
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.text.y = element_text(hjust = if (showHierarchy) 0.5 else 1),
            axis.ticks.y = element_blank()
        )
        p <- p + geom_path(aes_(x = ~y, y = ~x, group = ~id,
                                color = ~nOutliers),
                           alpha = I(0.25), data = data$bundles)
        if (!label) {
            p <- p + theme(
                axis.text.y = element_blank()
            )
        }
        p <- p + scale_y_continuous(breaks = data$labels$x,
                                    labels = data$labels$label,
                                    limits = range(data$labels$x))
        p <- p + scale_x_continuous(expand = c(0, 0))
        p <- p + expand_limits(x = c(0, max(data$bundles$y)*1.025))
        p <- p + scale_color_gradientn(
            '# Outlying\nelements',
            colours = brewer.pal(9, 'YlOrRd')[-(1:2)]
        )
        p <- ggplotGrob(p)
        if (showHierarchy) {
            denP <- ggplot() + style + theme(
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(),
                panel.grid = element_blank(),
                panel.border = element_blank()
            )
            denP <- denP + geom_segment(aes_(x = ~y, y = ~x, xend = ~yend,
                                            yend = ~xend),
                                        data = data$segments, lineend = 'round')
            denP <- denP + scale_y_continuous(breaks = data$labels$x,
                                        labels = data$labels$label,
                                        limits = range(data$labels$x))
            denP <- denP + scale_x_reverse(expand = c(0.025, 0))
            denP <- denP + expand_limits(x = c(0, max(data$segments$y)*1.025))
            denP <- ggplotGrob(denP)
            p <- gtable_add_cols(p, widths = unit(1, 'null'), 1)
            p <- gtable_add_grob(p, denP$grobs[[4]], t = 3, l = 2, clip = 'off')
        }
    }
    p
}
getCol <- function(n) {
    if (n <= 4) {
        n
    } else if (n <= 6) {
        3
    } else if (n <= 8) {
        4
    } else {
        ceiling(sqrt(n) * 1.1)
    }
}
createDendroData <- function(trees, type = 'rectangle') {
    plotData <- lapply(trees, dendro_data, type = type)
    labelJumps <- cumsum(sapply(plotData, function(x) nrow(x$labels)))
    labelJumps <- c(0, labelJumps[-length(labelJumps)])
    plotData <- Map(function(pdata, offset) {
        pdata$segments$x <- pdata$segments$x + offset
        pdata$segments$xend <- pdata$segments$xend + offset
        pdata$labels$x <- pdata$labels$x + offset
        pdata
    }, pdata = plotData, offset = labelJumps)
    list(
        segments = do.call(rbind, lapply(plotData, `[[`, 'segments')),
        labels = do.call(rbind, lapply(plotData, `[[`, 'labels'))
    )
}
createIcicleData <- function(trees) {
    treesRect <- lapply(trees, function(tree) {
        dendrapply(tree, function(x) {
            attr(x, 'height') <- attr(x, 'intersect')
            x
        })
    })
    treesDen <- lapply(trees, function(tree) {
        dendrapply(tree, function(x) {
            attr(x, 'height') <- attr(x, 'union')
            x
        })
    })
    plotData <- lapply(treesRect, icicle_data)
    plotData2 <- lapply(treesDen, dendro_data, type = 'rectangle')
    labelJumps <- cumsum(sapply(plotData, function(x) nrow(x$labels)))
    labelJumps <- c(0, labelJumps[-length(labelJumps)])
    plotData <- Map(function(pdata, pdata2, offset) {
        pdata$rectangles$xmin <- pdata$rectangles$xmin + offset
        pdata$rectangles$xmax <- pdata$rectangles$xmax + offset
        pdata$labels$x <- pdata$labels$x + offset
        pdata$segments <- pdata2$segments
        pdata$segments$x <- pdata$segments$x + offset
        pdata$segments$xend <- pdata$segments$xend + offset
        pdata
    }, pdata = plotData, pdata2 = plotData2, offset = labelJumps)
    list(
        rectangles = do.call(rbind, lapply(plotData, `[[`, 'rectangles')),
        segments = do.call(rbind, lapply(plotData, `[[`, 'segments')),
        labels = do.call(rbind, lapply(plotData, `[[`, 'labels'))
    )
}
createHeatData <- function(x) {
    denData <- createDendrogramixData(clusters(x))
    denData$circles <- pairSummary(sets(x)@p, sets(x)@i,
                                   match(denData$labels$label,
                                         set_names(x)))
    denData
}
#' @importFrom Matrix rowSums
createBarData <- function(sets) {
    elementSizes <- rowSums(sets)
    setUniques <- apply(sets, 2, function(x) {
        sum(x == elementSizes)
    })
    data.frame(
        x = seq_len(ncol(sets)),
        y = setUniques,
        label = colnames(sets)
    )
}
#' @importFrom stats quantile
createOutlierData <- function(x, quantiles, tension, circular,
                              evenHierarchy = TRUE, upperBound = 1) {
    out <- outlying_elements(x)
    splits <- quantile(out$nOutliers, quantiles)
    out$group <- NA
    for (i in seq_along(splits)) {
        out$group[out$nOutliers >= splits[i]] <- names(splits)[i]
    }
    out$group[out$nOutliers > quantile(out$nOutliers, upperBound)] <- NA
    out <- out[!is.na(out$group),]
    bundles <- createBundles(clusters(x), out[, 1:2], tension = tension,
                             circular = circular)
    bundles$nOutliers <- out$nOutliers[bundles$id]
    bundles$group <- factor(out$group[bundles$id], levels = names(splits))
    if (evenHierarchy) {
        x$clusters <- lapply(clusters(x), layoutSpread)
    }
    dendro <- createDendroData(clusters(x),
                               type = if (circular) 'triangle' else 'rectangle')
    if (circular) {
        yRange <- range(c(dendro$segments$y, dendro$segments$yend))
        xRange <- range(dendro$labels$x)
        circLab <- cartToCirc(dendro$labels, 1, yRange, xRange)
        circSeg <- cartToCirc(dendro$segments, 1, yRange, xRange)
        circSeg[, c('xend', 'yend')] <- cartToCirc(
            data.frame(
                x = dendro$segments$xend,
                y = dendro$segments$yend
            ),
            1,
            yRange,
            xRange
        )
        dendro$labels$x <- circLab$x
        dendro$labels$y <- circLab$y
        dendro$segments <- circSeg
    }
    list(
        bundles = bundles,
        segments = dendro$segments,
        labels = dendro$labels
    )
}
createDendrogramixData <- function(trees) {
    plotData <- lapply(trees, dendrogramix_data)
    labelJumps <- cumsum(sapply(plotData, function(x) nrow(x$labels)))
    labelJumps <- c(0, labelJumps[-length(labelJumps)])
    plotData <- Map(function(pdata, offset) {
        pdata$path$min <- pdata$path$min + offset
        pdata$path$max <- pdata$path$max + offset
        pdata$labels$x <- pdata$labels$x + offset
        pdata
    }, pdata = plotData, offset = labelJumps)
    plotData <- list(
        path = do.call(rbind, lapply(plotData, `[[`, 'path')),
        labels = do.call(rbind, lapply(plotData, `[[`, 'labels'))
    )
    plotData$path <- do.call(
        rbind, lapply(1:nrow(plotData$path), function(i, d) {
        data.frame(
            x = c(d$min[i], d$min[i], d$max[i]),
            y = c(d$min[i], d$max[i], d$max[i]),
            intersect = d$intersect[i],
            union = d$union[i],
            group = i
        )
    }, d = plotData$path))
    plotData
}
#' @importFrom stats is.leaf
dendro_data <- function(dendrogram, type) {
    if (is.leaf(dendrogram)) {
        list(
            segments = data.frame(x = numeric(), y = numeric(),
                                  xend = numeric(), yend = numeric()),
            labels = data.frame(x = 1, y = 0, label = attr(dendrogram, 'label'))
        )
    } else {
        ggdendro::dendro_data(dendrogram, type = type)
    }
}
#' @importFrom stats is.leaf
#' @importFrom utils tail
#'
icicle_data <- function(dendrogram, base = 0, left = 1) {
    height <- attr(dendrogram, 'height')
    degree <- attr(dendrogram, 'members')
    if (is.leaf(dendrogram)) {
        list(
            rectangles = data.frame(
                xmin = left - 0.5, xmax = left + 0.5,
                ymin = base, ymax = height,
                degree = degree
            ),
            labels = data.frame(
                x = left, y = 0, label = attr(dendrogram, 'label'),
                stringsAsFactors = FALSE
            )
        )
    } else {
        data1 <- icicle_data(dendrogram[[1]], base = height, left = left)
        data2 <- icicle_data(dendrogram[[2]], base = height,
                             left = left + attr(dendrogram[[1]], 'members'))
        data3 <- data.frame(
            xmin = tail(data1$rectangles$xmin, 1),
            xmax = tail(data2$rectangles$xmax, 1),
            ymin = base, ymax = height, degree = degree
        )
        list(
            rectangles = rbind(data1$rectangles, data2$rectangles, data3),
            labels = rbind(data1$labels, data2$labels)
        )
    }
}
#' @importFrom stats is.leaf
dendrogramix_data <- function(dendrogram, left = 1) {
    intersect <- attr(dendrogram, 'intersect')
    union <- attr(dendrogram, 'union')
    if (is.leaf(dendrogram)) {
        list(
            path = data.frame(
                min = left - 0.5, max = left + 0.5,
                intersect = intersect, union = union
            ),
            labels = data.frame(
                x = left, y = 0, label = attr(dendrogram, 'label'),
                stringsAsFactors = FALSE
            )
        )
    } else {
        data1 <- dendrogramix_data(dendrogram[[1]], left = left)
        data2 <- dendrogramix_data(dendrogram[[2]],
                             left = left + attr(dendrogram[[1]], 'members'))
        data3 <- data.frame(
            min = data1$path$min[1],
            max = data2$path$max[1],
            intersect = intersect, union = union
        )
        list(
            path = rbind(data3, data1$path, data2$path),
            labels = rbind(data1$labels, data2$labels)
        )
    }
}
findTopNode <- function(den, i) {
    if (all(i %in% attr(den[[1]], 'memberSets'))) {
        findTopNode(den[[1]], i)
    } else if (all(i %in% attr(den[[2]], 'memberSets'))) {
        findTopNode(den[[2]], i)
    } else {
        den
    }
}
#' @importFrom stats is.leaf
addCoord <- function(clusters) {
    setCoord <- function(den, offset) {
        if (is.leaf(den)) {
            attr(den, 'coord') <- offset
        } else {
            den[[1]] <- setCoord(den[[1]], offset)
            den[[2]] <- setCoord(den[[2]], offset + attr(den[[1]], 'members'))
            attr(den, 'coord') <- offset + attr(den, 'midpoint')
        }
        den
    }
    offsets <- cumsum(sapply(clusters, attr, 'members'))
    offsets <- c(0, offsets[-length(offsets)]) + 1
    Map(function(den, offset) {
        setCoord(den, offset)
    }, den = clusters, offset = offsets)
}
#' @importFrom stats is.leaf
layoutSpread <- function(den) {
    if (is.leaf(den)) {
        attr(den, 'height') <- 0
    } else {
        den[[1]] <- layoutSpread(den[[1]])
        den[[2]] <- layoutSpread(den[[2]])
        attr(den, 'height') <- max(sapply(den, attr, 'height')) + 1
    }
    den
}
createBundles <- function(clusters, pairs, tension=0.8, circular=FALSE,
                          detail = 100) {
    clusters <- lapply(clusters, layoutSpread)
    paths <- getPaths(clusters, pairs)
    if (circular) {
        paths$points <- cartToCirc(paths$points, 1)
    }
    paths <- do.call(rbind, lapply(seq_along(paths$paths), function(pathInd) {
        data.frame(paths$points[paths$paths[[pathInd]], ], id = pathInd)
    }))
    if (tension != 1) {
        paths <- bundleStrength(paths, tension)
    }
    splines <- getSplines(paths$x, paths$y, paths$id, detail)
    data.frame(x = splines$paths[,1], y = splines$paths[,2],
               id = splines$pathID)
}
getPaths <- function(clusters, pairs) {
    clusters <- enumerateClusters(addCoord(clusters))
    coordinates <- getCoordinates(clusters)
    if (length(clusters) > 1) {
        unconnectPoint <- nrow(coordinates) + 1
        coordinates[unconnectPoint, ] <- c(
            mean(sapply(clusters, attr, 'coord')),
            max(sapply(clusters, attr, 'height')) + 1,
            unconnectPoint
        )
    }
    coordinates <- coordinates[order(coordinates$id),]
    paths <- lapply(seq_len(nrow(pairs)), function(i) {
        pair <- pairs[i,]
        if (length(clusters) != 1) {
            clustMember <- sapply(pair, function(j) {
                which(sapply(clusters, function(c) {
                    j %in% attr(c, 'memberSets')
                }))
            })
        } else {
            clustMember <- c(1, 1)
        }
        if (clustMember[1] != clustMember[2]) {
            path1 <- getPath(clusters[[clustMember[1]]], pair[1])
            path2 <- getPath(clusters[[clustMember[2]]], pair[2])
            path <- c(path1, unconnectPoint, rev(path2))
        } else {
            topNode <- findTopNode(clusters[clustMember[1]], pair)
            if (pair[2] %in% attr(topNode[[1]], 'memberSets')) pair <- rev(pair)
            path1 <- getPath(topNode[[1]], pair[1])
            path2 <- getPath(topNode[[2]], pair[2])
            path <- c(path1, attr(topNode, 'enumerator'), rev(path2))
        }
        path
    })
    list(
        paths = paths,
        points = coordinates[, 1:2]
    )
}
cartToCirc <- function(points, sep, yRange = range(points$y),
                       xRange = range(points$x)) {
    if (diff(yRange) == 0) {
        points$y <- 1
    } else {
        points$y <- 1 - (points$y - yRange[1])/diff(yRange)
    }
    if (diff(xRange) == 0) {
        points$x <- 0
    } else {
        points$x <- (points$x - xRange[1])/(diff(xRange) + sep) * 2*pi
    }
    data.frame(x = points$y*cos(points$x), y = points$y*sin(points$x))
}
enumerateClusters <- function(clusters) {
    for (i in seq_along(clusters)) {
        if (i == 1) {
            start <- 1
        } else {
            start <- attr(clusters[[i - 1]], 'enumerator') + 1
        }
        clusters[[i]] <- enumerateNodes(clusters[[i]], start)
    }
    clusters
}
#' @importFrom stats is.leaf
enumerateNodes <- function(den, start) {
    if (is.leaf(den)) {
        attr(den, 'enumerator') <- start
    } else {
        den[[1]] <- enumerateNodes(den[[1]], start)
        den[[2]] <- enumerateNodes(den[[2]], attr(den[[1]], 'enumerator') + 1)
        attr(den, 'enumerator') <- attr(den[[2]], 'enumerator') + 1
    }
    den
}
#' @importFrom stats is.leaf
getCoordinates <- function(clusters) {
    getCoord <- function(den) {
        if (is.leaf(den)) {
            list(
                x = attr(den, 'coord'),
                y = attr(den, 'height'),
                id = attr(den, 'enumerator')
            )
        } else {
            coord1 <- getCoord(den[[1]])
            coord2 <- getCoord(den[[2]])
            list(
                x = c(coord1$x, coord2$x, attr(den, 'coord')),
                y = c(coord1$y, coord2$y, attr(den, 'height')),
                id = c(coord1$id, coord2$id, attr(den, 'enumerator'))
            )
        }
    }
    do.call(rbind, lapply(lapply(clusters, getCoord), data.frame))
}
#' @importFrom stats is.leaf
getPath <- function(den, leaf) {
    id <- attr(den, 'enumerator')
    if (!is.leaf(den)) {
        if (leaf %in% attr(den[[1]], 'memberSets')) {
            c(getPath(den[[1]], leaf), id)
        } else {
            c(getPath(den[[2]], leaf), id)
        }
    } else {
        id
    }
}
#' @importFrom utils head tail
bundleStrength <- function(path, strength) {
    formula <- function(p, startInd, endInd, pathLengths) {
        start <- rep(p[startInd], pathLengths)
        range <- rep(p[endInd] - p[startInd], pathLengths)
        ind <- unlist(lapply(pathLengths, seq_len)) - 1
        length <- rep(pathLengths, pathLengths)
        strength*p + (1 - strength)*(start + (ind/(length - 1))*range)
    }
    idInds <- split(seq_len(nrow(path)), path$id)
    pathLengths <- lengths(idInds)
    startInd <- sapply(idInds, head, n = 1)
    endInd <- sapply(idInds, tail, n = 1)
    path$x <- formula(path$x, startInd, endInd, pathLengths)
    path$y <- formula(path$y, startInd, endInd, pathLengths)
    path
}
#' @importFrom scales as.trans trans_new asn_trans atanh_trans boxcox_trans date_trans exp_trans identity_trans log10_trans log1p_trans log2_trans logit_trans log_trans probability_trans probit_trans reciprocal_trans reverse_trans sqrt_trans time_trans
transReverser <- function(name) {
    transformOrig <- as.trans(name)
    trans_new(
        name = paste0('reverse-', transformOrig$name),
        transform = function(x) {
            -transformOrig$transform(x)
        },
        inverse = function(x) {
            transformOrig$inverse(-x)
        },
        breaks = transformOrig$breaks,
        format = transformOrig$format,
        domain = transformOrig$domain
    )
}
#' Create a power transformation object
#'
#' This function can be used to create a proper trans object that encapsulates
#' a power transformation (x^n).
#'
#' @param n The degree of the power transformation
#'
#' @return A trans object
#'
#' @importFrom scales trans_new extended_breaks format_format
#' @importFrom MASS fractions
#'
#' @export
#'
power_trans <- function(n) {
    trans_new(
        name = paste0("power of ", fractions(n)),
        transform = function(x) {
            x ^ n
        },
        inverse = function(x) {
            x ^ (1/n)
        },
        breaks = extended_breaks(),
        format = format_format(),
        domain = c(0, Inf)
    )
}
getAxisTextSize <- function(theme, which = 'x') {
    baseElement <- paste0('axis.text.', which)
    path <- c(baseElement, 'axis.text', 'text')
    i <- 1
    size <- NA

    while (is.na(size) || inherits(size, 'rel')) {
        if (i > length(path)) stop('Could not determine axis text size')

        if (!is.null(theme[[path[i]]]$size)) {
            nextSize <- theme[[path[i]]]$size
            if (inherits(size, 'rel')) {
                size <- size * nextSize
                if (!inherits(nextSize, 'rel')) {
                    size <- as.numeric(size)
                }
            } else {
                size <- nextSize
            }
        }
        i <- i + 1
    }
    size
}
