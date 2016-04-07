#' @include hSet.R
NULL

#' @describeIn hSet Print method for hierarchicalSet objects
#'
#' @param x A hierarchicalSet object
#'
#' @export
#'
    cat('A hierarchicalSet object\n\n')
    cat('                 Universe size: ', nrow(x$sets), '\n', sep = '')
    cat('                Number of sets: ', length(x), '\n', sep = '')
    cat('Number of independent clusters: ', length(x$clusters), '\n', sep = '')
}
#' @describeIn hSet Number of sets in a hierarchicalSet object
#'
#' @export
#'
length.hierarchicalSet <- function(x) {
    ncol(x$sets)
print.HierarchicalSet <- function(x, ...) {
}
#' @describeIn hSet Extract dendrogram objects from hierarchicalSet objects
#'
#' @param i The index of the dendrogram
#'
#' @export
#'
    x$clusters[[i]]
`[[.HierarchicalSet` <- function(x, i) {
}
#' @describeIn hSet Subset hierarchicalSet object by dendrogram (preserving set
#' information and class)
#'
#' @importFrom stats dendrapply
#'
#' @export
#'
    clusters <- x$clusters[i]
    includedSets <- sort(unlist(lapply(clusters, attr, 'memberSets')))
    sets <- x$sets[, includedSets, drop = FALSE]
    sets <- sets[rowSums(sets) != 0, , drop = FALSE]
    clusters <- lapply(clusters, function(den) {
`[.HierarchicalSet` <- function(x, i) {
        dendrapply(den, function(node) {
            attr(node, 'memberSets') <- match(attr(node, 'memberSets'), includedSets)
            node
        })
    })
    res <- list(sets = sets, clusters = clusters)
    class(res) <- 'hierarchicalSet'
    res
}
