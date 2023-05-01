# TODO: Subclass with metadata?
#       t() function that transposes the presence absence matrix and creates a
#         hierarchy for the elements

#' Create and store hierarchcical sets
#'
#' HierarchicalSet object can be created using the `hSet()` constructor.
#' The resulting object will contain both the underlying sets as well as the
#' resulting clustering.
#'
#' @param sets The sets to analyse. Can either be a matrix/data.frame giving the
#' presence/absence pattern of elements, with elements as rows and sets as
#' columns, or a list of vectors giving the elements of the individual sets.
#'
#' @param intersectLimit The proportion of sets an element must be present in to
#' be considered part of the intersect. Standard intersects require it to be
#' present in all sets (`intersectLimit = 1`), which is also the default
#'
#' @return An object of class HierarchicalSet
#'
#' @details
#' The HierarchicalSet class contains both the clustering and the original sets.
#' The former is stored in a list of dendrogram objects in and the latter as a
#' presence/absence matrix. Both are retrivable using `$clusters` and
#' `$sets` respectively. Furthermore individual dendrograms can be
#' extracted directly using the `[[` operator. If multiple independent
#' clusters exists the object can be subsetted using the `[` operator.
#'
#' For plotting functionality see the separate plot documentation for
#' [plot.HierarchicalSet()].
#'
#' @aliases HierarchicalSet
#'
#' @importFrom stats is.leaf
#'
#' @export
#'
#' @examples
#' data('twitter')
#'
#' # Caclulate the clustering
#' twitSet <- create_hierarchy(twitter)
#'
#' # Some statistics on the data
#' n_sets(twitSet)
#' n_elements(twitSet)
#' n_clusters(twitSet)
#'
#' # Focus on the first two independent cluster
#' twitSet[1:2]
#'
#' # Extract a dendrogram representation of the firrst cluster
#' twitSet[[1]]
#'
create_hierarchy <- function(sets, intersectLimit = 1) {
    sets <- format_sets(sets)
    clusters <- setClustering(sets@p, sets@i, colnames(sets), intersectLimit)
    clusters <- lapply(clusters, dendrapply, function(x) {
        if (is.leaf(x)) {
            leafAttr <- attributes(x)
            x <- as.integer(x)
            attributes(x) <- leafAttr
        }
        x
    })
    res <- list(sets = sets, clusters = clusters)
    class(res) <- 'HierarchicalSet'
    res
}
#' Create a new hierarchy based on the outlying elements
#'
#' This function detects the outlying elements of a HierarchicalSet object and
#' creates a new clustering of the sets only based on these elements. The
#' returned HierarchicalSet object will only contain the outlying elements, thus
#' reducing the universe size. This operation is somewhat similar to principal
#' component analysis, in that the derived clustering is based on the structure
#' not captured by the first clustering, thus modeling the second most dominant
#' feature of the data.
#'
#' @param set A HierarchicalSet object
#'
#' @param intersectLimit The proportion of sets an element must be present in to
#' be considered part of the intersect. Standard intersects require it to be
#' present in all sets (intersectLimit = 1), which is also the default
#'
#' @return An object of class HierarchicalSet, based on the outliying elements
#' of `set`
#'
#' @seealso [outlying_elements()] for extracting outlying element
#' information from a HierarchicalSet object
#'
#' @export
#'
#' @examples
#' data('twitter')
#'
#' twitSet <- create_hierarchy(twitter)
#' twitSetOut <- outlier_hierarchy(twitSet)
#' twitSetOut
#'
outlier_hierarchy <- function(set, intersectLimit = 1) {
    outliers <- outlying_elements(set, FALSE)$outliers
    outliers <- unique(unlist(outliers))
    if (length(outliers) == 0) {
        stop('No outlying elements in set')
    }
    create_hierarchy(sets(set)[outliers, ], intersectLimit)
}
# HELPERS
#' Parse different set formats into ngCMatrix format
#'
#' This function is intended to ensure proper formatting of the supplied sets to
#' the [create_hierarchy()] constructor. Support for other input
#' objects can be created by writting a format_sets method for the class.
#'
#' @param x The sets to be formatted
#'
#' @return A presence/absence matrix with sets as columns and elements as rows,
#' formatted as a ngCMatrix
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' data('twitter')
#' format_sets(as.matrix(twitter))
#'
format_sets <- function(x) UseMethod('format_sets')
#' @export
#'
format_sets.data.frame <- function(x) {
    format_sets(as.matrix(x))
}
#' @export
#'
format_sets.list <- function(x) {
    class <- unique(sapply(x, class))
    if (length(class) != 1) {
        stop('All elements of list "sets" must be of the same class')
    }
    if (!class %in% c('integer', 'factor', 'character', 'numeric')) {
        stop('Invalid "sets" element class')
    }
    if (any(unlist(lapply(x, is.na)))) {
        warning('NA values ignored')
    }
    if (class == 'factor') {
        x <- lapply(x, as.character)
    }
    if (class == 'numeric') {
        warning('numeric coerced to integer')
        x <- lapply(x, as.integer)
    }
    universe <- unique(unlist(x))
    x <- lapply(x, function(set) {
        universe %in% set
    })
    setNames <- names(x)
    sets <- do.call(cbind, sets)
    colnames(x) <- setNames
    rownames(x) <- universe
    format_sets(x)
}
#' @importFrom Matrix Matrix
#' @importFrom methods as
#'
#' @export
#'
format_sets.matrix <- function(x) {
    mode(x) <- 'integer'
    naValues <- is.na(x)
    if (any(naValues)) {
        warning('NA values set to 0')
        x[naValues] <- 0
    }
    highValues <- x > 1
    if (any(highValues)) {
        warning('Values above 1 set to 1')
        x[highValues] <- 1
    }
    if (is.null(colnames(x))) {
        colnames(x) <- paste0('Set ', seq_len(ncol(x)))
    }
    as(Matrix(x, sparse = TRUE), 'nsparseMatrix')
}
#' @importFrom methods as
#'
#' @export
#'
format_sets.Matrix <- function(x) {
    if (is.null(colnames(x))) {
        colnames(x) <- paste0('Set ', seq_len(ncol(x)))
    }
    as(x, 'nsparseMatrix')
}
