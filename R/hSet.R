# TODO: Subclass with metadata?
#       t() function that transposes the presence absence matrix and creates a
#         hierarchy for the elements

#' Create and store hierarchcical sets
#'
#' hierarchicalSet object can be created using the \code{hSet()} constructor.
#' The resulting object will contain both the underlying sets as well as the
#' resulting clustering.
#'
#' @param sets The sets to analyse. Can either be a matrix/data.frame giving the
#' presence/absence pattern of elements, with elements as rows and sets as
#' columns, or a list of vectors giving the elements of the individual sets.
#'
#' @param intersectLimit The proportion of sets an element must be present in to
#' be considered part of the intersect. Standard intersects require it to be
#' present in all sets (intersectLimit = 1), which is also the default
#'
#' @return An object of class hierarchicalSet
#'
#' @details
#' The hierarchicalSet class contains both the clustering and the original sets.
#' The former is stored in a list of dendrogram objects in and the latter as a
#' presence/absence matrix. Both are retrivable using \code{$clusters} and
#' \code{$sets} respectively. Furthermore individual dendrograms can be
#' extracted directly using the \code{[[} operator. If multiple independent
#' clusters exists the object can be subsetted using the \code{[} operator.
#'
#' For plotting functionality see the separate plot documentation for
#' \code{\link{plot.hierarchicalSet}}.
#'
#' @export
#'
hSet <- function(sets, intersectLimit = 1) {
    sets <- formatSets(sets)
    clusters <- setClustering(sets@p, sets@i, colnames(sets), intersectLimit)
    res <- list(sets = sets, clusters = clusters)
    class(res) <- 'hierarchicalSet'
    res
}

# HELPERS
#' Format and check sets
#'
#' This function is intended to ensure proper formatting of the supplied sets to
#' the hSet constructor. The output will be a matrix of integers containing only
#' 0's and 1's, with named columns. If unable to parse the input an error will
#' be thrown. Acceptable input are matrices, data.frames and list of vectors.
#'
#' @param sets The sets to be formatted
#'
#' @return A presence/absence matrix with sets as columns and elements as rows
#'
#' @importFrom Matrix Matrix
#' @noRd
#'
formatSets <- function(sets) {
    if (inherits(sets, 'data.frame')) {
        sets <- as.matrix(sets)
    }
    if (inherits(sets, 'list')) {
        class <- unique(sapply(sets, class))
        if (length(class) != 1) {
            stop('All elements of list "sets" must be of the same class')
        }
        if (!class %in% c('integer', 'factor', 'character', 'numeric')) {
            stop('Invalid "sets" element class')
        }
        if (any(unlist(lapply(sets, is.na)))) {
            warning('NA values ignored')
        }
        if (class == 'factor') {
            sets <- lapply(sets, as.character)
        }
        if (class == 'numeric') {
            warning('numeric coerced to integer')
            sets <- lapply(sets, as.integer)
        }
        universe <- unique(unlist(sets))
        sets <- lapply(sets, function(x) {
            universe %in% x
        })
        setNames <- names(sets)
        sets <- do.call(cbind, sets)
        colnames(sets) <- setNames
        rownames(sets) <- universe
    }
    if (inherits(sets, 'matrix')) {
        mode(sets) <- 'integer'
        naValues <- is.na(sets)
        if (any(naValues)) {
            warning('NA values set to 0')
            sets[naValues] <- 0
        }
        highValues <- sets > 1
        if (any(highValues)) {
            warning('Values above 1 set to 1')
            sets[highValues] <- 1
        }
        if (is.null(colnames(sets))) {
            colnames(sets) <- paste('Set ', seq_len(ncol(sets)))
        }
        sets <- as(Matrix(sets, sparse = TRUE), 'ngCMatrix')
    } else if (inherits(sets, 'Matrix')) {
        sets <- as(sets, 'ngCMatrix')
    } else {
        stop('sets must be in either matrix, data.frame or list format')
    }
    sets
}
