#' @include hSet.R
NULL

#' @describeIn create_hierarchy Print method for hierarchicalSet objects
#'
#' @param x A hierarchicalSet object
#'
#' @param ... Currently ignored
#'
#' @export
#'
print.HierarchicalSet <- function(x, ...) {
    cat('A HierarchicalSet object\n\n')
    cat('                 Universe size: ', n_elements(x), '\n', sep = '')
    cat('                Number of sets: ', n_sets(x), '\n', sep = '')
    cat('Number of independent clusters: ', n_clusters(x), '\n', sep = '')
}
#' @describeIn create_hierarchy Extract dendrogram objects from hierarchicalSet objects
#'
#' @param i The index of the dendrogram
#'
#' @export
#'
`[[.HierarchicalSet` <- function(x, i) {
    clusters(x)[[i]]
}
#' @describeIn create_hierarchy Subset hierarchicalSet object by dendrogram (preserving set
#' information and class)
#'
#' @importFrom stats dendrapply
#'
#' @export
#'
`[.HierarchicalSet` <- function(x, i) {
    newClusters <- clusters(x)[i]
    includedSets <- sort(unlist(lapply(newClusters, attr, 'memberSets')))
    newSets <- sets(x)[, includedSets, drop = FALSE]
    newSets <- newSets[rowSums(newSets) != 0, , drop = FALSE]
    newClusters <- lapply(newClusters, function(den) {
        dendrapply(den, function(node) {
            attr(node, 'memberSets') <- match(attr(node, 'memberSets'), includedSets)
            node
        })
    })
    res <- list(sets = newSets, clusters = newClusters)
    class(res) <- 'hierarchicalSet'
    res
}
#' Getters for HierarchicalSet objects
#'
#' These utility functions makes it easy to extract raw information from a
#' HierarchicalSet object.
#'
#' @param x A HierarchicalSet object
#'
#' @return depending on the function. See details
#'
#' @details
#' \code{sets} Returns a ngCMatrix with sets as columns and elements as rows.
#'
#' @export
#'
#' @rdname hs-get
#'
#' @name HierarchicalSet-getters
#'
#' @examples
#' data('twitter')
#'
#' twitSet <- create_hierarchy(twitter)
#'
#' # Get the sets as a presence/absence matrix
#' head(sets(twitSet))
#'
sets <- function(x) UseMethod('sets')
#' @describeIn create_hierarchy Extract the sets as a sparse matrix
#'
#' @export
sets.HierarchicalSet <- function(x) x$sets
#' @rdname hs-get
#'
#' @details
#' \code{clusters} returns a list of dendrograms with the clustering in the
#' HierarchicalSet object
#'
#' @export
#'
#' @examples
#' # Get the clustering of the HierarchicalSet object
#' clusters(twitSet)
#'
clusters <- function(x) UseMethod('clusters')
#' @describeIn create_hierarchy Extract the clusters as a list of dendrograms
#'
#' @export
clusters.HierarchicalSet <- function(x) x$clusters
#' @rdname hs-get
#'
#' @details
#' \code{set_names} returns a character vector with the names of the sets.
#'
#' @export
#'
#' @examples
#' # Get the set names
#' set_names(twitSet)
#'
set_names <- function(x) UseMethod('set_names')
#' @describeIn create_hierarchy Get the names of the sets
#'
#' @export
set_names.HierarchicalSet <- function(x) colnames(sets(x))
#' @rdname hs-get
#'
#' @details
#' \code{element_names} returns a character vector with the names of the
#' elements
#'
#' @export
#'
#' @examples
#' # Get the element names or NULL if they are unnamed
#' element_names(twitSet)
#'
element_names <- function(x) UseMethod('element_names')
#' @describeIn create_hierarchy Get the names of the elements
#'
#' @export
element_names.HierarchicalSet <- function(x) rownames(sets(x))
#' @rdname hs-get
#'
#' @details
#' \code{n_sets} returns the number of sets
#'
#' @export
#'
#' @examples
#' # Get the number of sets
#' n_sets(twitSet)
#'
n_sets <- function(x) UseMethod('n_sets')
#' @describeIn create_hierarchy Get the number of sets
#'
#' @export
n_sets.HierarchicalSet <- function(x) ncol(x$sets)
#' @describeIn create_hierarchy Get the number of sets
#'
#' @export
length.HierarchicalSet <- n_sets.HierarchicalSet
#' @rdname hs-get
#'
#' @details
#' \code{n_elements} returns the number of elements
#'
#' @export
#'
#' @examples
#' # Get the number of elements
#' n_elements(twitSet)
#'
n_elements <- function(x) UseMethod('n_elements')
#' @describeIn create_hierarchy Get the number of elements
#'
#' @export
n_elements.HierarchicalSet <- function(x) nrow(x$sets)
#' @rdname hs-get
#'
#' @details
#' \code{n_clusters} returns the number of independent set families
#'
#' @export
#'
#' @examples
#' # Get the number of independent clusters
#' n_clusters(twitSet)
#'
n_clusters <- function(x) UseMethod('n_clusters')
#' @describeIn create_hierarchy Get the number of clusters
#'
#' @export
n_clusters.HierarchicalSet <- function(x) length(clusters(x))
