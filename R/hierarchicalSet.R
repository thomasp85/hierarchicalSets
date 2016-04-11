#' @include hSet.R
NULL

#' @describeIn create_hierarchy Print method for HierarchicalSet objects
#'
#' @param x A HierarchicalSet object
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
#' @describeIn create_hierarchy Extract dendrogram objects from HierarchicalSet objects
#'
#' @param i The index of the dendrogram
#'
#' @export
#'
`[[.HierarchicalSet` <- function(x, i) {
    clusters(x)[[i]]
}
#' @describeIn create_hierarchy Subset HierarchicalSet object by dendrogram (preserving set
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
    class(res) <- 'HierarchicalSet'
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
#' @rdname hs-get
#'
#' @details
#' \code{cluster_sizes} returns the number of sets in each independent set family
#'
#' @export
#'
#' @examples
#' # Get the size of each independent clusters
#' cluster_sizes(twitSet)
#'
cluster_sizes <- function(x) UseMethod('cluster_sizes')
#' @describeIn create_hierarchy Get the size of each clusters
#'
#' @export
cluster_sizes.HierarchicalSet <- function(x) {
    unlist(lapply(clusters(x), attr, 'members'))
}
#' @rdname hs-get
#'
#' @details
#' \code{cluster_members} returns the members of each independent set family
#'
#' @export
#'
#' @examples
#' # Get the members of each independent clusters
#' cluster_members(twitSet)
#'
cluster_members <- function(x) UseMethod('cluster_members')
#' @describeIn create_hierarchy Get the members of each clusters
#'
#' @export
cluster_members.HierarchicalSet <- function(x) {
    lapply(clusters(x), attr, 'memberSets')
}
#' @rdname hs-get
#'
#' @details
#' \code{set_membership} returns the cluster each set is member of
#'
#' @export
#'
#' @examples
#' # Get the membership of each set
#' set_membership(twitSet)
#'
set_membership <- function(x) UseMethod('set_membership')
#' @describeIn create_hierarchy Get the membership of each set
#'
#' @export
set_membership.HierarchicalSet <- function(x) {
    members <- cluster_members(x)
    membership <- rep(seq_along(members), lengths(members))
    membership[order(unlist(members))]
}
