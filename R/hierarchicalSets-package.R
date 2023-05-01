#' Hierarchical analysis and visualization of set data
#'
#' This package provides a framework for investigating large scale set data with
#' the use of hierarchical clustering. While hierarchical clustering has been
#' employed on set data numerous times, by converting the presence/absence
#' matrix to a distance matrix and using [stats::hclust()], this
#' approach completely removes any notion of underlying set structure from the
#' data. hierarchicalSets instead performs a clustering directly using set
#' algebra by continuously merging sets with the largest intersection (for ties
#' the one with the smallest union is chosen). This structure can then be used
#' in a variety of ways to visualize the relationships between sets. E.g. the
#' intersectionStack plot is a scalable pendant to Venn diagrams (showing the
#' same information but using a different visual mapping).
#'
#' @seealso [create_hierarchy()] For constructing HierarchicalSet
#' object and [plot.HierarchicalSet()] for visualization apporaches.
#'
#' @docType package
#'
#' @name hierarchicalSets
#'
#' @useDynLib hierarchicalSets
#' @importFrom Rcpp sourceCpp
#' @import ggplot2
#'
NULL

globalVariables(c(
    "x",
    "y",
    "group"
))
