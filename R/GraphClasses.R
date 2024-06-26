#' Base class for Graphs
#' @keywords internal
#' 
#' @description 
#' pkgnet uses R6 classes to define and encapsulate the graph
#' models for representing package networks. These classes implement
#' different types of graphs and functionality to calculate their respective
#' graph theory measures. The base class \code{AbstractGraph} defines the
#' standard interfaces and functionality.
#'
#' Currently the only implemented type of graph is \link{DirectedGraph}.
#' 
#' @concept Graph Classes
#' @keywords internal
#' @importFrom R6 R6Class
#' @importFrom igraph graph.edgelist make_empty_graph vertex
#' @importFrom data.table data.table
#' @importFrom assertthat assert_that
AbstractGraph <- R6::R6Class(
    classname = "AbstractGraph"
    , public = list(

        #' @description Instantiate new object of the class.
        #' @param nodes a data.table containing nodes
        #' @param edges a data.table containing edges
        #' @return Self, invisibly.
        initialize = function(nodes, edges) {

            # Input validation
            assertthat::assert_that(
                data.table::is.data.table(nodes)
                , 'node' %in% names(nodes)
                , data.table::is.data.table(edges)
                , all(c('SOURCE', 'TARGET') %in% names(edges))
            )

            # Store pointers to node and edge data.tables
            private$protected$nodes <- nodes
            private$protected$edges <- edges

            return(invisible(self))
        }
        #' @description 
        #' Return specified node-level measures, calculating if necessary.
        #' See \emph{Node Measures} section in \link{DirectedGraphMeasures} for details about each measure.
        #' @param measures character vector of measure names. 
        #' Default NULL will return those that are already calculated.
        #' @return a data.table with specified node meaures as columns
        , node_measures = function(measures = NULL){

            # If not specifying, return node table
            if (is.null(measures)) {
                return(self$nodes)
            }

            assertthat::assert_that(is.character(measures))

            for (m in measures) {
                # Input validation
                assertthat::assert_that(
                    all(m %in% self$available_node_measures)
                    , msg = sprintf('%s not in $available_node_measures()', m)
                )

                # If not already calculated it, calculate and add to node DT
                if (!m %in% names(self$nodes)) {
                    log_info(sprintf("Calculating %s...", m))
                    result <- private$node_measure_functions[[m]](self)
                    resultDT <- data.table::data.table(
                        node_name = names(result)
                        , result = result
                    )
                    setkeyv(resultDT, 'node_name')
                    self$nodes[, eval(m) := resultDT[node, result]]
                }
            }

            return(self$nodes[, .SD, .SDcols = c('node', measures)])
        }

        #' @description Return specified graph-level measures, calculating if necessary. 
        #' See \emph{Graph Measures} section in \link{DirectedGraphMeasures} for details about each measure.
        #' @param measures character vector of measure names. 
        #' Default NULL will return those that are already calculated.
        #' @return list with specified graph measures.
        , graph_measures = function(measures = NULL){

            # If not specifying, return full list
            if (is.null(measures)) {
                return(private$protected$graph_measures)
            }

            assertthat::assert_that(is.character(measures))

            for (m in measures) {
                # Input validation
                assertthat::assert_that(
                    m %in% self$available_graph_measures
                    , msg = sprintf('%s not in $available_graph_measures()', m)
                )

                # If not already calculated, calculate
                if (!m %in% names(private$protected$graph_measures)) {
                    log_info(sprintf("Calculating %s", m))
                    result <- private$graph_measure_functions[[m]](self)
                    private$protected$graph_measures[[m]] <- result
                }
            }
            return(private$protected$graph_measures[measures])
        }
        #' @description print igraph object
        #' @return Self, invisibly.
        , print = function(){
            print(self$igraph)
            invisible(self)
        }

    ) # /public

    , active = list(
        #' @field nodes node data.table, read-only.
        nodes = function(){return(private$protected$nodes)}

        #' @field edges edge data.table, read-only.
        , edges = function(){return(private$protected$edges)}

        #' @field igraph igraph object, read-only.
        , igraph = function(){
            if (is.null(private$protected$igraph)) {
                private$initialize_igraph()
            }
            return(private$protected$igraph)
        }

        #' @field available_node_measures character vector of all supported node measures. 
        #' See \emph{Node Measures} section in \link{DirectedGraphMeasures} for details about each measure.
        , available_node_measures = function(){
            return(names(private$node_measure_functions))
        }

        #' @field available_graph_measures character vector of all supported graph measures. 
        #' See \emph{Graph Measures} section in \link{DirectedGraphMeasures} for details about each measure. Read-only.
        , available_graph_measures = function(){
            return(names(private$graph_measure_functions))
        }

        #' @field default_node_measures character vector of default node measures. 
        #' See \emph{Node Measures} section in \link{DirectedGraphMeasures} for details about each measure.
        , default_node_measures = function(){
            log_fatal('Default node measures not implemented.')
        }

        #' @field default_graph_measures character vector of default graph measures. 
        #' See \emph{Graph Measures} section in \link{DirectedGraphMeasures} for details about each measure. Read-only.
        , default_graph_measures = function(){
            log_fatal('Default graph measures not implemented.')
        }
    ) # /active

    , private = list(
        protected = list(
            nodes = NULL
            , edges = NULL
            , igraph = NULL
            , graph_measures = list()
        )

        , initialize_igraph = function(directed){

            log_info("Constructing igraph object...")

            # Connected graph
            if (nrow(self$edges) > 0) {
                # A graph with edges
                connectedGraph <- igraph::graph.edgelist(
                    as.matrix(self$edges[,list(SOURCE,TARGET)])
                    , directed = directed
                )
            } else {
                connectedGraph <- igraph::make_empty_graph(directed = directed)
            }

            # Unconnected graph
            orphanNodes <- base::setdiff(
                self$nodes[, node]
                , unique(c(self$edges[, SOURCE], self$edges[, TARGET]))
            )
            unconnectedGraph <- igraph::make_empty_graph(directed = directed) + igraph::vertex(orphanNodes)

            # Complete graph
            completeGraph <- connectedGraph + unconnectedGraph

            # Store in protected cache
            private$protected$igraph <- completeGraph

            log_info("...done constructing igraph object.")

            return(invisible(NULL))
        } # /initialize_igraph

        # Functions for node measures
        # All functions should return a named vector of node measure values
        , node_measure_functions = list()

        # Functions for graph-level measures
        # All functions should return numeric of length 1
        , graph_measure_functions = list()

    )  # /private
)

#' Directed Graph Network Model
#' @description 
#' R6 class defining a directed graph model for representing a
#' network, including methods to calculate various measures from graph
#' theory. The \link[igraph:igraph-package]{igraph} package is used as a
#' backend for calculations.
#'
#' This class isn't intended to be initialized directly; instead,
#' \link[=AbstractGraphReporter]{network reporter objects} will initialize it as
#' its \code{pkg_graph} field. If you have a network reporter named
#' \code{reporter}, then you access this object's public
#' interface through \code{pkg_graph}---for example,
#'
#' \preformatted{reporter$pkg_graph$node_measures('hubScore')}
#'
#' @concept Graph Classes
#' @importFrom R6 R6Class
#' @importFrom igraph degree closeness betweenness
#' @importFrom igraph page_rank hub_score authority_score
#' @importFrom igraph neighborhood.size vcount V
#' @importFrom igraph centralize centr_degree_tmax
#' @importFrom igraph centr_clo_tmax centr_betw_tmax
#' @seealso DirectedGraphMeasures
DirectedGraph <- R6::R6Class(
    classname = "DirectedGraph"
    , inherit = AbstractGraph
    , public = list()
    , active = list(

        #' @field default_node_measures character vector of default node measures. 
        #' See \emph{Node Measures} section in \link{DirectedGraphMeasures} for details about each measure. Read-only.
        default_node_measures = function() {
            return(c(
                "outDegree"
                , "inDegree"
                , "numRecursiveDeps"
                , "numRecursiveRevDeps"
                , "betweenness"
                , "pageRank"
            ))
        }
        
        #' @field default_graph_measures character vector of default graph measures. 
        #' See \emph{Graph Measures} section in \link{DirectedGraphMeasures} for details about each measure. Read-only.
        , default_graph_measures = function() {
            return(c(
                "graphOutDegree"
                , "graphInDegree"
                , "graphBetweenness"
            ))
        }
    )
    , private = list(

        # Initialize igraph object
        initialize_igraph = function() {
            super$initialize_igraph(directed = TRUE)
        }

        # Functions for node measures
        # All functions should return a named vector of node measure values
        , node_measure_functions = list(

            # Out-Degree
            outDegree = function(self){
                result <- igraph::degree(
                    graph = self$igraph
                    , mode = "out"
                    , loops = TRUE
                )
                mode(result) <- "integer"
                return(result)
            }

            # In-Degree
            , inDegree = function(self){
                result <- igraph::degree(
                    graph = self$igraph
                    , mode = "in"
                    , loops = TRUE
                )
                mode(result) <- "integer"
                return(result)
            }

            # Out-Closeness
            # Closeness doesn't really work for directed graphs that are not
            # strongly connected.
            # igraph calculates a thing anyways and gives a warning
            # Typically given as normalized values
            , outCloseness = function(self){
                suppressWarnings(igraph::closeness(
                    graph = self$igraph
                    , mode = "out"
                    , normalized = TRUE
                ))
            }

            # In-Closeness
            # Closeness doesn't really work for directed graphs that are not
            # strongly connected.
            # igraph calculates a thing anyways and gives a warning
            # Typically given as normalized values
            , inCloseness = function(self){
                suppressWarnings(igraph::closeness(
                    graph = self$igraph
                    , mode = "out"
                    , normalized = TRUE
                ))
            }

            # Number of Recursive Dependencies
            , numRecursiveDeps = function(self){
                # Calculate using out-neighborhood size with order of longest
                # possible path
                result <- igraph::neighborhood.size(
                    graph = self$igraph
                    , order = igraph::vcount(self$igraph)
                    , mode = "out"
                )
                # Subtract 1 so we don't include the root node itself
                result <- result - 1
                names(result) <- igraph::V(self$igraph)$name
                mode(result) <- "integer"
                return(result)
            }

            # Number of Recursive Reverse Dependencies
            , numRecursiveRevDeps = function(self){
                # Calculate using in-neighborhood size with order of longest
                # possible path
                result <- igraph::neighborhood.size(
                    graph = self$igraph
                    , order = igraph::vcount(self$igraph)
                    , mode = "in"
                )
                # Subtract 1 so we don't include the root node itself
                result <- result - 1
                names(result) <- igraph::V(self$igraph)$name
                mode(result) <- "integer"
                return(result)
            }

            # Betweenness
            , betweenness = function(self){
                igraph::betweenness(
                    graph = self$igraph
                    , directed = TRUE
                )
            }

            # Page Rank
            , pageRank = function(self){
                igraph::page_rank(
                    graph = self$igraph
                    , directed = TRUE
                )$vector
            }

            # Hub Score
            , hubScore = function(self){
                igraph::hub_score(
                    graph = self$igraph
                    , scale = TRUE
                )$vector
            }

            # Authority Score
            , authorityScore = function(self){
                igraph::authority_score(
                    graph = self$igraph
                    , scale = TRUE
                )$vector
            }

        ) #/node_measure_functions

        # Functions for graph-level measures
        # All functions should return numeric of length 1
        , graph_measure_functions = list(

            graphOutDegree = function(self){
                measure <- 'outDegree'
                igraph::centralize(
                    scores = self$node_measures(measure)[, get(measure)]
                    , theoretical.max = igraph::centr_degree_tmax(
                        graph = self$igraph
                        , mode = "out"
                        , loops = TRUE
                    )
                    , normalized = TRUE
                )
            }

            , graphInDegree = function(self){
                measure <- 'inDegree'
                igraph::centralize(
                    scores = self$node_measures(measure)[, get(measure)]
                    , theoretical.max = igraph::centr_degree_tmax(
                        graph = self$igraph
                        , mode = "in"
                        , loops = TRUE
                    )
                    , normalized = TRUE
                )
            }

            , graphOutCloseness = function(self){
                measure <- 'outCloseness'
                igraph::centralize(
                    scores = self$node_measures(measure)[, get(measure)]
                    , theoretical.max = igraph::centr_clo_tmax(
                        graph = self$igraph
                        , mode = "out")
                    , normalized = TRUE
                )
            }

            , graphInCloseness = function(self){
                measure <- 'inCloseness'
                igraph::centralize(
                    scores = self$node_measures(measure)[, get(measure)]
                    , theoretical.max = igraph::centr_clo_tmax(
                        graph = self$igraph
                        , mode = "in")
                    , normalized = TRUE
                )
            }

            , graphBetweenness = function(self){
                measure <- 'betweenness'
                igraph::centralize(
                    scores = self$node_measures(measure)[, get(measure)]
                    , theoretical.max = igraph::centr_betw_tmax(
                        graph = self$igraph
                        , directed = TRUE)
                    , normalized = TRUE
                )
            }

        ) # /graph_measures_functions
    ) # /private
)

#' @title Measures for Directed Graph Class
#' @name DirectedGraphMeasures
#' @concept Graph Classes
#' @keywords internal
#' @description Descriptions for all available node and graph measures for
#'    networks modeled by \link{DirectedGraph}.
#' @section Node Measures:
#' \describe{
#'     \item{\bold{\code{outDegree}}}{outdegree, the number of outward edges (tail ends).
#'     Calculated by \code{\link[igraph:degree]{igraph::degree}}.
#'     [\href{https://en.wikipedia.org/wiki/Directed_graph#Indegree_and_outdegree}{Wikipedia}]}
#'     \item{\bold{\code{inDegree}}}{indegree, number of inward edges (head ends).
#'     Calculated by \code{\link[igraph:degree]{igraph::degree}}.
#'     [\href{https://en.wikipedia.org/wiki/Directed_graph#Indegree_and_outdegree}{Wikipedia}]}
#'     \item{\bold{\code{outCloseness}}}{closeness centrality (out), a measure of
#'     path lengths to other nodes along edge directions.
#'     Calculated by \code{\link[igraph:closeness]{igraph::closeness}}.
#'     [\href{https://en.wikipedia.org/wiki/Closeness_centrality}{Wikipedia}]}
#'     \item{\bold{\code{inCloseness}}}{closeness centrality (in), a measure of
#'     path lengths to other nodes in reverse of edge directions.
#'     Calculated by \code{\link[igraph:closeness]{igraph::closeness}}.
#'     [\href{https://en.wikipedia.org/wiki/Closeness_centrality}{Wikipedia}]}
#'     \item{\bold{\code{numRecursiveDeps}}}{number recursive dependencies, i.e., count of all nodes reachable by following edges
#'     out from this node.
#'     Calculated by \code{\link[igraph:neighborhood.size]{igraph::neighborhood.size}}.
#'     [\href{https://en.wikipedia.org/wiki/Rooted_graph}{Wikipedia}]}
#'     \item{\bold{\code{numRecursiveRevDeps}}}{number of recursive reverse dependencies (dependents), i.e., count all nodes reachable by following edges
#'     into this node in reverse direction.
#'     Calculated by \code{\link[igraph:neighborhood.size]{igraph::neighborhood.size}}.
#'     [\href{https://en.wikipedia.org/wiki/Rooted_graph}{Wikipedia}]}
#'     \item{\bold{\code{betweenness}}}{betweenness centrality, a measure of
#'     the number of shortest paths in graph passing through this node
#'     Calculated by \code{\link[igraph:betweenness]{igraph::betweenness}}.
#'     [\href{https://en.wikipedia.org/wiki/Betweenness_centrality}{Wikipedia}]}
#'     \item{\bold{\code{pageRank}}}{Google PageRank.
#'     Calculated by \code{\link[igraph:page_rank]{igraph::page_rank}}.
#'     [\href{https://en.wikipedia.org/wiki/PageRank}{Wikipedia}]}
#'     \item{\bold{\code{hubScore}}}{hub score from Hyperlink-Induced Topic
#'     Search (HITS) algorithm.
#'     Calculated by \code{\link[igraph:hub_score]{igraph::hub_score}}.
#'     [\href{https://en.wikipedia.org/wiki/HITS_algorithm}{Wikipedia}]}
#'     \item{\bold{\code{authorityScore}}}{authority score from
#'     Hyperlink-Induced Topic Search (HITS) algorithm.
#'     Calculated by \code{\link[igraph:authority_score]{igraph::authority_score}}.
#'     [\href{https://en.wikipedia.org/wiki/HITS_algorithm}{Wikipedia}]}
#' }
#' @section Graph Measures:
#' \describe{
#'     \item{\bold{\code{graphOutDegree}}}{graph freeman centralization for
#'     outdegree. A measure of the most central node by outdegree in relation to
#'     all other nodes.
#'     Calculated by \code{\link[igraph:centralize]{igraph::centralize}}.
#'     [\href{https://en.wikipedia.org/wiki/Centrality#Freeman_centralization}{Wikipedia}]}
#'     \item{\bold{\code{graphInDegree}}}{graph Freeman centralization for
#'     indegree. A measure of the most central node by indegree in relation to
#'     all other nodes.
#'     Calculated by \code{\link[igraph:centralize]{igraph::centralize}}.
#'     [\href{https://en.wikipedia.org/wiki/Centrality#Freeman_centralization}{Wikipedia}]}
#'     \item{\bold{\code{graphOutClosness}}}{graph Freeman centralization for
#'     out-closeness. A measure of the most central node by out-closeness in relation to
#'     all other nodes.
#'     Calculated by \code{\link[igraph:centralize]{igraph::centralize}}.
#'     [\href{https://en.wikipedia.org/wiki/Centrality#Freeman_centralization}{Wikipedia}]}
#'     \item{\bold{\code{graphInCloseness}}}{graph Freeman centralization for
#'     outdegree. A measure of the most central node by outdegree in relation to
#'     all other nodes.
#'     Calculated by \code{\link[igraph:centralize]{igraph::centralize}}.
#'     [\href{https://en.wikipedia.org/wiki/Centrality#Freeman_centralization}{Wikipedia}]}
#'     \item{\bold{\code{graphBetweennness}}}{graph Freeman centralization for
#'     betweenness A measure of the most central node by betweenness in relation to
#'     all other nodes.
#'     Calculated by \code{\link[igraph:centralize]{igraph::centralize}}.
#'     [\href{https://en.wikipedia.org/wiki/Centrality#Freeman_centralization}{Wikipedia}]}
#' }
NULL
