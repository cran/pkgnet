## ----setupVignette, include = FALSE--------------------------------------

## NOTE:    Vignettes are built within their own environment. 
##          Therefore, changing the library paths (libpaths) here will not change the global libpaths.

# Get test library path
testLibPath <- tempdir()

# Get current library paths
origLibPaths <- .libPaths()

# Create new library paths for TESTING
.libPaths(new = c(testLibPath, origLibPaths))

# Create packages within new library
pkgnet:::.BuildTestLib(targetLibPath = testLibPath)

knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  fig.align = 'center',
  out.width='100%'
)

## ----whatIsDepGraph, echo=FALSE, message=FALSE, fig.height=3, results='markup', fig.cap = "Example Dependency Graph"----
nodes <- data.frame(
  id = 1:4
  , label = LETTERS[1:4]
  , x = c(1,3,2,4)
  , y = c(1,1,2,3)
  , level = c(1,1,2,3)
  )

edges <- data.frame(
  from = c(3,3,4,4)
  , to = c(1,2,3,2)
  )

g <- visNetwork::visNetwork(nodes = nodes
                       , edges = edges
                       , width = "100%"
                       )
g <- visNetwork::visNodes(graph = g
                          , shape = "circle"
                          , font = list(size = 25
                                        , bold = TRUE
                                        , align = 'center'
                                        )
                       )
g <- visNetwork::visEdges(graph = g
                          , arrows = "to"
                       )
g <- visNetwork::visHierarchicalLayout(graph = g
                      , direction = "DU"
                      , sortMethod = "directed"
                      )
g <- visNetwork::visInteraction(graph = g
                                , dragNodes = TRUE
                                , dragView = TRUE
                                , zoomView = FALSE)
g

## ----pkgnetRunFirst, eval=FALSE------------------------------------------
#  library(pkgnet)
#  report1 <- CreatePackageReport(pkg_name = "baseballstats")

## ----pkgnetRunFAKE, eval=FALSE-------------------------------------------
#  library(pkgnet)
#  report2 <- CreatePackageReport(
#    pkg_name = "baseballstats"
#    , pkg_path = <path to the repo>
#  )

## ----demoVis1, fig.height=3, message=FALSE, warning=FALSE, echo=FALSE----
pkgnet:::silence_logger()
funcReporter1 <- pkgnet::FunctionReporter$new()
funcReporter1$set_package('baseballstats')
funcReporter1$layout_type <- "layout_as_tree"
g <- visNetwork::visHierarchicalLayout(
    graph = funcReporter1$graph_viz
    , direction = "UD"
    , sortMethod = "directed"
    , edgeMinimization = FALSE
)
g <- visNetwork::visInteraction(graph = g
                                , dragNodes = TRUE
                                , dragView = TRUE
                                , zoomView = FALSE)
g

## ----demoVis2, fig.height=3, message=FALSE, warning=FALSE, echo=FALSE----
pkgnet:::silence_logger()
funcReporter2 <- pkgnet::FunctionReporter$new()
funcReporter2$layout_type <- "layout_as_tree"
funcReporter2$set_package(
    pkg_name = "baseballstats"
    , pkg_path = system.file('baseballstats',package="pkgnet")
)
funcReporter2$calculate_default_measures()
g <- visNetwork::visHierarchicalLayout(
    graph = funcReporter2$graph_viz
    , direction = "UD"
    , sortMethod = "directed"
    , edgeMinimization = FALSE
)
g <- visNetwork::visInteraction(graph = g
                                , dragNodes = TRUE
                                , dragView = TRUE
                                , zoomView = FALSE)
g

## ----mockPackageReport, message=FALSE, warning=FALSE, results='hide', echo=FALSE----
# We initialized just the reporters because we didn't want to actually generate the full html report. So we'll put funcReporter2 into a list to mock the interface for the example
report2 <- list(FunctionReporter = funcReporter2)

## ----nodes---------------------------------------------------------------
dim(report2$FunctionReporter$nodes)
names(report2$FunctionReporter$nodes)

## ----networkMeasures-----------------------------------------------------
report2$FunctionReporter$network_measures

## ----networkObjAddtlMeasures---------------------------------------------
report2$FunctionReporter$pkg_graph$node_measures(c('hubScore', 'authorityScore'))

## ----networkObj----------------------------------------------------------
report2$FunctionReporter$pkg_graph$igraph

## ----fakeDetail1, eval=FALSE---------------------------------------------
#  # Run pkgnet
#  library(pkgnet)
#  report2 <- CreatePackageReport(
#      pkg_name = "lubridate"
#      , pkg_path = "~/pkgnet_example/lubridate"
#  )
#  
#  # Extract Nodes Table
#  funcNodes <- report2$FunctionReporter$nodes
#  
#  # List Coverage For Most Depended-on Functions
#  mostRef <- funcNodes[order(numRecursiveRevDeps, decreasing = TRUE),
#                       .(node, numRecursiveRevDeps, coverageRatio, totalLines)
#                       ][1:10]

## ----fakeDetail1Results, eval=FALSE--------------------------------------
#  #>             node numRecursiveRevDeps coverageRatio totalLines
#  #>  1:        month                  81             1          1
#  #>  2:           tz                  79             1          1
#  #>  3: reclass_date                  68             1          1
#  #>  4:         date                  67             1          1
#  #>  5:      is.Date                  60             1          1
#  #>  6:    is.POSIXt                  57             1          1
#  #>  7:         wday                  56             1          1
#  #>  8:   is.POSIXct                  55             1          1
#  #>  9:  .deprecated                  55             0         10
#  #> 10:      as_date                  52             1          1

## ----fakeDetail2, eval=FALSE---------------------------------------------
#  # Get igraph object
#  funcGraph <- report2$FunctionReporter$pkg_graph
#  funcNames <- igraph::vertex_attr(funcGraph, name = "name")
#  
#  # Jaccard Similarity
#  sim <- igraph::similarity(graph = funcGraph
#                            , mode = "out"
#                            , method = "jaccard")
#  diag(sim) <- 0
#  sim[sim < 1] <- 0
#  
#  simGraph <- igraph::graph_from_adjacency_matrix(adjmatrix = sim, mode = "undirected")
#  
#  # Find groups with same out-neighbors (similarity == 1)
#  sameDeps <- igraph::max_cliques(graph = simGraph
#                                  , min = 2
#                                  )
#  
#  # Write results
#  for (i in seq_along(sameDeps)) {
#      cat(paste0("Group ", i, ": "))
#      cat(paste(funcNames[as.numeric(sameDeps[[i]])], collapse = ", "))
#      cat("\n")
#  }

## ----resultFromFake, echo=FALSE, results='markup'------------------------
cat("Group 1: divisible_period, make_date
Group 2: parse_date_time2, fast_strptime
Group 3: .deprecated_fun, .deprecated_arg
Group 4: stamp_date, stamp_time
Group 5: epiweek, isoweek
Group 6: ms, hm
Group 7: quarter, semester
Group 8: am, .roll_hms
Group 9: modulo_interval_by_duration, modulo_interval_by_period
Group 10: .difftime_from_pieces, .duration_from_units
Group 11: divide_period_by_period, xtfrm.Period
Group 12: int_diff, %--%
Group 13: isoyear, epiyear
Group 14: nanoseconds, microseconds, picoseconds, milliseconds
Group 15: period_to_seconds, check_period, multiply_period_by_number, format.Period, divide_period_by_number, add_period_to_period
Group 16: myd, dmy, yq, ymd, dym, mdy, ydm
Group 17: hours, weeks, minutes, years, days, months.numeric, seconds, seconds_to_period
Group 18: C_force_tz, hour.default, mday.default, c.POSIXct, .mklt, yday.default, year.default, minute.default, second.default
Group 19: ehours, emilliseconds, eyears, eseconds, epicoseconds, enanoseconds, eminutes, olson_time_zones, edays, emicroseconds, eweeks
Group 20: dmy_h, ydm_hms, ymd_hms, dmy_hm, ymd_h, ydm_hm, ydm_h, dmy_hms, ymd_hm, mdy_hms, mdy_hm, mdy_h"
)

## ----removeDemoPackage, include=FALSE------------------------------------
utils::remove.packages(
    pkgs = c('baseballstats', 'sartre', 'pkgnet')
    , lib = testLibPath
)

# Just in case 
.libPaths(new = c(origLibPaths))
unlink(testLibPath)

