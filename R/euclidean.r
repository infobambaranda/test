#' Euclidean
#'
#' @param a A numeric scalar
#' @param b A numeric scalar
#'
#' @return Return the greatest common divisor of the parameters a and b
#' @export
#'
#' @examples euclidean(123612, 13892347912)
#' euclidean(13892347912,123612)
#' euclidean(100, 1000)
euclidean <- function(a,b){
  # while loop runs b != 0
  while (b != 0){
    # value of b is assigned to a temp variable
    temp <- b
    #remainder of a and b assigned to b
    b <- a %% b
    #temp value is assigned to a.if a is smaller in the first step that
    #will be assigned to b and val of b is assigned to a
    a <- temp
  }
  return(a)
}

#' dijkstra
#'
#'A version of Dijkstra's algorithm that measures the shortest distance from
#' one vector to another on a given graph. For more information about
#' Dijkstra's algorithm, please read the following article:
#' <https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm>
#'
#' @param graph A data frame with three columns: v1 (the starting vector),
#' v2 (the ending vector), and w (the weight of the edge between both vectors)
#' @param init_node The starting vector from where we will start measuring
#' on the graph
#'
#' @return Returns a vector with the shortest distances in the graph to
#' every other vector from the initial vector.
#' @export
#'
#' @examples v1 <-  c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6)
#' v2 <-  c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5)
#' w <-  c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)
#' example_graph <- data.frame(v1, v2, w)
#'
#' dijkstra(example_graph, 1)
#' dijkstra(example_graph, 3)
#'
dijkstra <- function(graph, init_node){

  # Before we begin building our algorithm, we must check if we have proper
  # parameters:
  if (!is.numeric(init_node)){
    stop("Error: init_node is not numeric")
  }

  if (!is.data.frame(graph)){
    stop("Error: graph is not a data frame")
  }

  if (ncol(graph) != 3){
    stop("Error: graph must contain only 3 columns")
  }

  stopifnot(is.numeric(graph[[1]]) &&
              is.numeric(graph[[2]]) &&
              is.numeric(graph[[3]]))

  # First, we must create a vector containing the unique nodes in the graph
  # and ensure that init_node is in said vector:
  vector_list <- unique(graph[[1]])
  if(init_node %in% vector_list == FALSE){
    stop("Error: init_node not in vector list")
  }

  # Next, we create a matrix representation of the graph where rows represent
  # the starting node, columns represent the destination node, and each entry
  # is the weight of that edge. Entries without an edge are marked with NA:
  na_vector <- rep(NA, (length(vector_list) * length(vector_list)))
  graph_matrix <- matrix(na_vector, nrow = length(vector_list))
  for (i in 1:nrow(graph)){
    graph_matrix[graph[,1][i], graph[,2][i]] = graph[,3][i]
  }

  # Then, we define some variables that will help us get to our output:
  # - distance_vector is our output, the vector of smallest distances. The
  #   entry for init_mode is set to zero.
  # - visit_list helps us keep track of what nodes have been visited and
  #   lets the loop know when to break
  # - neighbor_vec helps tell the loop what node should be visited next
  # - node is the current node being visited, initially set to init_mode
  # - n is an iterative variable for neighbor_vec, it helps ensure that we
  #   visit the closest neighbors first.
  distance_vector <- rep(Inf, length(vector_list))
  distance_vector[init_node] = 0
  visit_list <- rep(FALSE, length(vector_list))
  neighbor_vec = c()
  node <- init_node
  n = 0

  # Finally, we use a nested loop to implement our algorithm. The outer loop
  # keeps track if any nodes still need to be visited, while the inner loop
  # uses info from distance_vector and graph_matrix to calculate the shortest
  # paths and place them in distance_vector:
  while (FALSE %in% visit_list){

    for (j in 1:length(vector_list)){

      if (!is.na(graph_matrix[node, j])){
        distance = graph_matrix[node, j] + distance_vector[node]

        if (visit_list[j] == FALSE){
          neighbor_vec = append(neighbor_vec, j)
        }

        if (distance < distance_vector[j]){
          distance_vector[j] = distance
        }
      } else {
        distance_vector[j] = distance_vector[j]
      }
    }

    # Once we are done with each visit (iteration of the for loop), we mark
    # the current node as visited and make the next node listed in
    # neighbor_vec the new current node
    visit_list[node] = TRUE
    n = n + 1
    node = neighbor_vec[n]
  }

  # Now that the algorithm has been completed, we return distance_vector
  # as our output:
  return(distance_vector)
}
