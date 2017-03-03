clique.community.opt <- function(graph, k, Nodes.Text = FALSE){

  ###################################
  ### STEP #1: Clique discovery
  ###################################
  
  clq <- cliques(graph, min=k, max=k)

  
  ###################################
  ### STEP #2: Clique-graph creation
  ###################################
  
  #find edges between cliques
  edges <- c()
  for (i in 1:(length(clq)-1)) {
    for (j in (i+1):length(clq)) {
      if ( length(unique(c(clq[[i]], clq[[j]]))) == k+1 ) {
        edges <- c(edges, c(i,j))
      }
    }
  }
  
  #Create an empty graph and then adding edges
  clq.graph <- make_empty_graph(n = length(clq)) %>% add_edges(edges)
  clq.graph <- simplify(clq.graph)
  V(clq.graph)$name <- seq_len(vcount(clq.graph))
  
  
  comps <- decompose.graph(clq.graph)
  
  #Create an empty list and then add the different elements that have been created:
  # "clq" being the clique list; "clq.graph" being the clique graph; "comps" being the 
  # different components of the clique graph and lastly, the list of communities
  
  clq_list <- list()
  clq_list[[1]] <- clq
  clq_list[[2]] <- clq.graph
  clq_list[[3]] <- comps
  
if(Nodes.Text) {
    clq_list[[4]] <-   lapply(comps, function(x) {
      unique(names(unlist(clq[ V(x)$name ])))
    })
  } else {
    clq_list[[4]] <-   lapply(comps, function(x) {
      unique(unlist(clq[ V(x)$name ]))
    })
  }
  
  #Assign names to the four elements created previously, "clq" -> "Cliques"
  # "clq.graph" -> "Clq_Graph", "comps" -> "Components" and the 4th element -> "Communities"
  names(clq_list) <- c("Cliques", "Clq_Graph", "Components", "Communities")
  
  return(clq_list)
  }
