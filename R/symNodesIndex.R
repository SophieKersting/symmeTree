#' Symmetry Nodes Index of a rooted binary tree
#'
#' \code{symNodesIndex} - This function calculates the symmetry nodes index
#' value (i.e. the number of interior nodes that are not symmetry nodes) for
#' rooted binary trees.
#'
#' @author Sophie Kersting
#' @param tree Rooted binary tree of class "phylo"
#'                  (no specific node enumeration order required)
#' @return number of interior nodes that are not symmetry nodes (numeric)
#' @export
#' @examples
#' mat <- cbind(c(7,7,6,5,5,6),c(1,2,3,4,6,7))
#' tree <- list(edge=mat, tip.label=c("","","",""), Nnode=3)
#' attr(tree, "class") <- "phylo"
#' symNodesIndex(tree)
symNodesIndex <- function(tree){
    if (!inherits(tree,"phylo")) stop("The input tree must be in phylo-format.")
    n <- length(tree$tip.label)
    if (n == 1) return(0)
    n_interior <- tree$Nnode
    if (n_interior != n - 1) stop('The tree is not a rooted binary tree.')
    #___________________________________________________________________________
    Descs <- getDescMatrix(tree)
    Ancs <- getAncVec(tree)
    #Phase 1:-------------------------------------------------------------------
    depthResults <- getNodesOfDepth(mat=Descs,root=which(is.na(Ancs)),
                                    n=tree$Nnode+1)
    #Phase 2:-------------------------------------------------------------------
    worklab <- matrix(rep(NA,2*(2*n-1)), ncol = 2)
    inum <- rep(NA,2*n-1)
    for(d in depthResults$maxdepth:0){
        current_Nodes <- na.omit(as.vector(depthResults$nodesOfDepth[d+1,]))
        for(v in current_Nodes){
            if(is.na(Descs[v,1])){ #if leaf
                worklab[v,] <- c(0,0)
            }else{
                worklab[v,] <- c(inum[Descs[v,]])
                if(worklab[v,1]>worklab[v,2]){ #swap if not sorted in ascending order
                    temp <- worklab[v,1]
                    worklab[v,1] <- worklab[v,2]
                    worklab[v,2] <- temp
                }
            }
        }
        inum[current_Nodes] <- myBucketLexicoSort(worklab[current_Nodes,])
    }
    #___________________________________________________________________________
    #Counting symmetry nodes
    numb_symNodes <- 0
    for(i in 1:(2*n-1)){
        if(worklab[i,1]!=0){ #if not a leaf
            #check if worklab has two times the same entry
            if(worklab[i,1]==worklab[i,2]){
                numb_symNodes <- numb_symNodes + 1
            }
        }
    }
    return(n-1-numb_symNodes) # return number of nodes that are not sym. nodes
}
