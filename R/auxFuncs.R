#' Get descendants of nodes in matrix
#'
#' \code{getDescMatrix} - Determines a matrix that contains the descendants of
#' node i in row i.
#'
#' @author Sophie Kersting
#' @param tree Rooted binary tree of class "phylo", >= 2 leaves
#'                  (no specific node enumeration order required)
#' @return numeric matrix  (2n-1 rows, 2 columns)
#' @rdname auxFuncs
#' @examples
#' mat <- cbind(c(7,7,6,5,5,6),c(1,2,3,4,6,7))
#' tree <- list(edge=mat, tip.label=c("","","",""), Nnode=3)
#' attr(tree, "class") <- "phylo"
#' getDescMatrix(tree)
getDescMatrix <- function(tree){
    desc_mat <- matrix(rep(NA,2*(2*tree$Nnode+1)), ncol = 2)
    for(i in 1:nrow(tree$edge)){ # transfer every row (edge) to desc_mat
        if(is.na(desc_mat[tree$edge[i,1],1])){
            desc_mat[tree$edge[i,1],1] <- tree$edge[i,2]
        }else{
            desc_mat[tree$edge[i,1],2] <- tree$edge[i,2]
        }
    }
    return(desc_mat)
}
#' Get ancestors of nodes in vector
#'
#' \code{getAncVec} - Determines a vector that contains the ancestor of
#' node i in position i.
#'
#' @return numeric vector  (size 2n-1)
#' @rdname auxFuncs
#' @examples
#' getAncVec(tree)
getAncVec <- function(tree){
    anc_vec <- rep(NA,(2*tree$Nnode+1))
    for(i in 1:nrow(tree$edge)){ # transfer every row (edge) to anc_vec
        anc_vec[tree$edge[i,2]] <- tree$edge[i,1]
    }
    return(anc_vec)
}
#' Get all nodes at each depth
#'
#' \code{getAncVec} - Determines a matrix that contains the nodes of
#' depth i in row i.
#'
#' @param mat descendants matrix from \code{getDescMatrix}
#' @param root Number (label) of the root of the tree
#' @param n Number of leaves of the tree
#' @return numeric matrix  (n rows, n columns)
#' @rdname auxFuncs
#' @examples
#' getNodesOfDepth(mat=getDescMatrix(tree),
#' root=which(is.na(getAncVec(tree))), n=tree$Nnode+1)
getNodesOfDepth <- function(mat,root,n){
    nodesOfDep <- matrix(rep(NA,n*n), ncol = n) #maxdepth=n-1
    lastNodes <- root
    current_depth <- 0
    while(length(lastNodes)>0){
        nodesOfDep[current_depth+1,1:length(lastNodes)] <- lastNodes
        lastNodes <- na.omit(as.vector(mat[lastNodes,]))
        current_depth <- current_depth +1
    }
    return(list(nodesOfDepth=nodesOfDep, maxdepth=current_depth-1))
}
