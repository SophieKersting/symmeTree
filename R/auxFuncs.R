#' Auxiliary functions for handling nodes in trees
#'
#' \code{getDescMatrix} - Determines a matrix that contains the descendants of
#' node i in row i.
#'
#' @author Sophie Kersting
#' @param tree Rooted binary tree of class "phylo", >= 2 leaves
#'                  (no specific node enumeration order required)
#' @return \code{desc_mat} numeric matrix  (2n-1 rows, 2 columns)
#' @export
#' @rdname auxFuncs
#' @examples
#' mat <- cbind(c(7,7,6,5,5,6),c(1,2,3,4,6,7))
#' tree <- list(edge=mat, tip.label=c("","","",""), Nnode=3)
#' getDescMatrix(tree)
#' mat <- cbind(c(5,5,5,5),c(1,2,3,4))
#' tree <- list(edge=mat, tip.label=c("","","",""), Nnode=1)
#' getDescMatrix(tree)
getDescMatrix <- function(tree){
    n <- length(tree$tip.label)
    desc_mat <- matrix(rep(NA,n*(tree$Nnode+n)), ncol = n)
    currentCol <- rep(1,(tree$Nnode+n))
    for(i in 1:nrow(tree$edge)){ # transfer every row (edge) to desc_mat
        source <- tree$edge[i,1] 
        desc_mat[source,currentCol[source]] <- tree$edge[i,2]
        currentCol[source] <- currentCol[source]+1
    }
  return(desc_mat)
}
#' Auxiliary functions for handling nodes in trees
#'
#' \code{getAncVec} - Determines a vector that contains the ancestor of
#' node i in position i.
#'
#' @return \code{anc_vec} numeric vector  (size 2n-1)
#' @rdname auxFuncs
#' @export
#' @examples
#' getAncVec(tree)
getAncVec <- function(tree){
    anc_vec <- rep(NA,(2*tree$Nnode+1))
    for(i in 1:nrow(tree$edge)){ # transfer every row (edge) to anc_vec
        anc_vec[tree$edge[i,2]] <- tree$edge[i,1]
    }
    return(anc_vec)
}
#' Auxiliary functions for handling nodes in trees
#'
#' \code{getNodesOfDepth} - Determines a matrix that contains the nodes of
#' depth i in row i.
#'
#' @param mat Descendants matrix from \code{getDescMatrix}
#' @param root Number (label) of the root of the tree
#' @param n Number of leaves of the tree
#' @return \code{nodes_of_depth} numeric matrix  (n rows, n columns)
#' @rdname auxFuncs
#' @export
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
#' Auxiliary functions for handling nodes in trees
#'
#' \code{myBucketLexicoSort} - Sorts the pairs of numbers lexicographically and
#' returns ranking. Uses bucket sort.
#'
#' @param workLabs numeric matrix (2 columns)
#' @return \code{ranking} numeric vector
#' @export
#' @rdname auxFuncs
#' @examples
#' myWorkLabs <- cbind(c(0,1,2,3,1,0),c(0,2,2,4,1,0))
#' myBucketLexicoSort(myWorkLabs)
myBucketLexicoSort <- function(workLabs){
    rows <- nrow(workLabs)
    if(is.null(rows)){ #if single working lab
        return(1)
    }
    bigBuckets_numb <- max(workLabs[,1])+1 #buckets for 0,1,2,..,max first entry
    tinyBuckets_numb <- max(workLabs[,2])+1 #buckets for 0,1,2,..,max sec. entry
    ranks <- rep(NA,nrow(workLabs))
    #---------------fill big buckets according to first entry
    bigBucket <- matrix(rep(NA,bigBuckets_numb*rows),ncol = rows)
    bBFill <- rep(1,bigBuckets_numb)#bucket i can be filled at ith position
    for(i in 1:rows){
        firstEntry <- workLabs[i,1]
        bigBucket[firstEntry+1,bBFill[firstEntry+1]] <- i
        bBFill[firstEntry+1] <- bBFill[firstEntry+1] + 1 # next position
    }
    #----------now fill tiny buckets for every big one, according to sec. entry
    current_rank <- 1 #start with rank 1 in first big bucket
    for(buck in 1:bigBuckets_numb){ #buck=2
        tinyBucket <- matrix(rep(NA,tinyBuckets_numb*rows),ncol = rows)
        tBFill <- rep(1,tinyBuckets_numb)
        for(i in na.omit(bigBucket[buck,])){
            secEntry <- workLabs[i,2]
            tinyBucket[secEntry+1,tBFill[secEntry+1]] <- i
            tBFill[secEntry+1] <- tBFill[secEntry+1] + 1 # next position
        }
        for(tinybuck in 1:tinyBuckets_numb){ #tinybuck=2
            currentIndices <- na.omit(tinyBucket[tinybuck,])
            if(length(currentIndices)>0){ #if bucket not empty
                ranks[currentIndices] <- current_rank
                current_rank <- current_rank+1
            }
        }
    }
    return(ranks)
}
