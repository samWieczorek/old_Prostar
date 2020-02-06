##' Method to build the list of connex composant of the adjacency matrix
##' 
##' @title Build the list of connex composant of the adjacency matrix
##' @param X An adjacency matrix
##' @return A list of CC  
##' @author Thomas Burger, Samuel Wieczorek
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata') 
##' X <- BuildAdjacencyMatrix(Exp1_R25_pept[1:1000], "Protein_group_IDs", FALSE)
##' ll <- get.pep.prot.cc(X)
get.pep.prot.cc <- function(X){
  if (is.null(X)){
    warning("The adjacency matrix is empty")
    return()}
  #require(Matrix)
  #require(igraph)
  #require(graph)
  
  
  p <- dim(X)[2] # Nb proteins
  q <- dim(X)[1] # Nb peptides
  
  
  multprot.cc <- singprot.cc <- multprot.cc.pep <- singprot.cc.pep <- NULL
  A <- B <- g <- NULL
  ### Adjacency matrix construction
  # boolean matrix product
  A <- as.matrix(t(X) %&% X) 
  # remove self-connecting edges
  diag(A) <- rep(0,p)
  A <- matrix(as.numeric(A[,]), ncol=p) # goes back to classical matrix format
  colnames(A) <- rownames(A) <- colnames(X) # reset pep and prot names
  
  # proteins with no shared peptides
  SingleProt.CC.id <- which(rowSums(A)==0) 
  if (length(SingleProt.CC.id) > 0){
    ### Peptides from single prot CCs
    singprot.cc <- as.list(names(SingleProt.CC.id))
    singprot.cc.pep <- list()
    for(i in 1:length(singprot.cc)){
      peplist <- which(X[,singprot.cc[[i]]]!=0)
      singprot.cc.pep[[i]] <- names(peplist)
    }
  }
  
  
  if (length(SingleProt.CC.id) < nrow(A)){
    B <- A[-SingleProt.CC.id,-SingleProt.CC.id] # matrix with no 1-prot CC
  
    ### Protein CCs
    multprot.cc <- NULL
    g <- graph::graphAM(B, edgemode='undirected', values=NA)
    multprot.cc <- graph::connComp(as(g, 'graphNEL'))

    ### Peptides from multiple prot CCs
    multprot.cc.pep <- list()
    for(i in 1:length(multprot.cc)){
      protlist <- multprot.cc[[i]]
      subX <- as.matrix(X[,protlist])
      peplist <- which(rowSums(subX)!=0)
      multprot.cc.pep[[i]] <- names(peplist)
    }
  }
  

  ### Merge results into a single list
  prot.cc <- c(multprot.cc, singprot.cc)
  pep.cc <- c(multprot.cc.pep, singprot.cc.pep)
  global.cc <- list()
  for(i in 1:length(prot.cc)){
    prot <- prot.cc[[i]]
    pep <- pep.cc[[i]]
    tmp <- list(prot,pep)
    names(tmp) <- c("proteins", "peptides")
    global.cc[[i]] <- tmp
  }
  
  ### Clean memory and return result
  rm(A,B,g,multprot.cc,singprot.cc,multprot.cc.pep,singprot.cc.pep,prot.cc,pep.cc)
  gc()
  return(global.cc)
}





##' Jitter plot of CC
##' 
##' @title Jitter plot of CC
##' @param list.of.cc List of cc such as returned by the function get.pep.prot.cc
##' @return A plot  
##' @author Thomas Burger
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata') 
##' X <- BuildAdjacencyMatrix(Exp1_R25_pept[1:1000], "Protein_group_IDs", TRUE)
##' ll <- get.pep.prot.cc(X)
##' plotJitter(ll)
plotJitter <- function(list.of.cc){
  if (is.null(list.of.cc)){return()}
  
  length(list.of.cc) # number of CCs
  cc.summary <- sapply(list.of.cc, function(x){c(length(x[[1]]),length(x[[2]]))})
  rownames(cc.summary) <- c("Nb_proteins","Nb_peptides")
  colSums(cc.summary) # total amount of pep and prot in each CC
  colnames(cc.summary) <- 1:length(list.of.cc)
  cc.summary
  rowSums(cc.summary) # c(number of prot, number of pep)
  
  
  cc.summary <- as.data.frame(t(jitter(cc.summary)))
  plot(jitter(cc.summary[,2]),jitter(cc.summary[,1]), type="p", xlab="#peptides in CC", ylab="#proteins in CC")
  
}

# 
# GetDataForPlotJitter <- function(list.of.cc){
#   if (is.null(list.of.cc)){return()}
#   length(list.of.cc) # number of CCs
#   cc.summary <- sapply(list.of.cc, function(x){c(length(x[[1]]),length(x[[2]]))})
#   rownames(cc.summary) <- c("Nb_proteins","Nb_peptides")
#   colSums(cc.summary) # total amount of pep and prot in each CC
#   colnames(cc.summary) <- 1:length(list.of.cc)
#   cc.summary
#   rowSums(cc.summary) # c(number of prot, number of pep)
#   
#   return(as.data.frame(t(jitter(cc.summary))))
# }


##' Display a CC
##' @title Display a CC
##' @param The.CC A cc (a list)
##' @param X xxxxx
##' @return A plot  
##' @author Thomas Burger, Samuel Wieczorek
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata') 
##' X <- BuildAdjacencyMatrix(Exp1_R25_pept, "Protein_group_IDs", FALSE)
##' ll <- get.pep.prot.cc(X)
##' g <- buildGraph(ll[[1]], X)
buildGraph <- function(The.CC, X){
  
  subX <- as.matrix(X[The.CC$peptides, The.CC$proteins])
  nb.prot <- length(The.CC$proteins)
  nb.pep <- length(The.CC$peptides)
  nb.pep.shared <- length(which(rowSums(subX)>1))
  nb.pep.spec <- length(which(rowSums(subX)==1))
  nb.total = nb.prot + nb.pep
  edge.list <- as.data.frame(which(subX==1, arr.ind=TRUE))
  
  def.grp <- c(rep("shared.peptide", nb.pep), rep("protein", nb.prot))
  def.grp[which(rowSums(subX)==1)] <- 'spec.peptide'
  
  
  nodes <- data.frame(id = 1:nb.total,
                      group = def.grp,
                      label = c(rownames(subX), colnames(subX)),
                      title =  paste0("<p>", 1:nb.total,"<br>Tooltip !</p>"),
                      size = c(rep(10, nb.pep),rep(20, nb.prot)),
                      stringsAsFactors = FALSE
  )
  edges <- data.frame(from=c(edge.list$row), 
                      to=c(edge.list$col+ nb.pep),
                      stringsAsFactors = FALSE)
  
  return(list(nodes=nodes, edges=edges))
}


##' Display a CC
##' @title Display a CC
##' @param g A cc (a list)
##' @param layout xxxxx
##' @param obj xxx
##' @param prot.tooltip xxx
##' @param pept.tooltip xxx
##' @return A plot  
##' @author Thomas Burger, Samuel Wieczorek
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata') 
##' X <- BuildAdjacencyMatrix(Exp1_R25_pept, "Protein_group_IDs", FALSE)
##' ll <- get.pep.prot.cc(X)
##' g <- buildGraph(ll[[1]], X)
##' display.CC.visNet(g)
display.CC.visNet <- function(g, layout = layout_nicely, 
                       obj=NULL,
                       prot.tooltip=NULL, 
                       pept.tooltip=NULL){
  #require(visNetwork)
  
  col.prot <- "#ECB57C"
  col.spec <- "#5CA3F7"
  col.shared <- "#0EA513"
  
  
  visNetwork::visNetwork(g$nodes, g$edges, width = "100%", height = "100%") %>%
    visNetwork::visNodes(shape = "dot") %>%                        # square for all nodes
    visNetwork::visGroups(groupname = "spec.peptide", color = col.spec) %>%    # darkblue for group "A"
    visNetwork::visGroups(groupname = "shared.peptide", color = col.shared) %>%    # darkblue for group "A"
    visNetwork::visGroups(groupname = "protein", color = col.prot,shape = "dot") %>%
    visNetwork::visOptions(highlightNearest = FALSE) %>%
    #visLegend()
    #visPhysics(stabilization = FALSE)%>%
    visNetwork::visEdges(color = "#A9A9A9",width = 2)
  #%>%
   # visIgraphLayout(layout = "layout_with_fr")
  
  
}



##' Display a jitter plot for CC
##' @title Display a a jitter plot for CC
##' @param df xxxx
##' @param clickFunction xxxx
##' @return A plot  
##' @author Thomas Burger, Samuel Wieczorek
##' @examples
##' \dontrun{
##' }
plotJitter_rCharts <- function(df, clickFunction=NULL){
  
  #df <- GetDataForPlotJitter(list.of.cc)
  print("In Prostar::diffAnaVolcanoplot_rCharts")
  print(str(df))
  xtitle <- "TO DO"
  
  if (is.null(clickFunction)){
    clickFunction <- 
      JS("function(event) {Shiny.onInputChange('eventPointClicked', [this.index]+'_'+ [this.series.name]);}")
  }
  
  i_tooltip <- which(startsWith(colnames(df),"tooltip"))
  txt_tooltip <- NULL
  for (i in i_tooltip){
    t <- txt_tooltip <- paste(txt_tooltip,"<b>",gsub("tooltip_", "", 
                                                     colnames(df)[i], 
                                                     fixed=TRUE), 
                              " </b>: {point.", colnames(df)[i],"} <br> ", 
                              sep="")
  }
  
  h1 <-  highchart() %>%
    hc_add_series(data = df, type = "scatter") %>%
    my_hc_chart(zoomType = "xy",chartType="scatter") %>%
    hc_legend(enabled = FALSE) %>%
    hc_yAxis(title = list(text="Nb of proteins ic CC")) %>%
    hc_xAxis(title = list(text = "Nb of peptides ic CC")) %>%
    hc_tooltip(headerFormat= '',pointFormat = txt_tooltip) %>%
    hc_plotOptions(series = list( animation=list(duration = 100),
                                   cursor = "pointer", 
                                   point = list( events = list( 
                                     click = clickFunction ) ) ) ) %>%
    my_hc_ExportMenu(filename = "plotCC")
    
    
  return(h1)
}


