aode <-
function (x, grouping, prior = NULL, fL = 1, ...) 
{
  x <- data.frame(x)
  selector<-c()
  if(!is.factor(grouping))
    stop("grouping/classes object must be a factor")
  if (is.null(prior)) 
    apriori <- table(grouping) / length(grouping)
  else 
    apriori <- as.table(prior / sum(prior))
  Yname <- "grouping"
  
  LaplaceEst <- function(x, f = 0)
    t(apply(x, 1, function(u) (u + f)/(sum(u) + (length(u) * f))))
  
  #calculate P(ai=vi|c=d)
  est <- function(var){# var is a column from data frame
    LaplaceEst(table(grouping, var), f = fL)
  }
  
  getDependence <- function(lvl, columnNr){
    otherColumns<-names(x[-columnNr])
    selector<-which(x[columnNr]==lvl)
    lapply( x[otherColumns], function(column){
      LaplaceEst(table(grouping[selector], column[selector]), f=fL)
    })
  }
  
  #calculate P(ai=vi|c=d, aj=vj)
  estDouble<- function (columnNr){
    lvls<-levels(as.list(x[columnNr])[[1]])
    dependence <- lapply(lvls, function(lvl) getDependence(lvl, columnNr))
  }
  
  tables <- lapply(x, est)
  tables2 <- lapply(seq_along(x), estDouble)
  names(tables2)<-names(x)
  for (attrName in names(x)){ names(tables2[[attrName]])<-levels(x[[attrName]])  }
  structure(list(apriori = apriori, conditionals = tables, doubleConditionals = tables2, levels = levels(grouping), x = x, varnames = colnames(x)), class = "aode")
}
