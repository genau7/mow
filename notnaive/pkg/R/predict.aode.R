predict.aode <- function (model, newdata, threshold = 0.001, ...) 
{
  if (missing(newdata)) 
    newdata <- model$x
  
  ## (both colnames & varnames are given) & (varnames is a subset of colnames):
  if ((!any(is.null(colnames(newdata)), is.null(model$varnames))) && 
      all(is.element(model$varnames, colnames(newdata))))
    newdata <- data.frame(newdata[, model$varnames])
  nattribs <- ncol(newdata)
  isnumeric <- sapply(newdata, is.numeric)
  newdataMatrix <- data.matrix(newdata)
  
  
  prDual <-function (rowNum){
    rowData<-newdata[rowNum,]
    colSelector <- names(rowData) %in% names(model$conditionals)
    
    #(P(aj=vj|c=d)
    probabilitiesGivenClass<-mapply(function(attr, colNr) model$conditionals[[attr]][,colNr], names(model$conditionals), rowData[colSelector])
    
    #product(P(ai=vi|c=d, aj=vj))
    products<-sapply(names(newdata), function(attrName) {
      attrVal<-rowData[[attrName]]
      conditionalTables<-model$doubleConditionals[[attrName]][[attrVal]]
      colNum<- match(attrName, names(newdata))
      probabilitiesGivenClassAndVar<-apply(mapply(function(secondAttrName, observation) {
        as.data.frame(conditionalTables[[secondAttrName]])[[observation[[1]]]] }, 
        names(conditionalTables), rowData[-colNum]), 1, prod)
    })
    
    pr<-probabilitiesGivenClass*products
    pr<-apply(pr, 1, sum)
    pr<-pr/ncol(newdata)
    bnum<-model$apriori*pr
    classProbabilities<-bnum/sum(bnum)
  }
  
  posterior <- t(sapply(1:nrow(newdata), prDual))
  classdach <- factor(model$levels[apply(posterior, 1, which.max)], levels = model$levels)
  colnames(posterior) <- model$levels
  rownames(posterior) <- names(classdach) <- rownames(newdata)
  return(list(class = classdach, posterior=posterior))
  
}
