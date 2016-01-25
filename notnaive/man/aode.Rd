\name{aode}
\alias{aode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{AODE classifier}
\description{
  Computes the conditional a-posterior probabilities of a categorical
  class variable given predictor variables depnendant on one other variable using
  the Bayes rule. 
}
\usage{
aode(x, grouping, prior = NULL, fL = 1)
}
\arguments{
  \item{x}{data frame of categorical variables.}
  \item{grouping}{class vector (a factor).}
  \item{prior}{the prior probabilities of class membership. If unspecified, 
    the class proportions for the training set are used. If present, 
    the probabilities should be specified in the order of the factor levels.}
  \item{fL}{Factor for Laplace correction, default factor is 1 }
  \item{\dots}{arguments passed to \code{\link{density}}.}
}
\details{This implementation of AODE algorithm is based on Naive Bayes implementation taken from pakage klaR.}
\value{
An object of class \code{"aode"} including components:

  \item{apriori}{Class distribution for the dependent variable.}
  \item{conditionals}{A list of tables, one for each predictor variable. For each
    categorical variable a table giving, for each attribute level, the conditional
    probabilities given the target class. For each numeric variable, a
    table giving, for each target class, mean and standard deviation of
    the (sub-)variable or a object of \code{class} \code{\link{density}}.}
  \item{doubleConditionals}{A list of in which each attribute produces its own list - for each level an attrubute has, attributeNumber-1 tables are produced. This variable stores the probablities obtained with each attribute used as the single common predecessor.}
}
\author{Piotr Misiowiec, Katarzyna Stepek}
\seealso{\code{\link{predict.aode}}}
\examples{
data(iris)
ir<-data.frame(lapply(iris, as.factor))
x<-ir[,-5]
y<-ir$Species
model<-aode.default(x,y, fL=1)
}
\keyword{classif}
\keyword{category}

