\name{predict.aode}
\alias{predict.aode}
\title{AODE classifier}
\description{
  Computes the conditional a-posterior probabilities of a categorical
  class variable given predictor variables depnendant on one other variable using
  the Bayes rule. 
}
\usage{
predict.aode(model, newdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{A model returned by function aode}
  \item{newdata}{A dataframe of factors with new predictors}
}

\details{
This implementation of AODE algorithm is based on Naive Bayes implementation taken from pakage klaR.
}
\value{
A list with the conditional a-posterior
probabilities for each class and the estimated class are returned.
}
\author{Piotr Misiowiec, Katarzyna Stepek}
\seealso{\code{\link{predict.aode}}}
\examples{
data(iris)
ir<-data.frame(lapply(iris, as.factor))
x<-ir[,-5]
y<-ir$Species
model<-aode.default(x,y, fL=1)
aode.predict(model, newdata)
}
\keyword{classif}
\keyword{category}

