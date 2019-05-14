\name{cobDf}
\alias{cobDf}
\title{
Create Data Frame for Cob Web Method to Analyse Convergence of Series.
}
\description{
Let x axis represent x_n and y x_n+1, plot y=x and the update function.
Let the pen start from (x1,0)
For each iteration, draw a vertical line segment to find the updating curve and then horizontally the line y=x.
}
\usage{
cobDf(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The series being analyzed
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{ nIter}{ number of iterations }
\item{xcob}{x coordinates of the points on the Cob Web plot}
\item{ycob}{y coordinates of the points on the Cob Web plot}
\item{isVertical}{whether the line segment starting from the point is vertical}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{


nIteration=20
x=.4
x = numeric(nIteration)

x[1]=.4
a=.4
L=.1
for (i in 1:nIteration){
  x[i]=a
  a=1-L*a^2
}

plotCobDf(cobDf(x)[1:12,],nClip=NULL,limType = 0,xbase=(-100:100)/100, updateDf =data.frame(xb=(-100:100)/100,yf = 1-L*((-100:100)/100)^2),
parseTitle=F,engine='gg')



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~Cob }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~Web }% __ONLY ONE__ keyword per line
\keyword{ ~Convergence}
