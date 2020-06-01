\name{indices_clust}
\alias{indices_clust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cálculo de índices para elegir cantidad de clústers
%%  ~~function to do ... ~~
}
\description{
Compara diferentes índices para elegir cantidad de clúnsters
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
indices_clust(data, min, max, m_fuzzy)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Matriz de datos numérica (sin NA)}
  \item{min}{Mínimos de zonas a considerar}
  \item{max}{Máximo de zonas a considerar}
  \item{m_fuzzy}{Grado de difusión. Número mayor a uno, según la difusión: más cercano a uno es menos difuso}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
