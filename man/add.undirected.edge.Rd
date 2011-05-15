\name{add.undirected.edge}
\alias{add.undirected.edge}
\title{Add undirected edge to a 'bnlearn' object.}
\usage{add.undirected.edge(x, from, to, check.cycles, update=T, debug=F)}
\description{Add undirected edge to a 'bnlearn' object.}
\details{Adding undirected edges is not an exported function in \code{bnlearn}, so 
this function exposes this functionality.

This bridge to \code{bnlearn} can be temperamental because 
\code{bnlearn} also has an object called \code{bn}.}
\value{A new \code{bnlearn} object}
\arguments{\item{x}{A 'bnlearn' object, of class \code{bn}.}
\item{from}{A character string, the label of a node.}
\item{to}{A character string, the label of a node.}
\item{check.cycles}{a boolean value. If TRUE the graph is tested for 
acyclicity; otherwise the graph is returned anyway.}
\item{update}{A logical}
\item{debug}{a boolean value. If TRUE a lot of debugging output is 
printed; otherwise the function is completely silent.}}
