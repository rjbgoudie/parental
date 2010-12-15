\name{enumerateConfSpace}
\alias{enumerateConfSpace}
\title{Enumerate the space of Confounder Graphs...}
\usage{enumerateConfSpace(numberOfConfounders, response, effect,
    maxNumberParents=numberOfConfounders - 2)}
\description{Enumerate the space of Confounder Graphs}
\details{Enumerate (ie make a list of, not just count) the entire space of 
Confounder models, on a given number of nodes, for a 
particular response.}
\value{An object of class "parental.list", a list of objects of class 
"parental"}
\arguments{\item{numberOfConfounders}{An integer of length 1. The number of confounders}
\item{response}{An integer of length 1. Which node is the response.}
\item{effect}{?}
\item{maxNumberParents}{The maximum number of parents of node response.}}
