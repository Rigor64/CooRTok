% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_entity.R
\name{create_entity}
\alias{create_entity}
\title{Create entity function}
\usage{
create_entity(graph, database, result, get_cluster = TRUE)
}
\arguments{
\item{graph}{the coordinated behaviours graph}

\item{database}{the database}

\item{result}{output of the detected group}

\item{get_cluster}{boolean parameter for chosing if the cluster would be detected}
}
\description{
This function given, refer to the input, a dataset with specific data related to the graph of the coordinated behaviours,
with an indication about the clusters or components on each network
}
\examples{
create_entity(graph = graph, database = database, get_cluster = TRUE)
}
