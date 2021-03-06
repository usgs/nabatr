% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geospatial.R
\name{get_grts_from_ll}
\alias{get_grts_from_ll}
\title{Get GRTS information from Lat / Lon}
\usage{
get_grts_from_ll(
  token,
  latitude,
  longitude,
  branch = "prod",
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE
)
}
\arguments{
\item{token}{List token created from get_nabat_gql_token() or
get_refresh_token()}

\item{latitude}{Numeric latitude in EPSG 4326 (WGS 84)}

\item{longitude}{Numeric latitude in EPSG 4326 (WGS 84)}

\item{branch}{(optional) String that defaults to 'prod' but can also be
'dev'|'beta'|'local'}

\item{url}{(optional) String url to use for GQL}

\item{aws_gql}{(optional) String url to use in aws}

\item{aws_alb}{(optional) String url to use in aws}

\item{docker}{(optional) Boolean if being run in docker container or not}
}
\description{
Takes a latitude and longitude in EPSG 4326 (WGS 84)
and returns a
}
