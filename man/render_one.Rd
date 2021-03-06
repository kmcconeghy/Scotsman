% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render_one.R
\name{render_one}
\alias{render_one}
\title{render_one: Render a single .Rmd file, and output to location}
\usage{
render_one(f_prefix, rmd_path, html_path = NULL, stamp = T)
}
\arguments{
\item{f_prefix}{file prefix with Scotty canon name, e.g. 'A01'}

\item{rmd_path}{filepath where '.Rmd' files exist}

\item{html_path}{filepath to render to}

\item{timestamp}{default=True, will fix date-time to file name.}
}
\description{
render_one: Render a single .Rmd file, and output to location
}
\examples{
Scotty::render_one('A01', 'CodeFilesPath', paste0('ReportFilesPath'))

}
\author{
Kevin W. McConeghy
}
