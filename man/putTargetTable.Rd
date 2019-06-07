\name{putTargetTable}
\alias{putTargetTable}
\title{Put target table information}

\description{Asssuming that the targets are provided in a NIST mass spectral library, i.e. MSL, format, this function reads the list of targets and the related information.
}

\usage{
putTargetTable(target.table = list(), target.table.file.name = character())
}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{target.table}{a list containing the target table information, e.g. obtained using \code{getTargetTable}.}
    
    \item{target.table.file.name}{a string including the full name of a text file including the target compounds information.}    
}

\details{This function writes the target table information in a csv file so the user can open the results in text and table editors.
}

\value{A logical value}

\seealso{\code{\link{getTargetTable}}}

\references{\url{http://omics.georgetown.edu/SIMAT.html}}

\author{Mo R. Nezami Ranjbar}

\examples{
    # load the target table information
    data(target.table)

    # create a subset of the table
    target.table.sub <- list()
    target.table.sub$compound <- target.table$compound[1:2]
    target.table.sub$ms <- target.table$ms[1:2]
    
    # write the subset into a csv file
    putTargetTable(target.table = target.table.sub, 
                    target.table.file.name = "TargetTableSub.csv")
}