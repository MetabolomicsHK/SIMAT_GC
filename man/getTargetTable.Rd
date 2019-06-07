\name{getTargetTable}
\alias{getTargetTable}
\title{Get target table information}

\description{Asssuming that the targets are provided in a table format as a txt file, this function reads the list of targets and the related information.
}

\usage{
getTargetTable(target.table.file.name = character(), path = getwd())
}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{target.table.file.name}{a string including the full name of a text file including the target compounds information.}
    
        \item{path}{a string inlcuding the full path to the target table \code{.txt} file lication, optional}
}

\details{This function gets the targets table from user by reading the targets info including the names of the compounds together with the the mass of selected fragments, the names should be clear enough to be searched by getTarget function when required. Each line of the target table file icludes two keywords, "name", and "mass" or "numfrag", where the full name of the compound comes after "name" and the selected masses after "mass" or number of desired fragments after "numfrag". The "mass" is used when no optimization is expected and the corresponding fragments are defined by users. The "numfrag" is used when the user does not provide the fragments for monitoring and asks them to be selected by optimization. The function is not case-sensitive and the items can be separated by tabs or spaces. Also, it is not required to have the same number of masses for different compounds. The table can be created using any tool with any format, e.g. txt, while it is saved as a text file with a unicode format. Below is an example:

Name L-valine mass 55 72 118
Name urea mass 66 74 189 190
}

\value{A list containing:

    \item{compound}{a character vector of compound names}
    
    \item{ms}{a list of numeric vectors of selected mass for each target}
    
    \item{numFrag}{a numeric vector of the desired number of fragments for each target}
}

\seealso{\code{\link{getTarget}}}

\references{\url{http://omics.georgetown.edu/SIMAT.html}}

\author{Mo R. Nezami Ranjbar}

\examples{
    # load an example data set
    extdata.path <- system.file("extdata", package = "SIMAT")
    
    # read target table information form file
    target.table <- getTargetTable(target.table.file.name = "TargetTable.txt", 
                                                    path = extdata.path)
}