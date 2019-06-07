%% getTarget function %%
\name{getTarget}
\alias{getTarget}

\title{Get target information}

\description{Asssuming that the targets are provided in a NIST mass spectral library, i.e. MSL, format, this function reads the list of targets and the related information.
}

\usage{
getTarget(Method = "target", target.file.name = character(), 
            library.file.name = character(), path = getwd(), 
            library.path = getwd(), Library = list(), target.table = list(),
            deltaRI = numeric(), deltaRT = numeric(), Save = FALSE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{Method}{a srting with three possible values: "target", "library", and "combined". The "target" case means the user only provides a \code{.MSL} file as a list of targets. In the "library" mode, the user provides only a library in \code{.MSL} format, but it is required to provide a \code{target.table} to determine the name of the targets and the selected mass. The \code{target.table} can be obtained by using \code{getTargetTable} function. In the "combined" method, the user provides a list of targets as \code{target.table} and a library in \code{.MSL} format where the fragments are selected using optimization.}
    
    \item{target.file.name}{a string including the full name of a \code{.MSL} file including the target compounds information.}
    
    %\item{quantFrag}{a numeric value or vector providing the location of the quantifier fragment in the spectra of all targets, if one single value is provided, or every target, if a vector is provided. This parameter is optional and if it is not provided, the function selects the quantifier fragment automatically.}
    
    %\item{optFrags}{if TRUE, the function igonres the quantFrag option and optimizes the fragment selection, in that case a library should be provided. This is done by calling \code{optFrag} function. The library should not include any compounds in the target list.}
    
    %\item{numFrags}{a numeric value or vector to determine the number of fragments measured for each target or all targets, optional}

    \item{library.file.name}{a string including the full name of a \code{.MSL} file including the library compounds information. The library can include background compounds, e.g. an in-house library. A library can also directly imported using the \code{Library} argument.}
    
    \item{path}{a string including the full path to the location of the file.name, optional}

    \item{library.path}{a string inlcuding the full path to the location of the library file, optional}
    
    \item{Library}{a list of library information, this is used when the library file is not provided.}
    
    \item{target.table}{a list including the name of the compounds, this is required when a library is provided to extract the targets information from the library. This value can be obtained using \code{getTargetTable} function.}
    
    \item{deltaRI}{a numeric value for the penalty on the retention index similarity score}

    \item{deltaRT}{a numeric value for the penalty on the retention time similarity score}    
    
    \item{Save}{if \code{FALSE}, the function returns the targets as a list, otherwise, it saves the list as an \code{rds} object.}
}

\details{By calling \code{readMSL}, this function reads the target list and retrieves the reltated informatin such as retention time, retention index, mass and intensity of the fragments, and compound names. The user can determine the quantifier mass by using a single value for all targets, or a vector of values pointing to the index of the fragment of ineterest for each target. Also, the targets can be chosen by omptimization based on overlapping compounds if a library is provided. The library should not include any targets, but it can be an in-house library built from background compounds detected in measurements of the same type of samples.
}

\value{A list containing:
    \item{compound}{a character vector containing the names of the targets}
    
    \item{ms}{a list of numeric vectors of fragment mass of the targets}
    
    \item{sp}{a list of numeric vectors of fragment intensities of the targets}
    
    \item{rt}{a numeric vector of retention times of the targets}
    
    \item{ri}{a numeric vector of the retention indexed of the targets}
    
    \item{quantFrag}{a numeric vector showing the index of quantifier fragment in \code{ms} and \code{sp} fields.}
    
    \item{sortedFrag}{a list of numeric vectors showing the order of fragments from the most favorable to the list favorable choice for a quantifier.}
}

\seealso{\code{\link{getTargetTable}}, \code{\link{optFrag}}}

\references{\url{http://omics.georgetown.edu/SIMAT.html}}

\author{Mo R. Nezami Ranjbar}

%\note{Important note: the library should not include any compounds in the target list}

\examples{
    # load the target table information
    data(target.table)

    # load the background library to be used with fragment selection
    data(Library)
    
    # get targets info using target table and provided library
    Targets <- getTarget(Method = "library", Library = Library, 
                                                target.table = target.table)
}