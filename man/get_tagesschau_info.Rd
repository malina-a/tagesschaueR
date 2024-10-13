\name{get_tagesschau_info}
\alias{get_tagesschau_info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get_tagesschau_info
%%  ~~function to do ... ~~
}
\description{
This function retrieves information from Tagesschau Youtube videos including the views, length, video id, header and a timestamp for date of the data retrieval. Videos can be filtered by start and end date. Alternatively, the most recent videos will be scraped.
}
\usage{
get_tagesschau_transcript(api_key, start_date, end_date, num_videos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Specify your Youtube API key here.
}
  \item{start_date}{
  Set the date from which the retrieval should start.
  }
  \item{end_date}{
  Set the date at which the retrieval should stop.
  }
  \item{num_videos}{
  Specify the number of videos for which you want to retrieve the information.
  }
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
###Retrieving info on 25 videos for January 2024
tagesschau_videos_filtered <- get_tagesschau_info(
  api_key = "YOUR_API_KEY",
  start_date = "2024-01-01",
  end_date = "2024-01-31",
  num_videos = 31  # Specify the desired number of videos to retrieve
)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
