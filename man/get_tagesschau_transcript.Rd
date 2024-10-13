\name{get_tagesschau_transcript}
\alias{get_tagesschau_transcript}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get_tagesschau_transcript
%%  ~~function to do ... ~~
}
\description{
This function retrieves the transcript of a tagesschau Youtube video after providing the video id.
}
\usage{
get_tagesschau_transcript(video_id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{video_id}{
Requires the video id of the Tagesschau video which can be retrieved either by hand for a specific video or using the get_tagesschau_info function.
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
### Using the function on a single video id that can be found in the Youtube link following "v="

video_id <- "NI67ufMt9_Q"
transcript <- get_youtube_transcript(video_id)

### Using the function on the data frame created with get_tagesschau_info with sapply (requires purrr package)

tagesschau_videos_filtered <- tagesschau_videos_filtered %>%
  mutate(transcript = sapply(video_id, get_tagesschau_transcript))

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
