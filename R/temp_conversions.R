### GET TAGESSCHAU INFO

# Function to retrieve videos from a YouTube channel within a date range
get_tagesschau_info <- function(api_key, channel_id ="UC5NOEUbkLheQcaaRldYW5GA" , start_date, end_date, num_videos = 100) {

  library(pacman)
  p_load("httr", "jsonlite","dplyr")
  # Helper function to convert ISO 8601 duration (PT15M35S) to total seconds
  convert_duration_to_seconds <- function(duration) {
    # Use regular expressions to extract minutes and seconds
    mins <- as.numeric(sub(".*T(\\d+)M.*", "\\1", duration))
    secs <- as.numeric(sub(".*(\\d+)S$", "\\1", duration))

    # Calculate total seconds, defaulting to 0 if mins or secs are NA
    total_seconds <- (ifelse(!is.na(mins), mins, 0) * 60) + (ifelse(!is.na(secs), secs, 0))
    return(total_seconds)
  }

  # YouTube Data API endpoint
  search_url <- "https://www.googleapis.com/youtube/v3/search"
  videos_url <- "https://www.googleapis.com/youtube/v3/videos"

  # Initialize variables for pagination
  all_videos <- list()
  next_page_token <- NULL

  # Loop until we reach the desired number of videos
  while (length(all_videos) < num_videos) {

    # Parameters for the API search request
    search_params <- list(
      key = api_key,
      channelId = channel_id,
      part = "snippet",
      type = "video",
      maxResults = min(50, num_videos - length(all_videos)),  # Adjust maxResults for the last request
      publishedAfter = paste0(start_date, "T00:00:00Z"),
      publishedBefore = paste0(end_date, "T23:59:59Z"),
      order = "date",  # Order results by date
      pageToken = next_page_token  # Add the next page token if available
    )

    # API request for videos
    response <- GET(search_url, query = search_params)

    # Check for successful response
    if (response$status_code != 200) {
      stop("API request failed: ", response$status_code)
    }

    # Parse the search response
    parsed_search_data <- content(response, "parsed")

    # Check if there are items in the response
    if (length(parsed_search_data$items) == 0) {
      warning("No more videos found for the specified date range.")
      break
    }

    # Loop through each video item and filter by title
    for (item in parsed_search_data$items) {
      title <- item$snippet$title

      # Check if the title contains "tagesschau 20"
      if (grepl("tagesschau 20", title, ignore.case = TRUE)) {
        all_videos[[length(all_videos) + 1]] <- item$id$videoId
      }
    }

    # Check for a next page token
    next_page_token <- parsed_search_data$nextPageToken

    # If there are no more pages, break the loop
    if (is.null(next_page_token)) {
      break
    }
  }

  # If no videos match the title condition
  if (length(all_videos) == 0) {
    warning("No videos found with the specified title filter.")
    return(data.frame())
  }

  # Retrieve additional details for the collected video IDs
  video_info_list <- list()

  # Chunk video IDs into groups of 50 for the second request
  for (i in seq(1, length(all_videos), by = 50)) {
    video_ids_chunk <- paste(all_videos[i:min(i + 49, length(all_videos))], collapse = ",")

    # Parameters for the video details request
    video_params <- list(
      key = api_key,
      id = video_ids_chunk,
      part = "snippet,statistics,contentDetails"
    )

    # API request for video details
    video_response <- GET(videos_url, query = video_params)

    # Check for successful response
    if (video_response$status_code != 200) {
      stop("Video details request failed: ", video_response$status_code)
    }

    # Parse the video response
    parsed_video_data <- content(video_response, "parsed")

    # Loop through each video detail item
    for (video in parsed_video_data$items) {
      video_info_list[[length(video_info_list) + 1]] <- data.frame(
        title = video$snippet$title,
        video_id = video$id,
        published_at = as.Date(video$snippet$publishedAt),
        description = video$snippet$description,
        views = as.numeric(video$statistics$viewCount),
        length = convert_duration_to_seconds(video$contentDetails$duration),
        timestamp = Sys.time(),
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine into a single data frame
  video_info_df <- do.call(rbind, video_info_list)

  # Limit the number of returned videos if needed
  if (nrow(video_info_df) > num_videos) {
    video_info_df <- video_info_df[1:num_videos, ]
  }

  # Return the final data frame with filtered video information
  return(video_info_df)
}


### GET TAGESSCHAU TRANSCRIPTS


get_tagesschau_transcript <- function(video_id, language = "de") {

  p_load(reticulate)

  reticulate::virtualenv_create("myenv")
  reticulate::use_virtualenv("myenv", required = TRUE)
  reticulate::py_install("youtube-transcript-api", envname = "myenv")
  yt_api <- reticulate::import("youtube_transcript_api")
  tryCatch({
    transcripts <- yt_api$YouTubeTranscriptApi$get_transcript(video_id, languages = list(language))
    subtitle_text <- paste(sapply(transcripts, function(entry) entry$text), collapse = " ")
    return(subtitle_text)
  }, error = function(e) {
    return(NA)  # Return NA if there's no transcript or if an error occurs
  })
}
