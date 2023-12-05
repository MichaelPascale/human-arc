
library(dplyr)
library(purrr)
library(jsonlite)
library(stringr)
library(lubridate)

datapath <- 'data/ARC-data/MTurk/mturk subject data'
# datapath <- 'data/ARC-data/BU online/subject_data/all-sub-raw'

files <- dir(datapath, pattern='\\.json$', full.names = TRUE)

# Read in a single behavioral JSON file.
read_behavioral_json <- \(path) {
  x1 <- read_json(path)
  
  time <- 
    strptime(
      x1$start_time, '%a %b %d %Y %H:%M:%S',
      tz=case_match(str_extract(x1$start_time, 'GMT.\\d+'), 'GMT-0400' ~ 'EDT', 'GMT-0500' ~ 'EST')
    )
  
  x1$session |> map_dfr(\(session) {
    
    imap_dfr(session$attempts, \(attempt, index){
      data.frame(
        attempt=index,
        n_events=length(attempt),
        start=attempt[[1]]$time,
        end=attempt[[length(attempt)]]$time,
        first=attempt[[1]]$desc,
        last=attempt[[length(attempt)]]$desc
      )
    }) |>
      mutate(problem=session$id)
  }) |>
    mutate(
      subject=x1$subj_ID,
      elapsed= end - start,
      time=time,
      file=path
    ) |>
    relocate(subject, problem)
}

read_behavioral_json(files[1])


behavioral <- map_dfr(files, read_behavioral_json)
# write.csv(behavioral, 'data/MTurk-AttemptTiming-202312051210.csv', row.names=FALSE, na='')


# How many participants made it to the third attempt?
# What proportion was successful on the third attempt?
# How much time did participants spend on the third attempt?

behavioral <- rbind(behavioral, behavioral2)

behavioral |>
  group_by(attempt) |>
  summarize(
    edits_mean = mean(n_events),
    edits_median = median(n_events),
    edits_max = max(n_events)
  )

