
library(tidyr)
library(dplyr)
library(purrr)
library(jsonlite)
library(stringr)
library(lubridate)
library(ggplot2)

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

behavioral |>
  group_by(problem, attempt) |>
  summarize(
    edits_mean = mean(n_events),
    edits_median = median(n_events),
    edits_max = max(n_events),
    n = n()
  )

behavioral |>
  group_by(problem, attempt) |>
  summarize(
    edits_mean = mean(n_events),
    edits_median = median(n_events),
    elapsed_mean = mean(elapsed)/1000,
    n = n()
  ) |>
  group_by(problem) |>
  mutate(
    n_attempt_1 = n[attempt == 1],
  ) |>
  # pivot_wider(
  #   id_cols=problem,
  #   names_from=attempt,
  #   values_from=c(n, edits_median, elapsed_mean),
  #   names_glue = 'attempt_{.name}_{.value}'
  # ) |>
  ggplot() +
  # geom_boxplot(
  #   aes(as.factor(problem), n_events, fill=as.factor(attempt)),
  #   outlier.shape=NA
  # )
  geom_point(
    aes(problem, n/n_attempt_1),
    data = \(x) filter(x, attempt != 1),
    color = '#8da0cb', shape='+', size=6
  ) +
  
  # geom_point(aes(problem, edits_median), color='#fc8d62', shape='square') +
  
  # geom_point(aes(problem, elapsed_mean), color='#66c2a5', shape='triangle') +
  
  # scale_y_continuous('Number of Edits / Number of Seconds', limits=c(0,200), sec.axis=sec_axis(\(x) x/200, 'Proportion')) +
  # scale_y_continuous(expand=c(0,0)) +
  
  facet_wrap(vars(attempt)) +
  labs(x='Problem', y='Proportion Attempted') +
  theme_bw()

ggsave('data/SecondThirdAttempt.png', width=800, height=400, units='px')
ggsave('data/MedianEditsByAttempt.png', width=800, height=400, units='px')
ggsave('data/MeanTimeByAttempt.png', width=800, height=400, units='px')


