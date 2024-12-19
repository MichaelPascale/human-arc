
library(tidyr)
library(dplyr)
library(purrr)
library(jsonlite)
library(stringr)
library(lubridate)
library(ggplot2)

# Read in a single behavioral JSON file.
read_behavioral_json <- \(path) {
  x1 <- read_json(path)
  
  if (is.null(names(x1)) && length(x1) == 1)
    x1 <- x1[[1]]
  
  time <- 
    strptime(
      x1$start_time, '%a %b %d %Y %H:%M:%S',
      tz=case_match(str_extract(x1$start_time, 'GMT.\\d+'), 'GMT-0400' ~ 'EDT', 'GMT-0500' ~ 'EST')
    )
  
  if (!hasName(x1, 'session'))
    return(NULL)
  
  x1$session |>
    map(\(session) {
      imap(session$attempts, \(attempt, index){
        # if (hasName(attempt[[1]], 'problem')) print(attempt[[1]]$problem |> str_extract('^\\d+'))
        data.frame(
          attempt=index,
          n_events=length(attempt),
          start=attempt[[1]]$time,
          end=attempt[[length(attempt)]]$time,
          first=attempt[[1]]$desc,
          last=attempt[[length(attempt)]]$desc,
          problem = if (hasName(attempt[[1]], 'problem')) as.integer(str_extract(attempt[[1]]$problem, '^\\d+')) else NA
        )
      }) |>
      list_rbind() |>
      mutate(i=session$id)
    }) |>
    list_rbind() |>
    mutate(
      subject=x1$subj_ID,
      elapsed= end - start,
      time=time,
      file=path
    ) |>
    relocate(subject, i, problem) |>
    group_by(subject, i) |>
    fill(problem, .direction='downup') |>
    ungroup()
}

datapaths=c(
  # mturk='data/ARC-data/MTurk/mturk subject data',
  # bu='data/ARC-data/BU online/subject_data/all-sub-raw',
  pilot1='data/ARC-eyetracking-pilot1/data',
  pilot2='data/ARC-eyetracking/data'
)

files <- dir(datapaths, pattern='\\.json$', full.names = TRUE)

behavioral <-
  map(files, read_behavioral_json, .progress=TRUE) |>
  list_rbind() |>
  tibble() |>
  mutate(dataset=names(datapaths)[match(dirname(file), datapaths)])

behavioral

save(etdata, behavioral, training, subject_id_map, file='data/ET-Pilot.rda')
which(datapaths |> str_detect('BU'))

# behavioral |> 
#   filter(dataset == 'pilot1') |>
#   distinct(subject) |>
#   arrange(subject) |>
#   transmute(
#     subject.number = row_number(),
#     json.ID=subject
#   ) |>
#   write.csv(file.path(path, 'subIDs-NOTCHECKED.csv'), row.names=FALSE, na='')

path2
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


