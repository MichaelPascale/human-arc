
library(jsonlite)
library(purrr)
library(ggplot2)
library(dplyr)
library(stringr)

jsonlist <- dir('/restricted/projectnb/cd-lab/Michael/ARC/data/training-data-arc-backend-202311080945', pattern='\\.json$', full.names=TRUE)

jsonlist <- dir('/restricted/projectnb/cd-lab/Michael/ARC/data/ARC-behavioral/eye-tracking/training', pattern='\\.json$', full.names=TRUE)


train <- map(jsonlist, read_json)

dat = data.frame()
for (file_i in seq_along(train)) {
  file <- train[file_i]
  tasknum <- jsonlist[file_i] |> basename() |> str_extract('^\\d+') |> as.integer()
for (task_i in seq_along(file)) {
  task <- file[[task_i]]
  # train or test
  for (phase in names(task)) {
    for (item  in seq_along(task[[phase]])) {
      for (io in names(task[[phase]][[item]])) {
        for (x in seq_along(task[[phase]][[item]][[io]])) {
          for (y in seq_along(task[[phase]][[item]][[io]][[x]])){
            dat = rbind(dat, data.frame(
              task =tasknum, phase=phase, item=item, io=io, x=x, y=y, val=task[[phase]][[item]][[io]][[x]][[y]]
            ))
          }
        }
      }
    }
  }
}
}


dat |>
  mutate(
    val = as.factor(val),
    phase = factor(phase, c('train', 'test'), c('Train', 'Test')),
    io = factor(io, c('input', 'output'), c('Input', 'Output')),
    item = as.factor(item)
  ) -> training

training |> saveRDS('data/training-arc-behavioral-eye-tracking-202311081400.rds')

training |>
  filter(task == 1) |>
  ggplot() +
  coord_fixed() +
  geom_raster(aes(x, -y+max(y), fill=val)) +
  facet_grid(vars(interaction(phase, item, sep=' ', lex.order=TRUE)), vars(io), switch='y') +
  # from common.css
  scale_fill_manual(values=c('#2B2B2B',
                             '#248ADA',
                             '#C71010',
                             '#1FC719',
                             '#F7DE28',
                             '#878494',
                             '#F954F2',
                             '#EE6000',
                             '#6B23A9',
                             '#8B5A28')) +
  ettheme


