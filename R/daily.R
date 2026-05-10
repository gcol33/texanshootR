# Daily seed flavor: the calendar day picks a small thematic
# adjustment to which message tags are weighted. Pure cosmetic; no
# leaderboard, no comparison.

daily_flavor <- function(date = Sys.Date()) {
  wday <- as.integer(format(as.Date(date), "%u"))  # 1 = Mon
  switch(wday,
    list(label = "Manuscript Monday",   tag_boost = "narrative"),
    list(label = "Tabulation Tuesday",  tag_boost = "p_hacking"),
    list(label = "Wrap-up Wednesday",   tag_boost = "post_hoc"),
    list(label = "Throughput Thursday", tag_boost = "data_dredging"),
    list(label = "Final Friday",        tag_boost = "optional_stopping"),
    list(label = "Synthesis Saturday",  tag_boost = "story_first"),
    list(label = "Submission Sunday",   tag_boost = "publication_bias")
  )
}
