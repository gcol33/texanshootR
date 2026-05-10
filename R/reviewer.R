# Reviewer 2 probabilistic encounter.
#
# Outcomes (weighted): accept_enthusiasm | minor | major | reject.
# Weights shift with the run's reviewer_resistance modifier - a more
# resistant run is more likely to land at major / reject (the joke is
# that reviewer 2 is *more* hostile when the work pushes harder).

reviewer_roll <- function(reviewer_resistance = 0,
                          career_level = "Junior Researcher") {
  base <- c(accept_enthusiasm = 0.05,
            minor             = 0.30,
            major             = 0.45,
            reject            = 0.20)
  # Higher resistance shifts mass toward the harsh end. Clip to [-0.3, 0.3].
  r <- max(-0.3, min(0.3, reviewer_resistance))
  base["accept_enthusiasm"] <- max(0, base["accept_enthusiasm"] - r * 0.05)
  base["minor"]             <- max(0, base["minor"]             - r * 0.10)
  base["major"]             <- max(0, base["major"]             + r * 0.10)
  base["reject"]            <- max(0, base["reject"]            + r * 0.05)
  base <- base / sum(base)

  outcome <- sample(names(base), size = 1L, prob = base)

  resist_delta <- switch(outcome,
    accept_enthusiasm = 0.10,
    minor             = 0.05,
    major             = -0.05,
    reject            = -0.15
  )

  list(outcome = outcome, resist_delta = resist_delta)
}
