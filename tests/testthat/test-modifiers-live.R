test_that("new cursed-bestiary modifiers parse and apply", {
  new_mods <- c("aggregate", "compilation", "gapfill", "tier2",
                "counter_argument", "pseudo_spatial", "omit_control",
                "knife_edge", "iid_rescue", "defer_fix", "coffee_break",
                "mechanism_rebuttal", "misunderstanding", "narrow_answer",
                "complexity_shield", "frame_shift", "matter_of_taste")
  for (tok in new_mods) {
    expect_identical(texanshootR:::parse_mod_command(paste0("+", tok)), tok)
    expect_true(tok %in% names(texanshootR:::LIVE_MODIFIERS))
  }
})

test_that("modifier aliases resolve to their canonical token", {
  pmc <- texanshootR:::parse_mod_command
  expect_identical(pmc("+agg"),          "aggregate")
  expect_identical(pmc("+comp"),         "compilation")
  expect_identical(pmc("+gap"),          "gapfill")
  expect_identical(pmc("+relax"),        "tier2")
  expect_identical(pmc("+preempt"),      "counter_argument")
  expect_identical(pmc("+coords"),       "pseudo_spatial")
  expect_identical(pmc("+omit"),         "omit_control")
  expect_identical(pmc("+labeling"),     "knife_edge")
  expect_identical(pmc("+iid"),          "iid_rescue")
  expect_identical(pmc("+defer"),        "defer_fix")
  expect_identical(pmc("+next_step"),    "defer_fix")
  expect_identical(pmc("+side_meeting"), "coffee_break")
  expect_identical(pmc("+offline"),      "coffee_break")
  expect_identical(pmc("+intuition"),    "mechanism_rebuttal")
  expect_identical(pmc("+but_biologically"), "mechanism_rebuttal")
  expect_identical(pmc("+you_misread"),  "misunderstanding")
  expect_identical(pmc("+strawman"),     "narrow_answer")
  expect_identical(pmc("+its_complex"),  "complexity_shield")
  expect_identical(pmc("+oscillate"),    "frame_shift")
  expect_identical(pmc("+just_one_way"), "matter_of_taste")
})

test_that("applying a new modifier records the consumption and bias", {
  for (tok in c("aggregate", "pseudo_spatial", "knife_edge")) {
    state <- texanshootR:::apply_modifier(list(), tok)
    expect_true(tok %in% state$consumed_modifiers)
    expect_identical(state$bias$perturb_tag,
                     texanshootR:::LIVE_MODIFIERS[[tok]]$bias$perturb_tag)
  }
})
