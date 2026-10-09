# DeclareDesign 1.x needs future.seed = FALSE here when 'future' is absent;
# 2.0 removed the argument and errors on it.
simulate_once <- function(design) {
  if ("future.seed" %in% names(formals(DeclareDesign::simulate_design))) {
    DeclareDesign::simulate_design(design, sims = 1, future.seed = FALSE)
  } else {
    DeclareDesign::simulate_design(design, sims = 1)
  }
}
