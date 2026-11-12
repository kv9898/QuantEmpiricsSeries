library(fixest)
library(modelsummary)

# Data simulation ----
set.seed(123)

# Balanced panel: 500 units observed before and after treatment.
# All treated units start treatment in the same period.
n_units <- 500
tau <- 2 # True treatment effect
beta_x <- 1 # True effect of the control variable

units <- data.frame(
    id = seq_len(n_units),
    treated = sample(rep(c(0L, 1L), each = n_units / 2)),
    unit_fe = rnorm(n_units, mean = 0, sd = 1)
)

dat <- expand.grid(id = units$id, post = 0:1)
dat <- merge(dat, units, by = "id", sort = TRUE)
dat <- dat[order(dat$id, dat$post), ]
rownames(dat) <- NULL

dat$did <- dat$treated * dat$post

# Exogenous control: varies within units over time and is independent of
# treatment, unit effects, and outcome shocks.
dat$x <- rnorm(nrow(dat), mean = 0, sd = 1)

# Untreated outcomes have a common time trend (parallel trends).
# Treated units also have a different baseline level.
dat$y <- 5 +
    dat$unit_fe +
    1.5 * dat$treated +
    0.75 * dat$post +
    tau * dat$did +
    beta_x * dat$x +
    rnorm(nrow(dat), mean = 0, sd = 1)

# With a time-varying control, the two specifications need not give identical
# coefficients: TWFE uses within-unit variation in x; the interaction model
# uses both within-unit and between-unit variation.

# Interaction ----
model1 <- feols(y ~ treated * post + x, data = dat)

# TWFE ----
model2 <- feols(y ~ did + x | id + post, data = dat)

# Compare results ----
# Set the correction on each model explicitly before passing it to the table.
models <- list("Interaction" = model1, "TWFE" = model2)
clustered_models <- lapply(models, function(m) {
    summary(m, vcov = ~id, ssc = ssc(K.adj = FALSE))
})
modelsummary(
    clustered_models,
    stars = TRUE
)

# Focus on x: show default IID SEs and unadjusted unit-clustered SEs.
# The clustered comparison removes differences from correction factors.
control_comparison <- data.frame(
    model = names(models),
    estimate_x = sapply(models, function(m) unname(coef(m)["x"])),
    se_x_iid_default = sapply(models, function(m) {
        unname(se(m, vcov = "iid")["x"])
    }),
    se_x_clustered_unadjusted = sapply(models, function(m) {
        unname(se(m, vcov = ~id, ssc = ssc(K.adj = FALSE, G.adj = FALSE))["x"])
    }),
    row.names = NULL
)
print(control_comparison)
