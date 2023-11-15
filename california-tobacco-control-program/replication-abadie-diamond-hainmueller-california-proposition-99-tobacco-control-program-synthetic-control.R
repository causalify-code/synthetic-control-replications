#install.packages("Synth")

library(Synth)

california_data_url <- "https://raw.githubusercontent.com/causalify-code/synthetic-control-replications/main/california-tobacco-control-program/data-abadie-diamond-hainmueller-california-proposition-99-tobacco-control-program-synthetic-control.csv"

california_dataframe <- read.csv(california_data_url)

dataprep_out <- dataprep(
  foo = california_dataframe,
  predictors = c("lnincome", "age15to24", "beer", "retprice"),
  special.predictors = list(
    list("cigsale", 1975, c("mean")),
    list("cigsale", 1980, c("mean")),
    list("cigsale", 1988, c("mean"))
  ),
  dependent = "cigsale",
  unit.variable = "state",
  time.variable = "year",
  treatment.identifier = "California",
  controls.identifier = unique(california_dataframe$state_name[-which(california_dataframe$state_name == "California")]),
  time.predictors.prior = 1970:1988,
  time.optimize.ssr = 1970:1988,
  time.plot = 1970:2000,
  unit.names.variable = "state_name"
) 


synth_out <- synth(
  data.prep.obj = dataprep_out
)

# FIGURE 2: REAL VS SYNTH

synth_california <- dataprep_out$Y0 %*% synth_out$solution.w
plot(
  1970:2000,
  # Synthetic California: (control units matrix) X (weights column)
  synth_california,
  type="l",
  lty="dashed", ylim=c(0,140))

lines(1970:2000,
      dataprep_out$Y1
)
abline(v=1988,lty="dotted")


# GAPS PLOT

gaps <- dataprep_out$Y1 - synth_california

plot(
  1970:2000,
  gaps,
  type="l"
  
)
abline(h=0, lty="dotted")

