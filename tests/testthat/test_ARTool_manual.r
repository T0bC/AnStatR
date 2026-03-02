print("XXXXXXXXXXXXXXX")

box::use(app/logic/statistics/nonparametric_tests)
df <- expand.grid(f1 = c("A", "B"), f2 = c("X", "Y"), stringsAsFactors = FALSE)
df <- df[rep(1:nrow(df), each = 10), ]
df$measure <- rnorm(nrow(df))
result <- nonparametric_tests$perform_art2way(df = df, x_axis = c("f1", "f2"), measure_col = "measure")
print(result)