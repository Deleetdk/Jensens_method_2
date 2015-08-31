# libs --------------------------------------------------------------------

library(pacman)
p_load(stringr, shiny, shinyIncubator, plyr, lavaan, semPlot, qgraph, kirkegaard, rhandsontable, ggplot2)


# function ----------------------------------------------------------------

#string concatenation function
"+" = function(x,y) {
  if(is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}


# testing -----------------------------------------------------------------

DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
                small = letters[1:10],
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = FALSE)

DF_2 = rhandsontable(DF) %>%
  hot_table(highlightCol = TRUE, highlightRow = TRUE,
            allowRowEdit = FALSE, allowColEdit = FALSE)

# simulate factor structure -----------------------------------------------

#no cross loadings
d = data.frame(g = c(.77, .81, .65, .75, .66, .65, .82, .45, .59),
               F1 = c(rep(.5, 3), rep(0, 6)),
               F2 = c(rep(0, 3), rep(.5, 3), rep(0, 3)),
               F3 = c(rep(0, 6), rep(.5, 3)))
#cross loadings
d = data.frame(g = c(.77, .81, .65, .75, .66, .65, .82, .45, .59),
               F1 = c(.5, .4, .4, rep(0, 6)),
               F2 = c(0, .2, 0, rep(.5, 3), rep(0, 3)),
               F3 = c(0, 0, .2, rep(0, 3), rep(.5, 3)))


d$s = apply(d, 1, function(x) {
  var_g_group = sum(x^2)
  var_remain = 1 - var_g_group
  loading_specificity = sqrt(var_remain)
  return(loading_specificity)
})
rownames(d) = str_c("I", 1:9)
d

# Generate scores ---------------------------------------------------------

#settings
n = 1e3
set.seed(1)

#factor scores
d_fact_scores = data.frame(matrix(rnorm(4 * n), nrow = n))
colnames(d_fact_scores) = colnames(d)[-length(colnames(d))]

#indicator scores
d_case = data.frame(matrix(ncol = nrow(d), nrow=n))
colnames(d_case) = rownames(d)

for (row_i in 1:nrow(d_case)) {
  if (row_i %% 100 == 0) {
    print(str_c(row_i, " of ", n))
  }
  for (col_i in 1:ncol(d_case)) {
    scores = c(d_fact_scores[row_i, ], rnorm(1)) #generate std scores
    weights = d[col_i, ] #get weights
    wtd_score = sum(scores * weights)
    d_case[row_i, col_i] = wtd_score
  }
}

#standard hierchical analysis
fa_3 = fa(d_case, 3)
fa_3_1 = fa(fa_3$scores)


# CFA/SEM setup ---------------------------------------------------------------

d_indicators = list()
d_indicators$g = str_c(rownames(d)[d$g > 0], collapse = " + ")

#group_factors = grep("F", colnames(d), value = T) #grep method
group_factors = colnames(d)[str_detect(colnames(d), "F")]

for (gf in group_factors) {
  #list with structure
  d_indicators[[gf]] = str_c(rownames(d)[d[gf] > 0], collapse = " + ")
}

# SEM bi-factor model -------------------------------------------

model = ""
for (LV_i in seq_along(d_indicators)) {
  LV_name = names(d_indicators[LV_i])
  model = model + LV_name + " =~ " + d_indicators[LV_i] + "\n"
}

fit = sem(model, data = d_case, std.lv = T, orthogonal = T)
summary(fit)
coef(fit)


semPaths(fit, "model", "std", bifactor = "g", layout = "tree2", residuals = F, exoCov = F)


# SEM hierarchical model --------------------------------------------------

model = "g =~ " + str_c(group_factors, collapse = " + ") + "\n"
for (LV_i in seq_along(d_indicators)) {
  #skip g factor
  if (LV_i == 1) next
  
  #set loadings
  LV_name = names(d_indicators[LV_i])
  model = model + LV_name + " =~ " + d_indicators[LV_i] + "\n"
}

fit = sem(model, data = d_case, std.lv = T, orthogonal = F)
summary(fit, standardized = T)
parameterEstimates(fit, standardized = T)

semPaths(fit, "model", "std", layout = "tree2", residuals = F, exoCov = F)
