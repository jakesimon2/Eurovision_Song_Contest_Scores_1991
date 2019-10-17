library(readxl) # ONLY NEED ONCE TO INSTALL PACKAGE!
# Number of countries participating in the 1991 Eurovision Song Contest
num_cou <- 22
# Source of data: eschrome.net
# Imports a tibble storing the votes of each country in columns, 
# and the countries receiving these votes in rows.
# range prevents the row label (in the xlsx) from being read by R.
score_import <- read_xlsx("~/Documents/Messing With R/ESC_1991_Scoring_Grid.xlsx", 
                          sheet = "Sheet1", range = "B1:W23")
# Make a data frame scoring_grid from the score_import tibble
scoring_grid <- as.data.frame(score_import)
# Countries receiving votes become the row labels.
rows_import <- read_xlsx("~/Documents/Messing With R/ESC_1991_Scoring_Grid.xlsx", 
                         sheet = "Sheet1", range = "A1:A23")
dimnames(scoring_grid)[1] <- rows_import
# Shows the scoring grid in a chart of rows and columns.
View(scoring_grid)
# Stores the country names in a vector format
countries <- as.vector(rep("", times = num_cou))
for (i in 1:num_cou) {
  countries[i] <- rows_import[i,1]
}
# Matrix that shows the total score for each country after each round of voting
score_by_round <- matrix(data = 0, nrow = num_cou, ncol = num_cou, byrow = FALSE, 
                         dimnames = list(countries, countries))
for (round in 1:num_cou) {
  for (cou in 1:num_cou) {
    score_by_round[cou, round] <- sum(scoring_grid[cou, 1:round])
  }
}
# Shows the score_by_round grid in a chart of rows and columns.
View(score_by_round)
# Graph the scores for "round" rounds, with a y-limit of "y_limit"
round <- 22
# round must be less than or equal to num_cou
if (round > num_cou) { round <- num_cou }
y_limit <- max(score_by_round) + 5
my_colors <- rainbow(num_cou)
# Graphs the scores of each country by round
plot(1:round, score_by_round[i,1:round], ylim = c(0, y_limit), 
     xlab = "Countries by round", ylab = "Number of points received", type = "o", 
     col = my_colors[i], pch = "*")

# all countries are graphed, but the x-axis limit is from 1 to round
for (i in 2:num_cou) {
  # This will analyze France's score in particular
  if (row.names(score_by_round)[i] == "France") {
    points(1:round, score_by_round[i,1:round], col = my_colors[i], pch = "S")
    lines(1:round, score_by_round[i,1:round], col = my_colors[i], lty = 1)
  }
  # This will analyze Sweden's score in particular
  else if (row.names(score_by_round)[i] == "Sweden") {
    my_colors[i] <- "blue"
    points(1:round, score_by_round[i,1:round], col = my_colors[i], pch = "F")
    lines(1:round, score_by_round[i,1:round], col = my_colors[i], lty = 1)
  }
  else { # Everyone else will be simply be given colors from the "rainbow" palette
    points(1:round, score_by_round[i,1:round], col = my_colors[i], pch = "*")
    lines(1:round, score_by_round[i,1:round], col = my_colors[i], lty = 1)
  }
}
# legend(x-coord (left = 0), y-coord (bottom = 0), countries, colors, line type, 
# symbol type, # of columns, size of legend proportional to default size)
# par(xpd = T, mar = par()$mar + 0.1)
legend("topleft", 100, countries, col = my_colors, lty = 1, pch = "*",
       ncol = 3, cex = round / num_cou * 0.8, text.width = round / num_cou * 2 * 0.8, 
       seg.len = round / num_cou * 0.8)