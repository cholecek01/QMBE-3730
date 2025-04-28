# Problem 1
# a
install.packages('DiagrammeR')
library(DiagrammeR)

grViz("
digraph decision_tree {
  
  graph [layout = dot, rankdir = TB]

  node [shape = rectangle, style=filled, fillcolor=lightblue]

  Start -> Optimistic
  Start -> Conservative
  Start -> Minimax

  Optimistic -> CompareMaxPayoffs
  CompareMaxPayoffs -> D1_250
  CompareMaxPayoffs -> D2_100
  D1_250 -> ChooseD1

  Conservative -> CompareMinPayoffs
  CompareMinPayoffs -> D1_25
  CompareMinPayoffs -> D2_75
  D2_75 -> ChooseD2_Conservative

  Minimax -> CompareMaxRegrets
  CompareMaxRegrets -> D1_225
  CompareMaxRegrets -> D2_175
  D2_175 -> ChooseD2_Minimax

  Start [label = 'Start']
  Optimistic [label = 'Optimistic (Maximax)']
  Conservative [label = 'Conservative (Maximin)']
  Minimax [label = 'Minimax Regret']

  CompareMaxPayoffs [label = 'Compare max payoffs']
  D1_250 [label = 'd1 → 250']
  D2_100 [label = 'd2 → 100']
  ChooseD1 [label = 'Choose d1']

  CompareMinPayoffs [label = 'Compare min payoffs']
  D1_25 [label = 'd1 → 25']
  D2_75 [label = 'd2 → 75']
  ChooseD2_Conservative [label = 'Choose d2']

  CompareMaxRegrets [label = 'Compare max regrets']
  D1_225 [label = 'd1 → 225']
  D2_175 [label = 'd2 → 175']
  ChooseD2_Minimax [label = 'Choose d2']
}
")

# b

nature <- matrix(c(250, 100, 25,
                   100, 100, 75),
                 nrow = 2, byrow = TRUE)

rownames(nature) <- c("d1", "d2")
colnames(nature) <- c("s1", "s2", "s3")

nature

optimistic_decision <- apply(nature, 1, max)
optimistic_decision

conservative_decision <- apply(nature, 1, min)
conservative_decision

regret <- max(nature) - nature
regret

max_regret <- apply(regret, 1, max)
max_regret

# Using the Maximax approach I would select decision 1 since its highest payoff 250, is greater than decision 2's highest payoff of 100.
# Following the Maximin method decision 2 is the better choice because its worst-case payoff 75, is higher than the worst-case payoff of 25 from decision 1.
# Under the Minimax strategy, I would opt for decision 2 as its maximum regret 175 is less than the maximum regret for decision 1 225.
# Overall choosing decision 2 appears to be the safer and wiser option since it involves less risk compared to decision 1.

# Problem 2
# a
# The decision i need to make is the size of the plant.
# The decision at hand involves choosing the size of the plant to build. The uncertainty lies in the future demand, which could end up being low, medium, or high.

# b

library(DiagrammeR)

grViz("
digraph decision_tree {

graph [layout = dot, rankdir = TB]

node [shape = rectangle, style=filled, fillcolor=lightpink]

Start -> Optimistic
Start -> Conservative
Start -> Minimax

Optimistic -> CompareMaxPayoffs
CompareMaxPayoffs -> D1_200
CompareMaxPayoffs -> D2_500
D2_500 -> ChooseD2_Optimistic

Conservative -> CompareMinPayoffs
CompareMinPayoffs -> D1_150
CompareMinPayoffs -> D2_50
D1_150 -> ChooseD1_Conservative

Minimax -> CompareMaxRegrets
CompareMaxRegrets -> D1_350
CompareMaxRegrets -> D2_450
D1_350 -> ChooseD1_Minimax

Start [label = 'Start']

Optimistic [label = 'Optimistic (Maximax)']
Conservative [label = 'Conservative (Maximin)']
Minimax [label = 'Minimax Regret']

CompareMaxPayoffs [label = 'Compare max payoffs']
D1_200 [label = 'd1 → 200']
D2_500 [label = 'd2 → 500']
ChooseD2_Optimistic [label = 'Choose d2']

CompareMinPayoffs [label = 'Compare min payoffs']
D1_150 [label = 'd1 → 150']
D2_50 [label = 'd2 → 50']
ChooseD1_Conservative [label = 'Choose d1']

CompareMaxRegrets [label = 'Compare max regrets']
D1_350 [label = 'd1 → 350']
D2_450 [label = 'd2 → 450']
ChooseD1_Minimax [label = 'Choose d1']
}
")

# c
Demand <- matrix(c(150, 200, 200,
                   50, 200, 500),
                 nrow = 2, byrow = TRUE)

rownames(Demand) <- c("d1", "d2")
colnames(Demand) <- c("s1", "s2", "s3")

Demand

Demandoptimistic_decision <- apply(Demand, 1, max)
optimistic_decision

conservative_decision <- apply(Demand, 1, min)
conservative_decision

regret <- max(Demand) - Demand
regret

max_regret <- apply(regret, 1, max)
max_regret

# Using the Maximax approach, I would go with decision 2 as its best possible payoff of 500 is much greater than decision 1’s best outcome of 200.
# Following the Maximin method decision 1 is preferable because its worst-case payoff 150 is better than the worst-case for decision 2 only 50.
# Under the Minimax strategy decision 1 would be chosen because its maximum regret value 350 is smaller than the maximum regret for decision 2 450.
# Overall, selecting the smaller plant appears to be the safer and more cautious choice since it carries lower risk compared to the larger plant option.

# problem 4
# a
  # Computers:
    #10(0.2) + 8(0.5) + 6(0.3) = 7.8
  # Finacial Services
    #6(0.2) + 6(0.5) + 5(0.3) = 5.7
  # Manufacturing
    #8(0.2) + 5(0.5) + 4(0.3) = 5.3
  # Pharmaceuticals
    #6(0.2) + 4(0.5) + 2(0.3) = 3.8

# After calculating the expected values the Computers sector had the highest result 7.8 meaning Southland should invest in that market segment.

# b
# Computers:
#10(0.4) + 8(0.4) + 6(0.2) = 8.4
# Finacial Services
#6(0.4) + 6(0.4) + 5(0.2) = 5.8
# Manufacturing
#8(0.4) + 5(0.4) + 4(0.2) = 6.0
# Pharmaceuticals
#6(0.4) + 4(0.4) + 2(0.2) = 4.4 

# Even with the updated probabilities the Computers segment continues to offer the best expected value 8.4, so Southland’s best investment remains in computers.

# problem 6
# a

  # Decision 1 = 10(0.2) + 1(o.8) = 2.8
  # Decision 2 = 4(0.2) + 3(0.8) = 3.2
  # Based on the expected value calculations Decision 2 has a higher expected return 3.2 compared to Decision 1 2.8, so Decision 2 is the better choice.

# b
p_s1 <- 0.2
p_s2 <- 0.8

EV_d2 <- 3.2

y_fixed <- 1
x_needed <- (EV_d2 - p_s2 * y_fixed) / p_s1
x_needed

x_fixed <- 10
y_needed <- (EV_d2 - p_s1 * x_fixed) / p_s2
y_needed

change_in_x <- x_needed - 10
change_in_y <- y_needed - 1

cat("To make d1 optimal:")
cat("\n - Increase S1 payoff from 10 to", round(x_needed,2), "(change of", round(change_in_x,2), ")")
cat("\n - OR increase S2 payoff from 1 to", round(y_needed,2), "(change of", round(change_in_y,2), ")")

if (abs(change_in_x) > abs(change_in_y)) {
  cat("\n\nSolution is more sensitive to S2 payoff.")
} else {
  cat("\n\nSolution is more sensitive to S1 payoff.")
}

# The analysis shows that the outcome is more influenced by changes in the S2 payoff since it requires a smaller adjustment to make Decision 1 optimal.

