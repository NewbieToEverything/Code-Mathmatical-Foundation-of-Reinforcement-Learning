# The illustrative examples in the chapter 5 of "Mathematical Foundation of 
#   Reinforcement learning".
# a1: upward
# a2: rightward
# a3: downward
# a4: leftward
# a5: stay
# sc: current state
# sN: next state
# pi_a_sc: policy, a n_state \times n_action matrix with element = pi(a|sc)
# P_sn_sc_a: a n_state \times n_state times n_action 3D array with element = p(sn|sc, a)
# exp_r_sc_a: a n_state \times n_action matrix with element = \sum_r p(r|sc, a)*r

path_code <- "code/"
source(paste(path_code, "functions.R", sep = ""))
gamma <-  0.9
set.seed(123L)

# Figure 5.2  -------------------------------------------------------------

P_sn_sc_a <- P_sn_sc_a_grid(c(3L, 3L))
exp_r_sc_a <- exp_r_sc_a_grid(c(3L, 3L), c(6L, 7L), 9L)

results <- find_optimal_policy(
  exp_r_sc_a = exp_r_sc_a,
  P_sn_sc_a = P_sn_sc_a,
  type_iter = 3L,
  iter_max = 100L,
  method = "MCbasic",
  length_episode = 4L
)
print(results$policy)
print(matrix(results$state_value, nrow = 3L, byrow = TRUE), digits = 2)
print(results$action_value, digits = 2)

# 5.2.3 Illustrative example ----------------------------------------------

# P(s'|s,a)

n_state <- 25L
n_action <- 5L

P_sn_sc_a <- P_sn_sc_a_grid(c(5L, 5L))

# \sum_r P(r|s,a)r
exp_r_sc_a <- exp_r_sc_a_grid(
  c(5L, 5L), c(7L, 8L, 13L, 17L, 19L, 22L), 18L, r_forbidden = -10L
)

for (ile in c(1L, 2L, 3L, 4L, 14L, 15L, 30L, 100L)) { # 
  cat("-------------length of episode = ", ile, sep = "", "-------------\n")
  results <- find_optimal_policy(
    exp_r_sc_a = exp_r_sc_a,
    P_sn_sc_a = P_sn_sc_a,
    type_iter = 2L,
    iter_max = 1000L,
    method = "MCbasic",
    length_episode = ile
  )
  # print(results$policy)
  print(matrix(results$state_value, nrow = 5L, byrow = TRUE), digits = 2L)
  # print(results$action_value, digits = 2)
}


# Figure 5.5 --------------------------------------------------------------

# ϵ = 0
pi_a_sc <- matrix(0L, nrow = n_state, ncol = n_action)
pi_a_sc[c(6L, 7L, 11L, 16L, 21L, 23L), 1L] <- 1L  # upward
pi_a_sc[c(1L:4L, 8L, 9L, 14L, 17L, 22L), 2L] <- 1L  # right
pi_a_sc[c(5L, 10L, 13L, 15L, 20L), 3L] <- 1L  # downward
pi_a_sc[c(12L, 19L, 24L, 25L), 4L] <- 1L  # left
pi_a_sc[c(18L), 5L] <- 1L  # stay

results_epsilon_0 <- policy_evaluation(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  pi_a_sc = pi_a_sc, 
  is_closedform_solution = T
)
cat("-------------Figure 5.5 a, ϵ = 0, value iteration -------------\n")
print(
  matrix(results_epsilon_0$state_value, nrow = 5L, ncol = 5L, byrow = TRUE), 
  digits = 2L
)

# ϵ = 0.1
epsilon <- 0.1
epsilon_greedy <- 1L - epsilon * (n_action - 1L) / n_action
pi_a_sc <- matrix(epsilon / n_action, nrow = n_state, ncol = n_action)
pi_a_sc[c(6L, 7L, 11L, 16L, 21L, 23L), 1L] <- epsilon_greedy  # upward
pi_a_sc[c(1L:4L, 8L, 9L, 14L, 17L, 22L), 2L] <- epsilon_greedy  # right
pi_a_sc[c(5L, 10L, 13L, 15L, 20L), 3L] <- epsilon_greedy  # downward
pi_a_sc[c(12L, 19L, 24L, 25L), 4L] <- epsilon_greedy  # left
pi_a_sc[c(18L), 5L] <- epsilon_greedy  # stay

results_epsilon_0.1 <- policy_evaluation(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  pi_a_sc = pi_a_sc, 
  is_closedform_solution = TRUE
)
cat("-------------Figure 5.5 b, ϵ = 0.1, policy evaluation -------------\n")
print(
  matrix(results_epsilon_0.1$state_value, nrow = 5L, ncol = 5L, byrow = TRUE), 
  digits = 2L
)

# ϵ = 0.2
epsilon <- 0.2
epsilon_greedy <- 1L - epsilon * (n_action - 1L) / n_action
pi_a_sc <- matrix(epsilon / n_action, nrow = n_state, ncol = n_action)
pi_a_sc[c(6L, 7L, 11L, 16L, 21L, 23L), 1L] <- epsilon_greedy  # upward
pi_a_sc[c(1L:4L, 8L, 9L, 14L, 17L, 22L), 2L] <- epsilon_greedy  # right
pi_a_sc[c(5L, 10L, 13L, 15L, 20L), 3L] <- epsilon_greedy  # downward
pi_a_sc[c(12L, 19L, 24L, 25L), 4L] <- epsilon_greedy  # left
pi_a_sc[c(18L), 5L] <- epsilon_greedy  # stay

results_epsilon_0.2 <- policy_evaluation(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  pi_a_sc = pi_a_sc, 
  is_closedform_solution = TRUE
)
cat("-------------Figure 5.5 c, ϵ = 0.2, policy evaluation -------------\n")
print(
  matrix(results_epsilon_0.2$state_value, nrow = 5L, ncol = 5L, byrow = TRUE), 
  digits = 2L
)

# ϵ = 0.5
epsilon <- 0.5
epsilon_greedy <- 1L - epsilon * (n_action - 1L) / n_action
pi_a_sc <- matrix(epsilon / n_action, nrow = n_state, ncol = n_action)
pi_a_sc[c(6L, 7L, 11L, 16L, 21L, 23L), 1L] <- epsilon_greedy  # upward
pi_a_sc[c(1L:4L, 8L, 9L, 14L, 17L, 22L), 2L] <- epsilon_greedy  # right
pi_a_sc[c(5L, 10L, 13L, 15L, 20L), 3L] <- epsilon_greedy  # downward
pi_a_sc[c(12L, 19L, 24L, 25L), 4L] <- epsilon_greedy  # left
pi_a_sc[c(18L), 5L] <- epsilon_greedy  # stay

results_epsilon_0.5 <- policy_evaluation(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  pi_a_sc = pi_a_sc, 
  is_closedform_solution = TRUE
)
cat("-------------Figure 5.5 d, ϵ = 0.5, policy evaluation -------------\n")
print(
  matrix(results_epsilon_0.5$state_value, nrow = 5L, ncol = 5L, byrow = TRUE), 
  digits = 2L
)


# Figure 5.6 --------------------------------------------------------------

# search for optimal ϵ-greedy policy
epsilon <- 0
results_epsilon_0 <- find_optimal_policy(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  type_iter = 2L, 
  iter_max = 50000L,
  method = "MCes", 
  is_on_policy = TRUE, 
  length_episode = 100L,
  epsilon = epsilon
) # n_iter = 9848
cat("----Figure 5.6 a, ϵ = 0, policy iteration first, then evaluate abtained policy -----\n")
print(results_epsilon_0$policy, digits = 2L)
print(matrix(results_epsilon_0$state_value, nrow = 5L, ncol = 5L, byrow = TRUE), digits = 2L)
# check whether the obtained policy is the optimal one (have identical state value, 
#   calculated using closed form solution, to its counterpart in figure 5.5)
results_epsilon_0_closed <- policy_evaluation(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  pi_a_sc = results_epsilon_0$policy, 
  is_closedform_solution = T
)
print(
  matrix(results_epsilon_0_closed$state_value, nrow = 5L, ncol = 5L, byrow = TRUE), 
  digits = 2L
)

epsilon <- 0.1
results_epsilon_0.1 <- find_optimal_policy(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  type_iter = 2L, 
  iter_max = 50000L,
  method = "MCes", 
  is_on_policy = TRUE, 
  length_episode = 100L,
  epsilon = epsilon
) # n_iter = 8232
cat("----Figure 5.6 b, ϵ = 0.1, policy iteration first, then evaluate abtained policy -----\n")
print(results_epsilon_0.1$policy, digits = 2L)
print(
  matrix(results_epsilon_0.1$state_value, nrow = 5L, ncol = 5L, byrow = TRUE), 
  digits = 2L
)
results_epsilon_0.1_closed <- policy_evaluation(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  pi_a_sc = results_epsilon_0.1$policy, 
  is_closedform_solution = TRUE
)
print(
  matrix(results_epsilon_0.1_closed$state_value, nrow = 5L, ncol = 5L, byrow = TRUE), 
  digits = 2L
)

epsilon <- 0.2
results_epsilon_0.2 <- find_optimal_policy(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  type_iter = 2L, 
  iter_max = 50000L,
  method = "MCes", 
  is_on_policy = TRUE, 
  length_episode = 100L,
  epsilon = epsilon
) # n_iter = 11601
cat("----Figure 5.6 c, ϵ = 0.2, policy iteration first, then evaluate abtained policy -----\n")
print(results_epsilon_0.2$policy, digits = 2L)
print(
  matrix(results_epsilon_0.2$state_value, nrow = 5L, ncol = 5L, byrow = TRUE), 
  digits = 2L
)
results_epsilon_0.2_closed <- policy_evaluation(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  pi_a_sc = results_epsilon_0.2$policy,
  is_closedform_solution = T
)
print(
  matrix(results_epsilon_0.2_closed$state_value, nrow = 5L, ncol = 5L, byrow = TRUE), 
  digits = 2L
)

epsilon <- 0.5
results_epsilon_0.5 <- find_optimal_policy(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  type_iter = 2L, 
  iter_max = 50000L,
  method = "MCes", 
  is_on_policy = TRUE, 
  length_episode = 100L,
  epsilon = epsilon
) # n_iter = 19351
cat("----Figure 5.6 d, ϵ = 0.5, policy iteration first, then evaluate abtained policy -----\n")
print(results_epsilon_0.5$policy, digits = 2L)
print(
  matrix(results_epsilon_0.5$state_value, nrow = 5L, ncol = 5L, byrow = TRUE), 
  digits = 2L
)
results_epsilon_0.5_closed <- policy_evaluation(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  pi_a_sc = results_epsilon_0.5$policy,
  is_closedform_solution = TRUE
)
print(
  matrix(results_epsilon_0.5_closed$state_value, nrow = 5L, ncol = 5L, byrow = TRUE), 
  digits = 2L
)
