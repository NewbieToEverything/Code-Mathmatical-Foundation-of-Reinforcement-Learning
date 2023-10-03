# The illustrative examples in the chapter 4 of "Mathematical Foundation of 
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

# 3.5 Baseline example ----------------------------------------------------

# P(s'|s,a)
P_sn_sc_a <- P_sn_sc_a_grid(c(5L, 5L))

# \sum_r P(r|s,a)r
exp_r_sc_a <- exp_r_sc_a_grid(
  c(5L, 5L), 
  c(7L, 8L, 13L, 17L, 19L, 22L), 
  18L, 
  r_forbidden = -1L, 
  r_target = 1L, 
  r_boundary = -1L, 
  r_regular = 0L
)

# gamma = 0.9
results_0.9 <- find_optimal_policy(
  exp_r_sc_a = exp_r_sc_a,
  P_sn_sc_a = P_sn_sc_a,
  type_iter = 1L,
  method = "fromSV"
)
print(results_0.9$policy)
print(matrix(results_0.9$state_value, nrow = 5L, byrow = TRUE), digits = 2L)

# gamma = 0.5
results_0.5 <- find_optimal_policy(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  gamma = 0.5,
  type_iter = 1L,
  method = "fromSV"
)
print(results_0.5$policy)
print(matrix(results_0.5$state_value, nrow = 5L, byrow = TRUE), digits = 2L)

# gamma = 0
results_0 <- find_optimal_policy(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  gamma = 0L,
  type_iter = 1L,
  method = "fromSV"
)
print(results_0$policy)
print(matrix(results_0$state_value, nrow = 5L, byrow = TRUE), digits = 2L)

# r_forbidden = -10
exp_r_sc_a <- exp_r_sc_a_grid(
  c(5L, 5L), 
  c(7L, 8L, 13L, 17L, 19L, 22L), 
  18L, 
  r_forbidden = -10L, 
  r_target = 1L, 
  r_boundary = -1L, 
  r_regular = 0L
)
results_n10 <- find_optimal_policy(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  gamma = 0.9,
  type_iter = 2L,
  method = "fromSV"
)
print(results_n10$policy)
print(matrix(results_n10$state_value, nrow = 5L, byrow = TRUE), digits = 2L)


