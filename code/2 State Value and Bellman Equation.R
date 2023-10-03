# The illustrative examples in the chapter 2 of "Mathematical Foundation of 
#   Reinforcement learning".
# a1: upward
# a2: rightward
# a3: downward
# a4: leftward
# a5: stay
# sc: current state
# sn: next state
# pi_a_sc: policy, a n_state \times n_action matrix with element = \pi(a|sc)
# P_sn_sc_a: a n_state \times n_state times n_action 3D array with element = p(sn|sc, a)
# exp_r_sc_a: a n_state \times n_action matrix with element = \sum_r p(r|sc, a)*r
path_code <- "code/"
source(paste(path_code, "functions.R", sep = ""))
gamma <- 0.9

# 2.4.1 deterministic -----------------------------------------------------

n_state <- 4
n_action <- 5

# policy, green arrows
pi_a_sc <- matrix(0, nrow = n_state, ncol = n_action)
pi_a_sc[1, 3] <- 1 
pi_a_sc[2, 3] <- 1 
pi_a_sc[3, 2] <- 1 
pi_a_sc[4, 5] <- 1

# P(s'|s,a)
P_sn_sc_a <- P_sn_sc_a_grid(c(2, 2))

# \sum_r P(r|s,a)r
exp_r_sc_a <- exp_r_sc_a_grid(c(2, 2), 2, 4)

results <- policy_evaluation(exp_r_sc_a, P_sn_sc_a, pi_a_sc, is_closedform_solution = T)
sv1 <- results$state_value
q_1 <- results$action_value


# 2.4.1 stochastic --------------------------------------------------------

# policy, green arrows
pi_a_sc[1, 2] <- pi_a_sc[1, 3] <- 0.5

results <- policy_evaluation(exp_r_sc_a, P_sn_sc_a, pi_a_sc, is_closedform_solution = T)
sv2 <- results$state_value
q_2 <- results$action_value


# 2.5.4 illustrative examples ---------------------------------------------

n_state <- 25
n_action <- 5

# figure a up

# policy, the green arrows
pi_a_sc <- matrix(0, nrow = n_state, ncol = n_action)
pi_a_sc[6, 1] <- pi_a_sc[7, 1] <- pi_a_sc[11, 1] <- pi_a_sc[16, 1] <- 
  pi_a_sc[21, 1] <- pi_a_sc[23, 1] <- 1
pi_a_sc[1, 2] <- pi_a_sc[2, 2] <- pi_a_sc[3, 2] <- pi_a_sc[8, 2] <- 
  pi_a_sc[14, 2] <- pi_a_sc[17, 2] <- pi_a_sc[22, 2] <- 1
pi_a_sc[4, 3] <- pi_a_sc[5, 3] <- pi_a_sc[9, 3] <- pi_a_sc[10, 3] <- 
  pi_a_sc[13, 3] <- pi_a_sc[15, 3] <- pi_a_sc[20, 3] <- 1 
pi_a_sc[12, 4] <- pi_a_sc[19, 4] <- pi_a_sc[24, 4] <- pi_a_sc[25, 4] <- 1 
pi_a_sc[18, 5] <- 1

# P(s'|s, a)
P_sn_sc_a <- P_sn_sc_a_grid(c(5, 5))

# \sum_r P(r|s,a)r
exp_r_sc_a <- exp_r_sc_a_grid(c(5, 5), c(7, 8, 13, 17, 19, 22), 18)

results <- policy_evaluation(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  pi_a_sc = pi_a_sc,
  is_closedform_solution = T
)
sv3 <- results$state_value
q_3 <- results$action_value
print(matrix(sv3, nrow = 5, byrow = TRUE), digits = 2)

# figure a down

# policy, the green arrows
pi_a_sc <- matrix(0, nrow = n_state, ncol = n_action)
pi_a_sc[6, 1] <- pi_a_sc[7, 1] <- pi_a_sc[11, 1] <- pi_a_sc[16, 1] <- 
  pi_a_sc[21, 1] <- pi_a_sc[23, 1] <- 1
pi_a_sc[1, 2] <- pi_a_sc[2, 2] <- pi_a_sc[3, 2] <- pi_a_sc[8, 2] <- 
  pi_a_sc[14, 2] <- pi_a_sc[17, 2] <- pi_a_sc[22, 2] <- 1
pi_a_sc[4, 2] <- pi_a_sc[5, 3] <- pi_a_sc[9, 2] <- pi_a_sc[10, 3] <- 
  pi_a_sc[13, 3] <- pi_a_sc[15, 3] <- pi_a_sc[20, 3] <- 1 
pi_a_sc[12, 4] <- pi_a_sc[19, 4] <- pi_a_sc[24, 4] <- pi_a_sc[25, 4] <- 1 
pi_a_sc[18, 5] <- 1

# state values
results <- policy_evaluation(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  pi_a_sc = pi_a_sc,
  is_closedform_solution = T
)
sv4 <- results$state_value
q_4 <- results$action_value
print(matrix(sv4, nrow = 5, byrow = TRUE), digits = 2)

# figure b up

# policy, the green arrows
pi_a_sc <- matrix(0, nrow = n_state, ncol = n_action)
pi_a_sc[1:25, 2] <- 1

# state values
results <- policy_evaluation(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  pi_a_sc = pi_a_sc,
  is_closedform_solution = T
)
sv5 <- results$state_value
q_5 <- results$action_value
print(matrix(sv5, nrow = 5, byrow = TRUE), digits = 2)

# figure b bottom

# policy, the green arrows
pi_a_sc <- matrix(0, nrow = n_state, ncol = n_action)
pi_a_sc[c(4, 5, 18, 19), 1] <- 1 # a1
pi_a_sc[c(1, 8, 10, 12, 20, 22, 24), 2] <- 1 # a2
pi_a_sc[c(6, 9, 13, 17), 3] <- 1 # a3
pi_a_sc[c(2, 3, 11, 14), 4] <- 1 # a4
pi_a_sc[c(7, 15, 16, 21, 23, 25), 5] <- 1 # a5

# state values
results <- policy_evaluation(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  pi_a_sc = pi_a_sc,
  is_closedform_solution = T
)
sv6 <- results$state_value
q_6 <- results$action_value
print(matrix(sv6, nrow = 5, byrow = TRUE), digits = 2)
