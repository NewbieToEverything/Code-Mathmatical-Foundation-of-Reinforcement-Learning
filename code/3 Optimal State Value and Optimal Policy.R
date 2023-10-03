# The illustrative examples in the chapter 3 of "Mathematical Foundation of 
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
gamma <- 0.9


# 3.1 Motivating example --------------------------------------------------

n_state <- 4
n_action <- 5

# policy, green arrows
pi_a_sc <- matrix(0, nrow = n_state, ncol = n_action)
pi_a_sc[1, 2] <- 1 
pi_a_sc[2, 3] <- 1 
pi_a_sc[3, 2] <- 1 
pi_a_sc[4, 5] <- 1

# P(s'|s,a)
P_sn_sc_a <- P_sn_sc_a_grid(c(2, 2))

# \sum_r P(r|s,a)r
exp_r_sc_a <- exp_r_sc_a_grid(c(2, 2), 2, 4)

# state values
results <- policy_evaluation(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  pi_a_sc = pi_a_sc,
  is_closedform_solution = T
)
sv1 <- results$state_value
q_1 <- results$action_value
print(matrix(sv1, nrow = 2, ncol = 2, byrow = TRUE), digits = 2)
print(q_1, digits = 2)
