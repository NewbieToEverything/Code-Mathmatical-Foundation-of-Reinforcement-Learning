# The illustrative examples in the chapter 7 of "Temporal difference learning".
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

# Figure 7.1 --------------------------------------------------------------

# P(s'|s,a)

n_state <- 25L
n_action <- 5L

P_sn_sc_a <- P_sn_sc_a_grid(c(5L, 5L))

# \sum_r P(r|s,a)r
exp_r_sc_a <- exp_r_sc_a_grid(
  c(5L, 5L), 
  c(7L, 8L, 13L, 17L, 19L, 22L), 
  18L, 
  r_forbidden = -10L, 
  r_boundary = -10,
  r_target = 0, 
  r_regular = -1
)

results_sarsa <- find_optimal_policy(
  exp_r_sc_a = exp_r_sc_a, 
  P_sn_sc_a = P_sn_sc_a, 
  type_iter = 2L, 
  method = "Sarsa", 
  iter_max = 100000L, 
  s_target = 18L, 
  n_episode_max = 80000L
)

# Figure 7.2
