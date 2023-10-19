#' Generate \(P(s'|s,a)\) for grid-word example
#'
#' @param dim_grid numeric vector, 1st element = number of rows, 2nd element = number of cols.
#'
#' @return a \(n_state \times n_state \times n_action) numeric array wherein the element is \(P(s'|s, a)\).
P_sn_sc_a_grid <- function(dim_grid = NULL) {
  
  if (is.null(dim_grid)) stop("The numbers of rows and columns must be provided.")
  
  n_row <- dim_grid[1L]
  n_col <- dim_grid[2L]
  n_state <- n_row * n_col
  
  P_sn_sc_a <- array(
    0, dim = c(n_state, n_state, 5L),
    dimnames = list(
      paste("s", 1L:n_state, sep = ""), 
      paste("s", 1L:n_state, sep = ""), 
      c("a1", "a2", "a3", "a4", "a5")
    )
  )
  
  for (i in 1L:n_state) {
    # id of state to be transited into after taking a certain action
    s_above <- i - n_col
    s_right <- i + 1L
    s_below <- i + n_col
    s_left <- i - 1L
    # take a1 and move into the state above or stay (for states in the 1st row)
    P_sn_sc_a[i, ifelse(i <= n_col, i, s_above), 1L] <- 1
    # take a2 and move into the state right or stay (for states in the last column)
    P_sn_sc_a[i, ifelse(i %% n_col == 0L, i, s_right), 2L] <- 1 # a2
    # take a3 and move into the state below or stay (for states in the last row)
    P_sn_sc_a[i, ifelse(i > (n_row - 1L) * n_col, i, s_below), 3L] <- 1 # a3
    # take a4 and move into the state left or stay (for states in the 1st column)
    P_sn_sc_a[i, ifelse(i %% n_col == 1L, i, s_left), 4L] <- 1 # a4
    P_sn_sc_a[i, i, 5L] <- 1
  }
  
  return(P_sn_sc_a)
}


#' Generate \(\sum_r P(r|s,a)r\) for grid-word example
#' 
#' @param dim_grid numeric vector, 1st element = number of rows, 2nd element = number of cols.
#' @param id_forbidden integer vector, the indices of all forbidden area, could be `NULL`, means no forbidden area. 
#' @param id_target the index of target area, must be a length-1 integer vector.
#' @param r_forbidden integer scalar, the reward of entering hidden area.
#' @param r_boundary integer scalar, the reward of hitting boundary.
#' @param r_target integer scalar, the reward of entering target area.
#' @param r_regular integer scalar, the reward of entering regular area.
#' 
#' @details
#' The index of state starts from the top-left state and increment row-wise.
#' 
#' @return a \(n_state \times n_action\) numeric matrix wherein the element is \(\sum_r P(r|s,a)r\).
exp_r_sc_a_grid <- function(dim_grid = NULL, 
                            id_forbidden = NULL, 
                            id_target = NULL,
                            r_forbidden = -1L, 
                            r_boundary = -1L,
                            r_target = 1L, 
                            r_regular = 0L) {
  
  if (is.null(dim_grid)) 
    stop("The number of rows and columns of grid must be provided.")
  if (is.null(id_target)) 
    stop("At least 1 target has to be specified.")
  
  n_row <- dim_grid[1L]
  n_col <- dim_grid[2L]
  n_state <- n_row * n_col 
  
  exp_r_sc_a <- array(
    0L, dim = c(n_state, 5L), 
    dimnames = list(
      paste("s", 1L:n_state, sep = ""),
      c("a1", "a2", "a3", "a4", "a5")
    )
  )
  
  for (i in 1L:n_state) {
    # id of state to be transited into after taking a certain action
    s_above <- i - n_col
    s_right <- i + 1L
    s_below <- i + n_col
    s_left <- i - 1L
    r_a1 <- r_a2 <- r_a3 <- r_a4 <- r_a5 <- r_regular
    
    # boundary
    # take a1 and hit boundary (states in the 1st row)
    if (i <= n_col) r_a1 <- r_boundary
    # take a2 and hit boundary (states in last column)
    if (i %% n_col == 0L) r_a2 <- r_boundary
    # take a3 and hit boundary (states in the last row)
    if (i > (n_row - 1L) * n_col) r_a3 <- r_boundary
    # take a4 and hit boundary (states in the 1st column)
    if (i %% n_col == 1L) r_a4 <- r_boundary
    
    # forbidden area
    if (!is.null(id_forbidden)) {
      if (s_above %in% id_forbidden) r_a1 <- r_forbidden # take a1 and move into forbidden area
      if (s_right %in% id_forbidden) r_a2 <- r_forbidden # take a2 and move into forbidden area
      if (s_below %in% id_forbidden) r_a3 <- r_forbidden # take a3 and move into forbidden area
      if (s_left %in% id_forbidden) r_a4 <- r_forbidden # take a4 and move into forbidden area
      if (i %in% id_forbidden) r_a5 <- r_forbidden # stay in forbidden area
    }
    
    # target area
    if (s_above == id_target) r_a1 <- r_target # take a1 and move into target area
    if (s_right == id_target) r_a2 <- r_target # take a2 and move into target area
    if (s_below == id_target) r_a3 <- r_target # take a3 and move into target area
    if (s_left == id_target) r_a4 <- r_target # take a4 and move into target area
    if (i == id_target) r_a5 <- r_target # stay in target area
    
    exp_r_sc_a[i, 1L] <- r_a1
    exp_r_sc_a[i, 2L] <- r_a2
    exp_r_sc_a[i, 3L] <- r_a3
    exp_r_sc_a[i, 4L] <- r_a4
    exp_r_sc_a[i, 5L] <- r_a5
  }
  
  return(exp_r_sc_a)
}


#' Calculate the jumping matrix
#' 
#' \(p(s'|s)=\sum_a\pi(a|s)P(s'|s,a)\).
#'
#' @param pi_a_sc the policy, a \(n_state \times n_action\) numeric matrix wherein the element is \(\pi(a|s)\).
#' @param P_sn_sc_a a \(n_state \times n_state \times n_action) numeric array wherein the element is \(P(s'|s, a)\).
#'
#' @return the jumping matrix.
P_sn_sc <- function(pi_a_sc, P_sn_sc_a) {
  n_state <- nrow(pi_a_sc)
  n_action <- ncol(pi_a_sc)
  P_jump <- matrix(0L, nrow = n_state, ncol = n_state)
  
  for (i in 1L:n_state) {
    tmp <- t(sapply(1L:n_action, \(x) P_sn_sc_a[i, , x]))
    P_jump[i, ] <- pi_a_sc[i, ] %*% tmp
  }
  
  return(P_jump)
}


#' Simulate a single episode 
#' 
#' Simulate a single episode with given starting state-action pair. Dynamic 
#'    models must be provided.
#'
#' @param s_cur integer scalar, current state.
#' @param a_cur integer scalar, current action.
#' @param exp_r_sc_a a \(n_state \times n_action\) numeric matrix wherein the element is \(\sum_r P(r|s,a)r\).
#' @param P_sn_sc_a a \(n_state \times n_state \times n_action) numeric array wherein the element is \(P(s'|s, a)\).
#' @param pi_a_sc the policy, a \(n_state \times n_action\) numeric matrix wherein the element is \(\pi(a|s)\).
#' @param length_episode integer scalar, with default = `5L`, the length of each simulated episode.
#'
#' @return a list object, recording rewards as its first element, states as its second element, and actions as its third element.
simu_single_episode <- function(s_cur, 
                                a_cur, 
                                exp_r_sc_a, 
                                P_sn_sc_a, 
                                pi_a_sc, 
                                length_episode) {
  
  if (is.null(s_cur)) stop("s_curr is NULL, current state must be provided!")
  n_state <- nrow(exp_r_sc_a)
  n_action <- ncol(exp_r_sc_a)
  
  episode <- list(
    states = vector(mode = "integer", length = length_episode + 1L),
    actions = vector(mode = "integer", length = length_episode + 1L),
    rewards = vector(mode = "double", length = length_episode)
  )
  episode$states[1L] <- s_cur # s0
  episode$actions[1L] <- a_cur  # a0

  # steps in a single episode starting from s0
  for (ieps in 1L:length_episode) {
    episode$rewards[ieps] <- exp_r_sc_a[s_cur, a_cur]  # r_t
    s_cur <- sample(n_state, 1L, prob = P_sn_sc_a[s_cur, ,a_cur])  # s_t+1
    episode$states[ieps + 1L] <- s_cur
    a_cur <- sample(n_action, 1L, prob = pi_a_sc[s_cur, ])  # a_t+1
    episode$actions[ieps + 1L] <- a_cur
  }
  
  return(episode)
}


#' Action value
#' 
#' Calculate action value using either known state values or Monte Carlo-based method.
#'
#' @param exp_r_sc_a a \(n_state \times n_action\) numeric matrix wherein the element is \(\sum_r P(r|s,a)r\).
#' @param P_sn_sc_a a \(n_state \times n_state \times n_action) numeric array wherein the element is \(P(s'|s, a)\).
#' @param pi_a_sc the policy, a \(n_state \times n_action\) numeric matrix wherein the element is \(\pi(a|s)\).
#' @param gamma double scalar, discount rate \((0,1)\).
#' @param sv numeric vector, the state value vectors. Unless Monte Carlo method is applied to calculate action value, user must provide state value.
#' @param method a character string specifying the method used to estimate action values, must be one of "Sarsa" (state-action-reward-state-action, default), "SarsaExp" (expected Sarsa), "SarsaN" (n-step Sarsa), "Q" (Q-learning), "MCbasic" (Monte Carlo basic), "MCes" (Monte Carlo exploring start), "fromSV" (calculate using known state values).
#' @param length_episode integer scalar with default = `5L`, the length of each simulated episode.
#' @param n_epi_each_sa_pair integer scalar with default = `30L`, the number of simulated episode for each state-action pair.
#' @param type_visit integer scalar with default = `1L`, corresponding to the first-visit method, `2L` = every visit method. 
#' @param Q_ini double vector, the initial action value, has to be provided when `is_exploring_starts = TRUE`. 
#' @param counts integer vector, records the number of returns used in incremental implementation to estimate action value of each state-action pair. 
#' @param min_count integer scalar, with default = `80L`, the minimum number of counts, to avoid insufficient sampling of state-action pairs in Monte Carlo ES and resultant premature stop of policy iteration. 
#' @param alpha double scalar, with default = `0.1`, learning rate. 
#' @param s_cur integer scalar, current state, must be provided if method = either "Sarsa", "SarsaExp", "SarsaN", or "Q".
#' @param a_cur integer scalar, current action, must be provided if method = either "Sarsa", "SarsaExp", "SarsaN", or "Q".
#' @param s_target integer scalar, target state, must be provided if method = either "Sarsa", "SarsaExp", "SarsaN", or "Q".
#' @param n_episode integer scalar, number of episodes to generate,  must be provided if method = either "Sarsa", "SarsaExp", "SarsaN", or "Q".
#' 
#' @return the action value \(q_\pi(s,a)\).
action_value <- function(exp_r_sc_a, 
                         P_sn_sc_a, 
                         pi_a_sc = NULL, 
                         gamma = 0.9, 
                         sv = NULL, 
                         method = "Sarsa",
                         length_episode = 5L, 
                         n_epi_each_sa_pair = 30L, 
                         type_visit = 1L, 
                         Q_ini = NULL, 
                         counts = NULL,
                         min_count = 80L, 
                         alpha = 0.1, 
                         s_cur = NULL,
                         a_cur = NULL,
                         s_target = NULL,
                         n_episode = NULL) {
  
  n_state <- nrow(exp_r_sc_a)
  n_action <- ncol(exp_r_sc_a)
  
  # calculate action value immediately from known state value
  if (method == "fromSV") {
    if (is.null(sv)) stop("State value must be provided to calculate action value.")
    # to avoid loop when calculating action values, extend v so that it has 
    #   identical dimensionality to that of P(s'|s,a)
    Q_ini <- exp_r_sc_a + gamma * 
      apply(
        P_sn_sc_a * array(t(sv)[rep(1L, n_state), ], 
                          dim = c(n_state, n_state, n_action)),
        c(1L, 3L), 
        sum
      )
  }
  
  if (method != "fromSV") {
    if (is.null(Q_ini)) {
      # initialize the action value vector, if 1st iteration
      Q_ini <- matrix(0, nrow = n_state, ncol = n_action)
    }
    # Generate all state-action pairs
    sa_pairs <- as.matrix(expand.grid(
      state = 1L:n_state, 
      action = 1L:n_action
    ))
    n_sa_pair <- nrow(sa_pairs)
    if (method != "MCbasic") {
      # initialize the counts vector, if 1st iteration, for Monte Carlo exploring
      #   starts, Sarsa, Expected Sarsa, n-step Sarsa, and Q learning 
      if (is.null(counts)) 
        counts <- vector(mode = "integer", length = n_sa_pair)
    }
  }
  
  # Set starting state-action pair for Sarsa, expected Sarsa, n-step Sarsa, Q learning, and Monte Carlo exploring starts 
  if ((method %in% c("Sarsa", "SarsaExp", "SarsaN", "Q") & is.null(s_cur) ) | method == "MCes") {
      # increase the probability of under-sampled state-action pairs
      if (any(counts < min_count)) {
        id_sa_pair <- sample((1L:n_sa_pair)[counts < min_count], 1L)
      } else {
        id_sa_pair <- sample(n_sa_pair, 1L)
      }
      s_cur <- sa_pairs[id_sa_pair, 1L]
      a_cur <- sa_pairs[id_sa_pair, 2L]
      s_cur <- 1L  # fix starting state-action pair
      a_cur <- 2L
  }
  
  # Calculate action value
  if (startsWith(method, "MC")) {
    if (method == "MCes") {
      # Monte Carlo exploring start
      # randomly pick a state-action pair and simulate a single episode for it
      Q_ini <- as.vector(Q_ini)
      episode <- simu_single_episode(
        s_cur, 
        a_cur, 
        exp_r_sc_a, 
        P_sn_sc_a, 
        pi_a_sc, 
        length_episode
      )
      
      # update the action value based on simulated single episode
      G <- 0
      id_sa_pairs <- sapply(1L:length_episode, \(x){
        which(sa_pairs[, 1L] == episode$states[x] & sa_pairs[, 2L] == episode$actions[x])
      })
      for (ieps in length_episode:1L) {
        # update the action value backward
        s_cur <- episode$states[ieps]
        a_cur <- episode$actions[ieps]
        id_sa_pair <- id_sa_pairs[ieps]
        G <- gamma * G + episode$rewards[ieps]  # records return
        is_included <- TRUE  # every visit method
        if (type_visit == 1L & ieps > 1L) {
          # first visit method
          if (!id_sa_pair %in% id_sa_pairs[1L:(ieps - 1L)]) {
            is_included <- TRUE
          } else {
            is_included <- FALSE
          }
        }
        if (is_included) {
          counts[id_sa_pair] <- counts[id_sa_pair] + 1L
          if (counts[id_sa_pair] == 1L) {
            Q_ini[id_sa_pair] <- G
          } else {
            # incremental implementation, see more details in section 2.4 (Sutton et.al., 2018)
            Q_ini[id_sa_pair] <- 
              Q_ini[id_sa_pair] + 1L * (G - Q_ini[id_sa_pair]) / counts[id_sa_pair]
          }
        }
      }
    }
    
    # Monte Carlo basic
    if (method == "MCbasic") {
      n_episode <- n_sa_pair * n_epi_each_sa_pair
      sa_pairs_rep <- sa_pairs[rep(1L:n_sa_pair, each = n_epi_each_sa_pair), ]
      row.names(sa_pairs_rep) <- NULL
      episodes_rewards <- matrix(
        0, nrow = n_episode, ncol = length_episode
      )
      episodes_states <- matrix(
        0, nrow = n_episode, ncol = length_episode
      )
      episodes_actions <- matrix(
        0, nrow = n_episode, ncol = length_episode
      )
      gammas <- matrix(
        gamma ^ (0L:(length_episode - 1L)), 
        nrow = n_episode, 
        ncol = length_episode, 
        byrow = TRUE
      )
      
      # simulate episodes for all action-state pairs with given number of episode length
      for (ie in 1L:n_episode) {
        tmp <- simu_single_episode(
          sa_pairs_rep[ie, 1L], 
          sa_pairs_rep[ie, 2L], 
          exp_r_sc_a, 
          P_sn_sc_a, 
          pi_a_sc, 
          length_episode
        )
        episodes_rewards[ie, ] <- tmp$rewards
        episodes_states[ie, ] <- tmp$states[-length_episode]
        episodes_actions[ie, ] <- tmp$actions[-length_episode]
      }
      episodes_rewards <- rowSums(episodes_rewards * gammas)
      Q_ini <- vector(mode = "double", length = n_state * n_action)
      
      for (iq in 1L:length(Q_ini)) {
        is <- sa_pairs[iq, 1L]
        ia <- sa_pairs[iq, 2L]
        Q_ini[iq] <- mean(
          episodes_rewards[sa_pairs_rep[, 1L] == is & sa_pairs_rep[, 2L] == ia]
        )
      }
    }
    Q_ini <- matrix(Q_ini, nrow = n_state, ncol = n_action)
  }
  
  if (method %in% c("Sarsa", "SarsaExp", "SarsaN", "Q")) {
    # browser()
    # Single experience in Sarsa, expected Sarsa, and Q learning contains only 1 step
    if (method != "SarsaN") length_episode <- 1L
    episode <- simu_single_episode(
      s_cur, 
      a_cur, 
      exp_r_sc_a, 
      P_sn_sc_a, 
      pi_a_sc, 
      length_episode = length_episode
    )
    
    # if (s_target %in% episode$states) {
    #   
    # }
    
    id_sa_pair <- sapply(1L:length_episode, \(x){
      which(sa_pairs[, 1L] == episode$states[x] & 
              sa_pairs[, 2L] == episode$actions[x])
    })
    counts[id_sa_pair[1L]] <- counts[id_sa_pair[1L]] + 1L
    s_t <- episode$states[1L]
    a_t <- episode$actions[1L]
    r_t_plus_1 <- episode$rewards[1L]
    s_t_plus_1 <- episode$states[length_episode + 1L]
    a_t_plus_1 <- episode$actions[length_episode + 1L]
    
    if (method == "Sarsa") Q_t_plus_1 <- Q_ini[s_t_plus_1, a_t_plus_1]
    if (method == "SarsaExp") 
      Q_t_plus_1 <- sum(pi_a_sc[s_t_plus_1, ] * Q_ini[s_t_plus_1, ])
    if (method == "SarsaN") {
      gammas <- gamma ^ seq(0L, length_episode - 2L)
      Q_t_plus_1 <- sum(gammas*episode$rewards[-1L] + 
                          gamma^(length_episode - 1L) * Q_ini[s_t_plus_1, a_t_plus_1])
    }
    if (method == "Q") Q_t_plus_1 <- max(Q_ini[s_t_plus_1, ])
    
    Q_ini[s_t, a_t] <- 
      Q_ini[s_t, a_t] - alpha * 
      (Q_ini[s_t, a_t] - (r_t_plus_1 + gamma*Q_t_plus_1))
    if (s_t_plus_1 == s_target) {
      # if target state reached, reset the starting state-action pair
      n_episode <- n_episode + 1L
      print(n_episode)
      s_cur <- NULL
      a_cur <- NULL
    } else {
      s_cur <- s_t_plus_1
      a_cur <- a_t_plus_1
    }
  }
  
  return(list(
    action_value = Q_ini, 
    counts = counts, 
    s_cur = s_cur, 
    a_cur = a_cur,
    n_episode = n_episode
  ))
}


#' Policy evaluation
#' 
#' Calculate state value and action value using either matrix solution (closed-form), or iterative solution.
#'
#' @param exp_r_sc_a a \(n_state \times n_action\) numeric matrix wherein the element is \(\sum_r P(r|s,a)r\).
#' @param P_sn_sc_a a \(n_state \times n_state \times n_action) numeric array wherein the element is \(P(s'|s, a)\).
#' @param pi_a_sc the policy, a \(n_state \times n_action\) numeric matrix wherein the element is \(\pi(a|s)\).
#' @param gamma double scalar, discount rate \((0,1)\).
#' @param sv_ini numeric vector, the initial state value vector \(v_o\).
#' @param is_closedform_solution logical scalar, with default = `FALSE`, when `FALSE`, matrix solution is used to calculate \(v_\pi(s)\), otherwise the iterative solution is applied.
#' @param stop_at_almost_equal logical scalar, with default = `FALSE`, if `TRUE`, the algorithm will stop if `all.equal(sv_est, sv_ini)`.
#' @param stopping_crit double scalar, with default = `0.0001`, the algorithm will stop if \(||v_{\pi_{k+1} - v_{\pi_{k}}|| < `stopping_crit`\).
#' @param iter_max integer scalar, with default = `1000L`, the maximum number of iterations.
#' @param method a character string specifying the method used to estimate action values, must be one of "Sarsa" (state-action-reward-state-action, default), "SarsaExp" (expected Sarsa), "SarsaN" (n-step Sarsa), "Q" (Q-learning), "MCbasic" (Monte Carlo basic), "MCes" (Monte Carlo exploring start), "fromSV" (calculate using known state values).
#' @param length_episode integer scalar with default = `5L`, the length of each simulated episode.
#' @param n_epi_each_sa_pair integer scalar with default = `30L`, the number of simulated episode for each state-action pair.
#' @param type_visit integer scalar with default = `1L`, corresponding to the first-visit method, `2L` = every visit method. 
#' @param Q_ini double vector, the initial action value, has to be provided when `is_exploring_starts = TRUE`. 
#' @param counts integer vector, records the number of returns used in incremental implementation to estimate action value of each state-action pair. 
#' @param min_count integer scalar, with default = `80L`, the minimum number of counts, to avoid insufficient sampling of state-action pairs in Monte Carlo ES and resultant premature stop of policy iteration. 
#' @param alpha double scalar, with default = `0.1`, learning rate. 
#' @param s_cur integer scalar, current state, must be provided if method = either "Sarsa", "SarsaExp", "SarsaN", or "Q".
#' @param a_cur integer scalar, current action, must be provided if method = either "Sarsa", "SarsaExp", "SarsaN", or "Q".
#' @param s_target integer scalar, target state, must be provided if method = either "Sarsa", "SarsaExp", "SarsaN", or "Q".
#' @param n_episode integer scalar, number of episodes to generate,  must be provided if method = either "Sarsa", "SarsaExp", "SarsaN", or "Q".
#'
#' @return the state value vector \(v_\pi(s)\) and the action value matrix \(q_{(s,a)}\).
policy_evaluation <- function(exp_r_sc_a, 
                              P_sn_sc_a, 
                              pi_a_sc, 
                              gamma = 0.9, 
                              sv_ini = NULL,
                              is_closedform_solution = FALSE, 
                              stop_at_almost_equal = FALSE,
                              stopping_crit = 0.0001, 
                              iter_max = 1000L,
                              method = "Sarsa", 
                              length_episode = 5L, 
                              n_epi_each_sa_pair = 30L,
                              type_visit = 1L,  
                              Q_ini = NULL, 
                              counts = NULL, 
                              min_count = 80L, 
                              alpha = 0.1,
                              s_cur = NULL,
                              a_cur = NULL,
                              s_target = NULL,
                              n_episode = NULL) {
  
  n_state <- nrow(exp_r_sc_a)
  if (is.null(pi_a_sc))
    stop("policy must be provided for policy evaluation")
  
  if (is_closedform_solution) {
    # closed-form solution
    # closed-form means that dynamic models are known, therefore no need to use 
    #   Monte Carlo method to estimate action value
    
    # the jumping matrix \(P(s'|s)\).
    P <- P_sn_sc(pi_a_sc, P_sn_sc_a)
    # the expected immediate reward vector for all state.
    r <- matrix(rowSums(pi_a_sc * exp_r_sc_a), nrow = n_state, ncol = 1L)
    sv_est <- solve(diag(n_state) - gamma * P) %*% r
    
    # because exact state value is available, no need to use Monte Carlo method
    results <- action_value(
      exp_r_sc_a = exp_r_sc_a, 
      P_sn_sc_a = P_sn_sc_a, 
      pi_a_sc = pi_a_sc, 
      gamma = gamma, 
      sv = sv_est,
      method = "fromSV"
    )
  } 
  
  # iterative solution
  if (!is_closedform_solution) {
    if (is.null(sv_ini)) sv_ini <- matrix(0L, nrow = n_state)
    for (i in 1L:iter_max) {
      # calculate action value, use either model-based method, Monte Carlo methods
      #   (Monte Carlo basic and Monte Carlo ES), or TD methods (Sarsa)
      results <- action_value(
        exp_r_sc_a = exp_r_sc_a, 
        P_sn_sc_a = P_sn_sc_a, 
        pi_a_sc = pi_a_sc, 
        gamma = gamma, 
        sv = sv_ini, 
        method = method,
        length_episode = length_episode, 
        n_epi_each_sa_pair = n_epi_each_sa_pair,
        type_visit = type_visit,
        Q_ini = Q_ini,
        counts = counts,
        min_count = min_count, 
        alpha = alpha, 
        s_cur = s_cur, 
        a_cur = a_cur, 
        s_target = s_target, 
        n_episode = n_episode
      )
      
      # calculate state value using action value
      Q_est <- results$action_value
      sv_est <- rowSums(pi_a_sc * Q_est)
      counts <- results$counts
      
      if (method %in% c("Sarsa", "SarsaExp", "SarsaN", "Q")) {
        s_cur <- results$s_cur
        a_cur <- results$a_cur
        n_episode <- results$n_episode
      }
      
      # no need for iteration if estimating action value using Monte Carlo basic, 
      #   Monte Carlo exploring starts, Sarsa, expected Sarsa, n-step Sarsa and Q learning
      if (method != "fromSV") break
      
      # when to stop
      stop_cond_sv <- ifelse(
        stop_at_almost_equal, 
        isTRUE(all.equal(sv_est, sv_ini)), 
        sqrt(sum((sv_est - sv_ini) ^ 2L)) <= stopping_crit
      )
      stop_cond_Q <- ifelse(
        stop_at_almost_equal, 
        isTRUE(all.equal(Q_est, Q_ini)), 
        sqrt(sum((Q_est - Q_ini) ^ 2L)) <= stopping_crit
      )
      
      if (stop_cond_sv & stop_cond_Q) {
        # update action value use the latest state value
        results <- action_value(
          exp_r_sc_a = exp_r_sc_a, 
          P_sn_sc_a = P_sn_sc_a, 
          pi_a_sc = pi_a_sc, 
          gamma = gamma, 
          sv = sv_ini, 
          method = method,
          length_episode = length_episode, 
          n_epi_each_sa_pair = n_epi_each_sa_pair,
          type_visit = type_visit,
          Q_ini = Q_ini,
          counts = counts,
          min_count = min_count, 
          alpha = alpha
        )
        break
      } else {
        sv_ini <- sv_est
        Q_ini <- Q_est
      }
    }
  }
  
  return(list(
    state_value = sv_est, 
    action_value = results$action_value, 
    counts = results$counts,
    s_cur = s_cur,
    a_cur = a_cur,
    n_episode = n_episode
  ))
}


#' Find optimal policy
#' 
#' Find the optimal policy using either Value iteration, policy iteration, or truncated policy iteration.
#'
#' @param exp_r_sc_a a \(n_state \times n_action\) numeric matrix wherein the element is \(\sum_r P(r|s,a)r\).
#' @param P_sn_sc_a a \(n_state \times n_state \times n_action) numeric array wherein the element is \(P(s'|s, a)\).
#' @param pi_a_sc_ini the initial policy, a \(n_state \times n_action\) numeric matrix wherein the element is \(\pi(a|s)\).
#' @param gamma double scalar, discount rate \((0,1)\).
#' @param is_closedform_solution logical scalar with default = `FALSE`, when `FALSE`, matrix solution is used to calculate \(v_\pi(s)\), otherwise the iterative solution is applied.
#' @param stop_at_almost_equal logical scalar with default = `FALSE`, if `TRUE`, the algorithm will stop if `all.equal(sv_est, sv_ini)`.
#' @param stopping_crit numeric scalar with default = 0.0001, the algorithm will stop if \(||v_{\pi_{k+1} - v_{\pi_{k}}|| < `stopping_crit`\).
#' @param sv_ini numeric vector, the initial state value vector \(v_0\) or \(v_{\pi_0}\).
#' @param type_iter integer scalar with default = `2L`, `1L`= value iteration, `2L`= policy iteration, `3L`= truncated policy iteration.
#' @param iter_max integer scalar with default = `1000L`, the maximum number of iterations.
#' @param iter_truncate integer scalar with default = `500L`, the maximum number of \(j_truncate\).
#' @param method a character string specifying the method used to estimate action values, must be one of "Sarsa" (state-action-reward-state-action, default), "SarsaExp" (expected Sarsa), "SarsaN" (n-step Sarsa), "Q" (Q-learning), "MCbasic" (Monte Carlo basic), "MCes" (Monte Carlo exploring start), "fromSV" (calculate using known state values).
#' @param is_on_policy logical scalar with default = `TRUE`, whether to use on-policy (soft).
#' @param epsilon double scalar with default = `0.1`.
#' @param length_episode integer scalar with default = `5L`, the length of each simulated episode.
#' @param n_epi_each_sa_pair integer scalar with default = `30L`, the number of simulated episode for each state-action pair.
#' @param type_visit integer scalar with default = `1L`, corresponding to the first-visit method, `2L` = every visit method. 
#' @param min_count integer scalar, with default = `80L`, the minimum number of counts, to avoid insufficient sampling of state-action pairs in Monte Carlo ES and resultant premature stop of policy iteration. 
#' @param alpha double scalar, with default = `0.1`, learning rate. 
#' @param s_target integer scalar, target state, must be provided if method = either "Sarsa", "SarsaExp", "SarsaN", or "Q".
#' @param n_episode integer scalar, number of episodes to generate,  must be provided if method = either "Sarsa", "SarsaExp", "SarsaN", or "Q".
#'
#' @return a list containing optimal policy and optimal state values.
find_optimal_policy <- function(exp_r_sc_a, 
                                P_sn_sc_a, 
                                pi_a_sc_ini = NULL, 
                                gamma = 0.9,
                                is_closedform_solution = FALSE,
                                stop_at_almost_equal = FALSE,
                                stopping_crit = 0.0001, 
                                sv_ini = NULL,
                                type_iter = 2L,
                                iter_max = 1000L,
                                iter_truncate = 10L,
                                method = "Sarsa",
                                is_on_policy = TRUE,
                                epsilon = 0.1,
                                length_episode = 30L, 
                                n_epi_each_sa_pair = 30L, 
                                type_visit = 1L,
                                min_count = 100L,
                                alpha = 0.1,
                                s_target = NULL,
                                n_episode_max = NULL) {
  
  if (is.null(exp_r_sc_a)) 
    stop("the expected reward matrix of taking action a at state s must be provided.")
  n_state <- nrow(exp_r_sc_a)
  n_action <- ncol(exp_r_sc_a)
  if (is.null(P_sn_sc_a))
    stop("the probability matrix of transit to state s' from s by taking action a must be provided.")
  
  # Initialization
  if (is.null(pi_a_sc_ini)) {
    single_action <- matrix(0L, 1L, n_action)
    # single_action[sample(n_action, 1L)] <- 1L
    single_action[5L] <- 1L
    pi_a_sc_ini <- single_action[rep(1L, n_state), ]
  }
  if (method %in% c("Sarsa", "SarsaExp", "SarsaN", "Q")) {
    s_cur <- NULL
    a_cur <- NULL
    n_episode <- 1L  # initialize the cumulative number of episodes generated 
    pi_a_sc_ini[s_target, ] <- c(rep(0L, n_action - 1L), 1L)
    # always stay in target area, if reached
  }
  pi_a_sc_est <- pi_a_sc_ini
  sv_est <- sv_ini <- matrix(0L, nrow = n_state, ncol = 1L)
  Q_ini <- Q_est <- matrix(0, nrow = n_state, ncol = n_action)
  counts <- NULL
  
  # iterative estimation
  for (iter in 1L:iter_max) {
    # print(iter)
    # policy evaluation, calculate state value of updated policy using k-1 th 
    #   state values as previous state value vector.
    if (type_iter == 1L) {
      # value iteration
      results <- action_value(
        exp_r_sc_a = exp_r_sc_a, 
        P_sn_sc_a = P_sn_sc_a,
        pi_a_sc = pi_a_sc_est, 
        gamma = gamma, 
        sv = sv_est,
        method = method,
        length_episode = length_episode, 
        n_epi_each_sa_pair = n_epi_each_sa_pair, 
        type_visit =  type_visit, 
        Q_ini = Q_ini, 
        counts = counts, 
        min_count = min_count, 
        alpha = alpha, 
        s_cur = s_cur, 
        a_cur = a_cur, 
        s_target = s_target, 
        n_episode = n_episode
      )
    } else if (type_iter == 2L) {
      # policy iteration
      # browser()
      results <- policy_evaluation(
        exp_r_sc_a = exp_r_sc_a, 
        P_sn_sc_a = P_sn_sc_a, 
        pi_a_sc = pi_a_sc_est, 
        gamma = gamma, 
        sv_ini = sv_ini, # start from jth V for efficient updating
        is_closedform_solution = is_closedform_solution,
        stop_at_almost_equal = stop_at_almost_equal,
        stopping_crit = stopping_crit,
        iter_max = iter_max,
        method = method, 
        length_episode = length_episode, 
        n_epi_each_sa_pair = n_epi_each_sa_pair,
        type_visit = type_visit,
        Q_ini = Q_ini,
        counts = counts,
        min_count = min_count, 
        alpha = alpha, 
        s_cur = s_cur, 
        a_cur = a_cur, 
        s_target = s_target, 
        n_episode = n_episode
      )
    } else if (type_iter == 3L) {
      # truncated policy iteration
      results <- policy_evaluation(
        exp_r_sc_a = exp_r_sc_a, 
        P_sn_sc_a = P_sn_sc_a, 
        pi_a_sc = pi_a_sc_est, 
        gamma = gamma, 
        sv_ini = sv_ini,
        is_closedform_solution = is_closedform_solution,
        stop_at_almost_equal = stop_at_almost_equal,
        stopping_crit = stopping_crit,
        iter_max = iter_truncate,
        method = method, 
        length_episode = length_episode, 
        n_epi_each_sa_pair = n_epi_each_sa_pair,
        type_visit = type_visit,
        Q_ini = Q_ini,
        counts = counts,
        min_count = min_count,
        alpha = alpha, 
        s_cur = s_cur, 
        a_cur = a_cur, 
        s_target = s_target, 
        n_episode = n_episode
      )
    }
    Q_est <- results$action_value
    counts <- results$counts
    
    if (method %in% c("Sarsa", "SarsaExp", "SarsaN", "Q")) {
      s_cur <- results$s_cur
      a_cur <- results$a_cur
      n_episode <- results$n_episode
    }
    # policy improvement/update
    id_state_Qupdated <- (1L:n_state)[rowSums(Q_est == Q_ini) != n_action]
    
    indices_q_max <- vector(mode = "integer", length = length(id_state_Qupdated))
    for (i in seq_along(id_state_Qupdated)) {
      is <- id_state_Qupdated[i]
      # to avoid updating pi for target area, because we always want agent to stay at target area
      if (method %in% c("Sarsa", "SarsaExp", "SarsaN", "Q") & is == s_target) {
        next
      }
      indices_q_max[i] <- which.max(rank(Q_est[is, ], ties.method = "random"))
      if (is_on_policy) {
        # soft
        pi_a_sc_est[is, indices_q_max[i]] <- 1L - epsilon * (n_action - 1L) / n_action
        pi_a_sc_est[is, !(1L:n_action) %in% indices_q_max[i]] <- epsilon / n_action
        indices_q_max[i] <- sample(n_action, 1L, prob = pi_a_sc_est[is, ])
      } else {
        # greedy
        pi_a_sc_est[is, indices_q_max[i]] <- 1L
        pi_a_sc_est[is, !(1L:n_action) %in% indices_q_max[i]] <- 0L
      }
    }
    
    if (type_iter == 1L) {
      sv_est[id_state_Qupdated] <- sapply(
        seq_along(id_state_Qupdated), 
        \(x) Q_est[id_state_Qupdated[x], indices_q_max[x]]
      )
    } else {
      sv_est <- results$state_value
    }
    
    # when to stop
    stop_cond_sv <- ifelse(
      stop_at_almost_equal, 
      isTRUE(all.equal(sv_est, sv_ini)), 
      sqrt(sum((sv_est - sv_ini) ^ 2L)) <= stopping_crit
    )
    stop_cond_Q <- ifelse(
      stop_at_almost_equal, 
      isTRUE(all.equal(Q_est, Q_ini)), 
      sqrt(sum((Q_est - Q_ini) ^ 2L)) <= stopping_crit
    )
    
    # to avoid under-sampling
    if (method %in% c("Sarsa", "SarsaExp", "SarsaN", "Q", "MCes")) {
      if (any(counts < min_count)) stop_cond_Q <- FALSE
    }
    
    # to cap the maximum number of episodes
    if (method %in% c("Sarsa", "SarsaExp", "SarsaN", "Q")) {
      if (n_episode > n_episode_max) break
    }
    
    if (stop_cond_sv & stop_cond_Q) {
      break
    } else {
      sv_ini <- sv_est
      Q_ini <- Q_est
      pi_a_sc_ini <- pi_a_sc_est
    }
  }
  
  print(c(sqrt(sum((Q_est - Q_ini) ^ 2L)), sqrt(sum((Q_est - Q_ini) ^ 2L))))
  
  # add names for pi and Q
  if (!is.null(pi_a_sc_est)) {
    if (is.null(row.names(pi_a_sc_est)))
      row.names(pi_a_sc_est) <- paste("s", 1L:n_state, sep = "")
    if (is.null(colnames(pi_a_sc_est)))
      colnames(pi_a_sc_est) <- paste("a", 1L:n_action, sep = "")
  }
  
  if (!is.null(Q_est)) {
    if (is.null(row.names(Q_est)))
      row.names(Q_est) <- paste("s", 1L:n_state, sep = "")
    if (is.null(colnames(Q_est)))
      colnames(Q_est) <- paste("a", 1L:n_action, sep = "")
  }
  
  return(list(policy = pi_a_sc_est, state_value = sv_est, action_value = Q_est))
}
