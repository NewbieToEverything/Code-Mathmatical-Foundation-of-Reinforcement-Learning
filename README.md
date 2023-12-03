Hi everyone, this is the repository hosting the R scripts I wrote when self-learning the book "[Mathematical Foundations of Reinforcement Learning](https://github.com/MathFoundationRL/Book-Mathmatical-Foundation-of-Reinforcement-Learning)" by Shiyu. I am assistant professor at Jiangxi Normal University. Feel free to contact me if you have any question. This repository is still under development and will be updated to contain more reinforcement learning algorithms. Please star it if you found it helpful, thank you.

All R scripts are stored in the "code" folder, where

1. Algorithms: reinforcement learning algorithms are in the "functions.R", including:
 - Chapter 2: solving state values from Bellman equation using closed-form solution and iterative solution, and calculating action values from state values;
 - Chapter 4: value iteration, policy iteration, and truncated policy iteration;
 - Chapter 5: Monte Carlo basic, Monte Carlo exploring starts;
 - Chapter 7: Sarsa, expected Sarsa, and Q learning (on-policy).
 
 Algorithms calculating State values are in the "policy_evaluation" function; algorithms calculating action values are in the "action_value" function; algorithms aiming to find optimal policy, i.e. containing policy improvement step, can be found in the "find_optimal_policy" function.

2. Examples: all algorithms listed above have been tested using selected examples in Shiyu's book (Zhao, 2024), the results are either exactly or nearly equal to those in the book. The examples are in the R scripts with identical names to the chapter hosting those examples.