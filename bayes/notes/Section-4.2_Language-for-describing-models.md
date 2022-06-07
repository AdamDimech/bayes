# Section 4.2

## Definitions

*data*: Observable variables
*parameters*: Unobservable variables
*joint generative model*: The combination of variables and their probability distributions. These can be used to simulate hypothetical observations as well as analyse real ones.

Variables are defined in terms of other variables or in terms of a probability distribution.

For example, a model could be summarised as follows:

$$ y_i\sim Normal(\mu_i, \sigma ) $$
$$ \mu_i = \beta x_i $$
$$ \beta \sim Normal(0, 10) $$
$$ \sigma \sim Exponential(1) $$
$$ x_i \sim Normal(0,1) $$

# Globe-tossing model

$$ W \sim Binomial(N, p) $$
$$ p \sim Uniform(0, 1) $$

- The count of $W$ is distributed binomially with sample size $N$ and probabiliyy $p$.
- The prior for $p$ is assumed to be unform between 0 and 1.

Related to Bayes' theorem:

$$ Pr(p\mid w,n)= \frac{\textup{Binomial}(W\mid n,p)\textup{Uniform}(p\mid 0,1)}{\int \textup{Binomial}(w\mid n,p)\textup{Uniform}(p\mid 0,1)dp}\ $$
