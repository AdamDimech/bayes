# Section 4.4

## Getting weight into a Gaussian model of height

Let $x$ be the name for the column of weight measurements `d2$weight`. Let the average of the $x$ values be $\bar{x}$. To get weight into the model, we define the mean $\mu$ as a function of the values in $x$. 

For example, a model could be summarised as follows:

$$h_i\sim \textup{Normal}(\mu_i, \sigma )$$

$$ \mu_i = \alpha + \beta (x_i-\bar{x}) $$

$$ \alpha \sim \textup{Normal}(178, 20) $$

$$ \beta \sim \textup{Normal}(0, 10) $$

$$ \sigma \sim \textup{Uniform}(0, 50) $$

In this example:
- $\alpha$ answers the question *"What is the expected height when* $x_i=\bar{x}$*?"* The paramer $\alpha$ is the _intercept_.
- $\beta$ answers the question *"What is the expected height when* $x_i$ *changes by one unit?"* The paramer $\beta$ is the _slope_.

Jointly these two parameters seek to find a line that relates $x$ to $h$ and passes through $\alpha$ when $x_i=\bar{x}$ and has slope $\beta$.
