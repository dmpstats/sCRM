**Monthly In-flight Densities** express the number of daytime in-flight
birds/km^2 per month, within the wind farm footprint.

When **Stochastic Mode** is enabled, monthly densities are treated as random
variables. Inputs can be provided in three different ways:

1. Specify monthly **Mean** and Standard Deviation (**SD**) estimates, with densities
assumed to follow **Truncated Normal Distributions** bounded at 0.

2. Upload a set of **Percentile estimates** from estimated distributions of monthly
densities.

3. Upload **Random draws** from estimated distributions of monthly densities (e.g.
bootstrap samples)

Under the **Deterministic Mode**, monthly densities are treated as point
estimates, with only **Mean** values required.