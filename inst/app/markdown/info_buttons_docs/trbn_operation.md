**Maintenance Downtime** expresses the percentage of time in each month when
turbines are not operating due to maintenance.

- In **Stochastic Mode**, monthly downtimes are treated as random variables,
each defined by a **Mean** and a standard deviation (**SD**) and assumed to
follow a Trunc. Normal Distribution bounded at 0.

- When **Deterministic Mode** is enabled, monthly downtimes are taken
as point estimates (i.e. only **Mean** values required).

**Operational Wind Availability**: the percentage of time wind conditions
allow for turbine operation per month. Treated as point estimates.

**Proportion of Month Operational**: Proportion of month during which turbines
are operational. Computed as the difference between monthly wind availability as
monthly downtime, these are the actual values used in collision risk
calculations.

Months with missing values for either downtime or wind availability will be
excluded from collision calculations.


