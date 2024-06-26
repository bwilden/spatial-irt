# A Geospatial Model of Political Ideology in California


In this project I estimate county-level political ideology in California
using a new model I develop called
**IRT-MRP-BYM**—*Item-Response-Theory,
Multilevel-Regression-with-Poststratification, Besag-York-Mollié*
model.[^1]

**Item Response Theory (IRT)** models are used to measure latent
characteristics of individuals, such as political ideology, by using the
individuals’ observed choices on survey items. I use thirteen questions
on abortion (2), the environment (3), gun control (3), healthcare (2),
and immigration (3) from 7,998 respondents in the combined 2019, 2020,
and 2021 [Cooperative Election Survey
sample](https://cces.gov.harvard.edu/).

**Multilevel Regression with Poststratification (MRP)** is a method for
dealing with biased/unrepresentative survey samples. The first step
involves building a multilevel model to predict the quantity of interest
(ideology in this case). Then we poststratify these predictions using
Census data at the county level—which has the effect of weighting the
results by the prevalence of different demographic combinations in each
county.

**Besag-York-Mollié (BYM)** models are used to account for geospatial
dependencies in data. The basic idea is that counties which are close to
one another likely share similar political ideology compared to counties
which are further apart. BYM models provide a principled method for
incorporating this spatial autocorrelation.[^2]

## Results

Below are the results of the full **IRT-MRP-BYM** model for the county
ideology parameter $\theta_c^{MRP}$. The coastal counties of San Mateo,
Marin, and Ventura are among the most liberal, whereas the most
conservative counties are in the Central Valley (Kern, Merced).

<img src="README_files/figure-commonmark/unnamed-chunk-2-1.png"
data-fig-align="center" />

In contrast to the $\theta_c^{MRP}$ model estimates, if we were to
construct a simple additive scale of ideology based on the thirteen
survey questions, we get a very different picture of Californian
politics. According to the additive index, Mariposa (dark green in the
center of the map) is the most liberal county in the state. This defies
common sense for a county which has voted for the Republican candidate
every year since 1992![^3] Without the regularization provided by MRP +
BYM, small geographic units are apt to produce completely unreliable
estimates.

<img src="README_files/figure-commonmark/unnamed-chunk-3-1.png"
data-fig-align="center" />

## Notes

- Full paper:
  [paper.pdf](https://github.com/bwilden/spatial-irt/blob/main/paper.pdf)
- Model code in Stan:
  [ideal_mrp.stan](https://github.com/bwilden/spatial-irt/blob/main/stan/ideal_mrp.stan)
- R code and cleaning steps:
  [\_targets.R](https://github.com/bwilden/spatial-irt/blob/main/_targets.R)
  and [R files](https://github.com/bwilden/spatial-irt/tree/main/R)

[^1]: I’m open to any suggestions on a catchier name.

[^2]: See [Gao, Kennedy, Simpson, and Gelman
    2020](https://arxiv.org/abs/1908.06716) for using BYM in MRP models.

[^3]: MIT Election Data and Science Lab, 2018, “County Presidential
    Election Returns 2000-2020”, <https://doi.org/10.7910/DVN/VOQCHQ>,
    Harvard Dataverse, V12.
