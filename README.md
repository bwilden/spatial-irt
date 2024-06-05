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

![](README_files/figure-commonmark/unnamed-chunk-2-1.png)

[^1]: I’m open to any suggestions on a catchier name.

[^2]: See [Gao, Kennedy, Simpson, and Gelman
    2020](https://arxiv.org/abs/1908.06716) for using BYM in MRP models.
