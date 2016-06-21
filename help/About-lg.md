
> **Overview**


Mixed-effect modeling the standard method to statistically analyzee tumor growth 
datasets. Benefits of such approach are as follows:

- possibility to take into account the inter-animal variability in the model;
- reduction in type I error rate compared to single time point (cross-sectional) 
analysis;
- ability to cope with inconsistent sampling time points (e.g. from two 
different experiments) and/or irregularly-spaced data (e.g. no need to come on 
the week-end);
- incomplete datasets due to randomly missing measurements or from incomplete 
time course resulting from an excessive tumor size can still be analyzed;
- more complex questions can be addressed by means of adequate contrast 
calculations, even for not so balanced designs.

Nonetheless, the analysis relies on the assumption for the nature of the 
relationship between tumor size and time. Whereas complex forms of tumor growth 
are still of constant interest, <tumcode>TumGrowth</tumcode> has the general scope of providing a 
cutting-edge statistical framework for regression modeling in pre-clinical 
animal models. It assumes some form of linearity between the response and the 
model coefficients and can be used in typical applications with reduced time 
frames and sampling at a handful of time points. Departure from linearity is approached from two angles:

1. Tumor size is supposed to be proportional to the number of cancer cells but, in 
practice, inaccurate or misleading  in *in vivo* contexts. The choice of the 
response is debatable and its calculation experimentator dependant, nonlinear 
transformation is a general solution to convert the original measurement to an 
equivalent pseudo-size that satisfies linearity over the duration of the 
experiment.
2. Tumour growth or response to treatment is known to exhibit different phases 
and therefore an adequate description might be a composite of several models, an assumption that is beyond the scope of this software (increased model complexity 
comes at the price of larger sample size/adequacy of sampling). Nethertheless  <tumcode>TumGrowth</tumcode> allows for *linear-linear* model 
structure with a fixed common breaking point.

It should be noted that one size does not fit all and that situations involving heterogeous 
datasets comprising peculiar curvatures or non monotonous behaviors cannot be analyzed using the longitudinal analysis tool of <tumcode>TumGrowth</tumcode>.
Users may reformulate their questions and rather take advantage of the 
<tumcode>TumGrowth</tumcode> survival and cross-sectional functionalities or 
seek for specialized expertise adapted to their experimental set-up, if not 
revisit the sampling strategy of the later to adequately model the different 
phases of the response to experimental interventions.

While these models may fit the data quite well, one problem that affects many of these models 
is that the coefficients have limited biological interpretation 
*REWORD: The holy grail in TGD modelling is therefore to develop a method that i) fits 
the data well for a wide variety of cancers and therapies without detailed 
knowledge of their mechanism of action and ii) provide results that are 
biologically interpretable and actionable.
Thus, log is a natural scale to model tumor volume data. When tumor sizes are 
small, the log transformation can, however, lead to erroneous results*
 

> **Settings**

Data selection:

1. treatment groups of interest;
2. response variable as originally loaded in <tumcode>TumGrowth</tumcode>;
3. transformation of the response variable: 
 * None, the raw measurement is used for the measurement;
 * Log, log-transformation of the raw measurement (zero's may be excluded, see 
parsing options `5`/`6`);
 * SqRt, for the square root of the raw measurement;
 * CuRt, for the cubic root of the raw measurement.

Modeling:

1. piecewise regression mode - It corresponds to the two phase *linear-linear* 
model - A fixed breakpoint can be specified or left empty should the optimum be 
found automatically; 
2. outlier detection threshold - One round of outlier detection is performed to 
exclude automatically aberrant observations on the basis of 
Bonferroni-corrected p-value calculated from the residuals. <excode>Default: 
p<0.1</excode> is a not so stringent and reasonable cut-off. Note that aberrant 
animals are best excluded manually by the user before uploading the data by 
leaving empty the **Use** column in the raw datafile. Exclusion of data points following these objective criteria must be mentioned in the description of the data set, usually in the Materials and Methods of scientific papers.
(ADD explicit removal in report)

> **Output and interpretation**

Type II ANOVA:
 * `Time x Treat` correspond to the outcome of the test underlying that there is 
the tumor growth curve slopes differ between treatment groups. If the test is 
positive, then the between  pairwise comparison; 
 * `Time` and `Treat` factors are meaningful in the case of a non-significant 
``Time x Treat``. Calculations correspond to the additive model of 
*Time+Treat*; 
 * For the piecewise model, the table is augmented to describe the two segments, 
`Time(1)` and `Time(2)`. Maximal models, i.e. excluded non-significant 
contributions are presented. 

Pairwise comparisons:
 * Differences in slope between pairs of treatment groups. By default all 
possible pairs are selected. Adjustement for multiple testing is automatically calculated upon selection of the contrasts of interest.
 * Interactions: TO BE ADDED in next release

Diagnostics plots:
 * `QQ-plot`: data points should lie as closely as possible to the straight line. 
Any *S* or *banana*-like shapes are likely to be caused by poor transformation 
of the response and straight segments of points may be *ceiling* effects due to 
a constant portion in a time course;
 * `Resid/Fit`: residuals of the models are plotted against the fitted values 
(i.e. animal-level predictions). Data points are expected to be uniformly 
distributed around zero and at any value of the response. Issues observed in 
`QQ-Plot` can be outlined here;
 * `Fit`, `FitBt`: overlay of the time courses (excluding outlying measurements) and 
back-transformed predicted growth curves (with 95% confidence intervals). It 
gives a visual insight into the adequacy of the modelling strategy;
 * `Resid/Mice`, `Resid/Grp`, `Resid/Tp`: distribution across time points and 
mice. This can help at hightlighting deviant animals, poorly sampled time points 
or non-typical treatment response.


Model information:
 * Fixed effect coeffiecinet matrix;
 * Animal ids for any potentially excluded outliers;


> **Implementation**
Balance between number of measurement, adequate fitting, robustness of the 
algortihms, statisitcal framework not to confuse biologist, interpretation and 
interactivity.

`lme4:::lmer` + F test based on KR estimation for degree of freedoms + automatic 
selection of the random effects based on AIC + robustness

no AR for the residuals


somne references here


