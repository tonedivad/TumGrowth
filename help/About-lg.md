
> **Overview**


Mixed-effect modelling the standard method to statistically analyse tumor growth datasets. Benefits of such approach are as follows:

- possibility to take into account the inter-animal variability in the model;
- reduction in type I error rate compared to single time point (cross-sectional) analysis;
- able to cope with inconsistent sampling time points (e.g. from two different experiments) and/or irregularly-spaced data (e.g. no need to come on the week-end);
- incomplete datasets due to randomly missing measurements or from incomplete time course resulting from an excessive tumour size can still be analysed;
- more complex questions can be addressed by means of adequate contrast calculations, even for not so balanced designs.

Yet, it relies the assumption for the nature of the relationship between tumour size and time. Whereas complex forms of tumour growth are still of constant interest, <tumcode>TumGrowth</tumcode> is of general scope for regression modeling in pre-clinical animal models to provide a cutting-edge statistical framework . It assumes some form of linearity between the response and the model coefficients and is applicable in typical applications with reduced time frame and sampling at handful of time points. Departure from straight linearity is approached form two angles:

1. Tumour size is supposed to be proportional to the number of cancer and, in pratice, inaccurate or misleading  in *in vivo* contexts. The choice of the response is debatable and its calculation experimentator dependant, nonlinear transformation is a general solution to convert the original measurement to an equivalent pseudo-size that satisfies linearity over the duration of the experiment.
2. Tumour growth or response to treatment are known to exhibit different phases and therefore an adequate description is a composite of several models including  that transformation of the response does not efficiently overcome. Complexity of the model comes at the price of size and finer granularity of the sampling. <tumcode>TumGrowth</tumcode> implementation allows for *linear-linear* model structure with a fixed common breaking point.

A note on that one size does not fits all and situations involving heterogeous datasets comprising peculiar curvatures or non monotoneous behaviours are not provided with the general scope of <tumcode>TumGrowth</tumcode>. Users may be  reformulating their questions and use for example the <tumcode>TumGrowth</tumcode> survival and cross-sectional functionnalities or seek for specialized expertise adapted to their experimental set-up, if not revisit the sampling strategy of the later to adequately model the different phases of the .

While these models may fit the data quite well, one problem many of these models share is that the coefficients have limited biological interpretation 
 The holy grail in TGD modelling is therefore to develop a method that i) fits the data well for a wide variety of cancers and therapies without detailed knowledge of their mechanism of action and ii) provide results that are biologically interpretable and actionable.
 Thus, log is a natural scale to model tumor volume data. When tumor sizes are small, the log transformation can, however, lead to erroneous results
 
Balance between number of measurement, adequate fitting, robustness of the algortihms, statisitcal framework not to confuse biologist, interpretation and interactivity.

> **Settings**

Data selection:

1. treatment groups of interest;
2. response variable as orginally loaded in <tumcode>TumGrowth</tumcode>;
3. transformation of the response variable: 
 * None, the raw measurement is used for the measurement;
 * Log, log-transformatio of the raw measurement (zero's may be excluded, see parsing options `5`/`6`);
 * SqRt, for the square root of the raw measurement;
 * CuRt, for the cubic root of the raw measurement.

Modelling:

1. piecewise regression mode - It corresponds to the two phase *linear-linear* model - A fixed breakpoint can be specified or left empty should the optimum be found automatically; 
2. outlier detection threshold - One round of outlier detection is performed to exclude automatically abberrant observations on the basis of Bonferroni-corrected p-value calculated from the residuals. <excode>Default: p<0.1</excode> is a not so stringent and reasonable cut-off. Note that aberrant animals are best excluded manually by the user before uploading the data by leaving empty the **Use** column in the raw datafile.


> **Output and interpretation**

Type II ANOVA:
 * `Time x Treat` correspond to the outcome of the test underlying that there is the tumour growth curve slopes differ between treatment groups. If the test is positive, then the between  pairwise comparison; 
 * `Time` and `Treat` factors are meaningful in the case of a non-significant ``Time x Treat``. Calculations correspond to the additive model of *Time+Treat*; 
 * For the piecewise model, the table is augmented to describe the two segments, `Time(1)` and `Time(2)`. Maximal models, i.e. excluded non-significant contributions are presented. 

Pairwise comparisons:
 * Differences in slope between pairs of treatement groups. By default all possible pairs
 * Interactions???

Diagnostics plots:
 * `QQ-plot`: data points should lie as closely as possible to the straightline. Any *S* or *banana*-like shapes are likely to be caused by poor transformation of the response and straight segments of points may be *ceiling* effects due to a constant portion in a time course;
 * `Resid/Fit`: residuals of the models are plotted againts the fitted values (i.e. animal-level predictions). Data points are expected to be uniformly distributed around zero and at any value of the response. Issues observed in `QQ-Plot` can be outlined here;
 * `Fit`: overlay of the time courses (excluding outlying measuements) and back-transformed predicted growth curves (with 95% confidence intervals). It gives a visual insight into the adequacy of the modelling strategy;
 * `Resid/Mice`, `Resid/Grp`, `Resid/Tp`: distribution across time points and mice. This can help at hightlighting deviant animals, poorly sampled time points or non-typical treatment response.


Model information:
 * Fixed effect coeff matrix;
 * Ids of any potentially excluded outliers;


> **Implementation**

`lme4:::lmer` + F test based on KR estimation for degree of freedoms + automatic selection of the random effects based on AIC + robustness

no AR for the residuals


somne references here


