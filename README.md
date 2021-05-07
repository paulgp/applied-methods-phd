


# Applied Empirical Methods 
## Course Pages
1. https://github.com/paulgp/applied-methods-phd
2. https://yale.instructure.com/courses/64286 [Yale students only]

Course videos are available on [Youtube](https://www.youtube.com/playlist?list=PLWWcL1M3lLlojLTSVf2gGYQ_9TlPyPbiJ)

## Course Description
This course is primarily designed for graduate students interested in econometric methods used in empirical research. The goal of this class is to provide an overview of different empirical methods, with an emphasis on practical implementation.  I will provide a set of lecture slides and notes. There are additional background papers that are largely optional.

More generally, this is a course where I focus on providing my understanding and intuition of empirical methods, as they are used by practicioners. This means that this is not a course where we will spend a lot of time on the formal details (beyond what is necessary), but instead focus on the intuitive framework that guides these papers. I'll also do my best to communciate how any of these topics fit together.

This is a course very much focused on communication and artisanship. By the end of the term, my hope is for three things:

1. You will have been exposed to a wide range of empirical methods, and have at least a passing familiarity with their pros and cons. Moreover, you will know where to go look if you decide to use these methods. 
2. Much of the terminology and jargon that we use in econometric methods will be less intimidating to you. When someone says "I use semiparametric inference," now instead of intimidate you, it will bother you that they are not using clearer language.
3. You will approach research papers with the desire to disentangle the underlying framework and "experiment" that drives their causal inferences.

## Assignments
There will be problem sets every week. These will involve both theoretical calculations and computer exercises in which you will be asked to analyze data sets. You can use any computer package you wish to use. Solutions will be handed out written in R. Since there will be a fair number of problem sets, and in order to allow me to post the solutions quickly on the webpage for the course, I will not accept late problem sets. If you anticipate difficulty meeting the deadline, you can ask me for the problem set earlier to give you additional time to work on it.

You can work together on the problem sets and discuss them with classmates, but you need to write up the results individually and hand them in separately. Grades will be based on the problem sets, divided evenly over the problem sets.

I expect these assignments to be coded from "scratch." I will specify when canned packages are appropriate. In other words, when estimating a regression, I am not looking for the results of `lm( y ~ x)`. Rather, I expect you to construct two matrices and calculate the estimates using this. I also expect you to attempt to maintain good coding practices while doing so -- this will likely be challenging for those of you who are inexperienced at programming, so please plan accordingly -- I will not be providing additional instruction on coding beyond what I cover in class.  See the following resources in R for guidance (Many thanks to Max Kasy for organizing these materials):

1. Introduction to Base R: https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf
2. R for Data Science: https://r4ds.had.co.nz/
3. Guidance on Data Visualization: https://socviz.co/

## Main References:
This is a partial list of various interesting and useful books that will be touched during the course. 

* Joshua Angrist and J\"orn-Steffen Pischke,  *Mostly Harmless Econometrics*
* Scott Cunningham,  *Causal Inference: The Mixtape*,  https://mixtape.scunning.com/
* Benjamin T. Miller and Peter M. Aronow, *Foundations of Agnostic Statistics*
* Kieran Healy, *Data Visualization: A Practical Introduction*, https://socviz.co/

## Lectures ([Full Syllabus]())

1. **Causality, Statistics, and Economics**
	1. [**Potential Outcomes and Directed Acyclic Graphs**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/01_po_dags.pdf)
	2. [**Research Design, Randomization, and Design-Based Inference**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/02_randomization.pdf)
    3. [**Propensity Scores**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/03_propensity_scores.pdf)
    4.  [**Interference, Spillovers and Dynamics**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/04_interference_dynamics.pdf)
2. **Linear Regression**
	1. [**Inference**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/05_regression_1.pdf)
	2. [**Semiparametrics and Visualization**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/06_regression_2.pdf)
    3. [**Quantile Regression**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/07_regression_3.pdf)
    4. [**Penalized Regression Models**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/08_regression_4.pdf)
3. **Likelihood Methods**
   1. [**Binary Discrete Choice, GLM and Computational Methods**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/09_discrete_choice_1.pdf)
   2. [**Multiple Discrete Choices**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/10_discrete_choice_2.pdf)
   3. [**Duration models**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/11_duration_models.pdf)
   4. [**Hierarchical models + Bayesian Shrinkage**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/12_hierarchical_bayes.pdf)
4. **Canonical Research Designs**
   1. [**Difference-in-differences**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/13_dind.pdf)
   2. [**Event Studies, Synthetic Control + Synthetic DinD**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/14_synthetic_dind.pdf)
   3. [**Instrumental Variables (Part I)**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/15_iv_partI.pdf)
   4. [**Instrumental Variables (Part II)**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/16_iv_partII.pdf)
   5. [**Instrumental Variables (Part III)**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/17_iv_partIII.pdf)
   6. [**Bartik + Simulated Instruments**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/18_bartik_sim_iv.pdf)
   7. [**Examiner Designs aka Judge IV**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/19_judge_iv.pdf)
   8. [**Regression Discontinuity I: Identification and Groundwork**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/20_regression_discontinuity_1.pdf)
   9. [**Regression Discontinuity II: The Checklist**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/21_regression_discontinuity_2.pdf)
   10. [**Regression Discontinuity III: Extensions**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/22_regression_discontinuity_3.pdf)
5. **Machine Learning**
   1. [**Supervised Machine Learning I: Prediction**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/23_machine_learning_1.pdf)
   2. [**Supervised Machine Learning II: Heterogeneous Treatment Effects**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/24_machine_learning_2.pdf)
   3. [**Machine Learning III: Unstructured Data and Unsupervised ML**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/25_machine_learning_3.pdf) 
6. **Miscellaneous**
   1. [**Partial Identification**](https://github.com/paulgp/applied-methods-phd/blob/main/lectures/26_partial_identification.pdf)

