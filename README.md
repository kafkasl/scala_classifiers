# Bayesian Classifiers in action with Scala

This repository contains the Scala implementation of 3 classifiers:

* Maximum a posteriori
* Naive Bayes
* Tree Augmented Naive Bayes


## Maximum a posteriori

In Bayesian statistics, a maximum a posteriori probability (MAP) estimate is an estimate of an unknown quantity, that equals the mode of the posterior distribution. The MAP can be used to obtain a point estimate of an unobserved quantity on the basis of empirical data.
This method is quite slow and requires too many instances to be practical. For example:

Task of binary classification:
 - 10 attributes with four values each;
 - Probabilities needed: 
  - * Store 2^20 conditional probabilities;
  - * Estimate 2^20 conditional probabilities.


## Naive Bayes classifiers

Simple probabilistic classifiers based on applying Bayes' theorem with strong (naive) independence assumptions between the features.

In order to reduce the amount of instances and probabilities needed, we assume that parameters are independent conditioned on the target.

Pr(A1 . . . An|C) ∗ Pr(C) = Pr(A1|C) ∗ . . . ∗ Pr(An|C) ∗ Pr(C) 

It is an strong assumption, but works quite well even in scenarios were it does not hold.


## Tree Augmented Naive Bayes

Tree Augmented Naive Bayes (TANaiveBayes or TANB) method tries to soften the independence assumption between variables of Naive Bayes with a tree-like dependency structure.
