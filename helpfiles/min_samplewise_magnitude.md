### Filter by samplewise magnitude
***
This option filters the items according to their sample-wise magnitudes:
 
\\[
= \sum_{j:1}^n \frac{|Q_j|}{n} 
\\]


where \\(n\\) is the number of samples and \\(|Q_j|\\) are the log-fold changes for sample \\(j\\). 

This metric has two main differences than the regular way of computing the fold changes:
 - The missing values are considered to be 0 (no change between case/control) in this metric. Thus, the items with many missing values are penalized. This promotes consistency across the samples. 
 - Absolute value is taken. Thus, conflicting evidence (i.e., positive and negative fold changes in different samples) does not cancel each other out. This is particularly relevant if there is a hidden effect size modifier e.g., if the samples in group A exhibit strong positive change while the samples in group B are negative.  