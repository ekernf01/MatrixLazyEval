---
title: 'Lazy matrix evaluation in R'
tags:
  - R
  - PCA
  - single-cell genomics
authors:
  - name: Eric Kernfeld
    orcid: 0000-0002-2310-8191
    affiliation: "1" 
affiliations:
 - name: Department of Biomedical Engineering, Johns Hopkins University, USA
   index: 1
date: 19 July 2022
bibliography: paper.bib

---

# Summary

Modern single-cell genomics technologies hold tremendous promise for cancer genomics, developmental biology, and cellular engineering [@teichmann2017human; @lei2021applications; @griffiths2018using; @rackham2021challenges]. Due to the breadth of applications for single-cell genomics and due to the idiosyncracy of individual biological systems, single-cell genomics admits no one-size-fits-all approach to quality control or analysis. Rather, the rapid adoption of these measurement devices [@svensson2020curated] has suddenly conscripted an entire cohort of computational biologists into labor-intensive exploration and bespoke analysis of large, sparse count matrices. 

Individual datasets with minimal biological replication easily reach tens of thousands of observations across tens of thousands of genes or hundreds of thousands of functional genome elements, and RAM is a ubiquitous bottleneck. Preserving the initial sparsity of the data is a natural solution. Alas, this sparsity is often annihilated within the first few steps of a typical analysis -- for example, by centering each gene to have a mean of 0. There is a need for software that can perform common statistical analysis of matrices, such as centering, scaling, and PCA, while preserving sparsity [@booeshaghi2022depth].

# Statement of need

`MatrixLazyEval` is an `R` package for sparse matrix operations that prioritizes simplicity and ease of use, anticipating and avoiding typical misunderstandings. For ease of use, it is compatible with `R`'s native `matrix` class as well as the popular `Matrix` and `DelayedArray` packages (see https://cran.r-project.org/web/packages/Matrix/index.html and https://bioconductor.org/packages/release/bioc/html/DelayedArray.html). Its workhorse objects use overloaded versions of typical `R` matrix operations such as `%*%`, `t`, and `tcrossprod`. Since this leaves some ambiguity about whether a given operation will be performed immediately or put off, `MatrixLazyEval` explicitly documents every function as lazy or eager. 

The package provides implementations of:

- centering, 
- scaling, 
- replacing a matrix with its residuals after regression, and 
- randomized SVD or PCA. 

These operations can be easily chained/combined without loss of sparsity. The result is a viable drop-in replacement for matrix objects in single-cell genomics workflows.

# Acknowledgements

The author is grateful to Dr. René Maehr for access to single-cell genomics datasets at cutting-edge scale and complexity. 

# References
