# Literature and Tools Reviewed for the Improved Risk Incidence Methodology

Annotated list of every source consulted while drafting the improved methodology. Grouped by theme, matching the section structure of the main methodology document.

## FGM specific time to event and spatial studies

Female genital mutilation among children in Ethiopia, a time to event analysis of age at circumcision, PLOS One, 2025. Confirms time to event analysis of age at cutting is current, peer reviewed practice, using Ethiopian DHS data.
https://journals.plos.org/plosone/article?id=10.1371%2Fjournal.pone.0317966

Modeling determinants of time to circumcision of girls, a comparison of various parametric shared frailty models, FGM/C Research Initiative repository. Found a log logistic baseline hazard with inverse Gaussian shared frailty fit best on 2016 Ethiopian DHS data, and that regional clustering was statistically significant, the basis for recommending a frailty term in section 2.1 of the main document.
https://www.fgmcri.org/academic_repository/modeling-determinants-of-time-to-circumcision-of-girls-a-comparison-of-various-parametric-shared-frailty-models/

Prevalence of female genital mutilation and associated factors among daughters aged 0 to 14 years in sub Saharan Africa, a multilevel analysis of recent demographic health surveys, Frontiers in Reproductive Health, 2023. Pooled 14 countries, 2010 to 2021 DHS rounds, using modified multilevel Poisson regression with individual and community level covariates, the basis for the pooled multi country modeling recommendation for sparse data countries.
https://www.frontiersin.org/journals/reproductive-health/articles/10.3389/frph.2023.1105666/full

Evaluating changes in the prevalence of female genital mutilation and cutting among 0 to 14 years old girls in Nigeria using data from multiple surveys, a novel Bayesian hierarchical spatio temporal model, PLOS One, 2021. Pooled six DHS and MICS rounds from 2003 to 2016/2017 using structured additive regression fit in R2BayesX, with structured and unstructured spatial effects and a survey type fixed effect. Closest existing academic precedent for the SUMMER style adaptation recommended in section 2.2.
https://journals.plos.org/plosone/article?id=10.1371%2Fjournal.pone.0246661

Spatial Clustering in Temporal Trends of Female Genital Mutilation Risk, Leveraging Sparse Data in Ethiopia, Kenya, and Somalia, Weny and colleagues, Studies in Family Planning, 2023. Full text was not accessible during this review (subscription restricted), title and framing confirm this is very likely the direct academic basis for the cross border clustering maps shown in the UNFPA deck, worth obtaining in full through institutional access before finalizing the subnational approach.
https://onlinelibrary.wiley.com/doi/10.1111/sifp.12242

Kandala and colleagues, spatial and spatiotemporal Bayesian hierarchical modeling of FGM and cutting prevalence among girls aged 0 to 14 in Kenya, using Kenyan DHS rounds 1998 to 2014. Repository record confirms the Bayesian hierarchical spatial framework has precedent for a single country case as well as the pooled Nigeria case above.
https://eprints.soton.ac.uk/458049

## Discrete time and frailty survival methods

Survival prediction models, an introduction to discrete time modeling, BMC Medical Research Methodology, 2022. General methodological reference for the person year and person month discrete hazard construction recommended in section 2.1.
https://link.springer.com/article/10.1186/s12874-022-01679-6

Stata discrete time survival analysis manual, general technical reference for implementation.
https://www.stata.com/manuals13/stdiscrete.pdf

## UN IGME under five mortality methodology (the direct analogy the UNFPA deck itself draws)

Global estimation of child mortality using a Bayesian B spline bias reduction model, UN IGME technical documentation. Full methodology reviewed for the source specific bias modeling and cross country partial pooling logic recommended in section 2.3.
https://childmortality.org/wp-content/uploads/2018/12/Global-estimation-of-child-mortality-using-a-Bayesian-B-spline-Bias-reduction-model.pdf

Child Mortality Estimation 2013, an overview of updates in estimation methods by UN IGME, PLOS One.
https://journals.plos.org/plosone/article?id=10.1371%2Fjournal.pone.0101112

Global, regional, and national trends in under five mortality between 1990 and 2019 with scenario based projections until 2030, a systematic analysis by UN IGME, The Lancet Global Health, 2022.
https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(21)00515-5/fulltext

## Subnational small area hazard estimation tooling

SUMMER R package, case study estimating subnational under five mortality using DHS data, CRAN vignette. Reviewed in detail as the primary recommended tool for section 2.2, discrete hazard construction, direct estimation, and Bayesian spatial and temporal smoothing through INLA.
https://cran.r-project.org/web/packages/SUMMER/vignettes/u5mr-dhs-vignette.html

DHS Spatial Analysis Reports, guidelines on the use of DHS GPS data, the DHS Program. Reference for linking cluster coordinates into any spatial model, already in use for the Method 2 SAE work.
https://dhsprogram.com/pubs/pdf/SAR8/SAR8.pdf

A tutorial in spatial and spatio temporal models with R INLA. General technical reference for the INLA framework underlying SUMMER.
https://discovery.ucl.ac.uk/1415919/1/Baio_BlaCamBaiRue.pdf

## Visualization

Lexis diagram, general reference for the age, period and cohort visualization framework recommended in section 2.4.
https://en.wikipedia.org/wiki/Lexis_diagram

Hexamaps for age period cohort data visualization and implementation in R, Epidemiology journal. Reference for the perceptually corrected alternative to a standard rotated Lexis surface.
https://www.ovid.com/jnls/epidem/fulltext/10.1097/ede.0000000000001236~hexamaps-for-ageperiodcohort-data-visualization-and

## Change point and trend break detection

An overview of change point packages in R, a practical comparison of available R packages for both frequentist and Bayesian change point detection.
https://lindeloev.github.io/mcp/articles/packages.html

onlineBcp, an R package for online change point detection using a Bayesian approach, SoftwareX.
https://www.sciencedirect.com/science/article/pii/S2352711022000140

## Machine learning survival methods

Using random survival forests, scikit survival documentation. Reference for the covariate importance and cross check layer recommended in section 2.5.
https://scikit-survival.readthedocs.io/en/stable/user_guide/random-survival-forest.html

Gradient boosted models, scikit survival documentation, the gradient boosted survival alternative referenced alongside random survival forests.
https://scikit-survival.readthedocs.io/en/stable/user_guide/boosting.html

## Data access

The new DHS Program API, what non developers should know, the DHS Program blog. Confirms the public API only exposes aggregated STATcompiler indicators, not individual level records, the basis for the data pipeline recommendation in section 4.
https://blog.dhsprogram.com/dhs-api/

rdhs, an R package to interact with the Demographic and Health Surveys Program datasets. Recommended tool for scripted, reproducible individual recode microdata access across all 18 countries and every available survey round.
https://github.com/ropensci/rdhs

FAQ, Multiple Indicator Cluster Surveys, UNICEF MICS. Reference confirming MICS microdata access is a direct per country per round request rather than an API.
https://mics.unicef.org/faq
