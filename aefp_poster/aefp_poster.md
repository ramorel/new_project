---
title: Status Anxiety and Charter School Expansion in California
author:
  - name: Richard Paquin Morel
    affil: 1
affiliation:
  - num: 1
    address: Postdoctoral Research Associate, Learning Research and Development Center, University of Pittsburgh
column_numbers: 4
body_textsize: 35px
logoleft_name: RGB_Shield_Stacked_Education_3color.png 
output: 
  posterdown::posterdown_html:
    self_contained: false
    keep_md: true
bibliography: packages.bib
---



# Overview

While school choice policies are now ubiqitous and often presented as a strategy for addressing persistent racial disparities education, there is concern that such policies may maintain or increases such disparities due to resource hoarding by advantaged families or through increased racial segregation [@davisSchoolChoiceSegregation2014; @rodaSchoolChoicePolicies2013]. More specifically, white families may use charters to exit from racially diversifying schools to maintain status hierarchies [@renzulliSchoolChoiceCharter2005]. Using a group threat/status anxiety framework [@boboPerceptionsRacialGroup1996], I hypothesize that exposure to increased racial diversification may induce white families to enroll their children in charter schools. I distinguish between proximal exposure, where racial diversification occurs in neighboring school districts, and direct exposure, where racial diversification occurs within local traditional public schools. 

I find evidence that: 

1. both proximal and direct exposure are associated with both **increased foundings of charter schools** and with **increasd white enrollment in charter schools**;  
2. this is concentrated in **non-urban areas**; and  
3. threat-induced white enrollment is associated with **increased racial segregation**.  

# Data
Panel of California school district data from 2000-2015 (n = 12,603 district-by-year observations; 990 unique districts)

Sources: The Common Core of Data; Small Area Income and Poverty Estimates (both accessed via the Urban Institutes API); NCES EDGE database

Charter schools in California:

- Law enacted in 1992
- Authorization occurs at the district level
- In 2000, ~ 2% of students were enrolled in charters
- By 2015, ~ 10% were enrolled in charters

# Methods

**Measures**

1. _Proximal status threat_ [@andrewsGroupThreatPolicy2015]
$$ProxThreat_{ij} = \theta_{ij} * \gamma_{ij}$$
where $\theta_{ij} = \frac{White_{ij}}{Total_{ij}}$ and $\gamma_{ij} = \frac{\sum_1^k Blk_{kj}, Hisp_{kj}}{\sum_1^k Total_{kj}}$

2. _Direct status threat_
$$DirThreat_{ij} = \frac{\{BlkTPS{ij}, HispTPS{ij}\}}{\{TotalBlk_{ij},TotalHisp_{ij}\}}$$


3. _Racial segregation (white isolation)_ [@masseySuburbanizationSegregationMetropolitan1988]
$$WhiteIsolation_{ij} = \sum_{s=1}^n[(\frac{x_{sj}}{X_{ij}})(\frac{x_{sj}}{t_{ij}})]$$
where $x_{sj}$ is the white enrollment in a school, $X_{ij}$ is the total white enrollment in the district $s$ is in, and $t_{ij}$ is the total enrollment in the district.

**Analytic models**

1. Does proximal and/or direct exposure increase charter enrollment for white students?  
$$Y_{ij} = \beta_1ProxThreat_{ij} + \beta_2DirThreat_{ij} + X_{ij}\beta + \alpha_i + \gamma_j + \epsilon_{ij}$$

2. Does white enrollment induced by exposure contribute to racial segregation in schools?  

_First stage_  
$$\phi_{ij} = \pi_1ProxThreat_{ij} + \pi_1DirThreat_{ij}$$

_Second stage_: 
$$WhiteIsolation_{ij} = \beta_1\hat{\phi} + X_{ij}\beta + \alpha_i + \gamma_j + \epsilon_{ij}$$


# Charter School Expansion and Enrollment

<img src="aefp_poster_files/figure-html/map1-1.png" width="97%" style="display: block; margin: auto;" /><img src="aefp_poster_files/figure-html/map1-2.png" width="97%" style="display: block; margin: auto;" /><img src="aefp_poster_files/figure-html/map1-3.png" width="97%" style="display: block; margin: auto;" />

# Descriptive Analysis

<img src="aefp_poster_files/figure-html/unnamed-chunk-1-1.png" width="90%" style="display: block; margin: auto;" /><img src="aefp_poster_files/figure-html/unnamed-chunk-1-2.png" width="90%" style="display: block; margin: auto;" /><img src="aefp_poster_files/figure-html/unnamed-chunk-1-3.png" width="90%" style="display: block; margin: auto;" />


# Preliminary Results

<img src="aefp_poster_files/figure-html/standard-plot-1.png" width="100%" style="display: block; margin: auto;" />

<img src="aefp_poster_files/figure-html/morefigs-1.png" width="100%" style="display: block; margin: auto;" />

<img src="aefp_poster_files/figure-html/unnamed-chunk-2-1.png" width="100%" style="display: block; margin: auto;" />


# Summary and implications

- Both direct and proximal exposure to students of color appear to motivate white students to leave traditional public schools for charter schools
- This process appears to increase district segregation--white students are more isolated from students of color
- Findings suggest that white families may use charter schools to maintain exclusive access to educational goods--aligning with other research showing that white families have a preference for schools with more white students

# References
