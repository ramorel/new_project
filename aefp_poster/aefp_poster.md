---
title: Status Anxiety and Charter School Expansion in California
author:
  - name: Richard Paquin Morel
    affil: 1
affiliation:
  - num: 1
    address: Postdoctoral Research Associate, Learning Research and Development Center, University of Pittsburgh
column_numbers: 3
logoleft_name: RGB_Shield_Stacked_Education_3color.png 
output: 
  posterdown::posterdown_html:
    self_contained: false
    keep_md: true
bibliography: packages.bib
---



# Overview

While school choice policies are now ubiqitous are often presented as a strategy for addressing persistent racial disparities education, there is concern that such policies may maintain or increases such disparities due to resource hoarding by advantaged families or through increased racial segregation. More specifically, white families may use charters to exit from racially diversifying schools. Using a group threat/status anxiety framework, I hypothesize that increased racial diversification may induce white families to enroll their children in charter schools. I distinguish between proximal status threat, where racial diversification occurs in neighboring school districts, and direct status threat, where racial diversification occurs within local traditional public schools. I find evidence that both proximal and direct threat are associated with both increased foundings of charter schools and with white enrollment in charter schools. 

These results suggest that white families enroll in charter schools 

# Data Methods

**Data**. Panel of California school district data from 2000-2015. 

Sources:

- District enrollment and finance data: The Common Core of Data (accessed via the Urban Institutes API) 
- District poverty data: Small Area Income and Poverty Estimates (accessed via the Urban Institutes API) 
- District geographic boundaries: The NCES EDGE database

**Proximal status threat**

$$ProxThreat_{ij} = \theta_{ij} * \gamma_{ij}$$
where $\theta_{ij} = \frac{White_{ij}}{Total_{ij}}$ and $\gamma_{ij} = \frac{\sum_1^k Blk_{kj}, Hisp_{kj}}{\sum_1^k Total_{kj}}$

**Direct status threat**

$$DirThreat_{ij} = \frac{\{BlkTPS{ij}, HispTPS{ij}\}}{\{TotalBlk_{ij},TotalHisp_{ij}\}}$$
$i$ indexes districts, $j$ indexes years, and $k$ indexes districts that neighbor $i$

**Analytic model: Fixed Effects Regression**

$$ Y_{ij} = \beta_1ProxThreat_{ij} + \beta_2DirThreat_{ij} + X_{ij}\beta + \alpha_i + \gamma_j + \epsilon_{ij} $$
<br>  
<br>  
<br>  
<br>  

# Results

Usually you want to have a nice table displaying some important results that you have calculated. In `posterdown` this is as easy as using the `kable` table formatting you are probably use to as per typical R Markdown formatting.

<img src="aefp_poster_files/figure-html/standard-plot-1.png" width="90%" style="display: block; margin: auto;" />


<img src="aefp_poster_files/figure-html/morefigs-1.png" width="90%" style="display: block; margin: auto;" />

# Next Steps

<img src="aefp_poster_files/figure-html/map1-1.png" width="100%" style="display: block; margin: auto auto auto 0;" /><img src="aefp_poster_files/figure-html/map1-2.png" width="100%" style="display: block; margin: auto auto auto 0;" />


# Conclusion

Try `posterdown` out! Hopefully you like it!



# References
