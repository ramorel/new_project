---
title: Status Anxiety and Charter School Expansion in California
author:
  - name: Richard Paquin Morel
    affil: 1
affiliation:
  - num: 1
    address: Learning Research and Development Center, University of Pittsburgh
column_numbers: 3
logoleft_name: /Users/richardmorel/Documents/GitHub/new_project/aefp_poster/RGB_Shield_Stacked_Education_3color.png 
output: 
  posterdown::posterdown_html:
    self_contained: false
    keep_md: true
    css: /Users/richardmorel/Documents/GitHub/new_project/aefp_poster/pitt_colors.css
bibliography: packages.bib
---



# Overview

While school choice policies are now ubiqitous are often presented as a strategy for addressing persistent racial disparities education, there is concern that such policies may maintain or increases such disparities due to resource hoarding by advantaged families or through increased racial segregation. Using a group threat/status anxiety framework, I hypothesize that increased racial diversification may induce white families to enroll their children in charter schools. I distinguish between proximal status threat, where racial diversification occurs in neighboring school districts, and direct status threat, where racial diversification occurs within local traditional public schools. I find evidence that both proximal and direct threat are associated with both increased foundings of charter schools and with white enrollment in charter schools. [IMPLICATIONS]


# Data Methods

**Data**

Panel of California school district data from 2000-2015. Sources:

- District enrollment and finance data: The Common Core of Data (accessed via the Urban Institutes API) 
- District poverty data: Small Area Income and Poverty Estimates (accessed via the Urban Institutes API) 
- District geographic boundaries: The NCES EDGE database

**Proximal status threat**

$$ProxThreat_{ij} = \theta_{ij} * \gamma_{ij}$$
where $\theta_{ij} = \frac{White_{ij}}{Total_{ij}}$ and $\gamma_{ij} = \frac{\sum_1^k Blk_{kj}, Hisp_{kj}}{\sum_1^k Total_{kj}}$

**Direct status threat**

$$DirThreat_{ij} = \frac{BlkTPS{ij}, HispTPS{ij}}{Total_{ij}}$$
$i$ indexes districts, $j$ indexes years, and $k$ indexes districts that neighbor $i$

**Analytic model: Fixed Effects Regression**

$$ Y_{ij} = \beta_1ProxThreat_{ij} + \beta_2DirThreat_{ij} + X_{it}\beta + \alpha_i + \gamma_j + \epsilon_{ij} $$


# Results

Usually you want to have a nice table displaying some important results that you have calculated. In `posterdown` this is as easy as using the `kable` table formatting you are probably use to as per typical R Markdown formatting.

You can reference tables like so: Table \@ref(tab:mytable). Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam placerat augue at velit tincidunt semper. Donec elementum porta posuere. Nullam interdum, odio at tincidunt feugiat, turpis nisi blandit eros, eu posuere risus felis non quam. Nam eget lorem odio. Duis et aliquet orci. Phasellus nec viverra est.

<table>
<caption>(\#tab:mytable)Table caption.</caption>
 <thead>
  <tr>
   <th style="text-align:center;"> Sepal.Length </th>
   <th style="text-align:center;"> Sepal.Width </th>
   <th style="text-align:center;"> Petal.Length </th>
   <th style="text-align:center;"> Petal.Width </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 5.1 </td>
   <td style="text-align:center;"> 3.5 </td>
   <td style="text-align:center;"> 1.4 </td>
   <td style="text-align:center;"> 0.2 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 4.9 </td>
   <td style="text-align:center;"> 3.0 </td>
   <td style="text-align:center;"> 1.4 </td>
   <td style="text-align:center;"> 0.2 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 4.7 </td>
   <td style="text-align:center;"> 3.2 </td>
   <td style="text-align:center;"> 1.3 </td>
   <td style="text-align:center;"> 0.2 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 4.6 </td>
   <td style="text-align:center;"> 3.1 </td>
   <td style="text-align:center;"> 1.5 </td>
   <td style="text-align:center;"> 0.2 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 5.0 </td>
   <td style="text-align:center;"> 3.6 </td>
   <td style="text-align:center;"> 1.4 </td>
   <td style="text-align:center;"> 0.2 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 5.4 </td>
   <td style="text-align:center;"> 3.9 </td>
   <td style="text-align:center;"> 1.7 </td>
   <td style="text-align:center;"> 0.4 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 4.6 </td>
   <td style="text-align:center;"> 3.4 </td>
   <td style="text-align:center;"> 1.4 </td>
   <td style="text-align:center;"> 0.3 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 5.0 </td>
   <td style="text-align:center;"> 3.4 </td>
   <td style="text-align:center;"> 1.5 </td>
   <td style="text-align:center;"> 0.2 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 4.4 </td>
   <td style="text-align:center;"> 2.9 </td>
   <td style="text-align:center;"> 1.4 </td>
   <td style="text-align:center;"> 0.2 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 4.9 </td>
   <td style="text-align:center;"> 3.1 </td>
   <td style="text-align:center;"> 1.5 </td>
   <td style="text-align:center;"> 0.1 </td>
  </tr>
</tbody>
</table>

Or with figures: Figure \@ref(fig:standard-plot), or Figure \@ref(fig:morefigs).

<div class="figure" style="text-align: center">
<img src="aefp_poster_files/figure-html/standard-plot-1.png" alt="Great figure!" width="80%" />
<p class="caption">(\#fig:standard-plot)Great figure!</p>
</div>


```r
data <- iris

plot(x = data$Sepal.Length, 
     y = data$Sepal.Width, 
     col = data$Species,
     pch = 19, 
     xlab = "Sepal Length (cm)",
     ylab = "Sepal Width (cm)")
```

<div class="figure">
<img src="aefp_poster_files/figure-html/morefigs-1.png" alt="Amazing, right?!" width="80%" />
<p class="caption">(\#fig:morefigs)Amazing, right?!</p>
</div>

# Next Steps

Aliquam sed faucibus risus, quis efficitur erat. Vestibulum semper mauris quis tempus eleifend. Aliquam sagittis dictum ipsum, quis viverra ligula eleifend ut. Curabitur sagittis vitae arcu eget faucibus. In non elementum felis. Duis et aliquam nunc. Nunc pulvinar sapien nunc, vel pretium nisi efficitur in. Fusce fringilla maximus leo et maximus. Fusce at ligula laoreet, iaculis mi at, auctor odio. Praesent sed elementum justo. Aenean consectetur risus rhoncus tincidunt efficitur. Praesent dictum mauris at diam maximus maximus [@R-posterdown].

# Conclusion

Try `posterdown` out! Hopefully you like it!



# References
