---
title: "climateYear Manual"
subtitle: "v.0.1.0.9000"
date: "Last updated: 2026-10-09"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
always_allow_html: true
---

# climateYear Module

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:climateYear) *climateYear*



[![made-with-Markdown](figures/markdownBadge.png)](https://commonmark.org)

<!-- if knitting to pdf remember to add the pandoc_args: ["--extract-media", "."] option to yml in order to get the badge images -->

#### Authors:

Ian Eddy aut cre ian.eddy@nrcan-rncan.gc.ca
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

Provide a brief summary of what the module does / how to use the module.

Module documentation should be written so that others can use your module.
This is a template for module documentation, and should be changed to reflect your module.

### Module inputs and parameters

Describe input data required by the module and how to obtain it (e.g., directly from online sources or supplied by other modules)
If `sourceURL` is specified, `downloadData("climateYear", "..")` may be sufficient.

Table \@ref(tab:moduleInputs-climateYear) shows the full list of module inputs.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-climateYear)(\#tab:moduleInputs-climateYear)List of (ref:climateYear) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> historicalClimateRasters </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> optional list of SpatRasters, with layers corresponding to years. Each list element should be a different variable with corresponding names. Each layer should be named following the convention 'year&lt;year&gt;`, e.g. year2009. The object is used solely to determine the available years from which to sample </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> projectedClimateRasters </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> optional list of SpatRasters, with layers corresponding to years. Each list element should be a different variable with corresponding names. Each layer should be named following the convention 'year&lt;year&gt;`, e.g. year2009. The object is used solely to determine the available years from which to sample </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

Summary of user-visible parameters (Table \@ref(tab:moduleParams-climateYear))


<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-climateYear)(\#tab:moduleParams-climateYear)List of (ref:climateYear) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> .studyAreaName </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Human-readable name for the study area used - e.g., a hash of the studyarea obtained using `reproducible::studyAreaName()` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .seed </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Named list of seeds to use for each event (names). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should caching of events or module be used? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> samplingEndYear </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> if randomly sampling the year for a given climate, the simulation year at which to end this process, if applicable </td>
  </tr>
  <tr>
   <td style="text-align:left;"> samplingRange </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Vector giving years from which to sample. The default is all years in sort(unique(c(historicalClimateRaster, projectedClimateRasters))) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> samplingStartYear </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> if randomly sampling the year for a given climate, the simulation year at which to begin this process </td>
  </tr>
</tbody>
</table>

### Events

Describe what happens for each event type.

### Plotting

Write what is plotted.

### Saving

Write what is saved.

### Module outputs

Description of the module outputs (Table \@ref(tab:moduleOutputs-climateYear)).

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-climateYear)(\#tab:moduleOutputs-climateYear)List of (ref:climateYear) outputs and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> climateYear </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> a year from projectedClimateRasters, updated annually </td>
  </tr>
  <tr>
   <td style="text-align:left;"> climateYearRecord </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> record of which climate year was used for which simulation year </td>
  </tr>
  <tr>
   <td style="text-align:left;"> currentClimateRasters </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> a single-year subset of projected or historical rasters </td>
  </tr>
</tbody>
</table>

### Links to other modules

Describe any anticipated linkages to other modules, such as modules that supply input data or do post-hoc analysis.

### Getting help

-   provide a way for people to obtain help (e.g., module repository issues page)

## References

<!-- autogenerated from bibligraphy -->
