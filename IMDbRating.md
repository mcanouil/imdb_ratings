---
title: "My IMDb Ratings Analysis"
params:
  output_code: FALSE
  n_cores: !r parallel::detectCores()
  theme_dark: TRUE
  dpi: 150
  gg_fontsize: 9
output:
  html_document:
    theme: simplex
    fig_width: 5
    fig_height: 3.5
    self_contained: true
    keep_md: true
editor_options: 
  chunk_output_type: console
---







# Infography {.tabset}

## 2014   



<img src="./images/ggplot_time-1.png" style="display: block; margin: auto;" />

## 2015   



<img src="./images/ggplot_time-2.png" style="display: block; margin: auto;" />

## 2016   



<img src="./images/ggplot_time-3.png" style="display: block; margin: auto;" />

## 2017   



<img src="./images/ggplot_time-4.png" style="display: block; margin: auto;" />

## 2018   



<img src="./images/ggplot_time-5.png" style="display: block; margin: auto;" />

## 2019   

<img src="./images/ggplot_time-6.png" style="display: block; margin: auto;" />

# GIF

## Ratings distribution
<img src="./images/gif_distribution-1.gif" style="display: block; margin: auto;" />

## Movies at theatre {.tabset}




### Circular-smooth
<img src="./images/gif_smooth-1.gif" style="display: block; margin: auto;" />

### Radar-radius
<img src="./images/gif_radar-1.gif" style="display: block; margin: auto;" />

### Radar-waves-point
<img src="./images/gif_waves_points-1.gif" style="display: block; margin: auto;" />

### Radar-waves-path
<img src="./images/gif_waves_path-1.gif" style="display: block; margin: auto;" />




