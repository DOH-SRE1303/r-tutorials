# r-tutorials
Tutorials for common R workflows, presented at R Office Hours

Each notebook focuses on a different topic. The goal of the notebooks is to show examples of the topic on public health-style data. The notebooks try to show a variety of ways to do things and the various options available - the options are meant to be examples, but are not always best practices.
All data needed to run these notebooks can be found in the data folder in this repo. The data is all publicly available, or is synthetic data that has been created to mimic real world public-health data.
For each notebook there is a .qmd file, which is a Quarto document which can be used to run the code yourself, and there is a .md file, which is a markdown document which allows you to see the code and visualizations in their rendered form in this repo.

The notebooks (found in the notebooks folder) are:
- exploratory_data_analysis: this notebook shows an example of how to start exploring a data set, including cleaning the data, grouping and combining data, calculating summary statistics, and visualizing the data
- survey_analysis: this notebook gives examples of how to analyze survey data, using the R {survey} package

## Additional Learning Resources

- [Modern R Development Guide](https://gist.github.com/sj-io/3828d64d0969f2a0f05297e59e6c15ad): documentation of current best practices for R development, emphasizing modern tidyverse patterns, performance, and style
- [The Epidemiologist R Handbook](https://epirhandbook.com/en/): R reference manual, includes common epidemiological examples
- [R 4 Epidemiology](https://www.r4epi.com/): R textbook, with public health examples
- [Population Health Data Science with R](https://bookdown.org/medepi/phds/): R textbook aimed at public health epidemiologists and health care analysts
- [Statistical Inference via Data Science: A ModernDive into R and the Tidyverse](https://moderndive.com/index.html): examples of getting started in R, data visualization, wrangling, statistics
- [R for Data Science](https://r4ds.had.co.nz/): R textbook for data science workflows and learning tidyverse conventions
- [Advanced R](https://adv-r.hadley.nz/index.html): R textbook for more advanced R concepts
- [swirl](https://github.com/swirldev/swirl_courses?tab=readme-ov-file#swirl-courses): interactive R courses
- [Applied Epi Tutorials](https://www.appliedepi.org/resources/tutorials.html): Introductory R courses for Epis. You will need to make a free account before you can access the tutorials.

## Data Visualization Resources
- [The R Graph Gallery](https://r-graph-gallery.com/): a collection of many different types of charts made with R, with a focus on using tidyverse and ggplot2, good for inspiration and showing the different possibilities of creating visualizations with R. Also shows the various [theme options](https://r-graph-gallery.com/192-ggplot-themes.html) when using ggplot2.
- [Modern Data Visualization with R](https://rkabacoff.github.io/datavis/index.html): A textbook that walks through all the aspects of creating visualizations with ggplot2, and shows other options for visualizations
- [Accessible color palette generator](https://venngage.com/tools/accessible-color-palette-generator): creates many different options for an accessible color palette. The {[viridis](https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html)} package in R is also useful for accessible color palettes.
- [Saloni's guide to data visualization](https://www.scientificdiscovery.dev/p/salonis-guide-to-data-visualization): an interesting blog post about data visualization, why it is important, and how to make it better. [Our World in Data](https://ourworldindata.org/) has a lot of good examples of clear data visualizations. 
- [Create your own custom ggplot2 theme](https://rfortherestofus.com/2025/04/ggplot2-theme): a guide for creating a custom theme for ggplot2 plots, can be useful for establishing a consistent style guide for your team. There is also an [explainer for the BBC's custom ggplot2 theme](https://medium.com/bbc-visual-and-data-journalism/how-the-bbc-visual-and-data-journalism-team-works-with-graphics-in-r-ed0b35693535)

## Other Useful Resources & Packages
- [Mockaroo](https://mockaroo.com/): useful for creating mock data sets
- [esquisse](https://dreamrs.github.io/esquisse/): package for interactively exploring creating data visualizations