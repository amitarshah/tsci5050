# Ripcord
#### by Alex F. Bokov, PhD
#### UT Health Population Health Sciences and Institute for the Integration of Medicine and Science
Originally this was a collection of template scripts to support a data science course I teach in the TSCI program at UT-Health San Antonio. This is now a standalone framework for rapid setup of analysis projects in any directory you want, using any libraries you want, adding whatever code you want for any purpose you want. This just gives you access to some convenient libraries and functions along with some starter "scriports" (ordinary R scripts but with a comment format that allows them to _also_ get compiled into HTML/Word/PDF).

## Installation:
Through RStudio (recommended but not required) or any other means of _interactively_ accessing an R command-line, ```setwd()``` to a folder which is empty or which has files you want to be part of your project (_and which you have already backed up because this is still a work in progress_) and then execute the following command:

```r
source('https://raw.githubusercontent.com/bokov/ripcord/enh_wincompat/quickstart.R');
```    
You will be presented with a disclaimer and then prompted to select the data file/s you will use in your analysis. This process is designed to let you keep your data and code as separate as possible (so you can check your code into repos without disclosing your data). Therefore, it is recommended that you select your local input data files _wherever they currently are_ on your computer and _not_ move them into the project folder. The scripts will record their locations and find them whenever they are needed. They will also create a simulated copy of each data file in the `data` subfolder that will be created within your project folder. That way you can choose to check the simulated versions of your data into GitHub so that others can test out your scripts without needing access to your actual data. However, you aren't forced to manage your data in any particular way. You aren't even required to have `git` installed on your computer in order to use Ripcord.

After you are done selecting and naming files, a bunch of scriports will be triggered and if successful will generate further documentation about how to use Ripcord.

I am grateful to you for [bug reports and any other feedback you may have to offer](https://github.com/bokov/ripcord/issues).

-- Alex F. Bokov
