## GitHub Actions

Below is a breakdown of how I used GitHub Actions to automate the data collection and deployment of the app.

## Events

The first section of the workflow defines the event that triggers the workflow to run. 

For my app, the gas price data I use is updated once per week by the [EIA]() "around 5PM EST every Monday" according to their documentation. So, in order to automate the data collection for my app, I needed to kickoff the workflow every Monday night a little after 5PM EST so that it could collect the latest data. I decided to set the time to 7:30PM EST to err on the side of caution in case EIA was late updating their data.

The workflow requires the date/time to be in cron format, so I converted "7:30PM EST every Monday" to a cron date/time of '30 23 * * 1' by using this [cron time converter](https://crontab.guru/).

In addition to scheduling a time for the workflow to run, I also set the workflow to kickoff whenever I made a push to the main branch with `push: branches: main`. This is useful for whenever you need to make an update to your process but don't want to wait until the workflow is scheduled to run.

```
on:
  schedule:
    - cron: '30 23 * * 1'
  push:
    branches: main
```

## Runner environment

This is the start of the jobs that will run in a runner environment. There are multiple virtual environments available that can be used within `runs-on` but ubuntu-latest uses the fewest minutes. 

The number of minutes you use matters because there's a 2,000 minute per month limit on public repositories. And if you wish to use GitHub Actions on a private repository, you'll be charged per minute you use. 

One way that you can avoid racking up unnecessary minutes is by adding `timeout-minutes` to your workflow. Sometimes data pipelines can get stuck, especially if you're pulling data from another site. This will ensure that your workflow will automatically stop after it exceeds the number of minutes specified. 

```
name: Get EIA Gas Data

jobs:
  generate-data:
    name: update data
    runs-on: ubuntu-latest
    timeout-minutes: 75
```

## Environment variables

This section sets all of the environment variables for the virtual environment.

The first variable listed, `RENV_PATHS_ROOT`, is the directory of the R environment which is created by the {renv} package. 

The remaining variables all contain the necessary keys needed to access my GitHub repository (`GITHUB_PAT`), the EIA API (`EIA_API_KEY`), and my shinyapps.io account (`SHINY_ACCOUNT`, `SHINY_TOKEN`, `SHINY_SECRET`). 

Each of these variables are encrypted and stored in the repository as 'secrets'. This is a GitHub feature that allows you to store your keys secretly within your repository so that they don't appear anywhere in your underlying code. For more info about how to store secret keys in your repository, see this article: [GitHub Secrets](https://docs.github.com/en/actions/security-guides/encrypted-secrets).

```
env:
  RENV_PATHS_ROOT: ~/.local/share/renv
  GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
  EIA_API_KEY: ${{ secrets.EIA_API_KEY }}
  SHINY_ACCOUNT: ${{ secrets.SHINY_ACCOUNT }}
  SHINY_TOKEN: ${{ secrets.SHINY_TOKEN }}
  SHINY_SECRET: ${{ secrets.SHINY_SECRET }}
```

## Setup

This section begins the steps of the workflow.

The first command, `uses: actions/checkout@v2`, just does a quick check to make sure your repository is available. The second command, `uses: r-lib/actions/setup-r@v2`, sets up the virtual R environment for your workflow. 

```
steps:

  - uses: actions/checkout@v2
  - uses: r-lib/actions/setup-r@v2
    with:
      # don't reinstall R
      install-r: false
      # use RStudio's CRAN mirror with precompiled binaries
      use-public-rspm: true
```

## Restore packages

This step installs the [{renv}](https://rstudio.github.io/renv/index.html) package and loads the packages that are used within your repository. 

If any of the packages loaded within your project are not on CRAN or if you're using a newer developer version of the package, you'll need to install the {remotes} package first, so that those packages can be installed with `remotes::install_github()`.

<i>Note: before this step is done, you need to save the state of your project library in a lockfile with `renv::snapshot()`. This will create a `renv.lock` file that contains all the current version of the packages for your project.</i>

```
- name: Install remotes
  run: |
    install.packages("remotes")
    remotes::install_deps(dependencies = TRUE)
  shell: Rscript {0}
  
- name: Restore packages
  shell: Rscript {0}
  run: |
    if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
    renv::restore()
```

## Calling a script within your repository

In my [USgasprices](https://github.com/kcuilla/USgasprices/blob/main/data-raw/get_data.R) repository, I have a script called `get_data.R` that connects to the EIA API, pulls the data needed for my project, and saves it within my repository. The code chunk below kicks off that script within the workflow.  

```
- name: Get data
  run: Rscript -e 'source("data-raw/get_data.R")'
```

<i>Note: in order to connect to EIA's API, I needed to include a couple additional sections to the workflow. Please see the 'Additional Sections' section below for those additional commands.</i>

## Commit to repository

After the data has been refreshed and saved within my repository, GitHub Actions commits and pushes the results with the message "Weekly gas data updated".

If the GitHub Actions job fails for whatever reason, a report is emailed to my email address.

```
- name: Commit results
  run: |
    git config --local user.email "actions@github.com"
    git config --local user.name "GitHub Actions"
    git add data/*
    git commit -m 'Weekly gas data updated' || echo "No changes to commit"
    git push origin || echo "No changes to commit"
```

## Deploy app

Now that my repository is updated, the last step of the process is to deploy the app to shinyapps.io.

The functions needed to deploy the app come from the {rsconnect} package, so this package needs to be installed before deployment. 

To deploy the app to shinyapps.io, we can use our secret Shiny variables set earlier as the credentials needed for the account. 

```
- name: Install rsconnect
  run: |
    install.packages("rsconnect")
  shell: Rscript {0}
  
- name: Deploy to shinyapps.io
  run: |
    rsconnect::setAccountInfo(name="${{secrets.SHINY_ACCOUNT}}", token="${{secrets.SHINY_TOKEN}}", secret="${{secrets.SHINY_SECRET}}")
    rsconnect::deployApp(appName = 'usgasprices')
  shell: Rscript {0}
```

## Additional resources

The additional sections below were needed for my workflow to run, so unless your repository has similar requirements to mine, it's likely you won't need to add these sections to your workflow. 

### Create .Renviron file

Within the `get_data.R` script of my repository, I extract my EIA API key from my R environment so that I can connect to the EIA API and pull the data needed for my project. In order for this to occur during my workflow, I need to create an .Renviron file within my virtual environment and store the key within that environment.  

```
- name: Create and populate .Renviron file
  run: |
    echo EIA_API_KEY="$EIA_API_KEY" >> ~/.Renviron
  shell: bash
```

### Install additional package dependencies

It took a lot of debugging to figure out what was needed in the section below. Initially, I was getting an error message that the {eia} package could not be installed. Looking into it further, I learned that the {eia} package depends on the {httr} package which depends on {libcurl}, and since {libcurl} wasn't present in the virtual environment, it was failing to install and load the {eia} package. Therefore, the first command below, which is needed before the packages are installed and loaded in the workflow, installs {libcurl} to prevent that error and allow the use of {eia}. 

The second command below, installs the dev version of {rgdal} which, through more debugging, I learned is needed in order to run the {ggiraph} package on Shiny. Again, unless you are specifically using these packages within a virtual R environment, you likely can skip these two installs in your workflow.  

```
- name: Install libcurl
  run: |
    sudo apt-get update -y
    sudo apt-get install -y libcurl4-openssl-dev
    
- name: Install rgdal 
  run: |
    sudo apt install libgdal-dev 
```
