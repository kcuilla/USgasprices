## GitHub Actions

Below is a breakdown of how I used GitHub Actions to automate the data collection and deployment of the app.

## Section 1

This section tells GitHub Actions when to do it's thing.

The first part I will cover is the `schedule`. EIA updates the data needed for this app every Monday "around" 5PM EST. To err on the safe side, I scheduled the workflow to kickoff at 6PM EST on Mondays. the In order to convert that time to a cron date/time format, I used the [Crontab](https://crontab.guru/#5_21_*_*_1) website which was helpful in understanding what values I needed.

The second part is `push`. Whenever I make changes to the GitHub repo, such as adding a new chart to the Shiny app or changing the appearance of the app, when I push the changes to the main branch, the workflow will automatically kick off.

```
on:
  schedule:
    - cron: '5 22 * * 1'
  push:
    branches: main
```

## Section 2

This is the name of the entire workflow.

```
name: Get EIA Gas Data
```

## Section 3

This is the start of the jobs that will run in a runner environment. There are multiple virtual environments available that can be used within `runs-on` but ubuntu-latest uses the fewest minutes. 

I also specified a timeout with `timeout-minutes` to kill the job after 45 minutes in case the script gets stuck pulling the data.

```
jobs:
  generate-data:
    name: update data
    runs-on: ubuntu-latest
    timeout-minutes: 45
```

## Section 4

This section sets all of the environment variables for the virtual environment.

`RENV_PATHS_ROOT` is the directory of the r environment which is created by the {renv} package. 

`GITHUB_PAT` is the authentication key needed for GitHub

`EIA_API_KEY` is the API key needed to access the EIA API.

`SHINY_ACCOUNT`, `SHINY_TOKEN`, `SHINY_SECRET` are the login credentials for my shinyapps.io account.

For the environment variables below that contain the words 'secrets', these are all stored and encrypted within the repo itself. The benefit of using GitHub secrets is that your keys/tokens are safe and encrypted and won't show anywhere in the underlying code of your repo. For info about how to store the secrets, see this article: [GitHub Secrets](https://docs.github.com/en/actions/security-guides/encrypted-secrets).

```
env:
  RENV_PATHS_ROOT: ~/.local/share/renv
  GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
  EIA_API_KEY: ${{ secrets.EIA_API_KEY }}
  SHINY_ACCOUNT: ${{ secrets.SHINY_ACCOUNT }}
  SHINY_TOKEN: ${{ secrets.SHINY_TOKEN }}
  SHINY_SECRET: ${{ secrets.SHINY_SECRET }}
```

## Section 5

This section begins the steps of the workflow.

The first two things that happen is:
1) GitHub Actions checks the repo to make sure it's available
2) R is set up on the virtual environment

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

Note: you can see a history of the GitHub Actions workflow for each run by clicking on the Actions tab in your repo as shown below:

![image](https://user-images.githubusercontent.com/17436783/177876462-aa9c707e-2bf4-4fce-adb8-113252ef64c6.png)

## Section 6

This section creates and populates the .Renviron file and store the EIA API key within the environment so that it can be retrieved with `Sys.getenv("EIA_API_KEY")` within the `get_data.R` script in this repo.

```
- name: Create and populate .Renviron file
  run: |
    echo EIA_API_KEY="$EIA_API_KEY" >> ~/.Renviron
  shell: bash
```

## Section 7

It took a lot of debugging to figure out what was needed in the section below. Initially, I was getting an error message that the {eia} package could not be installed. Looking into it further, I learned that the {eia} package depends on the {httr} package which depends on libcurl, and since libcurl wasn't present in the virtual environment, it was failing to install and load the {eia} package. Therefore, this command, which is needed before the packages are installed and loaded in the workflow, installs libcurl to prevent that error and allow the use of {eia}.  

```
- name: Install libcurl
  run: |
    sudo apt-get update -y
    sudo apt-get install -y libcurl4-openssl-dev
```

## Section 8

The {remotes} and {rsconnect} packages are installed so that the app can be deployed on shinyapps.io.

```
- name: Install remotes and rsconnect
  run: |
    install.packages(c("remotes", "rsconnect"))
    remotes::install_deps(dependencies = TRUE)
  shell: Rscript {0}
```

## Section 9

These steps cache the packages within the environment so that they don't have to be re-installed each time the GitHub Actions workflow starts.

```
- name: Cache packages
  uses: actions/cache@v1
  with:
    path: ${{ env.RENV_PATHS_ROOT }}
    key: ${{ runner.os }}-renv-${{ hashFiles('**/renv.lock') }}
    restore-keys: |
      ${{ runner.os }}-renv-

- name: Restore packages
  shell: Rscript {0}
  run: |
    if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
    renv::restore()
```

## Section 10

This section runs the `get_data.R` script within this repo which pulls in the data from [EIA](https://www.eia.gov/petroleum/gasdiesel/) and saves it within the R environment.

```
- name: Get data
  run: Rscript -e 'source("data-raw/get_data.R")'
```

## Section 11

This section commits the updated data to the repo. If the GitHub Actions job fails for whatever reason, a report is emailed to my email address.

```
- name: Commit results
  run: |
    git config --local user.email "actions@github.com"
    git config --local user.name "GitHub Actions"
    git add data/*
    git commit -m 'Weekly gas data updated' || echo "No changes to commit"
    git push origin || echo "No changes to commit"
```

## Section 12

Now that the data is updated and commited to the repo, the app is deployed to shinyapp.io using my encrypted credentials below!

```
- name: Deploy to shinyapps.io
  run: |
    rsconnect::setAccountInfo(name="${{secrets.SHINY_ACCOUNT}}", token="${{secrets.SHINY_TOKEN}}", secret="${{secrets.SHINY_SECRET}}")
    rsconnect::deployApp(appName = 'usgasprices')
  shell: Rscript {0}
```
