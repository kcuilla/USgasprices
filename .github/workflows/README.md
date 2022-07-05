## GitHub Actions

Below is a breakdown of how I used GitHub Actions to automate the data collection and deployment of the app.

## Part 1

This section tells GitHub Actions when to do it's thing.

The first part I will cover is the `schedule`. EIA updates the data needed for this app every Monday at 5PM EST. In order to convert that to a cron date/time format, I used the [Crontab](https://crontab.guru/#5_21_*_*_1) website which was helpful in understanding what values I needed.

The second part is `push`. Whenever I make changes to the GitHub repo, such as adding a new chart to the Shiny app or changing the appearance of the app, when I push the changes to the main branch, the GitHub Actions will automatically kick off.

```
on:
  schedule:
    - cron: '5 21 * * 1'
  push:
    branches: main
```

## Part 2

This is the name of the entire workflow.

```
name: Get EIA Gas Data
```

## Part 3

To be continued...