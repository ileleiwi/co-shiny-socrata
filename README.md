# Colorado Public Health (Socrata v3) — R Shiny

R Shiny app that queries Colorado's open-data **Socrata v3** endpoint (`/api/v3/views/<VIEW_ID>/query.json`) and shows descriptive stats.

Default dataset: `a498-a63g` (swap the view id anytime).

Setup app token at `https://data.colorado.gov/profile/edit/developer_settings`
then run `export SOCRATA_APP_TOKEN="your_token_here"` before running the app