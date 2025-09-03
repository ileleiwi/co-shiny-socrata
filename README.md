# Colorado Public Health — R Shiny (Socrata SODA v2)

Interactive Shiny app that queries Colorado’s open-data **Socrata SODA v2** endpoint
(`/resource/<VIEW_ID>.json` using `$query`) and shows descriptive stats by date/group.

- **Default dataset:** `a498-a63g` (you can swap the view id anytime).  
  _Note: this demo view doesn’t have a true date column; the time-series will be blank until you use a dataset with dates._

---

## Requirements

- **R** (≥ 4.2)
- Internet access to `https://data.colorado.gov`
- (Recommended) a **Socrata App Token**

---

## 1) Get the code

```bash
# first time
git clone https://github.com/<your-username>/<your-repo>.git
cd <your-repo>

# later, to update
git pull
```

---

## 2) Install R packages

```bash
R -q -e "source('install.R')"
```

---

## 3) Set your Socrata App Token

Create a token at <https://data.colorado.gov/profile/edit/developer_settings>, then set it as an environment variable:

**macOS / Linux (bash/zsh)**
```bash
export SOCRATA_APP_TOKEN="your_token_here"
```

**Windows PowerShell**
```powershell
$env:SOCRATA_APP_TOKEN = "your_token_here"
```

**R (temporary for this session)**
```r
Sys.setenv(SOCRATA_APP_TOKEN = "your_token_here")
```

> The app reads the token from `config.yml` → `app_token_env` (defaults to `SOCRATA_APP_TOKEN`).  
> The sidebar will show **“Token detected ✓”** when it’s available.

---

## 4) Run the app locally

```bash
R -q -e "shiny::runApp(port=8080, host='127.0.0.1', launch.browser=TRUE)"
```

If your browser doesn’t open automatically, visit: <http://localhost:8080>

---

## 5) Use the app

1. Click **Health check** → should report row/column counts.  
2. In the **SoQL** box, start with:  
   ```sql
   SELECT * LIMIT 100
   ```
3. Click **Fetch**.  
4. In **Pickers**:
   - Choose a **Group** column (e.g., `race_ethnicity`).
   - Choose a **Numeric** column (e.g., `median_household_income`).
   - (Optional) Choose a **Date** column + range if your dataset has dates.
5. Explore:
   - **Time series** (sum per date; requires a date column).
   - **Grouped stats** bar chart (Sum / Mean / Median / Count).
   - **Grouped summary** table and **Raw data** (both downloadable as CSV).

---

## 6) Switch datasets

Two ways:

- **Quick (UI only):** Replace **Socrata View ID** in the sidebar (e.g., `abcd-1234`) and adjust your SoQL.
- **Config (persisted):** Edit `config.yml`:
  ```yaml
  socrata_domain: data.colorado.gov
  view_id: a498-a63g
  app_token_env: SOCRATA_APP_TOKEN
  ```

> Tip: Use SoQL for server-side aggregation, e.g.:
> ```sql
> SELECT date_trunc_ymd(date_col) AS day, group_col, SUM(value_col) AS n
> GROUP BY day, group_col
> ORDER BY day
> LIMIT 50000
> ```

---

## Troubleshooting

- **Token shows “missing”** → ensure the env var is set in the same shell you launch R from.  
- **HTTP 403** → token not sent or invalid.  
- **HTTP 400** → SoQL syntax error. Try `SELECT * LIMIT 10`.  
- **Port busy** → change the port, e.g. `port=8888`.  
- **Nothing loads** → open <http://localhost:8080> manually.

---

## Project structure (key files)

- `app.R` — Shiny UI/server (theme, grouped stats plot, downloads)  
- `R/socrata_v2.R` — robust SODA v2 client (raw→UTF-8, SoQL paging)  
- `install.R` — installs CRAN deps  
- `config.yml` — domain, default `view_id`, and token env var

---

## License

MIT
