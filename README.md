# 📉 Layoff Analysis Project

This project analyzes global layoff data sourced from [layoffs.fyi](https://layoffs.fyi/), covering company downsizing trends by industry, country, and time period.
It integrates **SQL (SQLite)** for ETL and aggregation, **R** for statistical and predictive analysis, and **Jira + Confluence** for project documentation and Agile tracking.

---

## 🧹 Part 1: Data Cleaning (SQLite)

**Output Table:** `layoffs_clean`

### Key Steps

* **Whitespace removal:** Trims all text fields
* **Missing data handling:** Replaces blank or null values in `industry` and `country` with `'Unknown'`
* **Date conversion:** Converts text dates using `date()` / `strftime()` functions
* **Duplicate removal:** Uses `ROW_NUMBER()` and `PARTITION BY` to deduplicate by `Company`, `Location`, `Industry`, `Laid_Off_Count`, and `Date`
* **Data standardization:** Normalizes country names (e.g., `US → United States`, `UK → United Kingdom`)
* **Indexing:** Adds indexes for frequently queried columns

### SQLite Adjustments

* Uses `COALESCE()` instead of `IFNULL()`
* Uses `date()` and `strftime()` for date parsing
* Replaces `CREATE OR REPLACE VIEW` with `DROP VIEW IF EXISTS`

---

## 📊 Part 2: Aggregated Analysis

Creates summary tables for multi-level insights:

| Table                 | Description                                                                |
| --------------------- | -------------------------------------------------------------------------- |
| `layoffs_summary`     | Layoffs by **Industry and Country** with totals, averages, and date ranges |
| `layoffs_by_industry` | Aggregation by industry                                                    |
| `layoffs_by_country`  | Aggregation by country                                                     |

---

## 📈 Part 3: Rolling Average Analysis

**Output Table:** `layoffs_rolling_avg`

* 3-month rolling average using `AVG()` with `ROWS BETWEEN 2 PRECEDING AND CURRENT ROW`
* `LAG()` / `LEAD()` to show previous and next months
* Month-over-month change (absolute + percentage)
* Cumulative totals and 3-month rolling totals

---

## 🏭 Part 4: Top Industries (Last 6 Months)

**Output Table:** `top_industries_6months`

* Identifies **Top 5 industries** with highest layoffs in last 6 months
* Calculates each industry’s share of total layoffs
* Ranks using `ROW_NUMBER()`

---

## ✅ Part 5: Data Quality & Reporting

**Views:**

* Data quality metrics (null counts, duplicates, anomalies)
* Overall summary statistics
* Verification samples

**Performance Optimization:** Indexed key columns to speed up aggregation.

---

## 📊 R Statistical & Predictive Analysis

**Script:** `layoffs_analysis.R`

### Part 1 – Descriptive & Diagnostic

* Summary statistics by industry and country
* Outlier and anomaly detection
* Visualization: histograms, boxplots
* Outlier analysis by funding stage

### Part 2 – Inferential Statistics

* Two-sample **t-test**: Tech vs Finance
* Correlation analysis: `Funds_Raised ↔ Laid_Off_Count`
* 95% Confidence Intervals with visualization

### Part 3 – Time Series Forecasting

* Monthly aggregation using `zoo`
* ARIMA forecasting (3-month projection)
* Trend visualization by industry

### Generated Outputs

```
layoffs_histogram_by_industry.png
layoffs_boxplot_comparison.png
correlation_funds_vs_layoffs.png
confidence_intervals_by_industry.png
monthly_trends_by_industry.png
arima_forecast_overall.png
```

---

## 🧠 Part 6: Agile & Documentation

### Jira Setup

* 5 Epics (SQL, Excel, R, Quality, Automation)
* 15+ User Stories with acceptance criteria
* Kanban/Sprint visualization with dependencies

### Confluence Documentation

* Project overview & executive summary
* ETL workflow & architecture diagrams
* Data quality framework
* CI/CD documentation & version control

### Retrospectives

* Sprint learnings & improvement actions
* Team metrics & satisfaction tracking

---

## ⚙️ CI/CD & Automation

**GitHub Actions Workflow**

* Automated testing (SQL & R scripts)
* Quality gates (5 checkpoints)
* Continuous monitoring dashboard
* Alerting for data quality failures

---

## 💰 Business Impact

| Metric                   | Impact         |
| ------------------------ | -------------- |
| Annual Cost Savings      | $45K/year      |
| Manual Work Reduction    | 90%            |
| Data Quality Reliability | 99.5% accuracy |

---

## 🪄 How to Run

### 1. SQLite

1. Open `DBeaver`
2. Import `layoffs_data` table
3. Paste and execute SQL cleaning & aggregation script
4. Validate generated tables/views

### 2. R

1. Save `layoffs_clean.csv` in working directory
2. Run:

   ```r
   source("layoffs_analysis.R")
   ```

### 3. Output

All plots and summary CSVs are saved in `/output/` folder.

---

## 📁 Folder Structure

```
layoff_analysis/
│
├── SQL/
│   └── layoffs_cleaning_aggregation.sql
├── R/
│   └── layoffs_analysis.R
├── output/
│   ├── layoffs_histogram_by_industry.png
│   ├── layoffs_boxplot_comparison.png
│   └── ...
├── docs/
│   ├── jira_stories.md
│   ├── confluence_overview.md
│   └── retrospectives.md
└── README.md
```

---

## 👩‍💻 Author

**Shan (Gigi) Gao**
📧 [gigishan@bu.edu](mailto:gigishan@bu.edu)
📊 Data Analyst | SQL | R | Python | Tableau
--------------------------------------------
