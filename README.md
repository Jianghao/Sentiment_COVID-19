# Global evidence of expressed sentiment alterations during the COVID-19 pandemic
Replication materials for J Wang#, Y Fan#, J Palacios, Y Chai, N Jeanrenaud, N Obradovich, C Zhou*, S Zheng* (2021).

The materials in this repository allow users to reproduce the data analysis and figures appearing in the paper.

If you have questions or suggestions, please contact Jianghao Wang at wangjh@mit.edu

## Computational requirement
- R 4.0+
- Python 3.7-3.9
- Stata 14.0+

## Organization of repository
- input: all the necessary input data 
- figures: the main text final figures
  - fig1/
  - fig2/
  - fig3/
- tables: output tables
- script:
  - 01_sentiment/ : sentiment imputation
  - 02_visual/: exploration analysis
  - 03_sentiment_recovery/: model the sentiment recovery half-life 
  - 04_sentiment_shock/: model the sentiment shock size
  - 05_lockdown_effect/: model the impact of lockdown on expressed sentiment
