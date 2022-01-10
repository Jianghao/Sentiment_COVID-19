# Global evidence of expressed sentiment alterations during the COVID-19 pandemic
Replication materials for J Wang#, Y Fan#, J Palacios, Y Chai, N Jeanrenaud, N Obradovich, C Zhou*, S Zheng* (2021).

The materials in this repository allow users to reproduce the data analysis and figures appearing in the paper.

If you have questions or suggestions, please contact Jianghao Wang at wangjh@mit.edu | wangjh@lreis.ac.cn

## Computational requirement
- R 4.0+
- Python 3.7-3.9
- Stata 14.0+

## Organization of repository
- input: all the necessary input data 
- figures: the main text final figures
- script:
  - 01_sentiment/ : sentiment imputation, see the repository: https://github.com/MIT-SUL-Team/global-sentiment
    - data: the traning and labeled_data for the global sentiment imputation
    - dict/sentiment_dicts: the emoji, hedonometer, and LIWC dictionaries
    - models: the multilingual data for the sentiment
    - notebooks: `sentiment clf evaluator.ipynb`
    - output
    - report
    - src: main model and sentiment imputation folders
      - `main_geography_imputer.py`
      - `main_sentiment_aggregator.py`
      - `main_sentiment_imputer.py`
      - `setup_emb_clf.py`
      - `setup_liwc.py`
      - utils: functions used for the sentiment imputation
        - `aggregation_utils.py`
        - `data_read_in.py`
        - `dict_sentiment_imputer.py`
        - `emb_clf_setup_utils.py`
        - `emb_sentiment_imputer.py`
  - 02_visual/: exploration analysis, see details in `figures`.
  - 03_sentiment_recovery/: This section reproduce the result of `Expressed sentiment alterations during COVID-19 pandemic`： the first measure--recovery half-life.
  - 04_sentiment_shock_and_lockdown_effect/: This section reproduce the result of `Expressed sentiment alterations during COVID-19 pandemic`： the second measure--sentiment drop and the results of `Impacts of lockdowns on expressed sentiment`
