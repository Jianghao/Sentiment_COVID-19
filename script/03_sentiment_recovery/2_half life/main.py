# -*- coding: utf-8 -*-
# @File       : main.py
# @Author     : Yuchen Chai
# @Date       : 2021-10-03 21:30
# @Description:

# ------------------------
# import packages
# ------------------------
import os
import FitCurve
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np

# Set workspace
DIR_HOME = "Sentiment_COVID19/"
DIR_DATA = os.path.join(DIR_HOME, "data/")
DIR_OUTPUT = os.path.join(DIR_HOME, "script/03_sentiment_recovery/2_half life/")

mFunction = "senti_bert_std_trend"

# Get the list of countries
df_final = pd.read_csv(os.path.join(DIR_DATA, "country_info_STL_0722.csv"),index_col=0)
df_valid = pd.read_csv(os.path.join(DIR_DATA, "country_info_Trend_Validity.csv"),index_col=0)
df_final = df_final.replace(0,np.nan)
full_nationList = sorted(list(df_final['country'].unique()))
df_country = pd.read_csv(os.path.join(DIR_DATA, "country_info_Drop_Min.csv"),index_col=0)

df_country = df_country.dropna(subset=['drop_senti_bert',"min_senti_bert"])

nationList = sorted(list(df_country['Nation'].unique()))
valid_nationList = sorted(list(df_valid[df_valid['Quality']==1]['Nation'].unique()))

minDateList = df_country.to_dict('record')
minDateDict = {}
dropDateDict = {}
for item in minDateList:
    minDateDict[item['Nation']] = item['min_senti_bert']
    dropDateDict[item['Nation']] = item['drop_senti_bert']
    pass

Parameter = []
ret_fit = []

special_country = []
for nation in full_nationList:
    print(nation)
    df = df_final[(df_final['country'] == nation)]
    if nation in nationList:
        try:
            df_dates = df["tweet_date"].tolist()

            mPara, mDraw, target, target_sd = FitCurve.Fit_Curve_V4(nation, df, minDateDict[nation], dropDateDict[nation],mFunction)

            Parameter = Parameter + mPara

            mDates = []
            for item in df_dates:
                mDates.append({"Date": item, "normal-ci": "","normal":"","normal+ci":""})
                pass
            mDraw = mDraw.to_dict("records")
            for i in range(len(mDraw)):
                date = str(mDraw[i]['Date'])[:10]
                for sitem in mDates:
                    if date == sitem['Date']:
                        sitem["normal-ci"] = mDraw[i]['normal-ci']
                        sitem["normal"] = mDraw[i]['normal']
                        sitem["normal+ci"] = mDraw[i]['normal+ci']
                        pass
                    pass
                pass
            mdf = pd.DataFrame(mDates)
            df[mFunction + "_fitted_normal-ci"] = mdf['normal-ci'].values
            df[mFunction + "_fitted_normal"] = mdf['normal'].values
            df[mFunction + "_fitted_normal+ci"] = mdf['normal+ci'].values
            df = df[['country', "tweet_date", "senti_bert_averageN", "senti_bert","senti_bert_std_trend",
                     "senti_bert_quan25", "senti_bert_quan75", mFunction + "_fitted_normal-ci", mFunction + "_fitted_normal", mFunction + "_fitted_normal+ci"]]
            mdf = df.to_dict("record")
            ret_fit = ret_fit + mdf

        except Exception as e:
            print(e)
            pass
        pass
    pass

df = pd.DataFrame(Parameter)
df.to_csv(os.path.join(DIR_OUTPUT, "out_recovery.csv"))
df = pd.DataFrame(ret_fit)
df.to_csv(os.path.join(DIR_OUTPUT, "out_fitted.csv"))
