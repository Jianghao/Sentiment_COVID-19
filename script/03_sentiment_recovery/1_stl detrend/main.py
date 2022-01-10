# -*- coding: utf-8 -*-
# @File       : main.py
# @Author     : Yuchen Chai
# @Date       : 2021-10-03 21:30
# @Description:

# ------------------------
# import packages
# ------------------------
import os
import STL
from tqdm import tqdm
import pandas as pd
from pandas.tseries.offsets import Day

# ------------------------
# read data
# ------------------------
DIR_HOME = "Sentiment_COVID19/"
DIR_DATA = os.path.join(DIR_HOME, "data")
DIR_OUTPUT = os.path.join(DIR_HOME, "script/03_sentiment_recovery/1_stl detrend/")

df_country = pd.read_csv(os.path.join(DIR_DATA, "country_info_raw_0722_STD.csv"), index_col=0)
nationList = sorted(list(df_country['country'].unique()))

df_share = pd.read_csv(os.path.join(DIR_DATA, "COVID_Share_v1.csv"), index_col=0)
df_share['tweet_date'] = pd.to_datetime(df_share['tweet_date'])
df_share['share'] = df_share['n_covid'] / df_share['n']


# ------------------------
# process data
# ------------------------
def find_min_date(p_data, p_date):
    """
    Find the date which has min value in trend
    :param p_data:
    :return:
    """
    p_data = p_data.dropna()
    min_value = p_data[(p_data['Date'] >= (p_date - Day(30))) & (p_data['Date'] <= (p_date + Day(30)))]['trend'].min()
    trend = p_data['trend'].tolist()
    Dates = p_data['Date'].tolist()

    min_index = 0
    # Locate the lowest date
    for i in range(len(trend)):
        if trend[i] == min_value:
            min_index = i
            break

    return str(Dates[min_index])


def find_covid(p_data, p_country):
    '''
    Find the date that people started to focus on COVID-19 on Twitter
    :param p_data:
    :param p_country:
    :return:
    '''
    country_data = p_data[p_data['country'] == p_country]
    country_data_max_date = country_data[country_data['share'] == country_data['share'].max()]['tweet_date'].tolist()[0]
    country_data_max_value = country_data['share'].max()
    country_data_before_max = country_data[country_data['tweet_date']<country_data_max_date]
    country_data_before_max = country_data_before_max[country_data_before_max['share']<=country_data_max_value/10]
    country_data_start_focus_date = country_data_before_max.iloc[-1]['tweet_date']

    return country_data_start_focus_date, country_data_max_date


MinDate = []
ret_Trend = []
for nation in tqdm(nationList):
    sentiments = ['senti_bert']
    df = df_country[df_country['country'] == nation]

    date_covid, date_covid_max = find_covid(df_share, nation)
    if nation == "CHN":
        date_covid_max = pd.to_datetime("2020-01-29")

    for sentiment in sentiments:
        _, detrend_data = STL.STL_Detection(df, sentiment)

        trend = pd.DataFrame(detrend_data.trend)
        trend['Date'] = trend.index
        seasonal = pd.DataFrame(detrend_data.seasonal)
        seasonal['Date'] = seasonal.index
        residual = pd.DataFrame(detrend_data.resid)
        residual['Date'] = residual.index

        # Get date which has mean value
        mDate = find_min_date(trend, date_covid_max)
        MinDate.append({"Nation": nation.strip(".csv"), "Sentiment": sentiment, "MinDate": mDate})

        df[sentiment + '_trend'] = trend['trend'].values
    df = df.drop(columns=['Date'])
    mdf = df.to_dict("record")
    ret_Trend = ret_Trend + mdf
    pass

# ------------------------
# export data
# ------------------------
df_detrend = pd.DataFrame(ret_Trend)
df_detrend.to_csv(os.path.join(DIR_OUTPUT, "country_info_STL.csv"), encoding="UTF-8")

df_mindate = pd.DataFrame(MinDate)
df_mindate = df_mindate.drop(columns=['Sentiment']).rename(columns={"MinDate":"senti_bert"})
df_mindate.to_csv(os.path.join(DIR_OUTPUT, "country_info_Min.csv"), encoding="UTF-8")
