# -*- coding: utf-8 -*-
# @File       : STL.py
# @Author     : Yuchen Chai
# @Date       : 2021-10-03 21:30
# @Description:

import pandas as pd
import statsmodels.api as sm


def STL_Detection(p_data, p_sentiment):
    """
    Get CUSUM result
    :param p_data: Pandas dataframe
    :param p_sentiment: Sentiment method (mean_hedono, mean_emoji, mean_liwc)
    :return:
    """
    p_data['Date'] = pd.to_datetime((p_data['tweet_date']))

    # Select middle part of the time
    # df_stl = p_data[(p_data['Date']>="2020-01-02")&(p_data['Date']<="2020-05-25")]
    p_data.fillna(value=0, inplace=True)
    sentiment_trend = p_data[p_sentiment].tolist()
    sentiment_date = p_data['Date'].tolist()

    # Get X axis
    df_stl = pd.DataFrame({"Date": sentiment_date, "Value": sentiment_trend})
    df_stl = df_stl.set_index("Date")
    df_stl['Date'] = df_stl.index

    # STL decompose
    ret = sm.tsa.STL(df_stl.Value, seasonal=7).fit()

    return df_stl, ret
