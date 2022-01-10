# -*- coding: utf-8 -*-
# @File       : FitCurve.py
# @Author     : Yuchen Chai
# @Date       : 2020-06-24 9:53 PM
# @Description:

import pandas as pd
import numpy as np
import scipy
from scipy import optimize


def curve_value(p_func,p_para,p_x):
    if p_func == "Exponential":
        return p_para['u'] + p_para['v'] * np.exp(-p_para['gama'] * p_x)
    elif p_func == "Linear":
        return p_para['b'] + p_para['a'] * p_x


def func_exp(t, u, v, gama):
    return u + v * np.exp(-gama * t)


def func_lin(t, b, a):
    return b + a * t


def find_recovery(p_function, p_parameter, p_x, p_raw, p_mindate, p_fitted_y, p_target):
    if p_function == 1:
        mFunc = "Exponential"
        left_value = p_x[0]
        right_value = p_x[-1]
        target = p_target

        index = 0
        half_value = 1 / 2 * (left_value + right_value)

        mValue = curve_value(mFunc, p_parameter, half_value)
        while ((mValue < target * 0.999999 or mValue > target * 1.000001) and index <= 100):
            index += 1
            if mValue < target:
                left_value = half_value
            else:
                right_value = half_value
            half_value = 1 / 2 * (left_value + right_value)
            mValue = curve_value(mFunc, p_parameter, half_value)
            pass
        if half_value >= len(p_x) - 1.1:
            return np.nan
        else:
            return half_value
        pass
    elif p_function == 2:
        mFunc = "Exponential"
        target = p_target

        xhat = -np.log((target - p_parameter['u']) / p_parameter['v']) / p_parameter['gama']
        left_value = xhat - 5
        right_value = xhat + 5

        index = 0
        half_value = 1 / 2 * (left_value + right_value)

        mValue = curve_value(mFunc, p_parameter, half_value)
        while ((mValue < target * 0.999999 or mValue > target * 1.000001) and index <= 100):
            index += 1
            if mValue < target:
                left_value = half_value
            else:
                right_value = half_value
            half_value = 1 / 2 * (left_value + right_value)
            mValue = curve_value(mFunc, p_parameter, half_value)
            pass
        return half_value
    elif p_function == 3:
        mFunc = "Linear"
        target = p_target

        xhat = (target - p_parameter['b']) / p_parameter['a']

        return xhat
    elif p_function == 4:
        pass


def R_Squared(y, y_fit):
    y_mean = np.mean(y)
    SSR = np.sum(np.square(y_fit - y_mean))
    SST = np.sum(np.square(y - y_mean))
    return SSR / SST


def Fit_Curve_V4(p_country, p_data, p_min_date, p_drop_date, p_function=None):
    """
    Calculate the time by splitting into 2 types
    :param p_country:
    :param p_data:
    :param p_min_date:
    :param p_drop_date:
    :param p_function:
    :return:
    """
    # Prepare Data
    first_point_weight = 100
    # fit_model = ['normal']
    fit_model = ['normal-ci', 'normal', 'normal+ci']

    mData = p_data
    mData['Date'] = pd.to_datetime(mData['tweet_date'])
    mData = mData.dropna(subset=[p_function])
    mData['Display'] = mData[p_function]

    mMindate = p_min_date
    mDropdate = p_drop_date

    # Too many countries have a drop at that time
    # Floyd Event happened on May 25th.
    mData_fit = mData[(mData['Date'] >= mMindate) & (mData['Date'] <= "2020-05-25")]
    mData_base = mData[(mData['Date'] <= mDropdate)]

    y = np.array(mData_fit['Display'].tolist())
    x = np.arange(len(y))

    y_expand = y.copy()
    x_expand = x.copy()
    for i in range(first_point_weight):
        y_expand = np.append(y_expand, [y_expand[0]])
        x_expand = np.append(x_expand, [x_expand[0]])

    mData_base = mData_base['Display'].tolist()
    mData_base = mData_base[:-7]
    baseline = np.mean(mData_base)
    baseline_sd = np.std(mData_base)

    # mType = 0: default value
    # mType = 1: Exponential fit, get 95%
    # mType = 3: Linear fit

    # model = "normal-ci"
    # model = "normal"
    # model = "normal+ci"

    # sd_position_half: the sd position based on the baseline for halflife place
    # sd_position_525: the sd position based on the baseline for May 25th
    # sd_position_final: the sd position based on the baseline for final value

    ret_value = {
        "country": "",
        "model": "",
        "mType": 0,
        "mMinDate": "",
        "mRecoveryTime": "",
        "u": "",
        "v": "",
        "gama": "",
        "a": "",
        "b": "",
        "sse": np.nan,
        "r-squared": np.nan,
        "sd_position_half": "",
        "sd_position_525": "",
        "sd_position_final": ""
    }
    ret_para = []
    ret_fitted = {}

    # Decide using which method:
    status = True
    try:
        fit_value_e, covar_e = scipy.optimize.curve_fit(func_exp, x_expand, y_expand, maxfev=1000)
        u, v, gama = fit_value_e[0], fit_value_e[1], fit_value_e[2]
        y_fit = u + v * np.exp(-gama * x)
        sse_e = np.sum(np.square(np.subtract(y, y_fit)))

        fit_value_l, covar_l = scipy.optimize.curve_fit(func_lin, x_expand, y_expand, maxfev=1000)
        b, a = fit_value_l[0], fit_value_l[1]
        y_fit = b + a * x
        sse_l = np.sum(np.square(np.subtract(y, y_fit)))

        if (sse_e <= sse_l):
            status = True
            covar = covar_e
            fit_value = fit_value_e
        else:
            status = False
            covar = covar_l
            fit_value = fit_value_l

    except:
        status = False

    if status:
        sigma_ab = np.sqrt(np.diagonal(covar))
        u_s = fit_value[0]
        u_n = fit_value[0]
        u_b = fit_value[0]
        v_s = fit_value[1]
        v_n = fit_value[1]
        v_b = fit_value[1]
        gama_s = fit_value[2] + 1.96 * sigma_ab[2]
        gama_n = fit_value[2]
        gama_b = fit_value[2] - 1.96 * sigma_ab[2]

        fit_value = {}
        fit_value['normal-ci'] = [u_s, v_s, gama_s]
        fit_value['normal'] = [u_n, v_n, gama_n]
        fit_value['normal+ci'] = [u_b, v_b, gama_b]

        y_fit = {}
        for item in fit_model:
            y_fit[item] = fit_value[item][0] + fit_value[item][1] * np.exp(-fit_value[item][2] * x)
            pass

        for item in fit_model:
            ret = ret_value.copy()
            ret['country'] = p_country
            ret['model'] = item
            ret['mMinDate'] = mMindate
            ret['u'] = fit_value[item][0]
            ret['v'] = fit_value[item][1]
            ret['gama'] = fit_value[item][2]

            fitted = y_fit[item]
            if ret['gama'] < 0:
                ret['mType'] = 0
                ret['mPercent'] = np.nan
                ret['mRecoveryTime'] = np.nan
            else:
                ret['mType'] = 1
                value_525 = fitted[-1]
                value_half_life = (fitted[0] + fitted[-1]) / 2
                ret['sd_position_half'] = (value_half_life - baseline) / baseline_sd
                ret['sd_position_525'] = (value_525 - baseline) / baseline_sd
                ret['sd_position_final'] = (ret['u'] - baseline) / baseline_sd
                ret['mRecoveryTime'] = find_recovery(1, ret, x, mData_fit, mMindate, y_fit[item], value_half_life)
                ret['r-squared'] = R_Squared(y, fitted)
                ret['sse'] = np.sum(np.square(np.subtract(y, fitted)))
            ret_para.append(ret)
            ret_fitted[item] = fitted
            pass
    else:
        try:
            fit_value, covar = scipy.optimize.curve_fit(func_lin, x_expand, y_expand, maxfev=1000)
        except:
            raise Exception("Neither exponential nor linear can fit this country: {0}".format(p_country))

        sigma_ab = np.sqrt(np.diagonal(covar))
        b_s = fit_value[0]
        b_n = fit_value[0]
        b_b = fit_value[0]
        a_s = fit_value[1] + 1.96 * sigma_ab[1]
        a_n = fit_value[1]
        a_b = fit_value[1] - 1.96 * sigma_ab[1]

        fit_value = {}
        fit_value['normal-ci'] = [b_s, a_s]
        fit_value['normal'] = [b_n, a_n]
        fit_value['normal+ci'] = [b_b, a_b]

        y_fit = {}
        for item in fit_model:
            y_fit[item] = fit_value[item][0] + fit_value[item][1] * x
            pass

        for item in fit_model:
            ret = ret_value.copy()
            ret['country'] = p_country
            ret['model'] = item
            ret['mMinDate'] = mMindate
            ret['b'] = fit_value[item][0]
            ret['a'] = fit_value[item][1]

            fitted = y_fit[item]
            if ret['a'] < 0:
                ret['mType'] = 0
                ret['mPercent'] = np.nan
                ret['mRecoveryTime'] = np.nan
            else:
                ret['mType'] = 3
                value_525 = fitted[-1]
                value_half_life = (fitted[0] + fitted[-1]) / 2
                ret['sd_position_half'] = (value_half_life - baseline) / baseline_sd
                ret['sd_position_525'] = (value_525 - baseline) / baseline_sd
                ret['sd_position_final'] = np.nan
                ret['mRecoveryTime'] = find_recovery(3, ret, x, mData_fit, mMindate, y_fit[item], value_half_life)
                ret['r-squared'] = R_Squared(y, fitted)
                ret['sse'] = np.sum(np.square(np.subtract(y, fitted)))
            ret_para.append(ret)
            ret_fitted[item] = fitted
            pass

    ret_draw = mData_fit
    ret_draw = ret_draw[['country', 'Date', 'Display']]
    for item in fit_model:
        ret_draw[item] = y_fit[item]

    return ret_para, ret_draw, baseline, baseline_sd
