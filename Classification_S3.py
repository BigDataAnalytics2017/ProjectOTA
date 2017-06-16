import pandas
import numpy
import sklearn
import sklearn.tree
from sklearn.multioutput import MultiOutputClassifier
from sklearn.ensemble import AdaBoostClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import cross_val_score
import sklearn.multiclass
import sklearn.metrics

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

import boto3

bucket = "bigdatabucket20170610"
file_name = "train.csv"

s3 = boto3.client('s3')
obj = s3.get_object(Bucket= bucket, Key=file_name)

train = pandas.read_csv(obj['Body'], nrows=1000, error_bad_lines=False, warn_bad_lines=False)

#prop_log_historical_price = 上一個trading period該飯店的平均價格；0代表該期間未出售
#promotion_flag = +1 if the hotel had a sale price promotion specifically displayed
#srch_booking_window = Number of days in the future the hotel stay started from the search date
#orig_destination_distance = 在搜尋的當下，消費者與飯店的地理距離；null代表無法計算 ← 與srch_destination_id相關(還沒解決)
#random_bool = +1 when the displayed sort was random, 0 when the normal sort order was displayed

def data_arrange(data):
    data.date_time = pandas.to_datetime(data.date_time)
    data = data.assign(date_year = [date.year for date in data.date_time])
    data = data.assign(date_month = [date.month for date in data.date_time])
    data = data.assign(date_day = [date.day for date in data.date_time])
    #data.visitor_location_country_id = train.visitor_location_country_id.astype(str)
    #data.prop_country_id = data.prop_country_id.astype(str)
    #data.prop_id = data.prop_id.astype(str)
    #data.srch_destination_id = data.srch_destination_id.astype(str)

    #visitor_hist_starrating = 該消費者對過去所有消費過的飯店，所給予的平均星等；null代表未曾消費 ← 評等機制仿prop_starrating，nan設為0
    data.visitor_hist_starrating[(data.visitor_hist_starrating > 0)==False] = 0
    #visitor_hist_adr_usd = 過去該消費者平均每晚消費多少USD；null代表未曾消費 ← nan設為0
    data.visitor_hist_adr_usd[(data.visitor_hist_adr_usd > 0)==False] = 0
    #prop_location_score2 = A (second) score outlining the desirability of the hotel’s location ←nan設為0，與prop_location_score1加總
    data.prop_location_score2[(data.prop_location_score2 > 0)==False] = 0.0
    data.prop_location_score1 += data.prop_location_score2
    #srch_query_affinity_score = The log of the probability a hotel will be clicked on in Internet searches (hence the values are negative) ← 轉成機率，null設為0
    data.srch_query_affinity_score = numpy.exp(data.srch_query_affinity_score)
    data.srch_query_affinity_score[(data.srch_query_affinity_score > 0)==False] = 0.0
    #orig_destination_distance = 在搜尋的當下，消費者與飯店的地理距離；null代表無法計算 ← 與srch_destination_id相關
    Decision = sklearn.tree.DecisionTreeClassifier()
    MulDecision = sklearn.multiclass.OneVsRestClassifier(Decision)
    X = data[data.orig_destination_distance > 0]
    Y = data.drop(list(X.axes[0]))
    M = MulDecision.fit(X.xs(['site_id', 'visitor_location_country_id', 'prop_country_id', 'srch_destination_id'], axis=1), X.orig_destination_distance.astype(int))
    data.orig_destination_distance[(data.orig_destination_distance > 0)==False] = M.predict(Y.xs(['site_id', 'visitor_location_country_id', 'prop_country_id', 'srch_destination_id'], axis=1))
    
    data = data.assign(rate_percent_diff = numpy.zeros(data.shape[0]))
    data.rate_percent_diff += data.comp1_rate.fillna(0.0) * data.comp1_rate_percent_diff.fillna(0.0)
    data.rate_percent_diff += data.comp2_rate.fillna(0.0) * data.comp2_rate_percent_diff.fillna(0.0)
    data.rate_percent_diff += data.comp3_rate.fillna(0.0) * data.comp3_rate_percent_diff.fillna(0.0)
    data.rate_percent_diff += data.comp4_rate.fillna(0.0) * data.comp4_rate_percent_diff.fillna(0.0)
    data.rate_percent_diff += data.comp5_rate.fillna(0.0) * data.comp5_rate_percent_diff.fillna(0.0)
    data.rate_percent_diff += data.comp6_rate.fillna(0.0) * data.comp6_rate_percent_diff.fillna(0.0)
    data.rate_percent_diff += data.comp7_rate.fillna(0.0) * data.comp7_rate_percent_diff.fillna(0.0)
    data.rate_percent_diff += data.comp8_rate.fillna(0.0) * data.comp8_rate_percent_diff.fillna(0.0)
    
    data.comp1_inv = (data.comp1_inv > 0) * 1
    data.comp2_inv = (data.comp2_inv > 0) * 1
    data.comp3_inv = (data.comp3_inv > 0) * 1
    data.comp4_inv = (data.comp4_inv > 0) * 1
    data.comp5_inv = (data.comp5_inv > 0) * 1
    data.comp6_inv = (data.comp6_inv > 0) * 1
    data.comp7_inv = (data.comp7_inv > 0) * 1
    data.comp8_inv = (data.comp8_inv > 0) * 1
    
    del data['comp1_rate'], data['comp1_rate_percent_diff']
    del data['comp2_rate'], data['comp2_rate_percent_diff']
    del data['comp3_rate'], data['comp3_rate_percent_diff']
    del data['comp4_rate'], data['comp4_rate_percent_diff']
    del data['comp5_rate'], data['comp5_rate_percent_diff']
    del data['comp6_rate'], data['comp6_rate_percent_diff']
    del data['comp7_rate'], data['comp7_rate_percent_diff']
    del data['comp8_rate'], data['comp8_rate_percent_diff']
    del data['srch_id'], data['date_time']
    
    return data.fillna(0)

train = data_arrange(train)
train.gross_bookings_usd = train.gross_bookings_usd.fillna(0.0)

keys_train = list(train.keys())
keys_train.remove('click_bool')
keys_train.remove('gross_bookings_usd')
keys_train.remove('booking_bool')

def random_sampling(data, percent_for_train=0.6):
    click = data[data.click_bool==1]
    click_train = click.sample(frac = percent_for_train)
    click_test = click.drop(list(click_train.axes[0]))
    
    not_click = data[data.click_bool==0]
    not_click_train = not_click.sample(frac = percent_for_train)
    not_click_test = not_click.drop(list(not_click_train.axes[0]))
    
    train = click_train.sample(n=not_click_train.shape[0], replace=True).append(not_click_train, ignore_index=True)
    test = click_test.append(not_click_test, ignore_index=True)
    return [train, test]

Ada = AdaBoostClassifier()
Random = RandomForestClassifier()
Decision = sklearn.tree.DecisionTreeClassifier()

sampling = random_sampling(train, 0.6)

x_train = sampling[0].drop(['click_bool', 'gross_bookings_usd', 'booking_bool'], axis=1)
y_train = sampling[0].booking_bool
x_test = sampling[1].drop(['click_bool', 'gross_bookings_usd', 'booking_bool'], axis=1)
y_test = sampling[1].booking_bool
keys = list(x_train.keys())

Ada_predict = Ada.fit(x_train, y_train).predict(x_test)
Ada_predict_proba = Ada.fit(x_train, y_train).predict_proba(x_test)
Ada_ROC = sklearn.metrics.roc_curve(y_test, Ada_predict_proba[:,1])

Random_predict = Random.fit(x_train, y_train).predict(x_test)
Random_predict_proba = Random.fit(x_train, y_train).predict_proba(x_test)
Random_ROC = sklearn.metrics.roc_curve(y_test, Random_predict_proba[:,1])

plt.plot(Ada_ROC[0], Ada_ROC[1], color='darkorange')
plt.plot([0, 1], [0, 1], color='navy', linestyle='--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve')
plt.legend(loc="lower right")
plt.show()

print(sklearn.metrics.classification_report(y_test, Ada_predict, target_names = ['not click', 'click']))
print(sklearn.metrics.confusion_matrix(y_test, Ada_predict))

print(sklearn.metrics.classification_report(y_test, Random_predict, target_names = ['not click', 'click']))
print(sklearn.metrics.confusion_matrix(y_test, Random_predict))