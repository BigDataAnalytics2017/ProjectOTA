import pandas
import numpy
import itertools
import sklearn
import sklearn.tree
import sklearn.svm
from sklearn.ensemble import AdaBoostClassifier
from sklearn.cluster import KMeans
import sklearn.multiclass
import sklearn.metrics
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

start = pandas.Timestamp.now()
data = pandas.read_csv('C:/Users/user/Documents/train.csv', nrows=5000, error_bad_lines=False, warn_bad_lines=False)

data.date_time = pandas.to_datetime(data.date_time)
data = data.assign(date_year = [date.year for date in data.date_time])
data = data.assign(date_month = [date.month for date in data.date_time])
data = data.assign(date_day = [date.day for date in data.date_time])

data.visitor_hist_starrating[(data.visitor_hist_starrating > 0)==False] = 0
data.visitor_hist_adr_usd[(data.visitor_hist_adr_usd > 0)==False] = 0
data.prop_location_score2[(data.prop_location_score2 > 0)==False] = 0.0
data.prop_location_score1 += data.prop_location_score2
data.srch_query_affinity_score = numpy.exp(data.srch_query_affinity_score)
data.srch_query_affinity_score[(data.srch_query_affinity_score > 0)==False] = 0.0
X = data[data.orig_destination_distance > 0]
Y = data.drop(list(X.axes[0]))
Cluster = KMeans(n_clusters=10).fit(X.xs(['site_id', 'visitor_location_country_id', 'prop_country_id', 'srch_destination_id'], axis=1), X.orig_destination_distance.astype(int))
Cluster_predict = KMeans(n_clusters=10).fit_predict(X.xs(['site_id', 'visitor_location_country_id', 'prop_country_id', 'srch_destination_id'], axis=1), X.orig_destination_distance.astype(int))
center = numpy.ones(10)
for i in range(10):
    center[i] = numpy.mean((Cluster_predict==i) * X.orig_destination_distance)
Cluster_predict = Cluster.predict(Y.xs(['site_id', 'visitor_location_country_id', 'prop_country_id', 'srch_destination_id'], axis=1))
data.orig_destination_distance[(data.orig_destination_distance > 0)==False] = numpy.sum((numpy.full([10,len(Cluster_predict)], Cluster_predict).T == numpy.full([len(Cluster_predict), 10], numpy.arange(10))) * center, axis=1)

del X, Y, Cluster_predict, center, data['srch_id'], data['date_time'], data['prop_location_score2']

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

data.gross_bookings_usd = data.gross_bookings_usd.fillna(0.0)
data = data.dropna()
data = pandas.get_dummies(data, prefix=['site_id', 'visitor_location_country_id', 'prop_country_id'], columns=['site_id', 'visitor_location_country_id', 'prop_country_id'])

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

sampling = random_sampling(data, 0.6)

x_train = sampling[0].drop(['click_bool', 'gross_bookings_usd', 'booking_bool'], axis=1)
y_train = sampling[0].booking_bool
x_test = sampling[1].drop(['click_bool', 'gross_bookings_usd', 'booking_bool'], axis=1)
y_test = sampling[1].booking_bool
keys = list(x_train.keys())

Ada = sklearn.model_selection.GridSearchCV(AdaBoostClassifier(), {'n_estimators':[10,20,50], 'learning_rate':[0.001,0.01,0.5,1]}, cv=3).fit(x_train, y_train).best_estimator_

Ada_predict = Ada.predict(x_test)
Ada_predict_proba = Ada.predict_proba(x_test)
Ada_ROC = sklearn.metrics.roc_curve(y_test, Ada_predict_proba[:,1])
Ada_confusion = (sklearn.metrics.confusion_matrix(y_test, Ada_predict)*len(y_test)**(-1)).round(4)
Ada_report = sklearn.metrics.classification_report(y_test, Ada_predict, target_names = ['not Click', 'Click'], digits=4)

figure, (ax1, ax2) = plt.subplots(nrows=1, ncols=2, figsize=(15,3))
#AdaBoostClassifier Confusion Matrix
plt.sca(ax1)
plt.imshow(Ada_confusion, interpolation='nearest', cmap=plt.cm.YlGn)
plt.title('Confusion Matrix of AdaBoostClassifier')
plt.colorbar()
plt.xticks(range(2))
plt.yticks(range(2))
for i, j in itertools.product(range(2),range(2)):
    plt.text(j, i, Ada_confusion[i, j], horizontalalignment="center", color="black", size='xx-large')
plt.ylabel('True label', size='large')
plt.xlabel('Predicted label', size='large')
plt.subplots_adjust(wspace=0.5, hspace=0.5)

#All ROC
plt.sca(ax2)
plt.plot(Ada_ROC[0], Ada_ROC[1], color='y',label='Ada')
plt.legend(loc=4)
plt.plot([0, 1], [0, 1], color='k', linestyle='--', linewidth=3)
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate', size='large')
plt.ylabel('True Positive Rate', size='large')
plt.title('ROC Curve')
plt.subplots_adjust(wspace=1, hspace=0.5)

figure.savefig('temp.png')

print('AdaBoostClassifier parameters with cross-validation')
print(Ada.get_params())

print('Feature importance or coef')
print(pandas.DataFrame(numpy.array(Ada.feature_importances_.round(4)).T, columns=['Ada'], index=keys))

print('AdaBoostClassifier report')
print(Ada_report)

print('Time: ' + str(pandas.Timestamp.now()-start))