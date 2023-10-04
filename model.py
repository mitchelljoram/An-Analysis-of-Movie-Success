import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import MultiLabelBinarizer
from sklearn import preprocessing
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LinearRegression
from sklearn.linear_model import Ridge
from sklearn.linear_model import Lasso
from sklearn import tree
from sklearn import neural_network
from sklearn.model_selection import train_test_split
from sklearn import metrics

## Load the data
data = pd.read_csv('movies.csv')
data = data[data['title_year'] >= 2000]

## Data Cleaning
# Color
data['color'].replace(['Color', ' Black and White'], [1, 0], inplace=True)

# Rating
mlb = MultiLabelBinarizer()
data['content_rating'] = data['content_rating'].str.split('|')
ratings = pd.DataFrame(mlb.fit_transform(data.pop('content_rating')), columns=mlb.classes_, index=data.index)

# Genres
data['genres'] = data['genres'].str.split('|')
genres = pd.DataFrame(mlb.fit_transform(data.pop('genres')), columns=mlb.classes_, index=data.index)

# Finish
data = data.join(ratings).join(genres)
df = data

## Model Choosing
lab_enc = preprocessing.LabelEncoder()
y = lab_enc.fit_transform(df['imdb_score'])
X = df[['duration', 'color', 'budget', 'gross']].join(ratings).join(genres)
X_train,X_test,y_train,y_test=train_test_split(X,y,test_size=0.4,random_state=100)

# Logistic Regression
logreg= LogisticRegression()
logreg.fit(X_train,y_train)

y_pred=logreg.predict(X_test)

print('\n')
print('--- Logistic Regression ---')
print('MSE: ',metrics.mean_squared_error(y_test, y_pred))
print('R^2: ',metrics.r2_score(y_test, y_pred),end='\n\n')

# Linear Regression
linreg = LinearRegression()
linreg.fit(X_train, y_train)

y_pred = linreg.predict(X_test)

print('--- Linear Regression ---')
print('MSE: ',metrics.mean_squared_error(y_test, y_pred))
print('R^2: ',metrics.r2_score(y_test, y_pred),end='\n\n' )

# Ridge Regression
ridge = Ridge()
ridge.fit(X_train, y_train)

y_pred = ridge.predict(X_test)

print('--- Ridge Regression ---')
print(X.join(df['imdb_score']).corr())
print('MSE: ',metrics.mean_squared_error(y_test, y_pred))
print('R^2: ',metrics.r2_score(y_test, y_pred),end='\n\n')

# Lasso Regression
lasso = Lasso()
lasso.fit(X_train, y_train)

y_pred = lasso.predict(X_test)

print('--- Lasso Regression ---')
print('MSE: ',metrics.mean_squared_error(y_test, y_pred))
print('R^2: ',metrics.r2_score(y_test, y_pred),end='\n\n')

# Decision Tree
dectree = tree.DecisionTreeRegressor()
dectree.fit(X_train, y_train)

y_pred = dectree.predict(X_test)

print('--- Decision Tree ---')
tree.plot_tree(dectree)
print('MSE: ',metrics.mean_squared_error(y_test, y_pred))
print('R^2: ',metrics.r2_score(y_test, y_pred),end='\n\n')

# Neural Network
nnet = neural_network.MLPRegressor()
nnet.fit(X_train, y_train)

y_pred = nnet.predict(X_test)

print('--- Neural Network ---')
print('MSE: ',metrics.mean_squared_error(y_test, y_pred))
print('R^2: ',metrics.r2_score(y_test, y_pred),end='\n\n')
