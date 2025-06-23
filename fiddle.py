# -*- coding: utf-8 -*-
"""
Created on Mon Dec 25 16:02:20 2023
@author: Sean Franco
"""
#%%
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
!pip install seaborn
import seaborn as sns
import os
!pip install scipy 
import scipy.stats

#%%
os.chdir("")
fiddle_df = pd.read_excel("Collection of Fiddle Tunes.xlsx") 
artist_df = pd.read_csv("artistlocation.csv") 

#%%
from geopy.geocoders import Nominatim

artist_df = artist_df[artist_df['State'] != 'Ireland']
county_state_df = pd.DataFrame()
county_state_df["Location"] = artist_df['County'] + ", " + artist_df['State']

#%%
from geopy.geocoders import Nominatim
lat_df = []
long_df = []
cell = []
coordinate_df = []

for i in range(0, len(county_state_df)):
    cell = county_state_df.iloc[i]["Location"]

    if cell:
        geolocator = Nominatim(user_agent="Your_Name")
        location = geolocator.geocode(cell)

        if location:
            print(location.address)
            print((location.latitude, location.longitude))
            coordinate_df.append([location.latitude, location.longitude])
            long_df.append(location.longitude)
            lat_df.append(location.latitude)
        else:
            print(f"Geocoding failed {cell}")
    else:
        print("Empty cell")

#%%
jitter = .09

artist_df["LAT_jittered"] = lat_df + (2 * (pd.np.random.rand(len(lat_df)) - 0.5) * jitter)
artist_df["LONG_jittered"] = long_df + (2 * (pd.np.random.rand(len(long_df)) - 0.5) * jitter)

artist_df.columns

#%%
import plotly.express as px
from plotly.offline import plot
!pip install folium
import folium

fig = px.scatter_mapbox(artist_df, lat="LAT_jittered", lon="LONG_jittered",
                        color="Count",
                        size='Count',
                        size_max = 30,
                        zoom=3,
                        mapbox_style="open-street-map",
                        opacity=.7,
                        title="Fiddlers Cited In Repertoire",
                        labels={'Count': 'Tune Count'},
                        text= 'Name',
                        )

fig.update_layout(legend=dict(
    title="Fiddlers Cited In Repertoire",
    traceorder="reversed",
    itemsizing="constant",
    itemclick="toggleothers"
))

plot(fig, auto_open=True)

#%%
import plotly.express as px
from plotly.offline import plot
import folium

fig = px.density_mapbox(artist_df, lat="LAT_jittered", lon="LONG_jittered",
                        z="Count",
                        radius=10,
                        center=dict(lat=artist_df['LAT_jittered'].mean(), lon=artist_df['LONG_jittered'].mean()),
                        zoom=3,
                        mapbox_style="open-street-map",
                        opacity=0.7,
                        title="Heatmap of Fiddlers Cited In Repertoire",
                        labels={'Count': 'Fiddlers Count'}
                        )

fig.update_layout(coloraxis_showscale=True)

plot(fig, auto_open=True)

#%%
import plotly.graph_objects as go

latitudes = artist_df['LAT_jittered']
longitudes = artist_df['LONG_jittered']
counts = artist_df['Count']

fig = go.Figure(go.Scattergeo(
    lat=latitudes,
    lon=longitudes,
    mode='markers',
    marker=dict(size=counts,
        sizemode='area',
        sizeref=.03,
        color=counts,
        colorscale='Viridis',
        colorbar=dict(title='Count')
)))

fig.update_geos(
    visible=False, resolution=50, scope="usa",
    showcountries=True, countrycolor="Black",
    showsubunits=True, subunitcolor="Black"
)

fig.update_layout(height=500, margin={"r": 0, "t": 0, "l": 0, "b": 0})
fig.show()
#%% 
fiddle_df['Source of Tune'].str.split(r'\s*[/]+\s*', n=1, expand=True)
#%%
test = fiddle_df['Source of Tune'].str.split(r'\s*[/]+\s*', n=1, expand=True)

if 'Source of Tune' in fiddle_df.columns:
    if r'\s*[/]+\s*' in fiddle_df['Source of Tune'].values:
        test = fiddle_df['Source of Tune'].str.split(r'\s*[/]+\s*', n=1, expand=True)
    
    else:
        fiddle_df['Name'] = fiddle_df['Source of Tune']
        
fiddle_df['Name'] = test[0]

#%%

fiddle_df['1 Tune Title'] = fiddle_df['1 Tune Title'].astype('category')

test = pd.merge(fiddle_df, artist_df, on='Name', how='outer')
test = test.drop(['Unnamed: 6', 'Unnamed: 0', 'Source of Tune'], axis=1)

#%%
test.to_csv('test2.csv', index=False)

#%%
!pip install scikit-learn
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, classification_report, confusion_matrix

colnames_df = fiddle_df.columns
y = fiddle_df['Tuning Notes']
x = fiddle_df[['Key',  'Crooked [-/+]']]
#y = y.fillna('Square')
x = pd.get_dummies(x)
y = pd.get_dummies(y)

X_train, X_test, y_train, y_test = train_test_split(x, y['AEAE'], test_size=0.2, random_state=42)

logreg_model = LogisticRegression()

logreg_model.fit(X_train, y_train)

y_pred = logreg_model.predict(X_test)

accuracy_fiddle = accuracy_score(y_test, y_pred)
conf_matrix_fiddle = confusion_matrix(y_test, y_pred)
classification_rep_fiddle = classification_report(y_test, y_pred)

print(f"Accuracy: {accuracy_fiddle}")
print("Confusion Matrix:")
print(conf_matrix_fiddle)
print("Classification Report:")
print(classification_rep_fiddle)

#%%
!pip install statsmodels
import statsmodels.api as sm
from statsmodels.formula.api import glm

X_train, X_test, y_train, y_test = train_test_split(x, y['AEAE'], test_size=0.2, random_state=42)

model = sm.GLM(y_train, X_train, family=sm.families.Binomial())
model_fit = model.fit()

print(model_fit.summary())

np.exp(model_fit.params)

#%%
X_train, X_test, y_train, y_test = train_test_split(x, y['DDAD'], test_size=0.2, random_state=42)

model = sm.GLM(y_train, X_train, family=sm.families.Binomial())
model_fit = model.fit()

print(model_fit.summary())

np.exp(model_fit.params)

#%%

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC
from sklearn.metrics import accuracy_score, classification_report

X_train_svm, X_test_svm, y_train_svm, y_test_svm = train_test_split(x, y['AEAE'], test_size=0.2, random_state=42)

scaler_svm = StandardScaler()
X_train_scaled_svm = scaler_svm.fit_transform(X_train_svm)
X_test_scaled_svm = scaler_svm.transform(X_test_svm)

svm_model = SVC(kernel='linear', C=1.0, random_state=42)

svm_model.fit(X_train_scaled_svm, y_train_svm)

y_pred_svm = svm_model.predict(X_test_scaled_svm)

accuracy_svm = accuracy_score(y_test_svm, y_pred_svm)
classification_rep_svm = classification_report(y_test_svm, y_pred_svm)

print('SVM Results:')
print(f'Accuracy: {accuracy_svm}')
print('Classification Report:')
print(classification_rep_svm)

# %%
from sklearn.linear_model import Lasso
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler

X_train, X_test, y_train, y_test = train_test_split(x, y['GDAE'], test_size=0.2, random_state=42)

scaler = StandardScaler()
X_train_scaled = scaler.fit_transform(X_train)
X_test_scaled = scaler.transform(X_test)

lasso_model = Lasso(alpha=0.05)  

lasso_model.fit(X_train_scaled, y_train)

predictions = lasso_model.predict(X_test_scaled)

coefficients = pd.DataFrame({'Feature': x.columns, 'Coefficient': lasso_model.coef_})
print(coefficients)

from sklearn.metrics import mean_squared_error, r2_score

mse = mean_squared_error(y_test, predictions)
r2 = r2_score(y_test, predictions)

print(f'Mean Squared Error: {mse}')
print(f'R-squared: {r2}')
