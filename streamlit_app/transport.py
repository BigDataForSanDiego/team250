# -*- coding: utf-8 -*-
"""
Created on Thu Oct 13 16:04:09 2022

@author: ttang
"""
import streamlit as st
import seaborn as sns

import pandas as pd
from pandas.api.types import is_numeric_dtype
import numpy as np
import matplotlib.pyplot as plt
app_mode = st.sidebar.radio('Select Page',['Home','Crash information','Driver','Victims']) #two pages


    
if app_mode=='Home':
    st.title('Traffic Accident Data Analyze :')  
    st.header('Dataset :')
    data=pd.read_csv("datacrash.csv",na_values=['NA'], usecols=[0,1, 5,8,10,14,22,23,36,37,38,39,40,42,45,46,47,49,50,52,60,74,75,77,78,79
])
    
    data.rename(columns={"POINT_X": "lon", "POINT_Y": "lat"},inplace = True)

    st.write(data.head())
    st.header('Car crashes data in Imperial County, CA')
    map_data = pd.DataFrame(data[['lat','lon']])
    #st.write(map_data['lat'].dtypes,np.sum(map_data['lat'].isna()))
    #map_data[map_data[0]==""] = np.NaN
    map_data = map_data.ffill()
    #st.write(map_data['lat'].dtypes,np.sum(map_data['lat'].isna()))

    st.map(map_data)
 
    st.header('Accident by time of the day ')
    nbins=st.slider('Pick your bin number', 10,50)
    fig, ax = plt.subplots()
    ax.hist(data['COLLISION_TIME'], bins=nbins)
    st.pyplot(fig)
    st.header('Accident lighting conditions')
    st.markdown(
        """
        - A - Daylight
- B - Dusk - Dawn
- C - Dark - Street Lights
- D - Dark - No Street Lights
- E - Dark - Street Lights Not Functioning
- -- Not Stated
"""	)
    fig = plt.figure(figsize=(10,5))
    sns.countplot(x=data['LIGHTING']) 
    st.pyplot(fig)        
    st.header('Correlation corner plot for factors relates to incidences')
    st.image('CorRplot.jpeg')


elif app_mode == 'Crash information':
    st.image('CorRplot.jpeg')
    dataname = st.selectbox('Pick the data of your interest',['ACCIDENT_YEAR','COLLISION_TIME','TYPE_OF_COLLISION','NUMBER_KILLED','COLLISION_SEVERITY','DAY_OF_WEEK','PARTY_COUNT','NUMBER_INJURED','PCF_VIOL_CATEGORY'])
    data=pd.read_csv("datacrash.csv",na_values=['NA'], usecols=[0,1, 5,8,10,14,22,23,36,37,38,39,40,42,45,46,47,49,50,52,60,74,75,77,78,79
])
    if is_numeric_dtype(data[dataname]):
        nbins=st.slider('Pick your bin number', 10,50)
        fig, ax = plt.subplots()
        ax.hist(data[dataname], bins=nbins)
        st.pyplot(fig)
    else:
        if dataname =='TYPE_OF_COLLISION':
            st.markdown("""
                - A - Head-On
                - B - Sideswipe
                - C - Rear End
                - D - Broadside: a car accident that occurs when the front of one vehicle slams into the side of another vehicle, typically at a high speed.
                - E - Hit Object
                - F - Overturned
                - G - Vehicle/Pedestrian
                - H - Other
                - -- Not Stated
            """)
        fig = plt.figure(figsize=(10,5))
        sns.countplot(x=data[dataname])
        st.pyplot(fig)  
elif app_mode == 'Driver':
    st.image('CorRplot.jpeg')
    data=pd.read_csv("parties.csv",na_values=['NA'])
    dataname = st.selectbox('Pick the data of your interest',['PARTY_AGE','PARTY_SEX','PARTY_SOBRIETY','VEHICLE_YEAR','VEHICLE_MAKE'])
    st.write(data['PARTY_AGE'].dtype)
    data.loc[data['PARTY_AGE']>200,'PARTY_AGE']=np.NaN
    if is_numeric_dtype(data[dataname]):
        nbins=st.slider('Pick your bin size', 10,50)
        fig, ax = plt.subplots()
        ax.hist(data[dataname], bins=nbins)
        st.pyplot(fig)
    else:
        if dataname == 'PARTY_SEX':
            st.image('gender.PNG')
        val_count  = data[dataname].value_counts()
        fig = plt.figure(figsize=(10,5))
        #sns.barplot(val_count.index, val_count.values, alpha=0.8)
        sns.countplot(x=data[dataname])
        st.pyplot(fig)        
elif app_mode == 'Victims':
    st.image('CorRplot.jpeg')
    data=pd.read_csv("victims.csv",na_values=['NA'])
    dataname = st.selectbox('Pick the data of your interest',['VICTIM_AGE','VICTIM_SEX','VICTIM_SEATING_POSITION'])
    data.loc[data['VICTIM_AGE']>200,'VICTIM_AGE']=np.NaN
    if is_numeric_dtype(data[dataname]):
        nbins=st.slider('Pick your bin number', 10,50)
        fig, ax = plt.subplots()
        ax.hist(data[dataname], bins=nbins)
        st.pyplot(fig)
    else:
        if dataname=='VICTIM_SEATING_POSITION':
            st.markdown("""
            - 1 - Driver
            - 2 thru 6 - Passengers
            - 7 - Station Wagon Rear
            - 8 - Rear Occupant of Truck or Van
            - 9 - Position Unknown
            - 0 - Other Occupants
            - A thru Z - Bus Occupants
            - -- Not Stated
            """)
        fig = plt.figure(figsize=(10,5))
        sns.countplot(x=data[dataname])
        #fig.title(dataname)
        #fig.ylabel('y label', fontsize=12)
        #fig.xlabel('x label', fontsize=12)
        # Add figure in streamlit app
        st.pyplot(fig)


