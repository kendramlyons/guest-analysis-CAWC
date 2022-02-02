# Python script for individualizing data 

import csv, pandas as pd, matplotlib.pyplot as plt

df = pd.read_csv("data/travel_less_raw_limited_1.20.csv")

col_names = df.columns
col_names

# Duplicate rows based on group size value in number_in_party.

in_df = df.loc[df.index.repeat(df['number_in_party'])]
in_df

# Save data to disk

in_df.to_csv("data/individual_travel_data_limited_1.20.csv")
