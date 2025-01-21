# %% [markdown]
# ---
# title: "Pythong script file"
# ---


# %% 
import requests
import json
import pandas as pd
from dotenv import load_dotenv

load_dotenv()
import os

current_dir = os.getcwd()
file_path = os.path.join(current_dir, "all_data_report.json")


# BLS API Key obtained at https://www.bls.gov/developers/home.htm
bls_key = os.environ.get("BLS_API_KEY")


class c_bls_data_api:
  """
  Reference: https://www.bls.gov/developers/api_python.htm
  """
  
  def __init__(self, parameters):
    self.data = self.get_report(parameters)
    
  def get_report(self, parameters):
    headers = {"Content-type:" "application/json"}
    response = requests.post(
      "https://api.bls.gov/publicAPI/v2/timeseries/data/",
      data = parameters,
      headers = headers,
    )
    
    if response.status_code != 200:
      raise Exception(f"API Error: {response.status_code}")
  
  return response.json()

table_ids = [
      "CUUR0000SA0",
    "CUUR0000SA0L1E",
    "CUUR0000SAF1",
    "CUUR0000SA0E",
    "CUUR0000SETB01",
    "CUUR0000SAM",
    "CUUR0000SEMC01",
    "CUUR0000SEMD01",
    "CUUR0000SEMF01",
    "CUUR0000SAH1",
]

id_to_label = {
      "CUUR0000SA0": "All groups CPI",
    "CUUR0000SA0L1E": "All items less food and energy",
    "CUUR0000SAF1": "Food",
    "CUUR0000SA0E": "Energy",
    "CUUR0000SETB01": "Gasoline",
    "CUUR0000SAM": "Medical care",
    "CUUR0000SEMC01": "Physicians' services",
    "CUUR0000SEMD01": "Hospital services",
    "CUUR0000SEMF01": "Prescription drugs",
    "CUUR0000SAH1": "Shelter",
}

all_data = {}

print("Program started.")

for table_id in table_ids:
  bls_key = os.environ.get("BLS_API_KEY")
  parameters = json.dumps(
    {
      "registrationkey": bls_key,
      "seriesid": [table_id],
      "startyear": "2019"
      "endyear": "2025",
      "calculations": "true",
    }
  )
  
  bls_data_object = c_bls_data_api(parameters)
  
  all_data[table_id] = bls_data_object.data
  

with open(file_path, "w") as f:
  json.dump(all_data, f, indent=6)

print("Program completed.")

with open(file_path, "r") as f:
  data = json.load(f)

print("Loaded JSON data")

dfs = []

for key, series in data.items():
  try:
    series_data = series["Results"]["series"][0]["data"]
    df_temp = pd.DataFrame(series_data)
    df_temp["Category"] = key
    dfs.append(df_temp)
  except KeyError:
    print(f"Proper keys not found for: {key}")

df = pd.concat(dfs, ignore_index=True)

df["Year-Month"] = df["year"] + "-" + df["period"].str[1:]

df["value"] = pd.to_numeric(df["value"], errors="coerce")

df["Category_Label"] = df["Category"].map(id_to_label)

df["Year-Month"] = df["year"].astype(str) + "-" + df["periodName"]

df["Year-Month"] = pd.to_datetime(df["Year-Month"])

january_2019_data = df[
  (df["Year-Month"].dt.year == 2019) & (df["Year-Month"].dt.month == 1)
]

january_2019_values = dict(
  zip(january_2019_data["Category_Label"], january_2019_data["value"])
)

df["Value_January_2019"] = df["Category_Label"].map(january_2019_values)
df["Difference_from_January_2019"] = df["value"] - df["Value_January_2019"]

df["Percent_Change_from_January_2019"] = 0

for category_label in january_2019_values.keys():
  df.loc[
    df["Category_Label"] == category_label, "Percent_Change_from_January_2019"
  ] = (
    df.loc(df["Category_Label"] == category_label, "Difference_from_January_2019")
    / january_2019_values[category_label]
  ) * 100

df = df.reset_index(drop=True)

# Calculate change from previous month
df = df.sort_values(by="Year-Month")
grouped = df.groupby("Category_Label")
df["Percent_Change_from_Previous_Month"] = grouped["value"].pct_ch
df = df.reset_index(drop=True)




