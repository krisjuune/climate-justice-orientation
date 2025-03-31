import pandas as pd
import numpy as np

# %% import data 

pd.set_option('display.max_columns', None)

files = {
    "CH1": "raw-data/wave1_CH_120624.csv",
    "US2": "raw-data/wave2_US_111224_1531.csv",
    "CH2": "raw-data/wave2_CH_111224_1531.csv",
    "CN2": "raw-data/wave2_CN_111224_1532.csv",
    "CH3": "raw-data/wave3_CH_240225_1004.csv",
    "CN3": "raw-data/wave3_CN_240225_1752.csv"
}


dataframes = {}

for country, file_name in files.items():
    # read the file, skipping the first two rows
    df = pd.read_csv(file_name, skiprows=[1, 2])
    
    # store the processed dataframe in the dictionary
    dataframes[country] = df


# %% homogenise column names

def rename_columns(df, original_str, replacement_str):
    df.rename(columns=lambda x: x.replace(original_str, replacement_str), inplace=True)
    return df

dataframes["CH1"].rename(columns={'languge': 'language'}, inplace=True)

for country, df in dataframes.items():
    df = rename_columns(df, 'justice-', 'justice_')
    df = rename_columns(df, 'subsidy_', 'sub_')
    df = rename_columns(df, 'general_', 'gen_')

# %% recode justice columns

just_columns = ['justice_gen_1', 
                'justice_gen_2', 
                'justice_gen_3', 
                'justice_gen_4',
                'justice_tax_1', 
                'justice_tax_2', 
                'justice_tax_3',
                'justice_tax_4',
                'justice_sub_1',
                'justice_sub_2',
                'justice_sub_3',
                'justice_sub_4',
                ]

likert_values_wave1 = ['Stimme überhaupt nicht zu', 
                       'Stimme nicht zu', 
                       'Stimme eher nicht zu', 
                       'Stimme eher zu', 
                       'Stimme zu', 
                       'Stimme voll und ganz zu']

likert_values_wave3 = ['Strongly disagree', 
                       'Somewhat disagree', 
                       'Disagree',
                       'Somewhat agree',
                       'Agree',
                       'Strongly agree']

numerical_values = [0, 1, 2, 3, 4, 5]
justice_dict_wave1 = {**dict(np.array(list(zip(likert_values_wave1, numerical_values))))}
justice_dict_wave3 = {**dict(np.array(list(zip(likert_values_wave3, numerical_values))))}

def apply_mapping(df, mapping_dict, column_pattern=None):
     # turn column_pattern into a list it already isn't
    if isinstance(column_pattern, str):
        column_patterns = [column_pattern]
    elif isinstance(column_pattern, list) and all(isinstance(pat, str) for pat in column_pattern):
        column_patterns = column_pattern
    elif column_pattern is None:
        column_patterns = []
    else:
        raise ValueError("column_pattern should be a string, list of strings, or None.")
    
    # identify columns to apply the mapping
    if column_patterns:
        columns_to_map = [col for col in df.columns if any(pat in col for pat in column_patterns)]
    else:
        columns_to_map = df.columns
    
    # apply mapping to the identified columns
    for column in columns_to_map:
        df[column] = df[column].replace(mapping_dict)
    
    return df

for country, df in dataframes.items():
    if country == "CH1":
        df = apply_mapping(df, justice_dict_wave1, column_pattern = 'justice')
    
    elif country in ["CH3", "CN3"]:
        df = apply_mapping(df, justice_dict_wave3, column_pattern = 'justice')

# %% fix data types

for country, df in dataframes.items():
    df['Finished'] = df['Finished'].replace(
        {'true': True, 'True': True, 'false': False, 'False': False}
    ).astype(bool)

    df['duration_min'] = (df['Duration (in seconds)'] / 60).round(3)

    for col in just_columns:
        pd.to_numeric(df[col], errors='coerce')

# %% filter out previews, incompletes, screened out, and failed traps

for country, df in dataframes.items():
    # filter out rows where DistributionChannel is 'preview' or not finished
    df = df[(df['DistributionChannel'] != 'preview') & (df['Finished'] != False)]
    
    # filter out screened out respondents, incl failed attention checks
    if country == "CH1":
        df = df.dropna(subset=['canton'])
        lower_threshold = df['duration_min'].quantile(0.025)
        upper_threshold = df['duration_min'].quantile(0.975)
        print(f"Lower threshold (lowest 5% quartile): {lower_threshold} minutes")
        print(f"Upper threshold (highest 5% quartile): {upper_threshold} minutes")
        df['speeder'] = df['duration_min'] < lower_threshold
        df['laggard'] = df['duration_min'] > upper_threshold 
    else:
        df = df[(df['Q_TerminateFlag'] != "QuotaMet")]
        df = df[(df['Q_TerminateFlag'] != "Screened")]
        lower_threshold = df['duration_min'].quantile(0.01)
        upper_threshold = df['duration_min'].quantile(0.99)
        print(f"Lower threshold (lowest 1% quartile): {lower_threshold} minutes")
        print(f"Upper threshold (highest 1% quartile): {upper_threshold} minutes")
        df['speeder'] = df['duration_min'] < lower_threshold
        df['laggard'] = df['duration_min'] > upper_threshold 

    # inattentives based on justice section straightlining
    attention_mask = (df[just_columns].nunique(axis=1) == 1)
    df['inattentive'] = attention_mask

    # update the dataframe in the dictionary
    dataframes[country] = df

# %% add response IDs

# Get the total number of rows across all dataframes
total_rows = sum(len(df) for df in dataframes.values())

# Initialize an ID counter
id_counter = 1

# Assign unique IDs across all countries
for country, df in dataframes.items():
    # Create a new 'ID' column starting from the current id_counter
    df['id'] = range(id_counter, id_counter + len(df))
    
    # Update the ID counter for the next dataframe
    id_counter += len(df)

    # Update the dataframe in the dictionary
    dataframes[country] = df

# %% clean data for LPA 

# make justice columns numeric
for country, df in dataframes.items():
    for col in just_columns:
        df[col] = pd.to_numeric(df[col], errors='coerce')

    dataframes[country] = df

lpa_data = []

for country, df in dataframes.items():
    cols = just_columns + ["id"]
    
    # create a new dataframe and add country and wave columns
    df_selected = df[cols].copy()
    df_selected['country'] = country[:2]
    df_selected['wave'] = country[-1]
    df_selected["question_type"] = np.where(df_selected["wave"] == "2", 
                                            "constant_sum", 
                                            "likert")
    
    # append the new dataframe to the list
    lpa_data.append(df_selected)

lpa_data = pd.concat(lpa_data, ignore_index=True)

# create dictionary for keys of justice principle
priniple_columns = {
    'utilitarian': ['justice_gen_1', 'justice_tax_1', 'justice_sub_1'], 
    'egalitarian': ['justice_gen_2', 'justice_tax_2', 'justice_sub_2'], 
    'sufficientarian': ['justice_gen_3', 'justice_tax_3', 'justice_sub_3'], 
    'limitarian': ['justice_gen_4', 'justice_tax_4', 'justice_sub_4']
}

# get mean for each key and append to dataframe
for key, col in priniple_columns.items():
    lpa_data[key] = lpa_data[col].sum(axis=1).round(3)

lpa_input = lpa_data[[
    'id', 
    "country",
    "wave",
    "question_type",
    'utilitarian',
    'egalitarian',
    'sufficientarian',
    'limitarian'
]]

# %% save lpa data

lpa_data.to_csv("data/cjo_icc_input.csv")
lpa_input.to_csv("data/lpa_input.csv")

# %% climate concern (waves 2 and 3)



# %% values (wave 3)

socio_econ_list = [
    "lreco_1",
    "lreco_2",
    "lreco_3",
]

socio_cult_list = [
    "galtan_1",
    "galtan_2",
    "net_zero_question"
]

socio_ecol_list = [
    "socio_ecological_1",
    "socio_ecological_2",
    "climate_worried"
]

value_columns = socio_econ_list + socio_cult_list + socio_ecol_list

reversed_scale_list = [
    "lreco_1", 
    "galtan_1",
    "galtan_2",
    "net_zero_question",
    "socio_ecological_2"
]

five_point_list = [
    "climate_worried"
]

likert_values_value_ques = [
    'Completely disagree',
    'Somewhat disagree',
    'Disagree',
    'Somewhat agree',
    'Agree',
    'Completely agree'
]

net_zero_translation = {
    "Absolutely sufficient":"Completely agree",
    "Sufficient":"Agree",
    "Slightly sufficient":"Somewhat agree",
    "Slightly insufficient":"Somewhat disagree",
    "Insufficient":"Disagree",
    "Absolutely insufficient":"Completely disagree"
}

for country, df in dataframes.items():
    df = apply_mapping(df, net_zero_translation, column_pattern='net_zero')
    dataframes[country] = df

likert_values_five = [
    "Not at all worried", 
    "Not very worried",
    "Somewhat worried",
    "Very worried",
    "Extremely worried"
]

numerical_values_normalised = [0, 0.2, 0.4, 0.6, 0.8, 1]
numerical_values_reversed =  [1, 0.8, 0.6, 0.4, 0.2, 0]
numerical_values_five = [0, 0.25, 0.5, 0.75, 1]

values_dict = {**dict(np.array(list(zip(likert_values_value_ques, numerical_values_normalised))))}
values_dict_reversed = {**dict(np.array(list(zip(likert_values_value_ques, numerical_values_reversed))))}
values_dict_five = {**dict(np.array(list(zip(likert_values_five, numerical_values_five))))}

def get_values_dict(column_name):
    if column_name in reversed_scale_list:
        return values_dict_reversed
    elif column_name in five_point_list:
        return values_dict_five
    else:
        return values_dict

for country, df in dataframes.items():
    # translate net zero question
    df = apply_mapping(df, net_zero_translation, column_pattern='net_zero')
    
    for col in value_columns:
        if col in df.columns:
            df = apply_mapping(df, get_values_dict(col), column_pattern=col)
            df[col] = df[col].replace(["Not sure", "Prefer not to say"], np.nan)

    if set(socio_econ_list).issubset(df.columns):
        df['lreco'] = df[socio_econ_list].apply(pd.to_numeric, errors='coerce').sum(axis=1)
        df.loc[df[socio_econ_list].isnull().any(axis=1), 'lreco'] = np.nan
    if set(socio_cult_list).issubset(df.columns):
        df['galtan'] = df[socio_cult_list].apply(pd.to_numeric, errors='coerce').sum(axis=1)
        df.loc[df[socio_cult_list].isnull().any(axis=1), 'galtan'] = np.nan
    if set(socio_ecol_list).issubset(df.columns):
        df['socio_ecol'] = df[socio_ecol_list].apply(pd.to_numeric, errors='coerce').sum(axis=1)
        df.loc[df[socio_ecol_list].isnull().any(axis=1), 'socio_ecol'] = np.nan

    dataframes[country] = df

value_columns = value_columns + ["lreco", "galtan", "socio_ecol"]

value_data = []

for country, df in dataframes.items():
    if country in ["CH3", "CN3"]:
        cols = value_columns + ["id"]
        df_selected = df[cols].copy()
        df_selected['country'] = country[:2]

        # append the new dataframe to the list
        value_data.append(df_selected)

value_data = pd.concat(value_data, ignore_index=True)

# %% save value data 

value_data.to_csv("data/value_data_wave3.csv")

# %% check data

wave1_ch = dataframes["CH1"]
wave2_us = dataframes["US2"]
wave2_ch = dataframes["CH2"]
wave2_cn = dataframes["CN2"]
wave3_ch = dataframes["CH3"]
wave3_cn = dataframes["CN3"]


# %% general demographics (all waves)
