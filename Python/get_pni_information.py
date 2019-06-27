import os
import pandas as pd

# =============================================================================
# Read clinical data and select all STHLM3 cases and slides with PNI.
# =============================================================================
clinical = os.path.join('/media/ps/WSI_images/',
                        'clinical_data/all clinical data STHLM3 2019-02-27/patdata.xlsx')
dta_clin = pd.read_excel(clinical)

dta_clin.columns = map(lambda x: str(x).upper(), dta_clin.columns)

# Temp position-mapping
pos_list = ['1A', '1B', '1C',
            '2A', '2B', '2C',
            '3A', '3B', '3C',
            '4A', '4B', '4C',
            '5A', '5B', '5C',
            '6A', '6B', '6C']

# 'melt' on columns containing each of pos_list.
cols = dta_clin.columns
df_list = []
for i in pos_list:
    cols_i = [col for col in cols if col.startswith(i)]
    cols_i += ['REGNR']
    tmp = dta_clin.filter(cols_i)
    cols_i = [s.replace(i + '_', '') for s in cols_i]
    tmp.columns = cols_i
    tmp['POSITION'] = i
    df_list.append(tmp)

dta_clin = pd.concat(df_list)
dta_clin = dta_clin.fillna(0)

dta_clin = dta_clin.astype({'POSITION': 'str',
                            'REGNR': 'str',
                            'OTHER': 'str',
                            'LENGTH': 'float64',
                            'GLEASON1': 'int',
                            'GLEASON2': 'int'})

dta_clin['GS'] = [' + '.join(str(j) for j in i) for i in zip(dta_clin.GLEASON1, dta_clin.GLEASON2)]

# For categorizing grade and cancer length.
GS_to_ISUP = {'0 + 0': 'Benign',
              '3 + 3': 'ISUP 1',
              '3 + 4': 'ISUP 2',
              '4 + 3': 'ISUP 3',
              '3 + 5': 'ISUP 4',
              '5 + 3': 'ISUP 4',
              '4 + 4': 'ISUP 4',
              '4 + 5': 'ISUP 5',
              '5 + 4': 'ISUP 5',
              '5 + 5': 'ISUP 5'}
dta_clin['ISUP'] = dta_clin['GS'].replace(GS_to_ISUP, inplace=False)

dta_clin['slide'] = dta_clin['REGNR'].str.lstrip('D').str.replace(' ', '') + ' ' + dta_clin['POSITION']
dta_clin['slide'] = dta_clin['slide'].str.replace('/', '_')

dta_clin['slide_pni'] = dta_clin['OTHER'].str.contains("ral inv", na=False)

dta_clin.to_csv('/home/ps/Studies/PNI_clinical/data/raw_data/pni_core.csv')
