# Load the libraries we need
import pandas as pd
import urllib
import time
import datetime as dt
import os
import geopandas as gp
import zipfile
import getpass 
import shutil
import glob  

start_of_production = time.time()

working_directory = os.getcwd()
temp_path = os.path.join('c:\\Users',getpass.getuser(),'Downloads')

# This option will supress the warning message on possbile copy issues - confirmed it is working as desired so turning it off
pd.set_option('chained_assignment',None)

acs_data_type = '5yr'
year = '2015'
api_key = '6d9263105b3ca3213e093323b4ece211ab49d4e5'

place_zip = temp_path + '\\tl_'+str(year)+'_53_place.zip'
place_url = 'https://www2.census.gov/geo/tiger/TIGER'+str(year)+'/PLACE/tl_'+str(year)+'_53_place.zip'

county_zip = temp_path + '\\tl_'+str(year)+'_us_county.zip'
county_url = 'https://www2.census.gov/geo/tiger/TIGER'+str(year)+'/COUNTY/tl_'+str(year)+'_us_county.zip'

def download_census_shapes(working_url,working_zip):
    
    with urllib.request.urlopen(working_url) as response, open(working_zip, 'wb') as out_file:
        shutil.copyfileobj(response, out_file)

    # Uncompress the Shapefile for use in the analysis and remove the zipfile
    working_archive = zipfile.ZipFile(working_zip, 'r')
    working_archive.extractall(temp_path)
    working_archive.close()
    os.remove(working_zip)

def get_data_profile(current_call,place_type,current_table):
    
    working_df =  pd.read_json(current_call)
    working_df = working_df.rename(columns=working_df.iloc[0]).drop(working_df.index[0])
    working_df = pd.melt(working_df, id_vars=['NAME','GEO_ID']) 

    # Clean up the data profiles to only include the estimate and margins of error
    working_df = working_df[~working_df['variable'].str.contains('EA')]
    working_df = working_df[~working_df['variable'].str.contains('MA')]
    working_df = working_df[~working_df['variable'].str.contains('PEA')]
    working_df = working_df[~working_df['variable'].str.contains('PMA')]
    working_df = working_df[working_df['variable'].str.contains('DP')]
    
    working_df['place_type'] = place_type
    working_df['table'] = current_table
  
    return working_df

def spatial_join(target_shapefile,join_shapefile,keep_columns):
    
    # open join shapefile as a geodataframe
    join_layer = gp.GeoDataFrame.from_file(join_shapefile)
    target_layer = gp.GeoDataFrame.from_file(target_shapefile)
    
    # Create PSRC Flag in the Join Layer and trim down before joining
    join_layer['PSRC'] = 0
    join_layer.loc[(join_layer['GEOID'] == '53033')|(join_layer['GEOID'] == '53035')|(join_layer['GEOID'] == '53053')|(join_layer['GEOID'] == '53061'), 'PSRC'] = 1
    cols_to_keep = ['geometry','PSRC']
    join_layer = join_layer[cols_to_keep]
    
    # spatial join
    merged = gp.sjoin(target_layer, join_layer, how = "inner", op='intersects')
    merged = pd.DataFrame(merged)
    merged = merged[keep_columns]
    
    return merged

end_5yr = int(year) - 2000
start_5yr = end_5yr - 4 

if start_5yr < 10:
    start_5yr = '0'+str(start_5yr)

download_date = dt.datetime.today().strftime('%Y-%m-%d')

data_set = 'acs/acs5/profile'
source_note = 'Source: U.S. Census Bureau, 20'+str(start_5yr)+'-20'+str(end_5yr)+' 5-Year American Community Survey Estimates'
    
# Choose the Notes Sheet to use based on year of data
if year == '2011':
    from census_profile_notes_2011 import *

elif year == '2012':
    from census_profile_notes_2012 import *

elif year == '2013':
    from census_profile_notes_2013 import *

elif year == '2014':
    from census_profile_notes_2014 import *

elif year == '2015':
    from census_profile_notes_2015 import *

elif year == '2016':
    from census_profile_notes_2016 import *    

elif year == '2017':
    from census_profile_notes_2017 import *   
    
elif year == '2018':
    from census_profile_notes_2018 import *   
    
elif year == '2019':
     from census_profile_notes_2019 import *     

elif year == '2020':
     from census_profile_notes_2020 import *  

##################################################################################################
##################################################################################################    
#  Dictionary of output types from the dataprofiles
##################################################################################################
################################################################################################## 
api_outputs = {'E':'Estimate',
               'M':'Margin of Error',
               'PE':'Percent',
               'PM':'Percent Margin of Error'}

numeric_columns = ['Estimate','Margin of Error','Percent','Percent Margin of Error']
data_tables = [['DP02','SELECTED SOCIAL CHARACTERISTICS IN THE UNITED STATES'],
               ['DP03','SELECTED ECONOMIC CHARACTERISTICS'],
               ['DP04','SELECTED HOUSING CHARACTERISTICS'],
               ['DP05','ACS DEMOGRAPHIC AND HOUSING ESTIMATES']]

detailed_tables= [['B08302','Departure Time to Work'],['B08303','Travel Time to Work']]

all_tables = [['DP02','SELECTED SOCIAL CHARACTERISTICS IN THE UNITED STATES'],
               ['DP03','SELECTED ECONOMIC CHARACTERISTICS'],
               ['DP04','SELECTED HOUSING CHARACTERISTICS'],
               ['DP05','ACS DEMOGRAPHIC AND HOUSING ESTIMATES'],
               ['B08302','Departure Time to Work'],
               ['B08303','Travel Time to Work']]

##################################################################################################
##################################################################################################    
# Download the Census Shapefiles and create a lookup for places in Washington
##################################################################################################
################################################################################################## 
print('Downloading the Census Place shapefile and uncompressing')
download_census_shapes(place_url, place_zip)

print('Downloading the Census County shapefile and uncompressing - this step can take awhile')
download_census_shapes(county_url, county_zip)

place_shapefile = os.path.join(temp_path,'tl_'+str(year)+'_53_place.shp')
county_shapefile = os.path.join(temp_path,'tl_'+str(year)+'_us_county.shp')

print('Creating a lookup of Place GEOIDs and a PSRC Flag')
keep_columns = ['GEOID','PSRC']
places = spatial_join(place_shapefile, county_shapefile, keep_columns)

##################################################################################################
##################################################################################################    
# Download the List of all Variables with labels and only save those for Data Profiles
##################################################################################################
################################################################################################## 
print('Downloading a list of all variables and labels for all available data-profiles')
variable_api_call = 'https://api.census.gov/data/' + str(year) + '/'+ data_set + '/variables'
labels_df =  pd.read_json(variable_api_call)
labels_df = labels_df.rename(columns=labels_df.iloc[0]).drop(labels_df.index[0])
labels_df  = labels_df.rename(columns={'name':'variable'})
labels_df = labels_df.drop('concept',axis=1)

# Clean up the labels dataframe so it only includes data profile labels for the estimate and removes Puerto Rico specific labels
labels_df = labels_df[~labels_df['variable'].str.contains('PE')]
labels_df = labels_df[labels_df['variable'].str.contains('DP')]
labels_df = labels_df[~labels_df['variable'].str.contains('PR')]

print('Downloading a list of all variables and labels for all available detailed tables')
variable_api_call = 'https://api.census.gov/data/' + str(year) + '/acs/acs5' + '/variables'
labels_table_df =  pd.read_json(variable_api_call)
labels_table_df = labels_table_df.rename(columns=labels_table_df.iloc[0]).drop(labels_table_df.index[0])
labels_table_df  = labels_table_df.rename(columns={'name':'variable'})
labels_table_df = labels_table_df.drop('concept',axis=1)

##################################################################################################
##################################################################################################    
# Create a master dataframe of all profiles for all geographies
##################################################################################################
################################################################################################## 
all_profiles = pd.DataFrame()
for tables in data_tables:
    print('Downloading data-profile '+tables[0])    
    current_profile = 'group(' + tables[0] + ')'

    print('Downloading all Places in Washington')
    census_api_call = 'https://api.census.gov/data/' + str(year) + '/'+ data_set + '?get=' + current_profile + '&' + 'for=place:*' +'&in=state:53' + '&key=' + api_key
    interim = get_data_profile(census_api_call,'pl',tables[0])
    interim['GEOID'] = interim.GEO_ID.str[9:]
    interim = pd.merge(interim,places,on='GEOID',suffixes=('_x','_y'),how='left')
    interim = interim[interim['PSRC'] == 1]
    interim = interim.drop('GEOID',axis=1)
    interim = interim.drop('PSRC',axis=1)

    if all_profiles.empty:
        all_profiles = interim
    else:
        all_profiles = pd.concat([all_profiles, interim], ignore_index=True, sort=False)
      
    print('Downloading all Counties in PSRC Region')
    census_api_call = 'https://api.census.gov/data/' + str(year) + '/'+ data_set + '?get=' + current_profile + '&' + 'for=county:033,035,053,061' +'&in=state:53' + '&key=' + api_key
    interim = get_data_profile(census_api_call,'co',tables[0]) 
    all_profiles = pd.concat([all_profiles, interim], ignore_index=True, sort=False)
 
print('Removing extra text from Census Place Names')
all_profiles.loc[all_profiles['NAME'].str.contains('CDP'), 'place_type'] = 'cdp'
all_profiles['NAME'] = all_profiles['NAME'].str.replace(', Washington','')
all_profiles['NAME'] = all_profiles['NAME'].str.replace(' city','')
all_profiles['NAME'] = all_profiles['NAME'].str.replace(' town','')
all_profiles['NAME'] = all_profiles['NAME'].str.replace(', WA Metro Area','')
all_profiles = all_profiles.drop('GEO_ID',axis=1)
all_profiles.columns = all_profiles.columns.str.lower()
  
print('Adding labels and cleaning up the profile dataframe before moving to excel')
all_profiles = pd.merge(all_profiles,labels_df,on='variable',suffixes=('_x','_y'),how='left')
all_profiles['var'] = all_profiles.variable.str[:9]
all_profiles['typ'] = all_profiles.variable.str[9:]
all_profiles = all_profiles.drop('variable',axis=1)

all_detailed = pd.DataFrame()
for tables in detailed_tables:
    print('Downloading Data Table '+tables[0])    
    current_profile = 'group(' + tables[0] + ')'

    print('Downloading all Places in Washington')
    census_api_call = 'https://api.census.gov/data/' + str(year) + '/acs/acs5?get=' + current_profile + '&' + 'for=place:*' +'&in=state:53' + '&key=' + api_key
   
    interim =  pd.read_json(census_api_call)
    interim = interim.rename(columns=interim.iloc[0]).drop(interim.index[0])
    interim = pd.melt(interim, id_vars=['NAME','GEO_ID']) 
    
    # Clean up the data profiles to only include the estimate and margins of error
    interim = interim[~interim['variable'].str.contains('EA')]
    interim = interim[~interim['variable'].str.contains('MA')]
    interim = interim[~interim['variable'].str.contains('PEA')]
    interim = interim[~interim['variable'].str.contains('PMA')]
    interim = interim[~interim['variable'].str.contains('place')]
    interim = interim[~interim['variable'].str.contains('state')]
    interim['place_type'] = 'pl'
    interim['table'] = tables[0]
    interim['GEOID'] = interim.GEO_ID.str[9:]
    interim = pd.merge(interim,places,on='GEOID',suffixes=('_x','_y'),how='left')
    interim = interim[interim['PSRC'] == 1]
    interim = interim.drop('GEOID',axis=1)
    interim = interim.drop('PSRC',axis=1)
    
    if all_detailed.empty:
        all_detailed = interim
    else:
        all_detailed = pd.concat([all_detailed, interim], ignore_index=True, sort=False)
    
    print('Downloading all Counties in PSRC Region')
    census_api_call = 'https://api.census.gov/data/' + str(year) + '/acs/acs5?get=' + current_profile + '&' + 'for=county:033,035,053,061' +'&in=state:53' + '&key=' + api_key
    interim =  pd.read_json(census_api_call)
    interim = interim.rename(columns=interim.iloc[0]).drop(interim.index[0])
    interim = pd.melt(interim, id_vars=['NAME','GEO_ID']) 
    
    # Clean up the data profiles to only include the estimate and margins of error
    interim = interim[~interim['variable'].str.contains('EA')]
    interim = interim[~interim['variable'].str.contains('MA')]
    interim = interim[~interim['variable'].str.contains('PEA')]
    interim = interim[~interim['variable'].str.contains('PMA')]
    interim = interim[~interim['variable'].str.contains('county')]
    interim = interim[~interim['variable'].str.contains('state')]
    interim['place_type'] = 'co'
    interim['table'] = tables[0]
    all_detailed = pd.concat([all_detailed, interim], ignore_index=True, sort=False)

print('Removing extra text from Census Place Names')
all_detailed.loc[all_detailed['NAME'].str.contains('CDP'), 'place_type'] = 'cdp'
all_detailed['NAME'] = all_detailed['NAME'].str.replace(', Washington','')
all_detailed['NAME'] = all_detailed['NAME'].str.replace(' city','')
all_detailed['NAME'] = all_detailed['NAME'].str.replace(' town','')
all_detailed['NAME'] = all_detailed['NAME'].str.replace(', WA Metro Area','')
all_detailed = all_detailed.drop('GEO_ID',axis=1)
all_detailed.columns = all_detailed.columns.str.lower()
  
print('Adding labels and cleaning up the dataframe before moving to excel')
all_detailed = pd.merge(all_detailed,labels_table_df,on='variable',suffixes=('_x','_y'),how='left')
all_detailed['var'] = all_detailed.variable.str[:10]
all_detailed['typ'] = all_detailed.variable.str[10:]
all_detailed = all_detailed.drop('variable',axis=1)

print('Combining Data Profiles and Detailed Tables into Final file')
all_profiles = pd.concat([all_profiles,all_detailed], ignore_index=True, sort=False)
all_profiles['name'] = all_profiles['name'].str.replace('Silverdale CDP','Silverdale') 

print('Getting list of unique Census Places to create Excel summaries for')
place_names = pd.read_csv(os.path.join(os.getcwd(),'jurisdictions.csv'), sep = ',')
place_names = place_names.juris_name.unique()

##################################################################################################
##################################################################################################    
# Create an Excel object to add the tables as worksheets into - one workbook for each geography
##################################################################################################
##################################################################################################  
for places in place_names:
    print('Working on Excel summaries for '+places)
    working_df = all_profiles[all_profiles.name == places]
    place_type = working_df.place_type.unique().tolist()
      
    end_5yr = int(year)
    start_5yr = end_5yr - 4
    
    out_file = str(start_5yr)+'-'+str(end_5yr)+'-acs-data-'+ str.lower(places.replace(" ", "-")) +'.xlsx'
 
    xl_path = working_directory + '\\' + out_file
    writer = pd.ExcelWriter(xl_path, engine="xlsxwriter")
    current_workbook  = writer.book
    notes_sheet = current_workbook.add_worksheet('Notes')

    ##################################################################################################
    ##################################################################################################
    ### A bunch of cell formats for the final excel files
    ### Needs to be here since they are objects passed to the workbook object
    ##################################################################################################
    ##################################################################################################
    meta_format = current_workbook.add_format({'bold': True,
                                               'text_wrap': True,
                                               'align': 'justify',
                                               'fg_color': '#D7E4BC'})

    note_format = current_workbook.add_format({'bold': False,
                                               'text_wrap': True,
                                               'indent': 2,
                                               'align': 'justify',
                                               'fg_color': '#D7E4BC'})

    header_format = current_workbook.add_format({'bold': True,
                                                 'text_wrap': True,
                                                 'align': 'center',
                                                 'fg_color': '#D7E4BC',
                                                 'border': 1})
        
    title_format = current_workbook.add_format({'bold': True,
                                                'text_wrap': True,
                                                'align': 'justify'})

    data_format = current_workbook.add_format({'num_format': '#,##0',
                                               'align': 'center',
                                               'bold': False})
    
    percentage_format = current_workbook.add_format({'num_format': '0.0',
                                                     'align': 'center',
                                                     'bold': False})

    float_format = current_workbook.add_format({'num_format': '0.00',
                                                'align': 'center',
                                                'bold': False})    

    heading0_format = current_workbook.add_format({'bold': True,
                                                   'text_wrap': False,
                                                   'align': 'left'})     

    heading1_format = current_workbook.add_format({'bold': False,
                                                   'text_wrap': True,
                                                   'align': 'justify'})    

    heading2_format = current_workbook.add_format({'bold': False,
                                                   'text_wrap': True,
                                                   'indent': 1})         

    heading3_format = current_workbook.add_format({'bold': False,
                                                   'text_wrap': True,
                                                   'indent': 2})  

    heading4_format = current_workbook.add_format({'bold': False,
                                                   'text_wrap': True,
                                                   'indent': 3}) 

    heading5_format = current_workbook.add_format({'bold': False,
                                                   'text_wrap': True,
                                                   'indent': 4}) 

    heading6_format = current_workbook.add_format({'bold': False,
                                                   'text_wrap': True,
                                                   'indent': 5})

    heading7_format = current_workbook.add_format({'bold': False,
                                                   'text_wrap': True,
                                                   'indent': 5})       
    notes_sheet.set_column('A:A', 120)

    ##################################################################################################
    ##################################################################################################
    ### Now a bunch of notes added to the Excel File
    ##################################################################################################
    ##################################################################################################
        
    # General Notes of dataset that was downloaded
    notes_sheet.write_string(0, 0, 'ACS dataset: ' + acs_data_type, meta_format)
    notes_sheet.write_string(1, 0, 'Data Year: ' + str(year), meta_format)
    notes_sheet.write_string(2, 0, 'Date of download from Census API: ' + download_date, meta_format)
    notes_sheet.write_string(3, 0, 'Each tab contains a distinct data profile', meta_format)
    notes_sheet.write_string(4, 0, 'Geography of Data: '+places, meta_format)   
    notes_sheet.write_string(5, 0, '', meta_format)
    
    notes_row = 6
    
    # Add in Symbol Related Notes
    current_note = 0
    for notes in symbol_notes:     
        if current_note == 0:
            notes_sheet.write_string(notes_row,0,notes,meta_format)
        else:
            notes_sheet.write_string(notes_row,0,notes,note_format)
        current_note += 1
        notes_row += 1

    # Add in General Data Profile Notes
    current_note = 0
    for notes in all_profile_notes:     
        if current_note == 0:
            notes_sheet.write_string(notes_row,0,notes,meta_format)
        else:
            notes_sheet.write_string(notes_row,0,notes,note_format)
        current_note += 1
        notes_row += 1
    
    # Add in Note about Data Source    
    notes_sheet.write_string(notes_row, 0, source_note, note_format) 
    notes_row += 1
    notes_sheet.write_string(notes_row, 0, '', note_format)
    notes_row += 1

    current_note = 0
    for notes in dp02_notes:     
        if current_note == 0:
            notes_sheet.write_string(notes_row,0,notes,meta_format)
        else:
            notes_sheet.write_string(notes_row,0,notes,note_format)
        current_note += 1
        notes_row += 1

    current_note = 0
    for notes in dp03_notes:     
        if current_note == 0:
            notes_sheet.write_string(notes_row,0,notes,meta_format)
        else:
            notes_sheet.write_string(notes_row,0,notes,note_format)
        current_note += 1
        notes_row += 1

    current_note = 0
    for notes in dp04_notes:     
        if current_note == 0:
            notes_sheet.write_string(notes_row,0,notes,meta_format)
        else:
            notes_sheet.write_string(notes_row,0,notes,note_format)
        current_note += 1
        notes_row += 1

    current_note = 0
    for notes in dp05_notes:     
        if current_note == 0:
            notes_sheet.write_string(notes_row,0,notes,meta_format)
        else:
            notes_sheet.write_string(notes_row,0,notes,note_format)
        current_note += 1
        notes_row += 1
    

    for tables in all_tables:
        print('Working on '+tables[0]+' for '+places)
        current_df = working_df[working_df.table == tables[0]]
        current_df = current_df.drop('name',axis=1)
        current_df = current_df.drop('place_type',axis=1)
        current_df = current_df.drop('table',axis=1)
    
        # Create Clean Table format of Estimate and Margins of Error
        for key, value in api_outputs.items():

            if key=='E':
                final_df = current_df[current_df['typ'] == key]
                final_df = final_df.drop('typ',axis=1)
                final_df  =final_df.rename(columns={'value':value})
        
            else:
                interim = current_df[current_df['typ'] == key]
                interim = interim.drop('label',axis=1)
                interim = interim.drop('typ',axis=1)
                interim  = interim.rename(columns={'value':value})
                final_df = pd.merge(final_df,interim,on='var',suffixes=('_x','_y'),how='left')
                
        # Replace NaN with Error Code that replaces with a hyphen
        final_df = final_df.fillna(-666666666)

        # Removing any Duplicate Records from the Dataframe
        final_df.drop_duplicates(keep = 'first', inplace = True)
        final_df = final_df.reset_index()

        # Adding a field for the heading level of the variable
        final_df['Level'] = 0
        final_df['Subject'] = ''
        final_df['Category'] = ''
        working = final_df['label']

        for i in range (0, len(working)):
            current_subject = working[i].split("!!")
            subject_items = len(working[i].split("!!"))
            final_df['Level'][i] = 'Heading' + str(subject_items - 2)
            final_df['Subject'][i] = current_subject[-1]
            final_df['Category'][i] = (current_subject[1]).upper()

        final_df = final_df.sort_values('var')
        final_df = final_df.reset_index()

        for my_columns in numeric_columns:
            final_df[my_columns] = final_df[my_columns].apply(float)    
        
        final_df['Estimate Format'] = 'integer'
        final_df['int_est'] = final_df['Estimate'].apply(int)    
        final_df['decimals'] = final_df['Estimate']-final_df['int_est']
        final_df.loc[final_df['decimals'] > 0, 'Estimate Format'] = 'float'

        final_df['MOE Format'] = 'integer'
        final_df['int_moe'] = final_df['Margin of Error'].apply(int)    
        final_df['decimals'] = final_df['Margin of Error']-final_df['int_moe']
        final_df.loc[final_df['decimals'] > 0, 'MOE Format'] = 'float'
   
        final_columns = ['Category','Subject','Estimate','Margin of Error','Percent','Percent Margin of Error','Level','Estimate Format','MOE Format']
        final_df = final_df[final_columns]  

        ##################################################################################################
        ##################################################################################################
        ### Formatting of the Excel Table of Data
        ##################################################################################################
        ##################################################################################################          
        current_worksheet = current_workbook.add_worksheet(tables[0])
        
        # Set the various final column foramts.
        current_worksheet.set_column('A:A', 2, title_format)
        current_worksheet.set_column('B:B', 60, title_format)
        current_worksheet.set_column('C:D', 16, data_format)        
        current_worksheet.set_column('E:F', 16, percentage_format)
        
        # Read in each row and format based on the contents of the level field
        heading_offset = 1
        for index, row in final_df.iterrows():
                
            # Formats for the Title Column
            if index == 0:
                current_worksheet.write(index+1, 0, row['Category'], heading0_format)
                heading_offset = heading_offset + 1
                previous_category = row['Category']

            if index > 0 and row['Category'] != previous_category:
                current_worksheet.write(index+heading_offset, 0, row['Category'], heading0_format)
                previous_category = row['Category']
                heading_offset = heading_offset + 1
        
            if row['Level'] == 'Heading0':
                current_worksheet.write(index+heading_offset, 1, row['Subject'], heading1_format)
            
            if row['Level'] == 'Heading1':
                current_worksheet.write(index+heading_offset, 1, row['Subject'], heading1_format)
            
            if row['Level'] == 'Heading2':
                current_worksheet.write(index+heading_offset, 1, row['Subject'], heading2_format)            

            if row['Level'] == 'Heading3':
                current_worksheet.write(index+heading_offset, 1, row['Subject'], heading3_format)

            if row['Level'] == 'Heading4':
                current_worksheet.write(index+heading_offset, 1, row['Subject'], heading4_format)

            if row['Level'] == 'Heading5':
                current_worksheet.write(index+heading_offset, 1, row['Subject'], heading5_format)
        
            if row['Level'] == 'Heading6':
                current_worksheet.write(index+heading_offset, 1, row['Subject'], heading6_format)
            
            if row['Level'] == 'Heading7':
                current_worksheet.write(index+heading_offset, 1, row['Subject'], heading7_format)
            
            if row['Estimate Format'] == 'float' and row['MOE Format'] == 'float':
                census_values = [['Estimate',2,float_format],['Margin of Error',3,float_format],['Percent',4,percentage_format],['Percent Margin of Error',5,percentage_format]]
                
            if row['Estimate Format'] == 'integer' and row['MOE Format'] == 'float':
                census_values = [['Estimate',2,data_format],['Margin of Error',3,float_format],['Percent',4,percentage_format],['Percent Margin of Error',5,percentage_format]]

            if row['Estimate Format'] == 'integer' and row['MOE Format'] == 'integer':
                census_values = [['Estimate',2,data_format],['Margin of Error',3,data_format],['Percent',4,percentage_format],['Percent Margin of Error',5,percentage_format]]

            if row['Estimate Format'] == 'float' and row['MOE Format'] == 'integer':
                census_values = [['Estimate',2,float_format],['Margin of Error',3,data_format],['Percent',4,percentage_format],['Percent Margin of Error',5,percentage_format]]
    
            for results in census_values:
            
                # Tests to replace error numbers with the error code
                if row[results[0]] == -555555555:
                    current_worksheet.write(index+heading_offset, results[1], '*****' , results[2])

                elif row[results[0]] == -666666666:
                    current_worksheet.write(index+heading_offset, results[1], '-' , results[2]) 

                elif row[results[0]] == -333333333:
                    current_worksheet.write(index+heading_offset, results[1], '***' , results[2]) 

                elif row[results[0]] == -222222222:
                    current_worksheet.write(index+heading_offset, results[1], '**' , results[2]) 

                elif row[results[0]] == -888888896:
                    current_worksheet.write(index+heading_offset, results[1], '*****' , results[2]) 

                elif row[results[0]] == -888888888:
                    current_worksheet.write(index+heading_offset, results[1], '(x)' , results[2])            

                elif row[results[0]] == -999999999:
                    current_worksheet.write(index+heading_offset, results[1], 'N' , results[2])             

                elif row[results[0]] == -1000000000:
                    current_worksheet.write(index+heading_offset, results[1], 'N' , results[2])

                else:
                    current_worksheet.write(index+heading_offset, results[1], row[results[0]], results[2])

            if row['Percent'] >= 100:
                current_worksheet.write(index+heading_offset, 4, '(x)' , percentage_format) 

        ##################################################################################################
        ##################################################################################################
        ### Formatting of the Excel Header, Footer and Print Options
        ##################################################################################################
        ##################################################################################################   
        # Format the printed pages
        notes_sheet.set_paper(1)
        notes_sheet.set_portrait()
        notes_sheet.fit_to_pages(1, 1)       
        notes_sheet.set_footer('&LPage &P of &N &R&D')
        notes_sheet.repeat_rows(0)

        current_worksheet.set_paper(1)
        current_worksheet.set_portrait()
        current_worksheet.fit_to_pages(1, 0)       
        current_worksheet.set_footer('&LPage &P of &N &R&D')
        current_worksheet.repeat_rows(0)
        
        data_title = tables[0] + ': ' + tables[1] + '\n' + str(year) + ' American Community Survey ' + acs_data_type + ' estimates, Geography: ' + places
        
        current_worksheet.set_header('&L&"Arial,Bold"'+data_title)

        final_columns = ['Category','Subject','Estimate','Margin of Error','Percent','Percent Margin of Error']
        final_df = final_df[final_columns]
        
        # Read in columns headings as the header row for the data table
        for col_num, value in enumerate(final_df.columns.values):
            current_worksheet.write(0, col_num, value, header_format)
        
        current_worksheet.merge_range('A1:B1','Subject', header_format)
  
    writer.save()

# Remove temporary census place shapefiles
for fl in glob.glob(temp_path + '\\tl_'+str(year)+'_53_place.*'):
    os.remove(fl)
    
# Remove temporary census county shapefiles
for fl in glob.glob(temp_path + '\\tl_'+str(year)+'_us_county.*'):
    os.remove(fl)  

end_of_production = time.time()
print ('The Total Time for all processes took', (end_of_production-start_of_production)/60, 'minutes to execute.')