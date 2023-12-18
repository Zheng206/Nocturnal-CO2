import subprocess
import json
from collections import Counter
import os

"""Use the Box API to download the zipped folders in a nested directory structure"""
def run_cmd(command):
    result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, shell=True)
    print(f"Command Exited with {result.returncode} status.")
    return result.stdout.strip()

def get_folder(id):
    return json.loads(run_cmd(f"box folders:items {id} --json"))

#def flatten_nested_list(list):
    list_flat = []
    for sub_list in list:
        for item in sub_list:
            list_flat.append(item)
    return list_flat
    
#def filter_keys(list_of_dicts):
    keys_to_keep = {"name", "id", "type", "parent"}
    return [{key: value for key, value in d.items() if key in keys_to_keep} 
            for d in list_of_dicts]
    
def download_data(id, dest):
    run_cmd(f'box files:download {id} --destination {dest} --no-overwrite')

#def filter_V1(ind, subdirs):
#    item = subdirs[ind]
#    select_item = [d for d in item if pd.Series(d['name']).str.contains("-V1").any()]
#    return select_item[0]

if __name__ == '__main__':
    
    #print(os.path.join(os.path.dirname(__file__), '../../data/All subjects/20231127'))
    os.chdir(os.path.join(os.path.dirname(__file__), '../../data/All subjects'))
    #if not os.path.exists('data'):
    #        os.mkdir('data')
    
    folder_ID = '200099045139' # ID in Box
    dirs = get_folder(folder_ID)

    [download_data(d['id'], f"20231127") for d in dirs]
        # run_cmd(f'box files:download {c["id"]} --destination {dirname}')