# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import yaml
import numpy as np
import pandas as pd
import os
from itertools import product

def evaluator(expr, tag_dict):

    tags = [x.split('}')[0] for x in expr.split('{')[1:]]
    
    for tag in tags:
        expr = expr.replace('{' + tag + '}', str(tag_dict[tag]))
        
    return eval(expr)
    

class Case(object):

    def __init__(self, casename, constants, variables, functions):
        self.gen_manifest(casename, constants, variables, functions)


    def __repr__(self):
        return self._man.__repr__()
    
    
    def gen_manifest(self, basename, Consts, Vars, Funcs):
        self.basename = basename
        # number of combinations:
        self.N = 1
        for n in [len(x) for x in Vars.values()]:
            self.N *= n
        # number of attributes:
        attributes = list(Consts.keys()) + list(Vars.keys()) + list(Funcs.keys())
        self.M = len(attributes)

        # generate a Pandas dataframe where each row has one of the combinations
        # of simulation tags
        manifest = []
        for v in product(*list(Vars.values())):
            v_dict = dict(zip(Vars.keys(), v))
            this_dict = {**Consts, **v_dict}

            for key, f in Funcs.items():

                this_dict[key] = evaluator(f, this_dict)

            manifest.append(list(this_dict.values()))

        self._man = pd.DataFrame(manifest, columns=attributes)





def generate_htc_files(case, template_fn, overwrite=True):
    # generates htc files using a template htc file located at template_fn.
    # uses parameters from self.params. if no destination folder is provided,
    # the files are written in self.HTCDir. If overwrite is False, only
    # htc files which do not exist will be written. Otherwise, all files will
    # be written.

    # error check
    if not os.path.isfile(template_fn):
        raise FileNotFoundError('Template file {} does not exist.'.format(template_fn))

    dest = 'htc/' + case.basename + '/'

    if not os.path.exists(dest):
        os.makedirs(dest)

    with open(template_fn) as f:
        TemplateText = f.read()

    for _, paramset in case._man.iterrows():
        FileText = TemplateText
        if not overwrite:
            if os.path.exists(dest + paramset.case_id + '.htc'):
                continue
        
        for key, value in paramset.items():
            if key[0] is '_':
                continue
            FileText = FileText.replace('{' + key + '}', str(value))
            
        with open(dest + paramset.case_id + '.htc', 'w') as f:
            f.write(FileText)
        print('{}.htc created.'.format(paramset.case_id))




def chunkify(lst, n):     
# splits a list into n groups of approximately the same length     
    return [lst[i::n] for i in range(n)]


def join_cases(cases):
    # joins the dataframe describing each case into a single large dataframe
    for i, case in enumerate(cases):
        if i == 0:
            out = case._man
        else:
            out = out.append(case._man, ignore_index=True)
    return out


def missingRes(sims):
    # returns htc file names of sims without results.
    toRun = []
    for _, sim in sims.iterrows():
        if os.path.isfile('res/' + sim.casename + '/' + sim.case_id + '.sel'):
            continue
        else:
            toRun.append('htc\\' + sim.casename + '\\' + sim.case_id + '.htc')
            
    return toRun
            


def allSims(sims):
    # returns htc file names of sims without results.
    toRun = []
    for _, sim in sims.iterrows():
        toRun.append('htc\\' + sim.casename + '\\' + sim.case_id + '.htc')            
    return toRun          


  
def makeBat2(toRun, N=1):
    bat_dir = 'bat\\'
    chunks = chunkify(toRun, N)
    
    for i, chunk in enumerate(chunks):
        with open(bat_dir + '{}.bat'.format(i), 'w') as f:
            f.write('cd ..\n')
            for file in chunk:
                f.write(f'hawc2mb {file}\n')   
                
                
                
                
def makeBat(case, N=1):
    htc_dir = 'htc\\' + case.basename 
    bat_dir = 'bat\\'
    htc_files = os.listdir(htc_dir)
    chunks = chunkify(htc_files, N)
    
    for i, chunk in enumerate(chunks):
        with open(bat_dir + case.basename + '_{}.bat'.format(i), 'w') as f:
            f.write('cd ..\n')
            for file in chunk:
                f.write(f'hawc2mb {htc_dir}\{file}\n')

    
    
    
    
if __name__ == '__main__':

    with open("definition.yml", 'r') as stream:
        try:
            data = yaml.load(stream)
        except yaml.YAMLError as exc:
            print(exc)
            
    cases = []
    
    for casename, case_def in data.items():
        cases.append(Case(casename, case_def['constants'], case_def['variables'], 
                          case_def['functions']))
        

    for case in cases:
        generate_htc_files(case, 'htc/_master/master.htc')
        
    toRun = missingRes(join_cases(cases))
    #toRun = allSims(join_cases(cases))
    makeBat2(toRun, N=4)
    
    # prep_launch(missingresults==True, case='blah')