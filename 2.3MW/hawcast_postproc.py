# -*- coding: utf-8 -*-
"""
Created on Mon Oct  1 11:42:38 2018

@author: jyli
"""

import yaml
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import os
from itertools import product



def readHawc2Res(filename,channels=None):
    #reads specific channels of HAWC2 binary output files and saves in a
    #pandas dataframe. Variable names and channels are defined in a dictionary
    # called channels

    if channels is None:
        raise NotImplementedError()


    #read .sel file
    with open(filename + '.sel') as f:
        lines = f.readlines()

    NCh             = int(lines[8].split()[1])
    NSc             = int(lines[8].split()[0])
    Format          = lines[8].split()[3]
    scaleFactor     = [float(x) for x in lines[NCh+14:]]

    #read .bin file
    data = {}
    fid = open(filename + '.dat', 'rb')
    for key,ch in channels.items():
        fid.seek((ch-1)*NSc*2)
        data[key] = np.fromfile(fid,'int16',NSc) * scaleFactor[ch-1]


    return pd.DataFrame(data)




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



if __name__ == '__main__':

    
    with open("definition.yml", 'r') as stream:
        try:
            data = yaml.load(stream)
        except yaml.YAMLError as exc:
            print(exc)
    
    # load case definitions        
    cases = []
    for casename, case_def in data.items():
        cases.append(Case(casename, case_def['constants'], case_def['variables'], 
                          case_def['functions']))
    
    # read result file data
    channels = {'Pelec': 8,
                'Gtorque':7,
                'omega':2}
    
    pelec, omega, wsp = {}, {}, {}
    for case in cases:
        wsp[case.basename] = []
        pelec[case.basename] = []
        omega[case.basename] = []
        
        res_dir = f'res/{case.basename}/'.lower()  
        
        for _, sim in case._man.iterrows():
            res_file = res_dir + sim.case_id.lower()
            data = readHawc2Res(res_file, channels)
            wsp[case.basename].append(sim.wsp)
            pelec[case.basename].append(data.Pelec.values[-1]/1e6)
            omega[case.basename].append(data.omega.values[-1])
            

    
    
    # plot
    styles = ['-', '--', '.']
    plt.figure()
    plt.xlim(0, 25)
    plt.xlabel('Wind speed [m/s]')
    plt.ylabel('Omega [rad/s]')
    for i, case in enumerate(cases):
        plt.plot(wsp[case.basename], omega[case.basename], styles[i], label=case.basename)
    plt.legend()
        
    
    
    plt.figure()
    plt.xlim(0, 25)
    plt.xlabel('Wind speed [m/s]')
    plt.ylabel('Power [MW]')
    for i, case in enumerate(cases):
        plt.plot(wsp[case.basename], pelec[case.basename], styles[i], label=case.basename)
    plt.legend()
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        