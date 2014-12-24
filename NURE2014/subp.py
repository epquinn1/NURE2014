# -*- coding: utf-8 -*-
"""
Created on Fri Jul 11 12:56:27 2014

@author: gquinn
"""

import subprocess
import random


for i in range(2):
    rs=int(100000*random.random())
    rstr="-"+str(rs)
    rc = subprocess.call(['R', 'CMD', 'BATCH', rstr , 'MCASsgpV2.r'])

print(str(rc))
