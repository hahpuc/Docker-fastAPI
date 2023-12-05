import os
import subprocess

from services.service_write_log import writeLogService

class ItemService(object):
    __instance = None
    
    @staticmethod
    def execute_model():        
        cur_dir = os.path.abspath(".")
        
        #1. Execute Rainfall Runoff
        writeLogService.write_content("🚀 Start execute Rainfall Runoff model....")
        
        if not os.path.exists('./rainfall-runoff/output'):
            os.makedirs('./rainfall-runoff/output')
            
        
        os.chdir(os.path.join(cur_dir,'rainfall-runoff'))
        os.system('./RAIN_42021.out')
        os.chdir(cur_dir)
        
        writeLogService.write_content("✅ Rainfall Runoff model executed successfully")
        
        #2. Execute Hydrodynamic
        writeLogService.write_content("🚀 Start execute Hydrodynamic model....")
        
        if not os.path.exists('./hydrodynamic/output'):
            os.makedirs('./hydrodynamic/output')
            
        os.chdir(os.path.join(cur_dir,'hydrodynamic'))
        os.system('./CONG_42021.out')
        os.chdir(cur_dir)
        
        writeLogService.write_content("✅ Hydrodynamic model executed successfully")

    @staticmethod
    def create_model_compliler():
        #1. Create Rainfall Runoff Model Compiler
        writeLogService.write_content("🚀 Creating Rainfall Runoff Model Compiler....")
        
        os.system('gfortran ./2d-model/rainfall-runoff/RAIN_42021.F90 -W -o ./2d-model/rainfall-runoff/RAIN_42021.out')
        
        writeLogService.write_content("✅ Rainfall Runoff Model Compiler created successfully")
        
        
        #2. Create Hydrodynamic Model Compiler
        writeLogService.write_content("🚀 Creating Hydrodynamic Model Compiler....")
        
        os.system('gfortran ./2d-model/hydrodynamic/CONG_42021.F90 -W -o ./2d-model/hydrodynamic/CONG_42021.out')
        
        writeLogService.write_content("✅ Hydrodynamic Model Compiler created successfully")
        
        # Change directory path to 2d-model
        cur_dir = os.path.abspath(".")
        print(cur_dir)
        
        os.chdir(os.path.join(cur_dir,'2d-model'))
        print(os.path.abspath("."))
