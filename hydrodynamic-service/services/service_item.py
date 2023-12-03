import os
import subprocess

class ItemService(object):
    __instance = None

    @staticmethod
    def update():
        return ""
    
    @staticmethod
    def execute_model():
        print("====> Start execute model")
        
        if not os.path.exists('./output'):
            os.makedirs('./output')
        
        os.system('./CONG_42021.out')
        
        print("====> End execute model")

    @staticmethod
    def create_model_compliler():
        print("====> Start create model compiler")
        
        os.system('gfortran ./2d-model/CONG_42021.F90 -W -o ./2d-model/CONG_42021.out')
        
        cur_dir = os.path.abspath(".")
        print(cur_dir)
        # Change directory path to 2d-model
        os.chdir(os.path.join(cur_dir,'2d-model'))
        print(os.path.abspath("."))
        print("====> Created Model Compiler")