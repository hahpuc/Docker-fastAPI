from datetime import datetime
import os

class WriteLogService(object):
    _instance = None 
    file_path = os.path.join(os.path.abspath("./logs"), 'logs.txt')

    def __new__(cls, *args, **kwargs):
        # Create a new instance only if it doesn't exist
        if not cls._instance:
            cls._instance = super(WriteLogService, cls).__new__(cls)
        return cls._instance

    def __init__(self):
        # Initialize the instance with the specific file path
        if not hasattr(self, 'initialized'):
            self.initialized = True

    def write_content(self, content):
        # Get the current time
        current_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        # Write content with the current time to the file
        with open(self.file_path, 'a') as file:
            file.write(f'{current_time}: {content}\n')
            
            
writeLogService = WriteLogService()