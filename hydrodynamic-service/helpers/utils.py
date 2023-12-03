

import os
from zipfile import ZipFile
import zipfile

def zip_folder(folder_path, output_path):
    with ZipFile(output_path, 'w', compression=zipfile.ZIP_DEFLATED, compresslevel=9) as zipf:
        for root, dirs, files in os.walk(folder_path):
            for file in files:
                if not file.endswith('.zip'):
                    zipf.write(os.path.join(root, file))



