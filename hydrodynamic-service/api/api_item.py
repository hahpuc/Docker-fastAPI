import logging
import os
import time

from typing import Any, List
import zipfile
from fastapi import APIRouter, HTTPException, Request
from helpers.exception_handler import CustomException
from models.model_item import ItemRequest, ItemResponse
from schemas.schema_base import DataResponse
from starlette.responses import FileResponse
from helpers.utils import zip_folder
from services.service_item import ItemService


logger = logging.getLogger()
router = APIRouter()

@router.get("/files/zip")
def get_zip_file() -> List[str]:
    """
    API Get list files
    """    
    folder_to_zip = './'
    output_zip_path = './result.zip'
    
    zip_folder(folder_to_zip,output_zip_path)
    
    return FileResponse(output_zip_path, media_type='application/octet-stream',filename='result.zip')
   
    
@router.get("/files",)
def get_files(request: Request) -> List[ItemResponse]:
    """
    API Get list files
    """
    #check exist folder output
    if not os.path.exists('./output'):
        return []
    
    ListFiles = []
    for file_name in os.listdir('./output/'):
        base_url = request.base_url
        item = ItemResponse(name=file_name, link=f'{base_url}ap1/v1/files/{file_name}')
        ListFiles.append(item)
    
    return ListFiles

@router.get("/files/{file_name}")
def get_file(file_name: str) -> Any:
    """
    API Get file
    """    
    for root, dirs, files in os.walk("./"):
        for file in files:
            file_path = os.path.join(root, file)
            if file == file_name:
                return FileResponse(file_path, media_type='application/octet-stream',filename=file_name)
        
    raise CustomException(http_code=400, code='400', message='File not found')
    
@router.get("/trigger/execute")
def trigger_execute(request: Request) -> Any:
    """
    API Trigger execute
    """    
    ItemService.execute_model()
    
    return DataResponse().success_response(data='Chạy model thành công')