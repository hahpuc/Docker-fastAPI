import logging
import os

from typing import Any, List
from fastapi import APIRouter, Request
from helpers.exception_handler import CustomException
from models.model_item import ItemResponse
from schemas.schema_base import DataResponse
from starlette.responses import FileResponse
from helpers.utils import zip_folder
from services.service_item import ItemService

logger = logging.getLogger()
router = APIRouter()


@router.get("/files/zip")
def get_zip_file() -> Any:
    """
    API Get list files
    """
    if not os.path.exists('./output'):
        return DataResponse().custom_response(code='200', message='Chưa có file nào được tạo', data=None)

    for root, dirs, files in os.walk("./output"):
        if len(files) == 0:
            return DataResponse().custom_response(code='200', message='Chưa có file nào được tạo', data=None)

    folder_to_zip = './output'
    output_zip_path = './result.zip'

    zip_folder(folder_to_zip, output_zip_path)

    return FileResponse(output_zip_path, media_type='application/octet-stream', filename='result.zip')


@router.get("/files")
def get_files(request: Request) -> List[ItemResponse]:
    """
    API Get list files
    """
    if not os.path.exists('./output'):
        return []

    list_files = []
    for file_name in os.listdir('./output/'):
        base_url = request.base_url
        item = ItemResponse(name=file_name, link=f'{base_url}api/v1/items/files/{file_name}')
        list_files.append(item)

    return list_files


@router.get("/files/{file_name}")
def get_file(file_name: str) -> Any:
    """
    API Get file
    """
    for root, dirs, files in os.walk("./"):
        for file in files:
            file_path = os.path.join(root, file)
            if file == file_name:
                return FileResponse(file_path, media_type='application/octet-stream', filename=file_name)

    raise CustomException(http_code=400, code='400', message='File not found')


@router.get("/trigger/execute")
def trigger_execute(request: Request) -> Any:
    """
    API Trigger execute
    """
    ItemService.execute_model()
    
    return DataResponse().success_response(data='Chạy model thành công')

@router.get("/logs")
def get_logs_file() -> Any:
    """
    API Get logs file
    """
    for root, dirs, files in os.walk("../logs"):
        for file in files:
            file_path = os.path.join(root, file)
            if file == 'logs.txt':
                return FileResponse(file_path, media_type='application/octet-stream',filename='logs.txt')
    
    
