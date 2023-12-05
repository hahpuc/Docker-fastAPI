from logging import getLogger
from typing import Any, List

from fastapi import APIRouter, Depends

from models.model_file import File
from services.service_file import FileService

logger = getLogger()
router = APIRouter()
file_service = FileService()


@router.post('/single', name='Upload single file')
async def upload_single_file(file: File = Depends(file_service)) -> Any:
    return file


@router.post('/multiple', name='Upload multiple file')
async def upload_multiple_files(files: List[File] = Depends(file_service)) -> Any:
    return files
