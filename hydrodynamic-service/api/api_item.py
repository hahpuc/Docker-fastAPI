from typing import Any
from fastapi import APIRouter, HTTPException
import logging
from helpers.exception_handler import CustomException

from models.model_item import ItemRequest, ItemResponse
from schemas.schema_base import DataResponse


logger = logging.getLogger()
router = APIRouter()

@router.get("",response_model=DataResponse[list[ItemResponse]])
def get() -> Any:
    """
    API Get list items
    """
    try:
        items = [
            ItemResponse(id=1, name="Item 1", description="Item 1 description"),
            ItemResponse(id=2, name="Item 2", description="Item 2 description"),
        ]
        return DataResponse().success_response(data=items) 
    
    except Exception as e:
        return HTTPException(status_code=400, detail=logger.error(e))


@router.post("", response_model=DataResponse[ItemResponse])
def create(item_data: ItemRequest) -> Any:
    """
    API Create Items
    """
    try:    
        new_user = ItemResponse(id=1, name=item_data.name, description=item_data.description)
        return DataResponse().success_response(data=new_user)
    
    except Exception as e:
        raise CustomException(http_code=400, code='400', message=str(e))