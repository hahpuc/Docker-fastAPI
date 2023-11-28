
from fastapi import APIRouter

from schemas.schema_base import ResponseSchemaBase

router = APIRouter()

@router.get("", response_model=ResponseSchemaBase)
async def get():
    return {"message": "Health check success"}