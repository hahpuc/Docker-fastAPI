from fastapi import APIRouter

from api import api_healthcheck, api_item, api_file

router = APIRouter()

router.include_router(api_healthcheck.router, tags=["health-check"], prefix="/healthcheck")
router.include_router(api_item.router, tags=["items"], prefix="/items")
router.include_router(api_file.router, tags=["files"], prefix="/files")
# router.include_router(api_process.router, tags=["process"], prefix="/process")
