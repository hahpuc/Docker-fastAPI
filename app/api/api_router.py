from fastapi import APIRouter

from api import api_healthcheck, api_item

router = APIRouter()

router.include_router(api_healthcheck.router, tags=["health-check"], prefix="/healthcheck")
router.include_router(api_item.router, tags=["items"], prefix="/items")
