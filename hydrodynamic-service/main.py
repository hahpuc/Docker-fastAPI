import logging

from services.service_item import ItemService
import uvicorn

from fastapi import FastAPI, Request
from core.config import settings
from api.api_router import router
from helpers.exception_handler import CustomException, http_exception_handler

from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from fastapi.responses import HTMLResponse, JSONResponse

from services.service_write_log import writeLogService

import os

logging.config.fileConfig(settings.LOGGING_CONFIG_FILE, disable_existing_loggers=False)


def get_application() -> FastAPI:
    application = FastAPI(
        title=settings.PROJECT_NAME, docs_url="/docs", redoc_url='/re-docs',
        openapi_url=f"{settings.API_PREFIX}/openapi.json",
        description='''
        Service for Hydrodynamic Model
        '''
    )

    application.include_router(router, prefix=settings.API_PREFIX)
    application.add_exception_handler(CustomException, http_exception_handler)

    return application

app = get_application()

# Mount static files: css, js
cur_dir = os.path.abspath(".")
app.mount('/static', StaticFiles(directory=os.path.join(cur_dir, 'static')), name='static')

# Mount templates html
templates = Jinja2Templates(directory="../templates")

# Dashboard UI at root
@app.get("/")
def read_root(request: Request):
    return templates.TemplateResponse("index.html", {"request": request})

# Create Fortran Model Compiler
ItemService.create_model_compliler()

writeLogService.write_content("Start service at port 8000")

if __name__ == '__main__':
    uvicorn.run(app, host="0.0.0.0", port=8000)
