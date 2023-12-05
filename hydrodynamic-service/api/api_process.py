import logging
import time
from fastapi import APIRouter, FastAPI, BackgroundTasks
from typing import List
from fastapi import UploadFile

logger = logging.getLogger()
router = APIRouter()

progress = 0


def long_running_task(background_tasks: BackgroundTasks):
    global progress

    for i in range(1, 101):
        progress = i
        time.sleep(5)

    progress = 0


@router.post("")
def start_task(background_tasks: BackgroundTasks):
    background_tasks.add_task(long_running_task, background_tasks)


@router.get("/progress")
def get_progress():
    return {"progress": progress}
