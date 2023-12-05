import asyncio
import time
from logging import getLogger

from core.config import Settings
from models.model_file import File
from fastapi import UploadFile

logger = getLogger()
setting = Settings()


class FileService:
    file_dir = ''

    def __init__(self):
        self.file_dir = setting.FILE_DIR

    async def __call__(self, file: UploadFile | None = None, files: list[UploadFile] | None = None) \
            -> File | list[File]:
        try:
            if file:
                return await self.upload(file=file)

            elif files:
                return await self.multi_upload(files=files)
            else:
                return File(status=False, error='No file or files provided', message='No file or files provided')
        except Exception as err:
            return File(status=False, error=str(err), message='File upload was unsuccessful')

    async def upload(self, *, file: UploadFile) -> File:
        try:
            stored_filename = f'{int(time.time())}-{file.filename}'
            dest = f'{self.file_dir}/{stored_filename}'
            print(dest)
            file_object = await file.read()
            with open(f'{dest}', 'wb') as fh:
                fh.write(file_object)
            await file.close()

            return File(path=stored_filename, message=f'{file.filename} saved successfully',
                        content_type=file.content_type,
                        size=file.size, filename=file.filename)
        except Exception as err:
            logger.error(f'Error uploading file: {err} in {self.__class__.__name__}')
            return File(status=False, error=str(err), message=f'Unable to save file')

    async def multi_upload(self, *, files: list[UploadFile]) -> list[File]:
        res = await asyncio.gather(*[self.upload(file=file) for file in files])
        return list(res)
