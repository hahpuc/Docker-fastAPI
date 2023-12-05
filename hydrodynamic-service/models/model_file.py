from pydantic import BaseModel


class File(BaseModel):
    path: str = ''
    size: int = 0
    filename: str = ''
    content_type: str = ''
    status: bool = True
    error: str = ''
    message: str = ''
