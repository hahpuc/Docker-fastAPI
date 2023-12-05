from pydantic import BaseModel


class Item(BaseModel):
    name: str

    class Config:
        orm_mode = True


class ItemRequest(Item):
    id: int
    name: str
    description: str = None


class ItemResponse(Item):
    name: str
    link: str = None
