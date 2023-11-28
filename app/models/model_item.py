from pydantic import BaseModel

class Item(BaseModel):
    name: str
    description: str = None
    
    class Config:
        orm_mode = True

class ItemRequest(Item):
    id: int
    name: str
    description: str = None    
    
class ItemResponse(Item):
    name: str
    description: str = None
    
