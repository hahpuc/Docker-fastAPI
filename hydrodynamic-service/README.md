# ENV
- python: 3.12

## Install packages

- Cài đặt Fortran Packages:
    conda install -c conda-forge gfortran 

## Run project 
- Cài đặt packages: 
    pip install -r requirements.txt

- Run project: 
    uvicorn main:app --host 0.0.0.0 --port 8000 --reload

- Swagger link: 
    http://localhost:8000/docs

