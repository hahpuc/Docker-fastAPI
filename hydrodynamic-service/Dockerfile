# Install Conda Env
FROM continuumio/miniconda3

RUN conda install -c conda-forge gfortran 

# Run project 
WORKDIR /hydrodynamic-service 

COPY /hydrodynamic-service .

COPY /hydrodynamic-service/.env.example .env

RUN python -m pip install -r requirements.txt

CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "80"]
