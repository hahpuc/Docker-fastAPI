FROM python:3.12.0

WORKDIR /hydrodynamic-service 

COPY /hydrodynamic-service .

COPY /hydrodynamic-service/.env.example .env

RUN pip install -r requirements.txt

CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "80"]
