FROM python:3.12.0

WORKDIR /app 

RUN pip install requirements.txt

COPY .env.example .env

CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "80"]