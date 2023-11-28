FROM python:3.12.0

WORKDIR /app 

COPY /app .

COPY /app/.env.example .env

RUN pip install -r requirements.txt

CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "80"]
