from celery import Celery
import time

app = Celery('tasks', backend='amqp', broker='amqp://')
app.conf.update(
    CELERY_TASK_SERIALIZER='json',
    CELERY_ACCEPT_CONTENT=['json'],  # Ignore other content
    CELERY_RESULT_SERIALIZER='json',
)


@app.task
def add(x, y):
    return x + y


@app.task
def add_delay(x, y):
    time.sleep(5)
    return x + y
