Asteroid - Websocket RPC and Pub/Sub erlang framework.
======================================================

Asteroid implement rpc interface for websocket.  
You can create erlang rps endpoints and python rpc endpoints.
Python rpc endpoint is a celery task. Asteroid call python rpc endpoint through rabbitmq.


Run erlang server
-----------------
You need to install rebar from https://github.com/rebar/rebar
Then compile and run project:
```
rebar get-deps  
rebar compile 
./start.sh
```

Run celery worker and tests
-----------------
You need to create virtualenv for python part of application(celery worker and tests)
```
virtualenv /tmp/asteroid_ve #or other path for your chouse
source /tmp/asteroid_ve/bin/activate
```
Then you need to setup requirements
```
pip install -r tests/requirements.txt
```
Run celery worker
```
cd test
./start_celery.sh
```

Run tests
```
cd test
py.test
```

Javascript example
------------------
You can open http://localhost:8008/static/index.html in your browser and test `add` and `add_delay` rpc calls.

