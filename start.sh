#!/bin/bash
erl -pa ebin deps/*/ebin -s asteroid -config asteroid
