#!/bin/sh

pipenv sync
pipenv run python lunch.py
pipenv run python variables.py
pipenv run python other.py
