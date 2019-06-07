# Python VirtualEnv Experiment
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

Date: May 2019

Simple sample project using Python to play around with virutal environments.

## General Operations
Virtual Environments, creating a requirements file:

* With python3.7 the command to create a virtual environment is `python3.7 -m venv .env` where `.env` is the name of the directory to install the virutal environment into.
* To activate the created virtual environment run `source .env/bin/activate` or `. .env/bin/activate`.
* To deactivate the vitual environment just run `deactivate`.
* Virtual environments are not meant to be committed to git.
  * Best practice: don't do it.
* `pip freeze > requirements.txt` can be used to create a file `requirements.txt` that can be used when installing a project's dependencies: `pip install -r requirements.txt`.
    * This requirements file can and should be committed to git.
* There does not seem to be a standard way to encode the python version a project was meant to be run with.
  * It is possible my Google-Fu is not strong enough here.
  * It seems project documentation may be the best place to specify the version of python to use.

## Pip
Operations with `pip`:

* Exact version: `python -m pip install flask==0.9`
  * Double equals
* Version range: `python -m pip install 'Django<2.0'`
  * Quotes necessary for range evaluation
* Upgrade: `python -m pip install -U flask`
  * `-U` switch used for upgrading to the latest version
* Upgrade pip itself: `python -m pip install -U pip`

## Pipenv
[Pipenv](https://github.com/pypa/pipenv) is a tool which generates and manages virtual environments for python on your machine.

Some features of pipenv:

* Generates a virtualenv for a project
* Manages all of your projects virtualenvs
* Generates a Pipfile
  * Encodes dependencies and project python version
* Generates a Pipfile.lock
  * Deterministic depenency tree locked
  * Commit to the project
* Run a virtual environment shell
* Print a project's dependency graph
* Use `pipenv -h` for options

## Requirements Files
* `requirements.txt`
  * Standard ([pip](https://pypi.org/project/pip/))
  * Not deterministic (with transitive dependencies)
* `Pipfile`
  * Dependency Manager: [pipenv](https://github.com/pypa/pipenv)
  * Custom format
  * Determininstic
* `pyproject.toml`
  * Dependency Manager: [poetry](https://github.com/sdispater/poetry)
  * Standard (PEP-518)

## Learn More
More concise and detailed notes about the various Python environment management tools can be found in the [Getting Started coure notes](../getting_started#virtual-environments)
