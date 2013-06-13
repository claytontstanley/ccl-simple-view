SHELL := /usr/bin/env bash
TOP := $(dir $(lastword $(MAKEFILE_LIST)))

pathToCCL = $(shell dirname `greadlink -f ${TOP}/testing/ccl`)
