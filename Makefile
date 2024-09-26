# This Makefile manages a local environment for CHICKEN Scheme.
# All dependencies are installed in the local '.chicken' directory.

LOCAL_COOP  := $(CURDIR)/.chicken

SYSTEM_REPO := $(shell chicken-install -repository)
BINARY_VER  := $(shell chicken-install -repository | grep -oe [[:digit:]]\+$$)
LOCAL_REPO  := $(LOCAL_COOP)/lib/chicken/$(BINARY_VER)

export CHICKEN_INSTALL_PREFIX     := $(LOCAL_COOP)
export CHICKEN_INSTALL_REPOSITORY := $(LOCAL_REPO)
export CHICKEN_REPOSITORY_PATH    := $(LOCAL_REPO):$(SYSTEM_REPO)

export PATH := $(LOCAL_COOP)/bin:$(PATH)

# Generated build files (should be cleaned, they're an eyesore)
BUILD_FILES = $(wildcard *.o) 			   \
							$(wildcard *.so)         \
							$(wildcard *.link) 			 \
							$(wildcard *.build.sh)   \
							$(wildcard *.import.scm) \
							$(wildcard *.install.sh)

all: build

build:
	chicken-install

test:
	chicken-install -test

tidy:
	@rm -f $(BUILD_FILES)

clean: tidy
	@rm -rf $(LOCAL_COOP)

.PHONY: all build test tidy clean
