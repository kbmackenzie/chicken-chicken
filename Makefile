# This Makefile creates and manages a local env for CHICKEN Scheme.
# All dependencies are installed in the local '.chicken' repository.

LOCAL_COOP  := $(CURDIR)/.chicken

SYSTEM_REPO := $(shell chicken-install -repository)
BINARY_VER  := $(shell chicken-install -repository | grep -oe [[:digit:]]\+$$)
LOCAL_REPO  := $(LOCAL_COOP)/lib/chicken/$(BINARY_VER)

export CHICKEN_INSTALL_PREFIX     := $(LOCAL_COOP)
export CHICKEN_INSTALL_REPOSITORY := $(LOCAL_REPO)
export CHICKEN_REPOSITORY_PATH    := $(LOCAL_REPO):$(SYSTEM_REPO)

export PATH := $(LOCAL_COOP)/bin:$(PATH)

all: build

build:
	chicken-install

tidy:
	@# Clean up generated build files (they pollute the directory):
	@rm -f ./*.build.sh ./*.import.scm ./*.install.sh ./*.link ./*.o ./*.so

clean:
	@rm -r $(LOCAL_COOP)

.PHONY: all build tidy clean
