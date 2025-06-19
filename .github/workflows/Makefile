TESTS_FILE="./dc-dlist-tests.lisp"
REPORTER=list
ROSWELL_PREFIX=$(HOME)/.local
ROSWELL=$(ROSWELL_PREFIX)/bin/ros
ROSWELL_SRC_DIR=roswell
ROSWELL_REPO=https://github.com/roswell/roswell.git

.PHONY: setup install-dependencies test

setup: install-roswell install-dependencies

install-roswell:
	# Build from source if Roswell is not present
	if ! command -v ros >/dev/null 2>&1; then \
		git clone -b release $(ROSWELL_REPO) $(ROSWELL_SRC_DIR); \
		cd $(ROSWELL_SRC_DIR); \
		sh bootstrap; \
		./configure --prefix=$(ROSWELL_PREFIX); \
		make; \
		make install; \
		cd ..; \
		rm -rf $(ROSWELL_SRC_DIR); \
		$(ROSWELL) setup; \
	fi

install-dependencies:
	# Install dependencies
	$(ROSWELL) install prove

test:
	$(ROSWELL) run -- --eval "(require :prove)" \
		--eval "(unless (prove:run (truename \"$(TESTS_FILE)\") :reporter :$(REPORTER)) (sb-ext:exit :code 1))" \
		--quit
