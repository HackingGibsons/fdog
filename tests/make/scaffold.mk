#-*- mode:makefile-gmake; -*-
TEST_DIR = .test
TEST_PATH = $(ROOT)/$(TEST_DIR)/

build-test-fdog:
	@echo "=> Building a test server in: $(TEST_PATH)"
	$(LISP) --load $(QL_ROOT_PATH)/setup.lisp \
	        --eval '(ql:quickload :fdog)' \
			--eval '(fdog-cli:fdog-main `("fdog-test" "init" "--no-input" "$(TEST_PATH)"))' \
			--eval '(quit)'
$(TEST_DIR): build-test-fdog

start-test-fdog: $(TEST_DIR)
	@echo "=> Starting instance in $(TEST_PATH)"
	( \
	 cd $(TEST_PATH); \
	 nohup $(LISP) --load $(QL_ROOT_PATH)/setup.lisp \
	 	   --eval '(ql:quickload :fdog)' \
		   --eval '(fdog-cli:fdog-main `("fdog-test" "start" "$(TEST_PATH)" "--no-fork"))' \
		   --eval '(quit)' & \
	);
	@echo "=> Sleeping to ensure test server boot."
	@sleep 6

stop-test-fdog:
	@echo "=> Stopping instance in $(TEST_PATH)"
	$(LISP) --load $(QL_ROOT_PATH)/setup.lisp \
	        --eval '(ql:quickload :fdog)' \
			--eval '(fdog-cli:fdog-main `("fdog-test" "stop" "$(TEST_PATH)"))' \
			--eval '(quit)'
	@echo "-> Killing stray mongrels"
	for m2pid in $$(find $(TEST_PATH)/server/run/ -name '*.pid'); do \
	 	 cat $$m2pid | xargs kill ; \
	done
	$(MAKE) clean-test-fdog

clean-test-fdog:
	@echo "=> Cleaning instance in $(TEST_PATH)"
	rm -rf $(TEST_PATH)
