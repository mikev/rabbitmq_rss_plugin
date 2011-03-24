PACKAGE:=rabbitmq-rss
APPNAME:=rabbit_rss
DEPS:=rabbitmq-erlang-client
DEPS_FILE:=deps.mk
TEST_APPS:=amqp_client rabbit_rss
TEST_COMMANDS:=rabbit_rss_tests:test()
START_RABBIT_IN_TESTS:=true

include ../include.mk

$(DEPS_FILE): $(SOURCES) $(INCLUDES)
	escript generate_deps $(INCLUDE_DIR) $(SOURCE_DIR) \$$\(EBIN_DIR\) $@

clean::
	rm -f $(DEPS_FILE)

-include $(DEPS_FILE)
