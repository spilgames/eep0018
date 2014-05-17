REBAR ?= rebar
DIALYZER_OTP_APPS = kernel stdlib
OTP_PLT ?= $(HOME)/.dialyzer_plt
LOCAL_PLT ?= $(PWD)/.dialyzer_plt

.PHONY: test

%.beam: %.erl
	erlc -o test/ $<

all:
	@mkdir -p ebin
	${REBAR} compile

check: test/etap.beam test/util.beam
	prove test/*.t

clean:
	${REBAR} clean
	rm test/*.beam

test: check

$(OTP_PLT):
	dialyzer --build_plt --apps $(DIALYZER_OTP_APPS) --output_plt $(OTP_PLT)

$(LOCAL_PLT): $(OTP_PLT)
	dialyzer --add_to_plt \
		--plt $(OTP_PLT) \
		--output_plt $(LOCAL_PLT)

dialyzer: ${LOCAL_PLT}
	$(REBAR) co skip_deps=true
	dialyzer --plt $(LOCAL_PLT) \
		-c $(PWD)/ebin \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions
