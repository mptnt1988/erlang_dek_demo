udg
=====

An OTP application

Build
-----

        To check upgrade:
        1. checkout from version 0.1.0
           * rebar3 compile
           * rebar3 release
           * rebar3 tar
        After this, rebar3 build a tar file in _build/default/rel/dek_demo
        Then, try to start application with above tar file

        2. checkout to version 0.2.0
           * rebar3 compile
           * rebar3 release
           * rebar3 appup generate
           * rebar3 relup tar
        After this, rebar3 build another tar file in _build/default/rel/dek_demo
        Copy new tar file into releases folder and then type
           * bin/dek_demo upgrade 0.2.0

        3. For downgrade, try
           * bin/dek_demo downgrade 0.1.0