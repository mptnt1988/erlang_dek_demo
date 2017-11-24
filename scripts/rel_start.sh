#!/usr/bin/env bash

export RELX_REPLACE_OS_VARS=true

IP=$1 _build/default/rel/dek_demo/bin/dek_demo console
