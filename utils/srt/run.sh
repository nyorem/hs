#! /usr/bin/env bash

stack build && cat tests/nge.srt | stack exec -- srt -00:01:00,000

