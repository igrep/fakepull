#!/bin/sh

set -e

stack build --pedantic
perl -nle 'print if /^-- BEGINNING OF EXAMPLE$/ .. /^-- END$/' README.md > example-tmp.hs
stack exec runhaskell example-tmp.hs
