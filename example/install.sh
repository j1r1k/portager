#!/bin/sh
set -o errexit -o nounset -o pipefail

(
    cd "$(dirname "$0")"

    stack build
    stack exec portager-local
)
