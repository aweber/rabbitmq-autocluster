#!/usr/bin/env bash
set -euo pipefail

echo "SIGNING:" "$@" 1>&2

if grep -q 'Precise' /etc/os-release; then
    # Hi, Travis =)
    cat - > /tmp/file-to-sign
    rm -f /tmp/signed
    echo "$GPG_PASSPHRASE" | gpg --passphrase-fd 0 "$@" --output /tmp/signed /tmp/file-to-sign
    cat /tmp/signed
else
    echo "$GPG_PASSPHRASE" | gpg --passphrase-fd 0 "$@"
fi
