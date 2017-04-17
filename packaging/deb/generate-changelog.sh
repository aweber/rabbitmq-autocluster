#!/usr/bin/env bash
set -euo pipefail

file="${1:?}"
version="${2:?}"

cat <<EOF > "$file"
rabbitmq-autocluster ($version-1) unstable; urgency=low

  * Upstream release for $version

 -- Autocluster Release <autocluster@autocluster.domain>  $(date -R)
EOF
