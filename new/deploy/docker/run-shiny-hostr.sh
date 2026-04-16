#!/usr/bin/env bash
set -euo pipefail

: "${APP_DIR:=/public/shiny/RiboTE/new}"
: "${HOST_R_BIN:=/public/liuqi/miniconda3/envs/R4.3.0/bin/R}"
: "${SHINY_PORT:=3838}"

if [[ ! -x "${HOST_R_BIN}" ]]; then
  echo "Host R binary not found or not executable: ${HOST_R_BIN}" >&2
  exit 1
fi

if [[ ! -d "${APP_DIR}" ]]; then
  echo "RiboTE app directory not found: ${APP_DIR}" >&2
  exit 1
fi

mkdir -p "${APP_DIR}/tmp"
cd "${APP_DIR}"

exec "${HOST_R_BIN}" --quiet -e "options(shiny.host='0.0.0.0', shiny.port=${SHINY_PORT}); shiny::runApp('${APP_DIR}', host='0.0.0.0', port=${SHINY_PORT}, launch.browser=FALSE)"
