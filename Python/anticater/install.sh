#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SERVICE_NAME="anticater-precise-scroll"

echo "Installing ${SERVICE_NAME}..."

set -x
sudo cp "${SCRIPT_DIR}/${SERVICE_NAME}.py" "/usr/local/bin/${SERVICE_NAME}.py"
sudo cp "${SCRIPT_DIR}/${SERVICE_NAME}.service" "/etc/systemd/system/${SERVICE_NAME}.service"

sudo systemctl daemon-reload
set +x

echo "Done. To enable and start the service, run:"
echo "  sudo systemctl enable --now ${SERVICE_NAME}"
