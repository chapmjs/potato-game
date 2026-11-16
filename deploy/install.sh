#!/usr/bin/env bash
# Install or update the Potato Game on an Ubuntu Shiny Server.
# Run from the repository folder:   sudo bash deploy/install.sh
# Optional:  APP_DIR=/srv/shiny-server/potato  sudo -E bash deploy/install.sh
set -euo pipefail

APP_DIR="${APP_DIR:-/srv/shiny-server/potato}"
SRC="$(cd "$(dirname "$0")/.." && pwd)"

echo "Installing R packages that are missing (shiny, bslib, DBI, RSQLite)..."
Rscript -e 'need <- setdiff(c("shiny","bslib","DBI","RSQLite"), rownames(installed.packages())); if (length(need)) install.packages(need, repos = "https://cloud.r-project.org")'

echo "Copying the app to $APP_DIR ..."
mkdir -p "$APP_DIR/data"
# the data folder (database + password) is never overwritten by an update
cp -r "$SRC/app.R" "$SRC/R" "$SRC/www" "$APP_DIR/"

if [ ! -f "$APP_DIR/data/instructor_password.txt" ]; then
  read -r -s -p "Choose an instructor password: " PW; echo
  printf '%s\n' "$PW" > "$APP_DIR/data/instructor_password.txt"
fi

chown -R shiny:shiny "$APP_DIR"
chmod 700 "$APP_DIR/data"
chmod 600 "$APP_DIR/data/instructor_password.txt"
touch "$APP_DIR/restart.txt"     # tells Shiny Server to reload the app

echo "Done. Open  https://<your-server>/$(basename "$APP_DIR")/  and click Instructor at the bottom."
