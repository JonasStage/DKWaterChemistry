#!/usr/bin/env bash
# ============================================================================
# miljoeportal_sync.sh - Download water chemistry data from Miljøportalen API
# ============================================================================
#
# Usage:
#   ./miljoeportal_sync.sh dk_water_all      Download all DKWaterChemistry data
#   ./miljoeportal_sync.sh --help            Show this help message
#   ./miljoeportal_sync.sh --check           Verify setup (credentials, dirs)
#
# Setup:
#   1. Copy credentials template:  sudo cp miljoeportal.conf.template /etc/miljoeportal.conf
#   2. Edit with real credentials: sudo nano /etc/miljoeportal.conf
#   3. Lock permissions:           sudo chmod 600 /etc/miljoeportal.conf
#   4. Install crontab:            crontab miljoeportal_crontab
#
# ============================================================================

set -euo pipefail

# --- Configuration -----------------------------------------------------------
CREDENTIALS_FILE="/etc/miljoeportal.conf"
BASE_URL="https://arealdata-api.miljoeportal.dk/data"
SHINY_DIR="/srv/shiny-server"
# Auto-detect writable log location (first writable path wins)
LOG_FILE=""
for _candidate in "/var/log/miljoeportal_sync.log" "${HOME}/miljoeportal_sync.log" "/tmp/miljoeportal_sync.log"; do
  if touch "$_candidate" 2>/dev/null; then
    LOG_FILE="$_candidate"
    break
  fi
done
CURL_OPTS="--retry 5 --retry-connrefused --retry-delay 10 --fail --silent --show-error"

# --- Colors & formatting -----------------------------------------------------
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Disable colors when not running in a terminal (e.g., from cron)
if [[ ! -t 1 ]]; then
  RED="" GREEN="" YELLOW="" BLUE="" BOLD="" NC=""
fi

# --- Helper functions --------------------------------------------------------
log() {
  local level="$1"
  local msg="$2"
  local timestamp
  timestamp="$(date '+%Y-%m-%d %H:%M:%S')"

  # Always write plain text to log file
  [[ -n "$LOG_FILE" ]] && echo "${timestamp} [${level}] ${msg}" >> "$LOG_FILE" 2>/dev/null || true

  # Pretty-print to terminal
  case "$level" in
    OK)    echo -e "  ${GREEN}✔${NC} ${msg}" ;;
    INFO)  echo -e "  ${BLUE}ℹ${NC} ${msg}" ;;
    WARN)  echo -e "  ${YELLOW}⚠${NC} ${msg}" ;;
    ERROR) echo -e "  ${RED}✖${NC} ${msg}" ;;
    *)     echo "  $msg" ;;
  esac
}

header() {
  echo ""
  echo -e "${BOLD}── $1 ──${NC}"
}

show_help() {
  echo ""
  echo -e "${BOLD}Miljøportalen Data Sync${NC}"
  echo ""
  echo "Downloads water chemistry data from the Danish Miljøportalen API"
  echo "and stores it for use by Shiny Server applications."
  echo ""
  echo -e "${BOLD}USAGE${NC}"
  echo "  $(basename "$0") <command>"
  echo ""
  echo -e "${BOLD}COMMANDS${NC}"
  echo "  dk_water_all     Download & filter all DKWaterChemistry datasets (scheduled: weekly)"
  echo "  --check          Verify credentials, directories, and connectivity"
  echo "  --help           Show this help message"
  echo ""
  echo -e "${BOLD}FILES${NC}"
  echo "  /etc/miljoeportal.conf   API credentials (API_USER, API_PASS)"
  echo "  Log: ${LOG_FILE}"
  echo ""
  echo -e "${BOLD}SETUP${NC}"
  echo "  1. sudo cp miljoeportal.conf.template /etc/miljoeportal.conf"
  echo "  2. sudo nano /etc/miljoeportal.conf   # add your credentials"
  echo "  3. sudo chmod 600 /etc/miljoeportal.conf"
  echo "  4. crontab miljoeportal_crontab"
  echo ""
}

load_credentials() {
  if [[ ! -f "$CREDENTIALS_FILE" ]]; then
    log "ERROR" "Credentials file not found: $CREDENTIALS_FILE"
    echo ""
    echo "  Run these commands to set up:"
    echo "    sudo cp miljoeportal.conf.template /etc/miljoeportal.conf"
    echo "    sudo nano /etc/miljoeportal.conf"
    echo "    sudo chmod 600 /etc/miljoeportal.conf"
    echo ""
    exit 1
  fi

  source "$CREDENTIALS_FILE"

  if [[ -z "${API_USER:-}" || -z "${API_PASS:-}" ]]; then
    log "ERROR" "API_USER or API_PASS not set in $CREDENTIALS_FILE"
    exit 1
  fi
}

download() {
  local dataset="$1"
  local output="$2"
  local tmpfile="${output}.tmp"
  local label
  label="$(basename "$output")"

  printf "  ${BLUE}↓${NC} Downloading %-30s " "$label"

  if curl $CURL_OPTS -u "${API_USER}:${API_PASS}" "${BASE_URL}/${dataset}/file?" -o "$tmpfile"; then
    mv "$tmpfile" "$output"
    local size
    size=$(du -h "$output" | cut -f1 | xargs)
    echo -e "${GREEN}✔${NC} ${size}"
    log "OK" "Downloaded ${dataset} -> ${output} (${size})"
  else
    rm -f "$tmpfile"
    echo -e "${RED}✖ failed${NC}"
    log "ERROR" "Failed to download ${dataset}"
    return 1
  fi
}

filter_oxygen() {
  local input="$1"
  local output="$2"
  local fields="$3"
  local label
  label="$(basename "$output")"

  printf "  ${BLUE}⚙${NC} Filtering %-32s " "$label"
  awk -F";" "(/Oxygen indhold/) {print $fields}" "$input" > "$output"

  local rows
  rows=$(wc -l < "$output" | xargs)
  echo -e "${GREEN}✔${NC} ${rows} rows"
  log "OK" "Filtered ${label}: ${rows} rows"
}

# --- Preflight check ---------------------------------------------------------
check_setup() {
  local errors=0

  header "Checking credentials"
  if [[ -f "$CREDENTIALS_FILE" ]]; then
    log "OK" "Credentials file exists: $CREDENTIALS_FILE"

    local perms
    perms=$(stat -c "%a" "$CREDENTIALS_FILE" 2>/dev/null || stat -f "%Lp" "$CREDENTIALS_FILE" 2>/dev/null)
    if [[ "$perms" == "600" ]]; then
      log "OK" "File permissions are 600 (secure)"
    else
      log "WARN" "File permissions are $perms (should be 600)"
    fi

    if source "$CREDENTIALS_FILE" 2>/dev/null; then
      if [[ -n "${API_USER:-}" && -n "${API_PASS:-}" ]]; then
        log "OK" "API_USER and API_PASS are set"
      else
        log "ERROR" "API_USER or API_PASS is missing"
        ((errors++))
      fi
    else
      log "WARN" "Cannot read $CREDENTIALS_FILE (run --check with sudo to verify contents)"
    fi
  else
    log "ERROR" "Credentials file not found: $CREDENTIALS_FILE"
    ((errors++))
  fi

  header "Checking directories"
  for dir in \
    "${SHINY_DIR}/DKWaterChemistry/data"; do
    if [[ -d "$dir" ]]; then
      log "OK" "$dir"
    else
      log "ERROR" "Missing directory: $dir"
      ((errors++))
    fi
  done

  header "Checking connectivity"
  if curl --silent --head --fail --max-time 10 "https://arealdata-api.miljoeportal.dk" > /dev/null 2>&1; then
    log "OK" "API endpoint is reachable"
  else
    log "WARN" "Cannot reach API (may need VPN or network access)"
  fi

  header "Checking log file"
  log "OK" "Log file: $LOG_FILE"

  echo ""
  if [[ $errors -eq 0 ]]; then
    echo -e "  ${GREEN}${BOLD}All checks passed ✔${NC}"
  else
    echo -e "  ${RED}${BOLD}${errors} issue(s) found — fix before running sync${NC}"
  fi
  echo ""

  return $errors
}

# --- Tasks -------------------------------------------------------------------
task_dk_water_all() {
  local dk_data="${SHINY_DIR}/DKWaterChemistry/data"

  header "Downloading datasets"
  download "vanda-ue-25" "${dk_data}/marin.csv"
  download "vanda-ue-26" "${dk_data}/sø.csv"
  download "vanda-ue-27" "${dk_data}/vandløb.csv"
  download "vanda-ue-34" "${dk_data}/marin_felt_raw.csv"
  download "vanda-ue-35" "${dk_data}/sø_felt_raw.csv"
  download "vanda-ue-36" "${dk_data}/vandløb_felt_raw.csv"

  header "Cleanup"
  log "INFO" "Removing old filtered files"
  rm -f "${dk_data}/marin_felt.csv" \
        "${dk_data}/sø_felt.csv" \
        "${dk_data}/vandløb_felt.csv"
  log "OK" "Old filtered files removed"

  header "Filtering oxygen measurements"
  filter_oxygen "${dk_data}/marin_felt_raw.csv" \
                "${dk_data}/marin_felt.csv" \
                '$3 ";" $9 ";" $11 ";" $19 ";" $20 ";" $23 ";" $24 ";" $25 ";" $27 ";" $29 ";" $30'

  filter_oxygen "${dk_data}/sø_felt_raw.csv" \
                "${dk_data}/sø_felt.csv" \
                '$3 ";" $9 ";" $11 ";" $19 ";" $20 ";" $23 ";" $24 ";" $25 ";" $26 ";" $27 ";" $28'

  filter_oxygen "${dk_data}/vandløb_felt_raw.csv" \
                "${dk_data}/vandløb_felt.csv" \
                '$3 ";" $9 ";" $11 ";" $19 ";" $20 ";" $23 ";" $24 ";" $25 ";" $26 ";" $27 ";" $28'
}

# --- Main --------------------------------------------------------------------
case "${1:-}" in
  dk_water_all)
    load_credentials
    task_dk_water_all
    echo ""
    log "OK" "DKWaterChemistry sync complete"
    ;;
  --check)
    header "Miljøportalen Sync — Preflight Check"
    check_setup
    ;;
  --help|-h)
    show_help
    ;;
  *)
    show_help
    exit 1
    ;;
esac
