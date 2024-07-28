set -e -o pipefail
echo "checking $1..."
LAZIER_QUIET=1 chez --script "$1" 2>&1 | awk -v prefix="$1: " '$0=prefix$0'
