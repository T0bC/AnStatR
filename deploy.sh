#!/bin/bash
set -e

# ── Configuration ──────────────────────────────────────
APP_DIR="/opt/shinyapps/TexAn2.0"    # Change to your app path
IMAGE_NAME="texan"                   # Change to your image name
# ───────────────────────────────────────────────────────

TIMESTAMP=$(date +%Y%m%d_%H%M%S)

echo ""
echo "================================================"
echo "  Deploying $IMAGE_NAME"
echo "  $(date)"
echo "================================================"
echo ""

# Pull latest code
echo "[1/4] Pulling latest code..."
cd "$APP_DIR"
git pull

# Build new image with timestamp tag + latest tag
echo ""
echo "[2/4] Building Docker image..."
echo "       (This is fast if only code changed)"
echo "       (Slow if renv.lock changed — packages recompile)"
echo ""
docker build -t "$IMAGE_NAME:$TIMESTAMP" -t "$IMAGE_NAME:latest" .

# Restart ShinyProxy to pick up the new image
echo ""
echo "[3/4] Restarting ShinyProxy..."
sudo systemctl restart shinyproxy

# Clean up old images (keeps tagged versions for rollback)
echo ""
echo "[4/4] Cleaning up old images (keeping 5 newest)..."

# List all timestamp-tagged images, sorted newest first
IMAGES=$(docker images --format "{{.Repository}}:{{.Tag}}" \
    | grep "^$IMAGE_NAME:" \
    | grep -v "latest" \
    | sort -r)

# Keep only the first 5, delete the rest
COUNT=0
echo "$IMAGES" | while read -r IMG; do
    COUNT=$((COUNT + 1))
    if [ $COUNT -le 5 ]; then
        echo "Keeping: $IMG"
    else
        echo "Removing: $IMG"
        docker rmi "$IMG"
    fi
done


echo ""
echo "================================================"
echo "  Deploy complete: $IMAGE_NAME:$TIMESTAMP"
echo "  Rollback available: docker tag $IMAGE_NAME:$TIMESTAMP $IMAGE_NAME:latest"
echo "================================================"
echo ""