#!/bin/bash
set -e

# ── Configuration ──────────────────────────────────────
APP_DIR="/opt/shinyapps/AnStatR"    # Change to your app path
IMAGE_NAME="anstatr"                   # Change to your image name
# ───────────────────────────────────────────────────────

TIMESTAMP=$(date +%Y%m%d_%H%M%S)

echo ""
echo "================================================"
echo "  Deploying $IMAGE_NAME"
echo "  $(date)"
echo "================================================"
echo ""

# Pull latest code
echo "[1/5] Pulling latest code..."
cd "$APP_DIR"
git fetch origin
git reset --hard origin/main

# Clean up old TexAn containers/images for migration
echo ""
echo "[2/5] Migrating from old TexAn image name..."
OLD_IMAGE="texan"
OLD_CONTAINERS=$(docker ps -aq --filter "ancestor=$OLD_IMAGE" 2>/dev/null || true)
if [ -n "$OLD_CONTAINERS" ]; then
    echo "       Stopping and removing old $OLD_IMAGE containers..."
    docker rm -f $OLD_CONTAINERS
fi
OLD_IMAGES=$(docker images -q "$OLD_IMAGE" 2>/dev/null || true)
if [ -n "$OLD_IMAGES" ]; then
    echo "       Removing old $OLD_IMAGE images..."
    docker rmi -f $OLD_IMAGES
fi

# Build new image with timestamp tag + latest tag
echo ""
echo "[3/5] Building Docker image..."
echo "       (This is fast if only code changed)"
echo "       (Slow if renv.lock changed — packages recompile)"
echo ""
docker build -t "$IMAGE_NAME:$TIMESTAMP" -t "$IMAGE_NAME:latest" .

# Restart ShinyProxy to pick up the new image
echo ""
echo "[4/5] Restarting ShinyProxy..."
sudo systemctl restart shinyproxy

# Clean up old images (keeps tagged versions for rollback)
echo ""
echo "[5/5] Cleaning up old images (keeping 5 newest)..."

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