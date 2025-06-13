#!/bin/bash
# Build script for hetid Docker images
# Implements 2025 best practices with BuildKit and multi-platform support

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default values
BUILD_TARGET="production"
PLATFORM="linux/amd64"
CACHE_FROM=""
PUSH=false
TAG_LATEST=false

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to show usage
usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Build hetid Docker images with 2025 best practices

OPTIONS:
    -t, --target TARGET     Build target (production|development|builder) [default: production]
    -p, --platform PLATFORM Target platform [default: linux/amd64]
    -c, --cache-from IMAGE  Use cache from image
    --push                  Push image to registry after build
    --tag-latest           Also tag as 'latest'
    -h, --help             Show this help message

EXAMPLES:
    $0                                          # Build production image
    $0 -t development                          # Build development image
    $0 -t production --push --tag-latest       # Build and push production with latest tag
    $0 -p linux/amd64,linux/arm64 --push      # Multi-platform build

EOF
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -t|--target)
            BUILD_TARGET="$2"
            shift 2
            ;;
        -p|--platform)
            PLATFORM="$2"
            shift 2
            ;;
        -c|--cache-from)
            CACHE_FROM="$2"
            shift 2
            ;;
        --push)
            PUSH=true
            shift
            ;;
        --tag-latest)
            TAG_LATEST=true
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            print_error "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done

# Validate build target
case $BUILD_TARGET in
    production|development|builder)
        ;;
    *)
        print_error "Invalid build target: $BUILD_TARGET"
        print_error "Valid targets: production, development, builder"
        exit 1
        ;;
esac

# Set image name based on target
case $BUILD_TARGET in
    production)
        IMAGE_NAME="hetid:latest"
        DOCKERFILE="Dockerfile"
        ;;
    development)
        IMAGE_NAME="hetid:dev"
        DOCKERFILE="Dockerfile.dev"
        ;;
    builder)
        IMAGE_NAME="hetid:builder"
        DOCKERFILE="Dockerfile"
        ;;
esac

print_status "Building hetid Docker image..."
print_status "Target: $BUILD_TARGET"
print_status "Platform: $PLATFORM"
print_status "Image: $IMAGE_NAME"

# Enable BuildKit
export DOCKER_BUILDKIT=1

# Build command
BUILD_CMD="docker build"

# Add platform if specified
if [[ "$PLATFORM" == *","* ]]; then
    # Multi-platform build requires buildx
    BUILD_CMD="docker buildx build"
    print_status "Multi-platform build detected, using buildx"
fi

BUILD_CMD="$BUILD_CMD --platform $PLATFORM"

# Add cache from if specified
if [[ -n "$CACHE_FROM" ]]; then
    BUILD_CMD="$BUILD_CMD --cache-from $CACHE_FROM"
fi

# Add target if not development (which uses different Dockerfile)
if [[ "$BUILD_TARGET" != "development" ]]; then
    BUILD_CMD="$BUILD_CMD --target $BUILD_TARGET"
fi

# Add tags
BUILD_CMD="$BUILD_CMD -t $IMAGE_NAME"

if [[ "$TAG_LATEST" == true && "$BUILD_TARGET" == "production" ]]; then
    BUILD_CMD="$BUILD_CMD -t hetid:latest"
fi

# Add push if requested
if [[ "$PUSH" == true ]]; then
    BUILD_CMD="$BUILD_CMD --push"
fi

# Add dockerfile and context
BUILD_CMD="$BUILD_CMD -f $DOCKERFILE ."

print_status "Executing: $BUILD_CMD"

# Execute build
if eval "$BUILD_CMD"; then
    print_success "Build completed successfully!"

    if [[ "$PUSH" == false ]]; then
        print_status "Image built locally: $IMAGE_NAME"
        print_status "To run: docker run --rm -it $IMAGE_NAME"
    else
        print_success "Image pushed to registry: $IMAGE_NAME"
    fi
else
    print_error "Build failed!"
    exit 1
fi

# Show image info if built locally
if [[ "$PUSH" == false ]]; then
    print_status "Image information:"
    docker images | grep hetid | head -5
fi
