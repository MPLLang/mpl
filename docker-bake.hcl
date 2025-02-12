group "default" {
    targets = ["amd64", "arm64"]
}

target "amd64" {
    dockerfile = "Dockerfile.amd64"
    tags = ["shwestrick/mpl:latest"]
    platforms = ["linux/amd64"]
}

target "arm64" {
    dockerfile = "Dockerfile.arm64"
    tags = ["shwestrick/mpl:latest"]
    platforms = ["linux/arm64"]
}
