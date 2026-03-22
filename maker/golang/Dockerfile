# ============================================
# ip2region xdb maker - Golang version
# Multi-stage build, resulting in an image of approximately 10 MB.
# ============================================

FROM golang:1.22-alpine AS builder

WORKDIR /build

COPY . ./

# Compile a static binary (disable CGO for fully static linking)
RUN CGO_ENABLED=0 GOOS=linux go build -ldflags="-s -w" -o xdb_maker

FROM alpine:3.19

RUN apk --no-cache add ca-certificates tzdata && \
    cp /usr/share/zoneinfo/Asia/Shanghai /etc/localtime && \
    echo "Asia/Shanghai" > /etc/timezone

# Create a non-root user.
RUN adduser -D -g '' appuser

WORKDIR /app

# Copy binaries from the build stage.
COPY --from=builder /build/xdb_maker /app/

RUN mkdir -p /app/data && chown -R appuser:appuser /app

USER appuser

ENTRYPOINT ["/app/xdb_maker"]

CMD ["--help"]
