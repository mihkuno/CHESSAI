FROM node:20-bookworm

# Install R and the required system libraries for node-pty
# 'bookworm' (non-slim) is used here to ensure all shared libs for terminal emulation are present
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    r-base \
    python3 \
    make \
    g++ \
    libc6-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Install pnpm
RUN npm install -g pnpm

# Copy dependency files
COPY package.json pnpm-lock.yaml* ./

# Install node deps and force native compilation
RUN pnpm install --unsafe-perm

# Copy app source
COPY . .

# Cloud Run port
EXPOSE 8080

# Use standard node to start
CMD ["node", "server.js"]