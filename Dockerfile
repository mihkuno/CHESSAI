FROM node:20-slim

# Install R and build dependencies for node-pty
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    r-base \
    python3 \
    make \
    g++ \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Install pnpm
RUN npm install -g pnpm

# Copy dependency files
COPY package.json pnpm-lock.yaml* ./

# Install node deps (this will now successfully compile node-pty)
RUN pnpm install

# Copy app source
COPY . .

# Cloud Run port
EXPOSE 8080

# Use standard node to start to ensure clean signal handling
CMD ["node", "server.js"]