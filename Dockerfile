FROM node:24

WORKDIR /app

# Install R
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    r-base \
    && rm -rf /var/lib/apt/lists/*

# Install pnpm
RUN npm install -g pnpm

# Copy dependency files
COPY package.json pnpm-lock.yaml ./

# Install node deps
RUN pnpm install --frozen-lockfile

# Copy app
COPY . .

# Cloud Run port
EXPOSE 8080

# Start server
CMD ["pnpm", "start"]
