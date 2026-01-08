# Use Node 22 LTS as indicated in your successful logs
FROM node:22-bookworm

WORKDIR /app

# 1. Install R and required build tools for native Node modules
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    python3 \
    r-base-core \
    r-base-dev \
    && ln -s /usr/bin/python3 /usr/bin/python \
    && rm -rf /var/lib/apt/lists/*

# 2. Install R packages 
RUN Rscript -e "install.packages('jsonlite', repos='https://cloud.r-project.org')"

# 3. Setup pnpm
RUN npm install -g pnpm

# 4. Copy ONLY dependency files first to leverage Docker caching
COPY package.json pnpm-lock.yaml ./

# 5. Install dependencies (this builds node-pty for Linux)
RUN pnpm install --frozen-lockfile

# 6. Copy the rest of the application
# This is where a .dockerignore is CRITICAL to avoid overwriting node_modules
COPY . .

# 7. Safety Net: Force a rebuild of native modules for this environment
RUN pnpm rebuild node-pty

# Cloud Run port 
EXPOSE 8080

CMD ["pnpm", "start"]