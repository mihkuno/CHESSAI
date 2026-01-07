FROM node:24

WORKDIR /app

# Install R + Rscript
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    r-base-core \
    r-base-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN Rscript -e "install.packages('jsonlite', repos='https://cloud.r-project.org')"

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
