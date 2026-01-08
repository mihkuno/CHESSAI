FROM node:24

WORKDIR /app

# 1. Install R PLUS the build tools required to compile node-pty
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    python3 \
    r-base-core \
    r-base-dev \
    && ln -s /usr/bin/python3 /usr/bin/python \
    && rm -rf /var/lib/apt/lists/*

# 2. Install required R packages 
RUN Rscript -e "install.packages('jsonlite', repos='https://cloud.r-project.org')"

# 3. Install pnpm 
RUN npm install -g pnpm

# 4. Copy dependency files
COPY package.json pnpm-lock.yaml ./

# 5. Install node deps
RUN pnpm install --frozen-lockfile

# Fix node-pty build issue on Debian/Linux
RUN sed -i '/-lutil/d' node_modules/node-pty/build/binding.gyp && \
    pnpm rebuild node-pty

# 6. Copy the rest of the application
COPY . .

# Cloud Run port 
EXPOSE 8080

# Start server 
CMD ["pnpm", "start"]