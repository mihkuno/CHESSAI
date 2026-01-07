# use node 24 as the base image
FROM node:24

# set the working directory in the container
WORKDIR /app

# copy the package.json file and package-lock.json file
COPY package*.json ./

# install dependencies
RUN npm install -g pnpm
RUN pnpm install

# copy the rest of the application code
COPY . .

# expose port 3000 to the outside world
EXPOSE 3000

# start the application
CMD ["pnpm", "start"]