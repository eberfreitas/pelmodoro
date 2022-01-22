FROM node:16 AS builder
USER node
WORKDIR /home/node/app
COPY --chown=node:node . .
RUN npm ci
RUN npm run build 

FROM nginx:1.17
COPY --from=builder /home/node/app/dist /usr/share/nginx/html
