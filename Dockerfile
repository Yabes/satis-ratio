FROM node:lts-alpine AS base
USER node
RUN mkdir -p ~/.npm-global && \
    npm config set prefix '~/.npm-global' && \
    npm install create-elm-app -g

FROM base AS builder
WORKDIR /home/node
COPY . .
RUN export PATH=~/.npm-global/bin:$PATH && \
    elm-app build


FROM nginx:1.17.9-alpine
COPY --from=builder /home/node/build/ /usr/share/nginx/html
