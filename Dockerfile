FROM node:lts-alpine AS base
USER node
RUN mkdir -p ~/.npm-global && \
    npm config set prefix '~/.npm-global' && \
    npm install create-elm-app -g

FROM base AS builder
WORKDIR /home/node
COPY public public
COPY src src
COPY tests tests
COPY elm.json package.json package-lock.json ./

RUN export PATH=~/.npm-global/bin:$PATH && \
    npm ci && \
    elm-app build


FROM nginx:1.17.9-alpine
COPY --from=builder /home/node/build/ /usr/share/nginx/html
COPY nginx.conf /etc/nginx/nginx.conf
COPY default.conf /etc/nginx/conf.d/default.conf
