FROM clojure as build

WORKDIR /app
ADD . /app/
RUN lein do clean, cljsbuild once optimized

FROM nginx:alpine as deploy
WORKDIR /usr/share/nginx/html
COPY --from=build /app/resources/public .
