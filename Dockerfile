FROM node:14-alpine

WORKDIR /usr/src/pp-timer

COPY . .

RUN npm ci
RUN npm run build

EXPOSE 9000

CMD [ "node", "src/server/index.js" ]

