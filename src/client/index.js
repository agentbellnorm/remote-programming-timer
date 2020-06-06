import './index.html';

import { Elm } from './elm/Main.elm';

const app = Elm.Main.init({
  node: document.getElementById('elm'),
  flags: window.location.href,
});

const socket = io();

app.ports.sendMessage.subscribe(function(message) {
  console.log('sending message: ', message);
  socket.emit('message', message);
});

socket.on('message', function(message) {
  console.log('recieve message: ', message);
  app.ports.receiveMessage.send(JSON.stringify(message));
});
