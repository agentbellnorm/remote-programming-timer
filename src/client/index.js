import './index.html';

import { Elm } from './elm/Main.elm';

const socket = io();

Notification.requestPermission();

const app = Elm.Main.init({
  node: document.getElementById('elm'),
  flags: {location: window.location.href},
});

app.ports.sendMessage.subscribe(message => {
  console.log('sending message: ', message);
  socket.emit('message', message);
});

app.ports.notify.subscribe(message => {
  console.log('notify: ', message);
  if (Notification.permission === "granted") {
    new Notification(message);
    document.querySelector('#bell').play();
  }

  else if (Notification.permission !== "denied") {
    Notification.requestPermission().then(permission => {
      if (permission === "granted") {
        new Notification("Hi there!");
      }
    });
  }
});

socket.on('message', message => {
  console.log('recieve message: ', message);
  app.ports.receiveMessage.send(JSON.stringify(message));
});
