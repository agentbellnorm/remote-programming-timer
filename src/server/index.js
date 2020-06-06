const express = require('express');
const app = express();
const server = require('http').Server(app);
const io = require('socket.io')(server);
const { v4: uuid } = require('uuid');

const sessions = {};

app.use(express.static('dist'));

const onCreateSession = (socket, msg) => {
  const sessionId = uuid();
  sessions[sessionId] = {...msg, sessionId};
  socket.join(sessionId);
  socket.emit('message', {sessionId, type: 'created-session'});
  console.log(`created session ${sessionId}, sessions: ${JSON.stringify(sessions)}`)
};

const onJoin = (socket, msg) => {
  const {sessionId} = msg;
  socket.join(sessionId);
  socket.emit('message', {...sessions[sessionId].state, type: 'action'});
};

const onAction = (socket, message) => {
  let session = sessions[message.sessionId];
  session.state = message;
  console.log('emitting message ', message);
  io.to(message.sessionId).emit('message', {...message, type: 'action',});
};

io.on('connection', socket => {
  console.log('connected: ', socket.id);

  socket.on('message', msg => {
    console.log('recieved message: ', msg);
    const parsed = JSON.parse(msg);

    switch (parsed.type) {
      case 'join':
        onJoin(socket, parsed);
        break;
      case 'action':
        onAction(socket, parsed);
        break;
      case 'create-session':
        onCreateSession(socket);
        break;
      default:
        console.log('no handler for message with type ' + parsed.type + " " + msg);
    }
  });

  socket.on('disconnect', () => {
    console.log(`disconnected: ${socket.id}`)
  })
});

server.listen(9000, function() {
  console.log('listening on *:9000');
});
