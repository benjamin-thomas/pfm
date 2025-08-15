import http from 'http';


const handleHello = (req, res) => {
  res.writeHead(200, { 'Content-Type': 'text/plain' });
  res.end('Hello World!\n');
}

const handleSSE = (req, res) => {
  res.writeHead(200, {
    'Content-Type': 'text/event-stream',
    'Cache-Control': 'no-cache',
    'Connection': 'keep-alive',
    'Access-Control-Allow-Origin': '*'
  });

  console.log('[SSE] Client connected');

  // Send initial message
  res.write('event: connected\n');
  res.write('data: {"message": "SSE connection established"}\n\n');

  // Send ping every second
  const interval = setInterval(() => {
    const time = new Date().toISOString();
    console.log(`[SSE] Sending ping at ${time}`);
    res.write('event: ping\n');
    res.write(`data: {"time": "${time}"}\n\n`);
  }, 1000);

  // Clean up on client disconnect
  req.on('close', () => {
    console.log('[SSE] Client disconnected');
    clearInterval(interval);
  });
}

const handleNotFound = (req, res) => {
  res.writeHead(404);
  res.end('Not Found\n');
}

const route = (req, res) => {
  switch (req.url) {
    case '/hello':
      return handleHello(req, res);
    case '/sse':
      return handleSSE(req, res);
    default:
      return handleNotFound(req, res);
  }

  throw new Error('Unreachable code');
}

const server = http.createServer((req, res) => {
  console.log(`[${new Date().toISOString()}] ${req.method} ${req.url}`);
  route(req, res);
});

const PORT = 3333;
server.listen(PORT, () => {
  console.log(`Server running at http://localhost:${PORT}/`);
  console.log('Test endpoints:');
  console.log(`  curl http://localhost:${PORT}/hello`);
  console.log(`  curl -N http://localhost:${PORT}/sse`);
});