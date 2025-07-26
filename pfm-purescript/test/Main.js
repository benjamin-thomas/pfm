import { existsSync, unlinkSync } from 'fs';
import { createServer } from 'net';
import { randomUUID } from 'crypto';

export const removeFile = (path) => () =>
    existsSync(path) && unlinkSync(path);

export const getRandomPort = () => {
    const server = createServer();
    server.listen(0);
    const port = server.address().port;
    server.close();
    return port;
};

export const getUniqueId = () => randomUUID();

export const registerExitHandler = (cleanup) => () => {
    process.on('exit', cleanup);
    process.on('SIGINT', cleanup);
    process.on('SIGTERM', cleanup);
};