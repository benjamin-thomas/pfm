import { existsSync, unlinkSync } from 'fs';
import { createServer } from 'net';

export const removeFile = (path) => () =>
    existsSync(path) && unlinkSync(path);

export const getRandomPort = () => {
    const server = createServer();
    server.listen(0);
    const port = server.address().port;
    server.close();
    return port;
};