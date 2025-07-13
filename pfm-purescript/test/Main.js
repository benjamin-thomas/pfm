import { existsSync, unlinkSync } from 'fs';

export const removeFile = (path) => () =>
    existsSync(path) && unlinkSync(path);