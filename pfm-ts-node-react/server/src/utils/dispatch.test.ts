import {describe, it, expect} from 'vitest';
import {dispatchInit} from './dispatch.js';

describe('dispatch', () => {
    it('should create a dispatch with onMatch, onMatchPstring, onMatchPnumber methods', () => {
        const dispatch = dispatchInit();

        expect(dispatch).toHaveProperty('onMatch');
        expect(dispatch).toHaveProperty('onMatchPstring');
        expect(dispatch).toHaveProperty('onMatchPnumber');
        expect(typeof dispatch).toBe('function');
    });

    it('should match exact routes and call the handler', async () => {
        const dispatch = dispatchInit();
        let handlerCalled = false;

        const handler = async (): Promise<void> => {
            handlerCalled = true;
        };

        // Register the route
        dispatch.onMatch('GET', '/health', handler);

        // Test dispatch with simple method and url
        const matched = await dispatch({method: 'GET', url: '/health'});

        expect(matched).toBe(true);
        expect(handlerCalled).toBe(true);
    });

    it('can extract number params', async () => {
        const dispatch = dispatchInit();
        let result = 0;

        // Register the route - handler factory returns handler that uses the number
        dispatch.onMatchPnumber('GET', '/inc/{n}', (n: number) => () => {
            result = n + 1;  // Extract 123, add 1 = 124
        });

        // Test dispatch with number parameter
        const matched = await dispatch({method: 'GET', url: '/inc/123'});
        expect(matched).toBe(true);
        expect(result).toBe(124);
    })
});