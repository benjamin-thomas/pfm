import { describe, it, expect } from 'vitest';
import { httpDispatchInit } from './httpDispatch.js';

describe('httpDispatch', () => {
    it('should create an httpDispatch with onMatch, onMatchP, onMatchP_Async methods', () => {
        const httpDispatch = httpDispatchInit();

        expect(httpDispatch).toHaveProperty('onMatch');
        expect(httpDispatch).toHaveProperty('onMatchP');
        expect(httpDispatch).toHaveProperty('onMatchP_Async');
        expect(typeof httpDispatch.dispatch).toBe('function');
    });

    it('should match exact routes and call the handler', async () => {
        const httpDispatch = httpDispatchInit();
        let handlerCalled = false;

        const handler = (): void => {
            handlerCalled = true;
        };

        // Register the route
        httpDispatch.onMatch('GET', '/health', handler);

        // Test dispatch with simple method and url
        const matched = await httpDispatch.dispatch({ method: 'GET', url: '/health' });

        expect(matched).toBe(true);
        expect(handlerCalled).toBe(true);
    });

    it('should handle number parameters with onMatchP<number> (sync)', async () => {
        const httpDispatch = httpDispatchInit();
        let result = 0;

        // Register route with typed parameter handler (sync)
        httpDispatch.onMatchP<number>('GET', '/inc/{n}', (n: number) => {
            result = n + 1;  // Extract 123, add 1 = 124
        }, (s) => parseInt(s, 10));

        // Test dispatch with number parameter
        const matched = await httpDispatch.dispatch({ method: 'GET', url: '/inc/123' });
        
        expect(matched).toBe(true);
        expect(result).toBe(124);
    });

    it('should handle string parameters with onMatchP<string> (sync)', async () => {
        const httpDispatch = httpDispatchInit();
        let result = '';

        // Register route with typed parameter handler (sync)
        httpDispatch.onMatchP<string>('GET', '/hello/{name}', (name: string) => {
            result = `Hello, ${name}!`;
        }, (s) => s);

        // Test dispatch with string parameter
        const matched = await httpDispatch.dispatch({ method: 'GET', url: '/hello/world' });
        
        expect(matched).toBe(true);
        expect(result).toBe('Hello, world!');
    });

    it('should handle number parameters with onMatchP_Async<number>', async () => {
        const httpDispatch = httpDispatchInit();
        let result = 0;

        // Register route with typed async parameter handler
        httpDispatch.onMatchP_Async<number>('GET', '/async-inc/{n}', async (n: number) => {
            // Simulate async work
            await new Promise(resolve => setTimeout(resolve, 1));
            result = n * 2;  // Extract 123, multiply by 2 = 246
        }, (s) => parseInt(s, 10));

        // Test dispatch with number parameter
        const matched = await httpDispatch.dispatch({ method: 'GET', url: '/async-inc/123' });
        
        expect(matched).toBe(true);
        expect(result).toBe(246);
    });

    it('should handle string parameters with onMatchP_Async<string>', async () => {
        const httpDispatch = httpDispatchInit();
        let result = '';

        // Register route with typed async parameter handler
        httpDispatch.onMatchP_Async<string>('GET', '/async-greet/{name}', async (name: string) => {
            // Simulate async work
            await new Promise(resolve => setTimeout(resolve, 1));
            result = `Async greeting: ${name}`;
        }, (s) => s);

        // Test dispatch with string parameter
        const matched = await httpDispatch.dispatch({ method: 'GET', url: '/async-greet/alice' });
        
        expect(matched).toBe(true);
        expect(result).toBe('Async greeting: alice');
    });

    it('should return false when no route matches', async () => {
        const httpDispatch = httpDispatchInit();

        httpDispatch.onMatch('GET', '/health', () => {});

        const matched = await httpDispatch.dispatch({ method: 'GET', url: '/nonexistent' });
        
        expect(matched).toBe(false);
    });

    it('should handle custom type with onMatchP<CustomType>', async () => {
        type UserId = { id: number };
        const httpDispatch = httpDispatchInit();
        let result: UserId | null = null;

        // Register route with custom type parameter handler
        httpDispatch.onMatchP<UserId>('GET', '/user/{userId}', (userId: UserId) => {
            result = userId;
        }, (s) => ({ id: parseInt(s, 10) }));

        // Test dispatch with custom type parameter
        const matched = await httpDispatch.dispatch({ method: 'GET', url: '/user/42' });
        
        expect(matched).toBe(true);
        expect(result).toEqual({ id: 42 });
    });

    it('should handle invalid number parameters gracefully', async () => {
        const httpDispatch = httpDispatchInit();

        httpDispatch.onMatchP<number>('GET', '/api/{id}', (id: number) => {
            // This should not be called for invalid numbers
        }, (s) => {
            const n = parseInt(s, 10);
            if (isNaN(n)) {
                throw new Error(`Invalid number parameter: ${s}`);
            }
            return n;
        });

        // Test with invalid number - should throw or return false
        await expect(
            httpDispatch.dispatch({ method: 'GET', url: '/api/invalid' })
        ).rejects.toThrow('Invalid number parameter: invalid');
    });
});