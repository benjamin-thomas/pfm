import { describe, it, expect } from 'vitest';
import { z } from 'zod';
import { httpDispatchInit } from './httpDispatch.js';

describe('httpDispatch', () => {
    it('should match exact routes and call the handler', async () => {
        const httpDispatch = httpDispatchInit();
        let handlerCalled = false;

        const handler = (): void => {
            handlerCalled = true;
        };

        // Register the route
        httpDispatch.onMatchSync('GET', '/health', handler);

        // Test dispatch with simple method and url
        const matched = await httpDispatch.run({ method: 'GET', url: '/health' });

        expect(matched).toBe(true);
        expect(handlerCalled).toBe(true);
    });

    it('should handle number parameters with onMatchP<number> (sync)', async () => {
        const httpDispatch = httpDispatchInit();
        let result = 0;

        // Register route with typed parameter handler (sync)
        httpDispatch.onMatchP_Sync('GET', '/inc/{n}', z.coerce.number(), (n) => {
            result = n + 1;  // Extract 123, add 1 = 124
        });

        // Test dispatch with number parameter
        const matched = await httpDispatch.run({ method: 'GET', url: '/inc/123' });

        expect(matched).toBe(true);
        expect(result).toBe(124);
    });

    it('should handle string parameters with onMatchP<string> (sync)', async () => {
        const httpDispatch = httpDispatchInit();
        let result = '';

        // Register route with typed parameter handler (sync)
        httpDispatch.onMatchP_Sync('GET', '/hello/{name}', z.string(), (name) => {
            result = `Hello, ${name}!`;
        });

        // Test dispatch with string parameter
        const matched = await httpDispatch.run({ method: 'GET', url: '/hello/world' });

        expect(matched).toBe(true);
        expect(result).toBe('Hello, world!');
    });

    it('should handle number parameters with onMatchP_Async<number>', async () => {
        const httpDispatch = httpDispatchInit();
        let result = 0;

        // Register route with typed async parameter handler
        httpDispatch.onMatchP_Async('GET', '/async-inc/{n}', z.coerce.number(), async (n) => {
            // Simulate async work
            await new Promise(resolve => setTimeout(resolve, 1));
            result = n * 2;  // Extract 123, multiply by 2 = 246
        });

        // Test dispatch with number parameter
        const matched = await httpDispatch.run({ method: 'GET', url: '/async-inc/123' });

        expect(matched).toBe(true);
        expect(result).toBe(246);
    });

    it('should handle string parameters with onMatchP_Async<string>', async () => {
        const httpDispatch = httpDispatchInit();
        let result = '';

        // Register route with typed async parameter handler
        httpDispatch.onMatchP_Async('GET', '/async-greet/{name}', z.string(), async (name) => {
            // Simulate async work
            await new Promise(resolve => setTimeout(resolve, 1));
            result = `Async greeting: ${name}`;
        });

        // Test dispatch with string parameter
        const matched = await httpDispatch.run({ method: 'GET', url: '/async-greet/alice' });

        expect(matched).toBe(true);
        expect(result).toBe('Async greeting: alice');
    });

    it('should return false when no route matches', async () => {
        const httpDispatch = httpDispatchInit();

        httpDispatch.onMatchSync('GET', '/health', () => { });

        const matched = await httpDispatch.run({ method: 'GET', url: '/nonexistent' });

        expect(matched).toBe(false);
    });

    it('should handle custom type with onMatchP<CustomType>', async () => {
        type UserId = { id: number };
        const httpDispatch = httpDispatchInit();
        let result: UserId | null = null;

        // Register route with custom type parameter handler using Zod schema
        const userIdSchema = z.string().transform(s => ({ id: parseInt(s, 10) }));
        httpDispatch.onMatchP_Sync('GET', '/user/{userId}', userIdSchema, (userId) => {
            result = userId;
        });

        // Test dispatch with custom type parameter
        const matched = await httpDispatch.run({ method: 'GET', url: '/user/42' });

        expect(matched).toBe(true);
        expect(result).toEqual({ id: 42 });
    });

    it('should handle invalid number parameters gracefully', async () => {
        const httpDispatch = httpDispatchInit();

        // Using z.coerce.number() which will throw ZodError for invalid numbers
        httpDispatch.onMatchP_Sync('GET', '/api/{id}', z.coerce.number(), (_id) => {
            // This should not be called for invalid numbers
        });

        // Test with invalid number - should throw ZodError
        await expect(
            httpDispatch.run({ method: 'GET', url: '/api/invalid' })
        ).rejects.toThrow();
    });
});