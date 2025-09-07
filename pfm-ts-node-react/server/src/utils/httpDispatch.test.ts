import { describe, it, expect } from 'vitest';
import { z } from 'zod';
import { httpDispatchInit } from './httpDispatch.js';

describe('httpDispatch', () => {
    it('should match exact routes and call the handler', async () => {
        const httpDispatch = httpDispatchInit();
        let handlerCalled = false;

        const handler = async (): Promise<void> => {
            handlerCalled = true;
        };

        // Register the route
        httpDispatch.matchP0('GET', '/health', handler);

        // Test dispatch with simple method and url
        const matched = await httpDispatch.run({ method: 'GET', url: '/health' });

        expect(matched).toBe(true);
        expect(handlerCalled).toBe(true);
    });

    it('should handle number parameters with onMatchP<number> (sync)', async () => {
        const httpDispatch = httpDispatchInit();
        let result = 0;

        // Register route with typed parameter handler
        httpDispatch.matchP1('GET', '/inc/?', z.coerce.number(), async (n) => {
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

        // Register route with typed parameter handler
        httpDispatch.matchP1('GET', '/hello/?', z.string(), async (name) => {
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
        httpDispatch.matchP1('GET', '/async-inc/?', z.coerce.number(), async (n) => {
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
        httpDispatch.matchP1('GET', '/async-greet/?', z.string(), async (name) => {
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

        httpDispatch.matchP0('GET', '/health', async () => { });

        const matched = await httpDispatch.run({ method: 'GET', url: '/nonexistent' });

        expect(matched).toBe(false);
    });

    it('should handle custom type with onMatchP<CustomType>', async () => {
        type UserId = { id: number };
        const httpDispatch = httpDispatchInit();
        let result: UserId | null = null;

        // Register route with custom type parameter handler using Zod schema
        const userIdSchema = z.string().transform(s => ({ id: parseInt(s, 10) }));
        httpDispatch.matchP1('GET', '/user/?', userIdSchema, async (userId) => {
            result = userId;
        });

        // Test dispatch with custom type parameter
        const matched = await httpDispatch.run({ method: 'GET', url: '/user/42' });

        expect(matched).toBe(true);
        expect(result).toEqual({ id: 42 });
    });

    it('should handle matchP2 with arithmetic operations', async () => {
        const httpDispatch = httpDispatchInit();
        let addResult = 0;
        let mulResult = 0;

        // Register route with 2 parameters for addition
        httpDispatch.matchP2('GET', '/add/?/?', z.coerce.number(), z.coerce.number(), async (a, b) => {
            addResult = a + b;
        });

        // Register route with 2 parameters for multiplication  
        httpDispatch.matchP2('GET', '/mul/?/?', z.coerce.number(), z.coerce.number(), async (a, b) => {
            mulResult = a * b;
        });

        // Test addition: 15 + 27 = 42
        const addMatched = await httpDispatch.run({ method: 'GET', url: '/add/15/27' });
        expect(addMatched).toBe(true);
        expect(addResult).toBe(42);

        // Test multiplication: 6 * 7 = 42
        const mulMatched = await httpDispatch.run({ method: 'GET', url: '/mul/6/7' });
        expect(mulMatched).toBe(true);
        expect(mulResult).toBe(42);
    });

    it('should handle invalid number parameters gracefully', async () => {
        const httpDispatch = httpDispatchInit();

        // Using z.coerce.number() which will throw ZodError for invalid numbers
        httpDispatch.matchP1('GET', '/api/?', z.coerce.number(), async (_id) => {
            // This should not be called for invalid numbers
        });

        // Test with invalid number - should throw ZodError
        await expect(
            httpDispatch.run({ method: 'GET', url: '/api/invalid' })
        ).rejects.toThrow();
    });
});