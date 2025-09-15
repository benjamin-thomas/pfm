import { describe, it, expect } from 'vitest';
import { z } from 'zod';
import { httpDispatch2Init } from './httpDispatch2.js';

/*

npm test -- httpDispatch2
npm run test:watch -- httpDispatch2

*/

// ======================================================================
// Test Suite
// ======================================================================

describe('httpDispatch2', () => {
    // Define empty context type for tests
    type TestContext = Record<string, never>;

    it('should match exact routes and call the handler', async () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (_ctx, msg) => { badParamRef = msg; },
            onMatchNotFound: (_ctx, msg) => { notFoundRef = msg; }
        });

        dispatcher.matchP0('GET', '/health', async (_ctx) => {
            successRef = 'health check ok';
        });

        await dispatcher.run({}, 'GET', '/health');

        expect(successRef).toBe('health check ok');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });

    it('should handle number parameters with matchP1', async () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (_ctx, msg) => { badParamRef = msg; },
            onMatchNotFound: (_ctx, msg) => { notFoundRef = msg; }
        });

        dispatcher.matchP1('GET', '/inc/?', z.coerce.number(), async (_ctx, n) => {
            successRef = `result: ${n + 1}`;
        });

        await dispatcher.run({}, 'GET', '/inc/123');

        expect(successRef).toBe('result: 124');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });

    it('should handle string parameters with matchP1', async () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (_ctx, msg) => { badParamRef = msg; },
            onMatchNotFound: (_ctx, msg) => { notFoundRef = msg; }
        });

        dispatcher.matchP1('GET', '/hello/?', z.string(), async (_ctx, name) => {
            successRef = `hello, ${name}!`;
        });

        await dispatcher.run({}, 'GET', '/hello/world');

        expect(successRef).toBe('hello, world!');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });

    it('should handle matchP2 with arithmetic operations', async () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (_ctx, msg) => { badParamRef = msg; },
            onMatchNotFound: (_ctx, msg) => { notFoundRef = msg; }
        });

        dispatcher.matchP2('GET', '/add/?/?', z.coerce.number(), z.coerce.number(), async (_ctx, a, b) => {
            successRef = `sum: ${a + b}`;
        });

        await dispatcher.run({}, 'GET', '/add/15/27');

        expect(successRef).toBe('sum: 42');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });

    it('should call onMatchNotFound when no route matches', async () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (_ctx, msg) => { badParamRef = msg; },
            onMatchNotFound: (_ctx, msg) => { notFoundRef = msg; }
        });

        dispatcher.matchP0('GET', '/health', async (_ctx) => {
            successRef = 'health ok';
        });

        await dispatcher.run({}, 'GET', '/nonexistent');

        expect(successRef).toBe('');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('No route found for GET /nonexistent');
    });

    it('should call onMatchBadParam for invalid parameters', async () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (_ctx, msg) => { badParamRef = msg; },
            onMatchNotFound: (_ctx, msg) => { notFoundRef = msg; }
        });

        dispatcher.matchP1('GET', '/api/?', z.coerce.number(), async (_ctx, id) => {
            successRef = `id: ${id}`;
        });

        await dispatcher.run({}, 'GET', '/api/invalid');

        expect(successRef).toBe('');
        expect(badParamRef).toContain("Decoding error: invalid parameter 'invalid'");
        expect(notFoundRef).toBe('');
    });

    it('should call onMatchBadParam for invalid HTTP method', async () => {
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (_ctx, msg) => { badParamRef = msg; },
            onMatchNotFound: (_ctx, msg) => { notFoundRef = msg; }
        });

        await dispatcher.run({}, 'INVALID', '/health');

        expect(badParamRef).toBe("Unsupported method 'INVALID'");
        expect(notFoundRef).toBe('');
    });

    it('should handle custom type with matchP1', async () => {
        // type UserId = { id: number };
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (_ctx, msg) => { badParamRef = msg; },
            onMatchNotFound: (_ctx, msg) => { notFoundRef = msg; }
        });

        const userIdSchema = z.string().transform(s => ({ id: parseInt(s, 10) }));
        dispatcher.matchP1('GET', '/user/?', userIdSchema, async (_ctx, userId) => {
            successRef = `user id: ${userId.id}`;
        });

        await dispatcher.run({}, 'GET', '/user/42');

        expect(successRef).toBe('user id: 42');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });

    it('should handle route priority - exact before wildcard', async () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (_ctx, msg) => { badParamRef = msg; },
            onMatchNotFound: (_ctx, msg) => { notFoundRef = msg; }
        });

        dispatcher.matchP0('GET', '/api/health', async (_ctx) => {
            successRef = 'exact route called';
        });

        dispatcher.matchP1('GET', '/api/?', z.string(), async (_ctx) => {
            successRef = 'wildcard route called';
        });

        await dispatcher.run({}, 'GET', '/api/health');

        expect(successRef).toBe('exact route called');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });

    it('should handle method mismatch', async () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (_ctx, msg) => { badParamRef = msg; },
            onMatchNotFound: (_ctx, msg) => { notFoundRef = msg; }
        });

        dispatcher.matchP0('GET', '/health', async (_ctx) => {
            successRef = 'health ok';
        });

        await dispatcher.run({}, 'POST', '/health');

        expect(successRef).toBe('');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('No route found for POST /health');
    });

    it('should handle P2 with partial decoding failures', async () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (_ctx, msg) => { badParamRef = msg; },
            onMatchNotFound: (_ctx, msg) => { notFoundRef = msg; }
        });

        dispatcher.matchP2('GET', '/add/?/?', z.coerce.number(), z.coerce.number(), async (_ctx, a, b) => {
            successRef = `sum: ${a + b}`;
        });

        // First parameter invalid
        await dispatcher.run({}, 'GET', '/add/invalid/2');

        expect(successRef).toBe('');
        expect(badParamRef).toContain("Decoding error: invalid parameters 'invalid', '2'");
        expect(notFoundRef).toBe('');
    });

    it('should handle routes with different HTTP methods', async () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (_ctx, msg) => { badParamRef = msg; },
            onMatchNotFound: (_ctx, msg) => { notFoundRef = msg; }
        });

        dispatcher.matchP0('POST', '/api/users', async (_ctx) => {
            successRef = 'created user';
        });

        dispatcher.matchP1('DELETE', '/api/users/?', z.coerce.number(), async (_ctx, id) => {
            successRef = `deleted user ${id}`;
        });

        // Test POST
        await dispatcher.run({}, 'POST', '/api/users');
        expect(successRef).toBe('created user');

        // Reset and test DELETE
        successRef = '';
        badParamRef = '';
        notFoundRef = '';
        await dispatcher.run({}, 'DELETE', '/api/users/1');
        expect(successRef).toBe('deleted user 1');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });
});