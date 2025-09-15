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
    // Define context type for tests - execution functions
    type TestContext = () => void;
    type TestContextAsync = () => Promise<void>;

    it('should match exact routes and call the handler', () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (msg) => () => { badParamRef = msg; },
            onMatchNotFound: (msg) => () => { notFoundRef = msg; }
        });

        /*
        First thunk means no extracted request params (because we use P0)
        Second thunk is the execution context (empty here, but could just as well be a promise)
        */
        dispatcher.matchP0('GET', '/health', () => {
            successRef = 'health check ok';
        });

        dispatcher.run('GET', '/health', (execCtx) => execCtx());

        expect(successRef).toBe('health check ok');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });

    it('should handle number parameters with matchP1', () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (msg) => () => { badParamRef = msg; },
            onMatchNotFound: (msg) => () => { notFoundRef = msg; }
        });

        dispatcher.matchP1('GET', '/inc/?', z.coerce.number(), (n) => () => {
            successRef = `result: ${n + 1}`;
        });

        dispatcher.run('GET', '/inc/123', (executionContext) => executionContext());

        expect(successRef).toBe('result: 124');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });

    it('should handle string parameters with matchP1', () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (msg) => () => { badParamRef = msg; },
            onMatchNotFound: (msg) => () => { notFoundRef = msg; }
        });

        dispatcher.matchP1('GET', '/hello/?', z.string(), (name) => () => {
            successRef = `hello, ${name}!`;
        });

        dispatcher.run('GET', '/hello/world', (execCtx) => execCtx());

        expect(successRef).toBe('hello, world!');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });

    it('should handle matchP2 with arithmetic operations', () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (msg) => () => { badParamRef = msg; },
            onMatchNotFound: (msg) => () => { notFoundRef = msg; }
        });

        dispatcher.matchP2('GET', '/add/?/?', z.coerce.number(), z.coerce.number(), (a, b) => () => {
            successRef = `sum: ${a + b}`;
        });

        dispatcher.run('GET', '/add/15/27', (executionContext) => executionContext());

        expect(successRef).toBe('sum: 42');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });

    it('should handle matchP2 with arithmetic operations ASYNC', async () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContextAsync>({
            onMatchBadParam: (msg) => async () => { badParamRef = msg; },
            onMatchNotFound: (msg) => async () => { notFoundRef = msg; }
        });

        dispatcher.matchP2('GET', '/add/?/?', z.coerce.number(), z.coerce.number(), (a, b) => async () => {
            successRef = `sum: ${a + b}`;
        });

        dispatcher.run('GET', '/add/15/27', (executionContext) => executionContext());

        expect(successRef).toBe('sum: 42');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });

    it('should call onMatchNotFound when no route matches', () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (msg) => () => { badParamRef = msg; },
            onMatchNotFound: (msg) => () => { notFoundRef = msg; }
        });

        dispatcher.matchP0('GET', '/health', () => {
            successRef = 'health ok';
        });

        dispatcher.run('GET', '/nonexistent', (executionContext) => executionContext());

        expect(successRef).toBe('');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('No route found for GET /nonexistent');
    });

    it('should call onMatchBadParam for invalid parameters', () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (msg) => () => { badParamRef = msg; },
            onMatchNotFound: (msg) => () => { notFoundRef = msg; }
        });

        dispatcher.matchP1('GET', '/api/?', z.coerce.number(), (id) => () => {
            successRef = `id: ${id}`;
        });

        dispatcher.run('GET', '/api/invalid', (executionContext) => executionContext());

        expect(successRef).toBe('');
        expect(badParamRef).toContain("Decoding error: invalid parameter 'invalid'");
        expect(notFoundRef).toBe('');
    });

    it('should call onMatchBadParam for invalid HTTP method', () => {
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (msg) => () => { badParamRef = msg; },
            onMatchNotFound: (msg) => () => { notFoundRef = msg; }
        });

        dispatcher.run('INVALID', '/health', (executionContext) => executionContext());

        expect(badParamRef).toBe("Unsupported method 'INVALID'");
        expect(notFoundRef).toBe('');
    });

    it('should handle custom type with matchP1', () => {
        // type UserId = { id: number };
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (msg) => () => { badParamRef = msg; },
            onMatchNotFound: (msg) => () => { notFoundRef = msg; }
        });

        const userIdSchema = z.string().transform(s => ({ id: parseInt(s, 10) }));
        dispatcher.matchP1('GET', '/user/?', userIdSchema, (userId) => () => {
            successRef = `user id: ${userId.id}`;
        });

        dispatcher.run('GET', '/user/42', (executionContext) => executionContext());

        expect(successRef).toBe('user id: 42');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });

    it('should handle route priority - exact before wildcard', () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (msg) => () => { badParamRef = msg; },
            onMatchNotFound: (msg) => () => { notFoundRef = msg; }
        });

        dispatcher.matchP0('GET', '/api/health', () => {
            successRef = 'exact route called';
        });

        dispatcher.matchP1('GET', '/api/?', z.string(), (_param) => () => {
            successRef = 'wildcard route called';
        });

        dispatcher.run('GET', '/api/health', (executionContext) => executionContext());

        expect(successRef).toBe('exact route called');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });

    it('should handle method mismatch', () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (msg) => () => { badParamRef = msg; },
            onMatchNotFound: (msg) => () => { notFoundRef = msg; }
        });

        dispatcher.matchP0('GET', '/health', () => {
            successRef = 'health ok';
        });

        dispatcher.run('POST', '/health', (executionContext) => executionContext());

        expect(successRef).toBe('');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('No route found for POST /health');
    });

    it('should handle P2 with partial decoding failures', () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (msg) => () => { badParamRef = msg; },
            onMatchNotFound: (msg) => () => { notFoundRef = msg; }
        });

        dispatcher.matchP2('GET', '/add/?/?', z.coerce.number(), z.coerce.number(), (a, b) => () => {
            successRef = `sum: ${a + b}`;
        });

        // First parameter invalid
        dispatcher.run('GET', '/add/invalid/2', (executionContext) => executionContext());

        expect(successRef).toBe('');
        expect(badParamRef).toContain("Decoding error: invalid parameters 'invalid', '2'");
        expect(notFoundRef).toBe('');
    });

    it('should handle routes with different HTTP methods', () => {
        let successRef = '';
        let badParamRef = '';
        let notFoundRef = '';

        const dispatcher = httpDispatch2Init<TestContext>({
            onMatchBadParam: (msg) => () => { badParamRef = msg; },
            onMatchNotFound: (msg) => () => { notFoundRef = msg; }
        });

        dispatcher.matchP0('POST', '/api/users', () => {
            successRef = 'created user';
        });

        dispatcher.matchP1('DELETE', '/api/users/?', z.coerce.number(), (id) => () => {
            successRef = `deleted user ${id}`;
        });

        // Test POST
        dispatcher.run('POST', '/api/users', (executionContext) => executionContext());
        expect(successRef).toBe('created user');

        // Reset and test DELETE
        successRef = '';
        badParamRef = '';
        notFoundRef = '';
        dispatcher.run('DELETE', '/api/users/1', (executionContext) => executionContext());
        expect(successRef).toBe('deleted user 1');
        expect(badParamRef).toBe('');
        expect(notFoundRef).toBe('');
    });
});