// Type tests for httpDispatch2
// This file exists to trigger vitest typecheck mode
// It will check types in all TypeScript files, not just this one

import { httpDispatch2Init } from './httpDispatch2.js';

// Minimal usage to trigger type checking - no pointless assertions
httpDispatch2Init({
  onMatchBadParam: (_msg: string) => {},
  onMatchNotFound: (_msg: string) => {}
});