// SSE message types
type SSEMessage =
  | { kind: 'connected'; data: string }
  | { kind: 'disconnected'; data: string }
  | { kind: 'ping'; data: string }
  | { kind: 'shouldRefresh'; data: string };

export type SSEState = {
  lastMessage: SSEMessage | null;
}

const initialState: SSEState = {
  lastMessage: null,
};

// Action types (union for exhaustiveness checking)
export type SSEAction =
  | { type: 'rcvSseMessage'; payload: SSEMessage }
  | { type: 'NOOP' }; // temporary, enables exhaustiveness checking (requires more than one action type)

// Export actions as a record for clean API
export const sseActions = {
  rcvSseMessage: (payload: SSEMessage): SSEAction => ({ type: 'rcvSseMessage', payload }),
};

// Pure reducer with proper object spreading
export const sseReducer = (
  state: SSEState = initialState,
  action: SSEAction
): SSEState => {
  switch (action.type) {
    case 'rcvSseMessage':
      return {
        ...state,
        lastMessage: action.payload,
      };
    case 'NOOP':
      return state;
    default:
      const _exhaustive: never = action;
      return state;
  }
};

export type { SSEMessage };