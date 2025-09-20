// Theme state - simple UI preferences
export type ThemeState = {
  isDarkMode: boolean;
}

const initialState: ThemeState = {
  isDarkMode: true,
};

// Action types - civilized naming (union for exhaustiveness checking)
export type ThemeAction =
  | { type: 'clickedThemeToggle' }
  | { type: 'NOOP' }; // temporary, enables exhaustiveness checking (requires more than one action type)

// Export actions as a record for clean API
export const themeActions = {
  clickedThemeToggle: { type: 'clickedThemeToggle' } as ThemeAction,
};

// Pure reducer with proper object spreading
export const themeReducer = (
  state: ThemeState = initialState,
  action: ThemeAction
): ThemeState => {
  switch (action.type) {
    case 'clickedThemeToggle':
      return {
        ...state,
        isDarkMode: !state.isDarkMode,
      };
    case 'NOOP':
      return state;
    default:
      const _exhaustive: never = action;
      return state;
  }
};