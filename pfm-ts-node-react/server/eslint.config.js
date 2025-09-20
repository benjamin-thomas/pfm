import tseslint from 'typescript-eslint';

export default [
  {
    ignores: ['dist/', 'node_modules/', 'coverage/'],
  },
  ...tseslint.configs.recommended,
  ...tseslint.configs.strict,
  {
    files: ['src/**/*.{ts,js}'],
    languageOptions: {
      ecmaVersion: 2022,
      parser: tseslint.parser,
      parserOptions: {
        project: true,
        tsconfigRootDir: import.meta.dirname,
      },
    },
    rules: {
      'semi': ["error", "always"],
      '@typescript-eslint/no-use-before-define': 'error',
      '@typescript-eslint/explicit-function-return-type': [
        'error',
        {
          allowExpressions: true,
          allowTypedFunctionExpressions: true,
        },
      ],
      '@typescript-eslint/no-explicit-any': 'error',
      '@typescript-eslint/no-unused-vars': [
        'warn',
        {
          argsIgnorePattern: '^_',
          varsIgnorePattern: '^_',
        },
      ],
      '@typescript-eslint/no-deprecated': 'warn',
    },
  },
];