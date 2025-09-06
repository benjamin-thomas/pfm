import React from 'react';
import type { AccountBalanceRead } from '@shared/types';
import { BalanceCard } from './BalanceCard';

type BalanceCardsProps = {
  balances: AccountBalanceRead[];
};

export const BalanceCards: React.FC<BalanceCardsProps> = ({ balances }) => {
  return (
    <div>
      <h2 className="section-title">Balances</h2>
      <div className="balances">
        {balances.map((balance) => (
          <BalanceCard key={balance.accountId} balance={balance} />
        ))}
      </div>
    </div>
  );
};