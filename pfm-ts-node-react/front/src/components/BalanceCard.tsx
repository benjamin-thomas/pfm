import React from 'react';
import type { AccountBalanceRead } from '@shared/types';

type BalanceCardProps = {
  balance: AccountBalanceRead;
};

export const BalanceCard: React.FC<BalanceCardProps> = ({ balance }) => {
  const formatAmount = (cents: number): string => {
    const intPart = Math.floor(Math.abs(cents) / 100);
    const decPart = Math.abs(cents) % 100;
    const decStr = decPart < 10 ? `0${decPart}` : decPart.toString();
    const sign = cents < 0 ? '-' : '';
    return `${sign}${intPart}.${decStr} €`;
  };

  const getColorAccent = (categoryName: string): string => {
    if (categoryName === 'Assets') {
      return '#3498db';
    } else if (categoryName === 'Expenses') {
      return '#e74c3c';
    } else {
      return '#9b59b6';
    }
  };

  return (
    <div
      className="balance-card"
      style={{ borderLeftColor: getColorAccent(balance.categoryName) }}
    >
      <div className="balance-card__category">
        {balance.categoryName}
      </div>
      <div className="balance-card__account">
        {balance.accountName}
      </div>
      <div className="balance-card__amount">
        {formatAmount(balance.accountBalance)}
      </div>
    </div>
  );
};