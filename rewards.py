from typing import List, Optional, Tuple, Dict
from decimal import *
from collections import OrderedDict
from dataclasses import dataclass, field
import string
from datetime import datetime
import argparse
import uvicorn
from fastapi import FastAPI
import numpy as np
from starlette.responses import HTMLResponse, Response
import requests


# UTILS

class SuperFormatter(string.Formatter):
    """World's simplest Template engine."""

    def format_field(self, value, spec):
        if spec.startswith('repeat'):
            template = spec.partition(':')[-1]
            if type(value) is dict:
                value = value.items()
            return ''.join([self.format(template, item=item) for item in value])
        elif spec == 'call':
            return value()
        elif spec.startswith('if'):
            return (value and spec.partition(':')[-1]) or ''
        else:
            return super(SuperFormatter, self).format_field(value, spec)


# TEMPLATE

html = '''
<html>
<head>
<style>
@import url('https://fonts.googleapis.com/css2?family=Source+Code+Pro&display=swap');
* {{
    box-sizing: border-box;
}}
body {{
    font-family: 'Source Code Pro', monospace;
    padding: 10px;
    margin:30px;
    text-align:center;
}}
a {{
    color: black;
    text-decoration: none;
}}
a:hover {{
    color: red;
}}
table {{ 
    margin-left: auto;
    margin-right: auto;
    border-collapse:separate; 
    border-spacing: 40px 10px;
}}
td  {{
    
}}
.left {{
    text-align: left;  
}}
.right {{
    text-align: right;
}}
</style>
</head>
<body>
    <table>
        <tr>
            <th class="left">Rank</th>
            <th class="left">Address</th>
            {holders.pool_names:repeat:
            <th class="right">{{item}}</th>
            }
            <th class="right">Alphadrop</th>
        </tr>
        {holders.holders:repeat:<tr>
            <td class="left">{{item.rank}}</td>
            <td class="left">
                <a href="https://etherscan.io/token/0x7cdc560cc66126a5eb721e444abc30eb85408f7a?a={{item.address}}" target="_blank">
                    {{item.address}}
                </a>
            </td>
            {{item.pool_shares:repeat:
            <td class="right">{{{{item:.4f}}}} %</td>
            }}
            <td class="right">{{item.alphadrop_share:.4f}} %</td>
        </tr>}
    </table>
    <br/><br/>
    <a href="/export-csv">Export as CSV</a>
    <br/><br/>
    Last updated: {holders.last_updated}
    <br/><br/>
</body>
</html>
'''


@dataclass
class Holder:
    address: str
    pool_shares: List[float]
    alphadrop_share: float
    rank: int = 0


@dataclass
class Transaction:
    address: str
    timestamp: datetime
    value: Decimal


@dataclass
class Pool:
    address: str
    name: str
    transactions: List[Transaction] = field(default_factory=list)
    average_daily_shares: Dict[str, float] = field(default_factory=dict)
    share: float = 0.0
    addresses: List[str] = field(default_factory=list)
    balances: np.ndarray = None


class HolderProvider:
    def __init__(self, init_pools):
        self._holders = None
        self._last_updated = None  # date
        self._init_pools = init_pools

    @property
    def last_updated(self):
        return self._last_updated

    @property
    def pool_names(self):
        return [name for address, name in self._init_pools]

    @property
    def holders(self):
        if self._holders is None or self._last_updated < datetime.utcnow().date():
            self._load_and_calculate()
        return self._holders

    def _load_and_calculate(self):
        loaded_pools = load_pools([Pool(*p) for p in self._init_pools])
        self._holders = calculate_alphadrop_shares(loaded_pools)
        self._last_updated = datetime.utcnow().date()


zero_address = '0x0000000000000000000000000000000000000000'
days = 60
min_dgvc_share = 0.5
max_alphadrop_share = 0.2
etherscan_api_key = '1TP4S4C6I5SD3SDH9G5S1QUN7IVXXY7WBX'

# first address needs to be DGVC
pools = [('0x7cdc560cc66126a5eb721e444abc30eb85408f7a', 'DGVC'),
         ('0x3633259d244b2f70e77118aa9132b45f9f6ab4d9', 'RAUX'),
         ('0x54965801946d768b395864019903aef8b5b63bb3', 'EYE')]


def extract_transactions(transactions: List[dict]) -> List[Transaction]:
    transaction = transactions[0]
    from_ = transaction['from']
    to = transactions[-1]['to']

    # TODO additional check of order transactions

    def extract_value_timestamp(transaction: dict):
        value = Decimal(transaction['value']) / (10 ** Decimal(transaction['tokenDecimal']))
        timestamp = datetime.fromtimestamp(int(transaction['timeStamp']))
        return value, timestamp

    def extract_token_transfer_transaction(transaction: dict) -> List[Transaction]:
        value, timestamp = extract_value_timestamp(transaction)
        return [
            Transaction(address=transaction['from'], value=-1 * value, timestamp=timestamp),
            Transaction(address=transaction['to'], value=value, timestamp=timestamp),
        ]

    value, timestamp = extract_value_timestamp(transaction)

    if from_ == zero_address:
        # add to pool
        transactions = [Transaction(address=to, value=value, timestamp=timestamp)]
    elif to == zero_address:
        # remove from pool
        transactions = [Transaction(address=from_, value=-1 * value, timestamp=timestamp)]
    else:
        # token transfer
        transactions = [unrolled_transaction
                        for transaction in transactions
                        for unrolled_transaction in extract_token_transfer_transaction(transaction)]

    return transactions


def load_transactions(address):
    url = f"https://api.etherscan.io/api?module=account&action=tokentx&contractaddress={address}" \
          + f"&sort=asc&apikey={etherscan_api_key}"

    transactions = requests.get(url).json()['result']
    grouped_transactions = OrderedDict()

    for transaction in transactions:
        tx_hash = transaction['hash']
        if tx_hash not in grouped_transactions:
            grouped_transactions[tx_hash] = []
        grouped_transactions[tx_hash].append(transaction)

    return [t for transaction_group in grouped_transactions.values() for t in extract_transactions(transaction_group)]


def assign_timestamp_to_day(last_snapshot: datetime, timestamp: datetime) -> Optional[int]:
    time_delta = last_snapshot - timestamp
    if time_delta.days < 0:
        return None

    index = min(days - 1, time_delta.days)
    return index


def group_transactions_by_day(transactions: List[Transaction], last_snapshot: datetime):
    transactions_with_day = [(day, t) for day, t in
                             ((assign_timestamp_to_day(last_snapshot, transaction.timestamp), transaction)
                              for transaction in transactions) if day is not None]

    # day zero is the day before the snapshot
    grouped = [list() for _ in range(0, days)]

    for day, transaction in transactions_with_day:
        grouped[day].append(transaction)

    return list(enumerate(grouped))


def replay_transactions(transactions: List[Transaction], last_snapshot: datetime) -> Tuple[List[str], np.ndarray]:
    addresses = sorted({transaction.address for transaction in transactions})
    indexes = {address: index for index, address in enumerate(addresses)}

    balances = np.zeros((len(indexes), days), dtype=np.float64)
    accumulated_balances = {address: 0 for address in addresses}

    grouped_transactions = group_transactions_by_day(transactions, last_snapshot)

    # day zero is the day before the snapshot
    for day, transactions in reversed(grouped_transactions):
        for transaction in transactions:
            accumulated_balances[transaction.address] += transaction.value
        for address, index in indexes.items():
            balances[index, day] = accumulated_balances[address]

    return addresses, balances


def limit_balances(balances: np.ndarray):
    rows, cols = balances.shape
    for row in range(0, rows):
        min_value = balances[row, 0]
        for col in range(1, cols):
            min_value = min(min_value, balances[row, col])
            balances[row, col] = min_value


def calculate_average_daily_shares(addresses: List[str], balances: np.ndarray) -> Dict[str, float]:
    daily_shares = np.divide(balances, balances.sum(axis=0, keepdims=True),
                             out=np.zeros_like(balances), where=balances != 0)
    average_daily_shares = np.average(daily_shares, axis=1)
    normalized = average_daily_shares/average_daily_shares.sum()
    return {address: share for address, share in zip(addresses, normalized)}


def calculate_pool_shares(pool_count: int) -> Tuple[float, float]:
    count_alphadrops = pool_count - 1
    alphadrop_share = 0 if count_alphadrops == 0 else min(max_alphadrop_share, (1 - min_dgvc_share)/count_alphadrops)
    dgvc_share = 1 - alphadrop_share * count_alphadrops

    return dgvc_share, alphadrop_share


def load_pools(pools: List[Pool]):
    now = datetime.utcnow()
    last_snapshot = datetime(now.year, now.month, now.day)

    for pool in pools:
        pool.transactions = load_transactions(pool.address)

    # extract all addresses from DGVC pool
    main_addresses = {t.address for t in pools[0].transactions}

    for pool in pools:
        # allow only transactions from wallets also pooling DGVC
        pool.transactions = [t for t in pool.transactions if t.address in main_addresses]
        addresses, balances = replay_transactions(pool.transactions, last_snapshot)
        limit_balances(balances)
        pool.addresses = addresses
        pool.balances = balances

    dgvc_pool = pools[0]
    purge_addresses = {address for address, balance in zip(dgvc_pool.addresses, dgvc_pool.balances[:, 0]) if balance == 0}

    for pool in pools:
        address_indexes = {address: index for index, address in enumerate(pool.addresses)}
        for purge_address in purge_addresses:
            row_index = address_indexes.get(purge_address)
            if row_index:
                pool.balances[row_index, :] = 0
        pool.average_daily_shares = calculate_average_daily_shares(pool.addresses, pool.balances)

    return pools


def calculate_alphadrop_shares(pools: List[Pool]):
    dgvc_pool = pools[0]
    addresses = list(dgvc_pool.average_daily_shares.keys())
    address_indexes = {address: index for index, address in enumerate(addresses)}

    shares = np.zeros((len(address_indexes), len(pools) + 1), dtype=np.float64)

    for pool_index, pool in enumerate(pools):
        for wallet, share in pool.average_daily_shares.items():
            shares[address_indexes[wallet], pool_index] = share

    dgvc_share, alphadrop_share = calculate_pool_shares(len(pools))
    shares[:, -1] = dgvc_share * shares[:, 0] + np.sum([alphadrop_share * shares[:, i] for i in range(1, len(pools))], axis=0)

    rounded_shares = np.around(shares * 100, 4)

    holders = [Holder(address=addresses[index], pool_shares=pool_shares[:-1],
                      alphadrop_share=pool_shares[-1])
               for index, pool_shares in enumerate(rounded_shares.tolist())]

    holders = sorted(holders, key=lambda holder: holder.alphadrop_share, reverse=True)

    for index, holder in enumerate(holders):
        holder.rank = index + 1

    return holders


def generate_csv(holders: HolderProvider) -> str:
    lines = [','.join(['rank', 'address', *holders.pool_names, 'alphadrop'])]
    lines.extend([','.join([str(holder.rank), holder.address, *[f"{s:.4f}" for s in holder.pool_shares], f"{holder.alphadrop_share:.4f}"])
                  for holder in holders.holders])
    return '\n'.join(lines)


holder_provider = HolderProvider(pools)
app = FastAPI()


@app.get("/", response_class=HTMLResponse)
def root():
    return SuperFormatter().format(html, holders=holder_provider)


@app.get("/export-csv")
def csv_export():
    return Response(content=generate_csv(holder_provider), media_type='text/csv',
                    headers={'Content-Disposition': 'attachment; filename="export-holder-wallets.csv"'})


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Calculating Degen Rewards')
    parser.add_argument('--web', dest='mode', action='store_const', const='web',
                        default='csv', help='Launch web dashboard.')
    parser.add_argument('--host', type=str, default='127.0.0.1',
                        help='Host interface of web dashboard (host=127.0.0.1)')
    parser.add_argument('--port', type=int, default=8383,
                        help='Port of web dashboard (port=8383)')
    parser.add_argument('output_file', type=str, default='export-holder-wallets.csv', nargs='?',
                        help='Output file name of the exported CSV (output_file=export-holder-wallets.csv')

    args = parser.parse_args()

    if args.mode == 'csv':
        with open(args.output_file, 'w') as f:
            f.write(generate_csv(holder_provider))
            print(f"Exported alphashares of holders as CSV to {args.output_file}")

    if args.mode == 'web':
        uvicorn.run(app, host=args.host, port=args.port)
