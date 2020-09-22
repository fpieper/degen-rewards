# degen-rewards

### Setup instructions

````
git clone https://github.com/fpieper/degen-rewards.git
pip3 install -r requirements.txt
python3 rewards.py --web
````

### Etherscan API calls

https://api.etherscan.io/api?module=account&action=tokentx&contractaddress=0x7cdc560cc66126a5eb721e444abc30eb85408f7a&sort=asc&apikey=1TP4S4C6I5SD3SDH9G5S1QUN7IVXXY7WBX&page=1&offset=10000

https://api.etherscan.io/api?module=proxy&action=eth_blockNumber&apikey=1TP4S4C6I5SD3SDH9G5S1QUN7IVXXY7WBX