**0.6.0**

* The DEX's Order Match transaction has been changed. This is the main reason for restarting Testnet. Now, a second asset of transaction's pair is used to set an amount of the transaction.
* LPOS was implemented. New Leasing and Leasing Cancel transactions were added.
* New, HOCON based, configuration file. Old configuration file (JSON based) is supported in this release for backward compatibility. Automatic configuration file conversion added to DEB packages.

**0.3.2**

* By default walletDir and dataDir located in $HOME/mir

**0.3.1**

* HTTP API /scorex removed. Use /node instead.

**0.2.2**

* Switch network by "testnet" in settings. Default value is true."
* /scorex/* HTTP API deprecated. Use /node/* instead.
* All logs goes to stdout and stderr. Use "loggingLevel" in config. 

**0.2.1**

* peers.dat format changed. Delete old version.
* Different HTTP status codes in replies in HTTP API were implemented
* Mir' Scorex v1.3.2

**0.2.0**

* Peers blacklist ttl configuration via "p2p"/"blacklistResidenceTimeMilliseconds"
* Upgrade to Mir' Scorex v1.3.1

**0.2.0-RC7**

* New API /mir/payment returns senderPublicKey
* New API /mir/create-signed-payment
* /mir/external-payment deprecated. 
  Use new /mir/broadcast-signed-payment.
* New API /mir/payment/signature
* minimumTxFee verification for API

**0.2.0-RC5**

* /mir/external-payment returns error for incorrect recipient

**0.2.0-RC4**

* Fixed issue with incorrect Handshake
* Balance with confirmations is the minimum balance
* /mir/external-payment returns error if account balance invalid
* New API method /consensus/generatingbalance/{address}

**0.2.0-RC3**

* Incompatible with 0.1.3
* Upgrade to Scorex 1.2.8
* New Address format
* New hash chain for Address - Blake2b256, Keccak32
* New Testnet Genesis

**0.1.3**

* Upgrade to Scorex 1.2.6.
* New http api method /external-payment for lite client

**0.1.2**

* Upgrade to Scorex 1.2.4. Clean /scorex/mir/data/ before run.
