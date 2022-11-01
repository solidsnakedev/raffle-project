# Cardano Raffle

<img src="https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fimages.squarespace-cdn.com%2Fcontent%2Fv1%2F5a6e8cce7131a536de9f9b2d%2F1561088112853-LT4ODOADSZIG172GO892%2Fke17ZwdGBToddI8pDm48kIERNFmC6VNA8t3456dw3aJZw-zPPgdn4jUwVcJE1ZvWQUxwkmyExglNqGp0IvTJZamWLI2zvYWH8K3-s_4yszcp2ryTI0HqTOaaUohrI8PI8v_75RFJCfuzG5xvdM2vETzcuGv7TKDXg0OX_ZfiGhI%2FRaffle%2BTickets.png&f=1&nofb=1&ipt=494c12ee885b076f340c7850041d62d46c4e3b03abbacde7155652d931e6f96d&ipo=images" width="80">

## Introduction

A raffle is a game of chance in which each player purchases one or more tickets, each of which is assigned a unique number. The winner is determined by a random drawing of numbers.
Raffles are a popular form of gambling, and they are often used to raise funds for charitable causes. They are also used as promotional tools by businesses, in which the prize is usually a product or service offered by the business.


## Raffle in Cardano

Due to the deterministic nature of the `Extended UTXO Model` (EUTXO) it is hard to generate a random number and/or to trust an entity to provide such value. the following solutions are proposed in order to find a simple and practical implementation in Cardano.

## Solutions

### 1) Secret-Reveal-Draw method

Users generate a hashed secret number and store it in a list
```math
secret \quad numbers =
\begin{bmatrix}
    s_0 \\
    s_1 \\
    \vdots \\
    s_i
\end{bmatrix}
```

$$\left( \sum_{k=1}^n a_k b_k \right)^2 \leq \left( \sum_{k=1}^n a_k^2 \right) \left( \sum_{k=1}^n b_k^2 \right)$$


where :

$s$ = hashed secret number

After some deadline all the user should reveal their numbers and store it in a new list, then a computation like the modulo of the sum of all revealed numbers can be made to select the winner.

$$
revealed \quad numbers =
\begin{bmatrix}
    r_0 \\
    r_1 \\
    \vdots \\
    r_i
\end{bmatrix}
$$
$$
random \quad number =
\begin{equation}
\left(\sum_{i=0}^{j} r_i\right) \bmod k
\end{equation}
$$
where:

$j$ = array length

$i$ = array index

$k$ = number of participants that reveal the secret number

This process can be divided in 3 periods

#### Secret Period

Every user commits to:

 1. send funds to pay for the ticket
 2. generate a random number, this number is then hashed with an algorithm like sha256
 3. save the hashed number in the list of secret numbers.

#### Reveal Period

At certain point all users are encourage to reveal the secret number so they can get the prize.
Every user can :

 1. reveal the secret number if this one match the number is the secret list.

#### Draw Winner

Once the reveal period ends, just the users who reveal the secret number can participate and chose an algorithm to select the winner within the list with the collection of all the random numbers.
One solution could be using the modulo
where
    [a] = list of user that reveal the number
    [n] = collection of reveal numbers
     w =  winner
     i = index of list of user
     w = sum[n] % [a]

#### Conclusion

Although this approach looks feasible and random, this can be quite expensive and slow, due to the fact that every user must reveal the secret number with a new transaction.

### 2) Oracle

An oracle is a good way to delegate the responsibility of drawing a random number, therefore there must be a verifiable mechanism to trust a third party provider to prove the number is randomly generated.
A decent solution could be having a oracle script using the VRF function to generate a random number and at the same time the player can prove this number was correctly generated.
More on this. [link to VRF](https://en.wikipedia.org/wiki/Verifiable_random_function)

Conclusion:

Currently there's no such a thing like VRF function in plutus, therefore you can not build this solution yet.

### 3) Minimum Hash Method

Why a Hash?
A hash is a math function that takes an input with arbitrary length, and returns a encrypted output with a fixed length, a hash function like SHA256 returns a fixed 32 byte length output.
This function can be used to compute the minimum hash in a list of arbitrary inputs.
Why minimum Hash?
sha256 function returns a unique value of 32 byte fixed length, therefore this value can be used to compare it with others

In order to calculate the min hash, participants should commit to use an `immutable value`, a `mutable value` and an `unique value`.

- There are values shared with all participants
  - raffle seed (`immutable value`)
  - sold tickets (`mutable value`)
- There is one value coming from the participant
  - token name (`unique value`)

#### Function to compute minimum hash

$$
\begin{equation}
\text{minimum hash =}
Hash[tn [st \quad rs]]
\end{equation}
$$
where

$\text{tn = token name}$

$\text{st = sold tickets}$

$\text{rs = random seed}$

Plutus representation

```haskell
sha2_256 (appendByteString ticketName $ consByteString soldTickets randomSeed)
```

Conclusion :
There is a limited period where every user can individually calculate and claim to have the minimum hash, this considerably reduce the amount of interactions with the script as the computation happens off-chain.

## Cardano Raffle - Minimum hash method

This method can be divided in 4 Periods

### Start Raffle
Datum is used as a shared state of the game where :

- ticket price : the value in ADA the user has to paid to get a ticket
- random seed : random bytestring used to compute the minimum hash
- max ticket : maximum amount of ticket to be sold
- sold ticket : mutable value 
- minimum hash : value inserted by the user claiming to have the min hash
- tickets : list of sold tickets 
- lottery intervals : intervals to delimit buy-claim-close period

The raffle initiator commits to:

1. Set the ticket price in Lovelace
2. Set the maximum amount of tickets to be sold
3. Set an initial random seed to be used in the minimum hash calculation
4. Set the minimum hash to be empty
5. Set ticket list to be empty
6. Set the intervals for the Buy | Claim | Close period

### Buy Period

Every user commits to :

1. Send funds to pay for the ticket
2. Use the consumed utxo to generate a unique ticket name
3. Mint only 1 ticket
4. Keep the initial raffle settings

#### Function to mint an unique token name


The minting ticket policy force the user to pick the spent utxo, and hash it to get an unique token name

```
sha2_256 (consByteString (txOutRefIdx utxo) ((getTxId . txOutRefId) utxo))
```

### Claim Period

Once the buy period ends the user can claim to have the minimumhash by using the following parameters:

1. random seed
2. sold tickets
3. ticket name

> Due to the deterministic nature of the eUTxo model, the only value that can not be deterministic is the number of sold tickets.

if an user claims to have the minimum hash value, this one is allow to update the datum field of minimumhash.
if another user claim to have the minimum hash of the previous user, this one can update the datum field of minimumhash.

### Close Period

Just the user holding the token that computes the minimum hash can consumed the funds from the script

```
cabal run raffle-project e1c0c4132b498ae79762c7df068834b7e2b4841e6ce934084a1ecf94dccbff97#0 raffle
```

## Front End - NextJS

### Install nvm

```
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.2/install.sh | bash
```

[source](https://github.com/nvm-sh/nvm#install--update-script)

## Install Node and NPM

```
nvm install --lts
```

## Go to front-end folder

```
npm install
```

```
npm run dev
```
