# TP_PFONC_RSA


## Table of Contents

- [Description](#description)
- [Features](#features)
- [How to Use](#how-to-use)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Usage](#usage)
- [Examples](#examples)
- [Contributing](#contributing)
- [License](#license)

# TP-RSA Haskell Project

This Haskell project implements the RSA encryption and decryption algorithm. It allows you to encrypt and decrypt messages using the RSA algorithm with user-provided keys.

## Description

The TP-RSA project provides a Haskell implementation of the RSA encryption and decryption algorithm. It includes functions for message padding, block creation, and modular exponentiation, which are essential components of RSA encryption and decryption.

## Features

- Message encryption with RSA
- Message decryption with RSA
- Automatic prime number selection for key generation
- Padding and block creation for message processing

## How to Use

To use this Haskell code, you need to have Haskell and GHC installed on your system. Here's how to compile and run the code:

### Prerequisites

- Haskell
- GHC

### Installation

If you don't have Haskell and GHC installed, you can download them from the official Haskell website: [https://www.haskell.org/ghc/](https://www.haskell.org/ghc/)

### Usage

1. Compile the code:

   ```bash
   ghc -o TP-RSA TP-RSA.hs
   ```

2. Run the program:

   ```bash
   ./TP-RSA
   ```

### Examples

Here are some examples of using this code:

- Encrypt a message using the public key:

  ```haskell
  encrypt (e, n) block_size "Your message"
  ```

- Decrypt a message using the private key:

  ```haskell
  decrypt (d, n) block_size encrypted_message
  ```

For more usage examples, refer to the provided Haskell code.

## Contributing

If you'd like to contribute to this project or report issues, please submit a pull request or open an issue on GitHub.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.