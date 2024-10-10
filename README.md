# ElGamalCipher 🔐✉️

The **ElGamalCipher** program implements the ElGamal encryption algorithm 🔒, a public-key cryptographic system that provides a means of securely transmitting messages. This algorithm is based on the difficulty of solving the discrete logarithm problem, making it a robust choice for encrypting sensitive information 🔐.

With the ElGamalCipher, users can easily encrypt plaintext messages into ciphertext ✉️, which can be transmitted over insecure channels without the risk of unauthorized access. The program also includes a decryption feature that allows users to retrieve the original plaintext message from the ciphertext, ensuring that the information remains confidential and secure 🔑.

The program provides a clear and user-friendly interface, displaying both the encrypted and decrypted messages for easy verification of the encryption process 📜. By utilizing modular arithmetic, the ElGamalCipher ensures high levels of security while maintaining usability for users seeking to protect their communications. 🌟


## Steps to Run

1. Import the project into your IDE.
2. Build the project.
3. Run the implementation file `Impl.c`.

## Requirements

- A C compiler (e.g., GCC)
- Standard libraries (e.g., `stdlib.h`, `math.h`)

## How It Works

The ElGamal encryption algorithm relies on the difficulty of the discrete logarithm problem. It consists of key generation, encryption, and decryption processes, utilizing modular arithmetic to secure the plaintext messages.

## Features

- Encrypts plaintext messages using the ElGamal algorithm.
- Decrypts the messages to retrieve the original plaintext.
- Displays both encrypted and decrypted outputs.

## Sample Output

The program displays the encrypted and decrypted messages.
```cpp
Please input a number (plaintext) :
7856
Encrypted Number:
c1=840  c2=1975
Decrypted number :
1523
```

## Acknowledgments

- The ElGamal encryption algorithm.
