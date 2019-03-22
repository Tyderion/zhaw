import javax.naming.OperationNotSupportedException;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.security.SecureRandom;
import java.util.Scanner;

public class CommandLineRSA {

    public static void main(String[] args) throws IOException, BadMessageException {

        if (args.length != 1) {
            printUsage();
            return;
        }
//
//        BigInteger a = BigInteger.probablePrime(1000, new SecureRandom());
//        BigInteger b = BigInteger.probablePrime(1000, new SecureRandom());
//        String outputFilename = "test.t";
//        try (ObjectInputStream ois = FileHelper.write(outputFilename)) {
//            FileHelper.writeToFile(outputFilename, a);
//            FileHelper.writeToFile(outputFilename, b);
//        } catch (IOException e) {
//            e.printStackTrace();
//        }
//
//        try {
//            ObjectInputStream ois = FileHelper.readFile(outputFilename);
//            BigInteger a  = (BigInteger) ois.readObject();
//            BigInteger b = (BigInteger) ois.readObject();
//        } catch (IOException | ClassNotFoundException e) {
//            System.out.println("Could not read file '" + outputFilename + "'");
//            e.printStackTrace();
//            return;
//        }

        if (args[0].equals("--encrypt") || args[0].equals("-e")) {
            encrypt();
        } else if (args[0].equals("--decrypt") || args[0].equals("-d")) {
            decrypt();
        } else if (args[0].equals("--sign") || args[0].equals("-s")) {
            sign();
        } else if (args[0].equals("--verify") || args[0].equals("-v")) {
            verify();
        } else {
            printUsage();
        }
    }

    private static void encrypt() throws IOException, BadMessageException {
        System.out.println("Please input your plaintext");
        Scanner scanner = new Scanner(System.in);

        String line = scanner.nextLine();
        BigInteger plaintext = new BigInteger(stringToBytesASCII(line));
        System.out.println("Please input the name of the file containing the public key");
        String pubKeyName = scanner.nextLine();

        System.out.println("Please input the name of the file to write the encrypted data to");
        String encryptedFilename = scanner.nextLine();


        try (ObjectInputStream ois = FileHelper.readFile(pubKeyName)) {
            RSA rsa = new RSA(ois);
            FileHelper.writeToFile(encryptedFilename, rsa.encrypt(plaintext));
        }

    }

    private static void sign() throws IOException, BadMessageException {
        System.out.println("Please input your plaintext");
        Scanner scanner = new Scanner(System.in);

        String line = scanner.nextLine();
        BigInteger plaintext = new BigInteger(stringToBytesASCII(line));
        System.out.println("Please input the name of the file containing the private and public key");
        String pubKeyName = scanner.nextLine();

        System.out.println("Please input the name of the file to write the data to");
        String outputFilename = scanner.nextLine();


        try (ObjectInputStream ois = FileHelper.readFile(pubKeyName)) {
            RSA rsa = new RSA(ois);
            BigInteger signature = rsa.sign(plaintext);
            FileHelper.writeToFile(outputFilename, plaintext, signature);
        } catch (OperationNotSupportedException e) {
            System.out.println("Cannot sign without private key");
            e.printStackTrace();
        }

        try {
            ObjectInputStream ois = FileHelper.readFile(outputFilename);
            BigInteger a  = (BigInteger) ois.readObject();
            BigInteger b = (BigInteger) ois.readObject();
        } catch (IOException | ClassNotFoundException e) {
            System.out.println("Could not read file '" + outputFilename + "'");
            e.printStackTrace();
            return;
        }

    }

    private static void verify() {
        System.out.println("Please input the name of the file containing the message and signature");
        Scanner scanner = new Scanner(System.in);
        String cipherTextFilename = scanner.nextLine();


        BigInteger message;
        BigInteger signature;
        try {
            ObjectInputStream ois = FileHelper.readFile(cipherTextFilename);
            message = (BigInteger) ois.readObject();
            signature = (BigInteger) ois.readObject();
        } catch (IOException | ClassNotFoundException e) {
            System.out.println("Could not read file '" + cipherTextFilename + "'");
            e.printStackTrace();
            return;
        }

        System.out.println("Please input the name of the file containing the public key");
        String pubKeyName = scanner.nextLine();


        RSA rsa;
        try {
            ObjectInputStream privateKey = FileHelper.readFile(pubKeyName);
            rsa = new RSA(privateKey);
        } catch (IOException e) {
            System.out.println("Could not read private key from file  " + pubKeyName + "'");
            e.printStackTrace();
            return;
        }

        try {
            if (rsa.verify(message, signature)) {
                String plainText = new String(message.toByteArray(), "UTF-8");
                System.out.println(plainText);
            } else {
                System.out.println("Could not verify message");
            }
        } catch (UnsupportedEncodingException e) {
            System.out.println("Error decrypting text");
            e.printStackTrace();
        }
    }


    private static void decrypt() {
        System.out.println("Please input the name of the file containing the ciphertext");
        Scanner scanner = new Scanner(System.in);
        String cipherTextFilename = scanner.nextLine();


        BigInteger cipher;
        try {
            cipher = (BigInteger) FileHelper.readFile(cipherTextFilename).readObject();
        } catch (IOException | ClassNotFoundException e) {
            System.out.println("Could not read file '" + cipherTextFilename + "'");
            return;
        }

        System.out.println("Please input the name of the file containing the private key");
        String privateKeyFileName = scanner.nextLine();


        RSA rsa;
        try {
            ObjectInputStream privateKey = FileHelper.readFile(privateKeyFileName);
            rsa = new RSA(privateKey);
        } catch (IOException e) {
            System.out.println("Could not read private key from file  " + privateKeyFileName + "'");
            e.printStackTrace();
            return;
        }

        BigInteger plain = null;
        try {
            plain = rsa.decrypt(cipher);
            String plainText = new String(plain.toByteArray(), "UTF-8");
            System.out.println(plainText);
        } catch (BadMessageException | UnsupportedEncodingException | OperationNotSupportedException e) {
            System.out.println("Error decrypting text");
            e.printStackTrace();
        }
    }

    private static byte[] stringToBytesASCII(String str) {
        byte[] b = new byte[str.length()];
        for (int i = 0; i < b.length; i++) {
            b[i] = (byte) str.charAt(i);
        }
        return b;
    }

    private static void printUsage() {
        System.out.println("Use either --encrypt (-e) or --decrypt (-d) to encrypt or decrypt respectively");
    }
}
