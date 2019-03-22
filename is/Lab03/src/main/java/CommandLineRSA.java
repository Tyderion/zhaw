import javax.naming.OperationNotSupportedException;
import java.io.*;
import java.math.BigInteger;
import java.util.Scanner;

public class CommandLineRSA {

    public static void main(String[] args) throws IOException, BadMessageException {

        if (args.length != 1) {
            printUsage();
            return;
        }

        if (args[0].equals("--encrypt") || args[0].equals("-e")) {
            encrypt();
        } else if (args[0].equals("--decrypt") || args[0].equals("-d")) {
            decrypt();
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
            String plainText = new String(plain.toByteArray(),"UTF-8");
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
