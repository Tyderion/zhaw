import javax.naming.OperationNotSupportedException;
import java.io.*;
import java.math.BigInteger;
import java.security.SecureRandom;
import java.util.Objects;
import java.util.Random;

import static java.math.BigInteger.ONE;
import static java.math.BigInteger.ZERO;

public class RSA {

    private BigInteger e;
    private BigInteger d = null;
    private BigInteger n;

    public RSA() {
        Random random = new SecureRandom();
        BigInteger phiN;
        do {
            BigInteger p = BigInteger.probablePrime(1033, random);
            BigInteger q = BigInteger.probablePrime(1015, random);

            this.n = p.multiply(q);

            phiN = p.subtract(BigInteger.ONE).multiply(q.subtract(BigInteger.ONE));
            this.e = BigInteger.valueOf(65535);
        } while(!phiN.gcd(e).equals(BigInteger.ONE));
        d = extendedEuclidX(e, phiN);
    }

    public RSA(ObjectInputStream is) throws IOException {
        try {
            this.n = (BigInteger) is.readObject();
            this.e = (BigInteger) is.readObject();
        } catch (ClassNotFoundException e1) {
            throw new RuntimeException("This shoul dprobably not happen");
        }
        try {
            try {
                this.d = (BigInteger) is.readObject();
            } catch (IOException e) {

            }
        } catch (ClassNotFoundException e1) {
            throw new RuntimeException("This shoul dprobably not happen");
        }
    }

    public static void main(String[] args) throws IOException, OperationNotSupportedException, BadMessageException {
        RSA rsa = new RSA();

        try (FileOutputStream file = new FileOutputStream("pub.key"); ObjectOutputStream os = new ObjectOutputStream(file)) {
            rsa.savePublic(os);
        }

        try (FileOutputStream file = new FileOutputStream("full.key"); ObjectOutputStream os = new ObjectOutputStream(file)) {
            rsa.save(os);
        }

        BigInteger plain = BigInteger.valueOf(155);

        BigInteger cipher = rsa.encrypt(plain);

        BigInteger decrypted = rsa.decrypt(cipher);

        try (FileInputStream in = new FileInputStream("full.key"); ObjectInputStream is = new ObjectInputStream(in)) {
            RSA rsa2 = new RSA(is);

            System.out.println(rsa2.equals(rsa));
        }
    }

    public void savePublic(ObjectOutputStream os) throws IOException {
        os.writeObject(n);
        os.writeObject(e);
    }

    public void save(ObjectOutputStream os) throws IOException, OperationNotSupportedException {
        if (d == null) {
            throw new OperationNotSupportedException();
        }

        os.writeObject(n);
        os.writeObject(e);
        os.writeObject(d);
    }

    public BigInteger encrypt(BigInteger plain) throws BadMessageException {
        checkNumber(plain);
        return plain.modPow(e, n);
    }

    public BigInteger decrypt(BigInteger cipher) throws BadMessageException, OperationNotSupportedException {
        if (d == null) {
            throw new OperationNotSupportedException();
        }
        checkNumber(cipher);
        return cipher.modPow(d, n);
    }

    private void checkNumber(BigInteger number) throws BadMessageException {
        // >= 1
        if (number.compareTo(BigInteger.ONE) < 0) {
            throw new BadMessageException();
        }

        // <= n-1
        if (number.compareTo(n) >= 0) {
            throw new BadMessageException();
        }
    }


    @Override
    public boolean equals(Object obj) {
        if (obj instanceof RSA) {
            RSA other = (RSA) obj;
            return Objects.equals(d, other.d) && this.e.equals(other.e) && this.n.equals(other.n);
        }
        return false;
    }

    private static BigInteger extendedEuclidX(BigInteger a, BigInteger b) {

        BigInteger x = ZERO, y = ONE, lastx = ONE, lasty = ZERO, temp;
        while (!b.equals(ZERO)) {
            BigInteger q = a.divide(b);
            BigInteger r = a.mod(b);

            a = b;
            b = r;

            temp = x;
            x = lastx.subtract(q.multiply(x));
            lastx = temp;

            temp = y;
            y = lasty.subtract(q.multiply(y));
            lasty = temp;
        }

        return lastx;
    }


}
