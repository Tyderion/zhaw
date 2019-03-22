import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.math.BigInteger;

public class RSA {

    public RSA() {

    }

    public RSA(ObjectInputStream is) throws IOException {

    }

    public BigInteger encrypt(BigInteger plain) throws BadMessageException {
        return BigInteger.ONE;
    }


    public BigInteger decrypt(BigInteger cipher) throws BadMessageException {
        return BigInteger.ONE;
    }

    public void save(ObjectOutputStream os) throws IOException {

    }

    public void savePublic(ObjectOutputStream os) throws IOException {

    }


}
