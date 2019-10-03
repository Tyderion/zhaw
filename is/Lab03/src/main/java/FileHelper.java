import java.io.*;

public class FileHelper {
    public static ObjectInputStream readFile(String filename) throws IOException {
        FileInputStream in = new FileInputStream(filename);
        return new ObjectInputStream(in);
    }

    public static void writeToFile(String filename, Object... obs) throws IOException {
        try (FileOutputStream out = new FileOutputStream(filename); ObjectOutputStream os = new ObjectOutputStream(out)) {
            for (Object o : obs) {
                os.writeObject(o);
            }
        }
    }
}
