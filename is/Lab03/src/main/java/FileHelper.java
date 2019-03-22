import java.io.*;

public class FileHelper {
    public static ObjectInputStream readFile(String filename) throws IOException {
        FileInputStream in = new FileInputStream(filename);
        ObjectInputStream is = new ObjectInputStream(in);
        return is;
    }

    public static void writeToFile(String filename, Object o) throws IOException {
        try (FileOutputStream out = new FileOutputStream(filename); ObjectOutputStream os = new ObjectOutputStream(out)) {
            os.writeObject(o);
        }
    }
}
