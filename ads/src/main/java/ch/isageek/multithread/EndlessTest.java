package ch.isageek.multithread;

public class EndlessTest {

    public static void main(String[] args) {
        EndlessThread fiji = new EndlessThread("Fiji");

        fiji.start();
        try {
            Thread.sleep(5000);
        } catch (InterruptedException e) {}
        fiji.terminate();
        System.out.println("main exits " + Thread.currentThread().getName());
    }
}
