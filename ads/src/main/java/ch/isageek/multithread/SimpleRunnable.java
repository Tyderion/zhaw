package ch.isageek.multithread;

public class SimpleRunnable implements Runnable {

    @Override
    public void run() {
        for (int i = 0; i < 10; i++){
            System.out.println(i + " " + Thread.currentThread().getName());
            Thread.yield();
        }

        System.out.println("DONE!" + Thread.currentThread().getName());
    }
}
