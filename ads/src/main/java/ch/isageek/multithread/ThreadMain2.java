package ch.isageek.multithread;

public class ThreadMain2 {

    public static void main(String[] args) {
        Runnable run = new SimpleRunnable();
        new Thread(run, "Jamaica").start();
        new Thread(run, "Fiji").start();
        System.out.println("main exited" + Thread.currentThread());
    }
}
