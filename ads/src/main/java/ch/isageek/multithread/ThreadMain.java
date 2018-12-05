package ch.isageek.multithread;

public class ThreadMain {

    public static void main(String[] args) {
        new SimpleThread("Jamaica").start();
        new SimpleThread("Fiji").start();
        System.out.println("main exited" + Thread.currentThread());
    }
}
