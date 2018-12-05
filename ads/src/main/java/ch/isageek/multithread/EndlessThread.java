package ch.isageek.multithread;

public class EndlessThread extends Thread {
    volatile boolean doContinue = true;

    public EndlessThread(String src) {
        super(src);
    }

    public void terminate() {
        doContinue = false;
        this.interrupt();
    }

    public void run() {
        while(doContinue) {
            System.out.println("Thread " + getName());
            try {
                Thread.sleep((int)(Math.random()*1000));
            } catch (InterruptedException e) {}

        }
        System.out.println("DONE!" + getName());
    }
}
