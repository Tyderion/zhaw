package ch.isageek.ads.p8;

public class QuickSortClassic extends QuickSortBase {
    @Override
    protected int getPivot(int[] numbers, int low, int high) {
        return (low + high) / 2;
    }
}
