package serie8;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.core.StringStartsWith.startsWith;
import static org.junit.Assert.assertThat;
import static org.unitils.reflectionassert.ReflectionAssert.assertReflectionEquals;

public class ParkingMeterTest {

    @Test
    public void testBookParkingCoins() {
        int[] input = {1, 2, 3};
        ParkingMeter.coinsParkingMeter = new int[]{1, 1, 1};

        ParkingMeter.bookParkingCoins(input);

        // Should add all coins
        assertReflectionEquals(new int[]{2, 3, 4}, ParkingMeter.coinsParkingMeter);
    }

    @Test
    public void testBookIgnoreNegativeParkingCoins() {
        int[] input = {-1, 0, 1};
        ParkingMeter.coinsParkingMeter = new int[]{1, 1, 1};

        ParkingMeter.bookParkingCoins(input);

        // Should not change
        assertReflectionEquals(new int[]{1, 1, 2}, ParkingMeter.coinsParkingMeter);
    }

    @Test
    public void testReceiptCreated() {
        double[] input = {0.5, 0.5};
        String receipt = ParkingMeter.parkingMeterPayment(1, input);

        assertThat(receipt, startsWith("Beleg Parkhaus"));
        assertThat(receipt, containsString("Bezahlte Gebühr CHF 1,00"));

    }
}
