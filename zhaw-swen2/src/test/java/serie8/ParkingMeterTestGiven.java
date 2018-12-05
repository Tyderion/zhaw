package serie8;

import static org.junit.Assert.assertThat;
import static org.hamcrest.core.StringStartsWith.startsWith;
import static org.hamcrest.CoreMatchers.*;

import org.junit.Test;

public class ParkingMeterTestGiven {

	@Test
	public void testReceiptFilled() {
		
		// Wir steuern alle Verarbeitungsschritte aus in dem wir der Parkuhr 0 € übergeben
		double[] insertedCoins = {0.0};
		
		// Da der Beleg in parkingMeterPayment generiert werden sollte lassen wir diese Methode einmal laufen und speichern das Resultat
		String receipt = ParkingMeter.parkingMeterPayment(1, insertedCoins);
		
		System.out.println(receipt);
		
		// In dem Rückgabefeld receipt sollte nun der Beleg vorhanden sein
		assertThat(receipt, startsWith("Beleg Parkhaus"));
	}
	
	@Test
	public void testBooking() {
		
		// Nehmen wir an der Benutzer hat 2 € bezahlt
		int[] parkCoins = {0,  // Anzahl 0.5 € Münzen (default 5 Stück)
				 		   0,  // Anzahl 1.0 € Münzen (default 5 Stück)
				 		   1   // Anzahl 2.0 € Münzen (default 5 Stück)
				 		  };
		
		// überprüfen wir ob wirklich 5 Stück der 2.0 € Münzen vorhanden sind
		assertThat(ParkingMeter.coinsParkingMeter[0], is(equalTo(5)));
		assertThat(ParkingMeter.coinsParkingMeter[1], is(equalTo(5)));
		assertThat(ParkingMeter.coinsParkingMeter[2], is(equalTo(5)));
		
		// Jetzt buchen wir die 2.0 € Münze in die Münzeinheit
		ParkingMeter.bookParkingCoins(parkCoins);
		
		// Jetzt sollte die Anzahl der 2.0 € Münzen von 5 auf 6 Stück angestiegen sein 
		assertThat(ParkingMeter.coinsParkingMeter[0], is(equalTo(5)));
		assertThat(ParkingMeter.coinsParkingMeter[1], is(equalTo(5)));
		assertThat(ParkingMeter.coinsParkingMeter[2], is(equalTo(6)));
	}
}
