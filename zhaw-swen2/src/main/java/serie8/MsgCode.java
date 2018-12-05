package serie8;


import lombok.AllArgsConstructor;

@AllArgsConstructor
public enum MsgCode {
    ENTER_NEW_COINS("Neuer Münzbestand eingeben"),
    ERROR_NEGATIVE_COUNT_NOT_ALLOWED("Die Anzahl Münzen kann nicht negativ sein."),
    ENTER_NEW_COINS_VALUE("Bitte geben sie die Anzahl Münzen vom Wert CHF %.2f ein");
    private final String message;

    public static String getMessageText(MsgCode code) {
        return code.message;
    }

}
