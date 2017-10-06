package ch.isageek.ads.p3;

import java.text.*;
import java.util.*;

public final class Competitor implements Comparable<Competitor> {

    private final SimpleDateFormat TIME_FORMAT
            = new SimpleDateFormat("HH:mm:ss.S");

    private int number;

    private String firstName;

    private String lastName;

    private int yearOfBirth;

    private String city;

    private Date time;

    public Competitor(int number, String firstName, String lastName,
                      int yearOfBirth, String city, String timeString)
            throws NullPointerException, ParseException {

        /* Check and adopt parameters. */
        if (firstName == null) {
            throw new NullPointerException("Parameter \"firstName\" is null.");
        }
        if (lastName == null) {
            throw new NullPointerException("Parameter \"lastName\" is null.");
        }
        if (city == null) {
            throw new NullPointerException("Parameter \"city\" is null.");
        }
        this.number = number;
        this.firstName = firstName;
        this.lastName = lastName;
        this.yearOfBirth = yearOfBirth;
        this.city = city;
        this.time = TIME_FORMAT.parse(timeString);
    }

    @Override
    public boolean equals(Object object) {

        /* Fuegen Sie hier Ihren Code ein. */

        return object instanceof Competitor && this.compareTo((Competitor) object) == 0;
    }

    @Override
    public int hashCode() {

        /* Fuegen Sie hier Ihren Code ein. */
        return Objects.hash(lastName, firstName);
    }

    @Override
    public String toString() {

        /* Fuegen Sie hier Ihren Code ein. */

        return String.format("%d\t%s\t%s %s (%d, %s)", number, TIME_FORMAT.format(time), lastName, firstName, yearOfBirth, city);
    }

    @Override
    public int compareTo(Competitor competitor) {
        int lastNameComparison = getLastName().compareTo(competitor.getLastName());
        if (lastNameComparison != 0) {
            return lastNameComparison;
        }
        return getFirstName().compareTo(competitor.getFirstName());
    }

    public int getNumber() {
        return number;
    }

    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public int getYearOfBirth() {
        return yearOfBirth;
    }

    public String getCity() {
        return city;
    }

    public Date getTime() {
        return time;
    }

    public String returnTimeString() {
        return TIME_FORMAT.format(time);
    }
}
