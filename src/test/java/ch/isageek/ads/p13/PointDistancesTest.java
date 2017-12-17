package ch.isageek.ads.p13;

import org.junit.Test;
import org.unitils.reflectionassert.ReflectionComparatorMode;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.List;

import static java.util.Arrays.asList;
import static org.unitils.reflectionassert.ReflectionAssert.assertReflectionEquals;

public class PointDistancesTest {

    @Test
    public void testComputeDistancesSimple() throws Exception {
        assertReflectionEquals(asList(2), callComputeDistances(asList(0, 2)), ReflectionComparatorMode.LENIENT_ORDER);
    }

    @Test
    public void testComputeDistancesMedium() throws Exception {
        assertReflectionEquals(asList(2, 3, 1), callComputeDistances(asList(0, 2, 3)), ReflectionComparatorMode.LENIENT_ORDER);
    }
    @Test
    public void testComputeDistancesExerciseExample() throws Exception {
        assertReflectionEquals(asList(2, 5, 7, 7, 9, 9, 14, 14, 16, 23), callComputeDistances(asList(0, 7, 9, 14, 23)), ReflectionComparatorMode.LENIENT_ORDER);
    }

    private List<Integer> callComputeDistances(List<Integer> args) throws Exception {
        Method computeDistances = PointDistances.class.getDeclaredMethod("computeDistances", List.class);
        computeDistances.setAccessible(true);
        Constructor[] constructors = PointDistances.class.getDeclaredConstructors();
        constructors[0].setAccessible(true);
        PointDistances solver = (PointDistances) constructors[0].newInstance();

        return (List<Integer>) computeDistances.invoke(solver, args);
    }
}
