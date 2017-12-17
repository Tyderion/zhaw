package ch.isageek.ads.p13;

import org.junit.Test;
import org.unitils.reflectionassert.ReflectionComparatorMode;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.List;
import java.util.function.Supplier;

import static java.util.Arrays.asList;
import static junit.framework.TestCase.assertEquals;
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

    @Test
    public void testValidateFull() throws Exception {
        assertEquals(PointDistances.Result.SOLUTION, callValidate(new int[]{2}, asList(0, 2)));
    }

    @Test
    public void testValidateFullMoreComplex() throws Exception {
        assertEquals(PointDistances.Result.SOLUTION, callValidate(new int[]{2, 3, 1}, asList(0, 2, 3)));
    }

    @Test
    public void testValidateFullExerciseExample() throws Exception {
        assertEquals(PointDistances.Result.SOLUTION, callValidate(new int[]{2, 5, 7, 7, 9, 9, 14, 14, 16, 23}, asList(0, 7, 9, 14, 23)));
    }

    @Test
    public void testValidatePartialValid() throws Exception {
        assertEquals(PointDistances.Result.VALID, callValidate(new int[]{2, 3}, asList(0, 2)));
    }

    @Test
    public void testValidatePartialValidExerciseExample() throws Exception {
        assertEquals(PointDistances.Result.VALID, callValidate(new int[]{2, 5, 7, 7, 9, 9, 14, 14, 16, 23}, asList(0, 7, 9, 14)));
    }

    @Test
    public void testValidatePartialInValid() throws Exception {
        assertEquals(PointDistances.Result.INVALID, callValidate(new int[]{2, 3}, asList(0, 2, 5)));
    }

    @Test
    public void testValidatePartialInValidExerciseExample() throws Exception {
        assertEquals(PointDistances.Result.INVALID, callValidate(new int[]{2, 5, 7, 7, 9, 9, 14, 14, 16, 23}, asList(0, 7, 9, 14, 15)));
    }

    @Test
    public void testSimpleSolution() {
        int[] points = PointDistances.findPoints(new int[]{2});
        assertReflectionEquals(new int[] {0, 2}, points, ReflectionComparatorMode.LENIENT_ORDER);
    }

    @Test
    public void testMultipleDistancesSolution() {
        int[] points = PointDistances.findPoints(new int[]{2, 3, 1});
        assertReflectionEquals(new int[] {0, 2, 3}, points, ReflectionComparatorMode.LENIENT_ORDER);
    }

    @Test
    public void testExerciseSolution() {
        int[] points = PointDistances.findPoints(new int[]{2, 5, 7, 7, 9, 9, 14, 14, 16, 23});
        assertReflectionEquals(new int[] {0, 7, 9, 14, 23}, points, ReflectionComparatorMode.LENIENT_ORDER);
    }

    @SuppressWarnings("unchecked")
    private List<Integer> callComputeDistances(List<Integer> args) throws Exception {
        Method computeDistances = PointDistances.class.getDeclaredMethod("computeDistances", List.class);
        return (List<Integer>) callMethod(computeDistances,noArgs(),  args);
    }

    @SuppressWarnings("unchecked")
    private PointDistances.Result callValidate(int[] distances, List<Integer> selectedPoints) throws Exception {
        Method computeDistances = PointDistances.class.getDeclaredMethod("validate", List.class);
        return (PointDistances.Result) callMethod(computeDistances, withArgs(distances), selectedPoints);
    }

    private Object callMethod(Method method, Supplier<PointDistances> pointdistance, Object... args) throws Exception {
        method.setAccessible(true);
        return method.invoke(pointdistance.get(), (Object[])args);
    }

    private Supplier<PointDistances> noArgs() throws Exception {
        Constructor[] constructors = PointDistances.class.getDeclaredConstructors();
        constructors[0].setAccessible(true);
        final PointDistances distances = (PointDistances) constructors[0].newInstance();
        return () -> distances;
    }

    private Supplier<PointDistances> withArgs(int[] args) throws Exception {
        Constructor[] constructors = PointDistances.class.getDeclaredConstructors();
        constructors[1].setAccessible(true);
        final PointDistances distances = (PointDistances) constructors[1].newInstance((Object)args);
        return () -> distances;
    }
}
