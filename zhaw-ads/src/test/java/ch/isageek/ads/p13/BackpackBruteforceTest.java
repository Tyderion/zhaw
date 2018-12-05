package ch.isageek.ads.p13;

import org.junit.Test;
import org.unitils.reflectionassert.ReflectionComparatorMode;

import java.util.Collections;

import static ch.isageek.ads.p13.BackpackSolver.*;
import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.unitils.reflectionassert.ReflectionAssert.assertReflectionEquals;

public class BackpackBruteforceTest {

    private BackpackSolver solver = new BackpackBruteforce();

    @Test(expected = IllegalArgumentException.class)
    public void shouldThrowIfMoreWeightsThanValues() throws Exception {
        solver.solve(new int[]{1,2,3}, new int[]{1,2}, 10);
    }

    @Test(expected = IllegalArgumentException.class)
    public void shouldThrowIfMoreValuesThanWeights() throws Exception {
        solver.solve(new int[]{1,2}, new int[]{1,2,3}, 10);
    }

    @Test(expected = IllegalArgumentException.class)
    public void shouldThrowIfMaxValueBelowZeroArrayMethod() throws Exception {
        solver.solve(new int[]{1,2,3}, new int[]{1,2,3}, -10);
    }

    @Test
    public void shouldSolveSimpleBackpackingProblem_1() {
        int[] weights = new int[]{1, 2, 5};
        int[] values = new int[]{1, 2, 5};

        Solution solution = solver.solve(weights, values, 1);

        assertEquals(1, solution.getValue());
        assertReflectionEquals(
                Collections.singletonList(new Thing(1, 1)),
                solution.things,
                ReflectionComparatorMode.LENIENT_ORDER
        );
    }

    @Test
    public void shouldSolveSimpleBackpackingProblem_1_2() {
        int[] weights = new int[]{1, 2, 5};
        int[] values = new int[]{1, 2, 5};

        Solution solution = solver.solve(weights, values, 4);

        assertEquals(3, solution.getValue());
        assertReflectionEquals(
                asList(new Thing(1, 1), new Thing(2, 2)),
                solution.things,
                ReflectionComparatorMode.LENIENT_ORDER
        );
    }

    @Test
    public void shouldSolveSimpleBackpackingProblem_2_3() {
        int[] weights = new int[]{1, 2, 5};
        int[] values = new int[]{1, 2, 5};

        Solution solution = solver.solve(weights, values, 7);

        assertEquals(7, solution.getValue());
        assertReflectionEquals(
                asList(new Thing(5, 5), new Thing(2, 2)),
                solution.things,
                ReflectionComparatorMode.LENIENT_ORDER
        );
    }

    @Test
    public void shouldSolveSimpleBackpackingProblem_1_3() {
        int[] weights = new int[]{1, 2, 5};
        int[] values = new int[]{1, 2, 5};

        Solution solution = solver.solve(weights, values, 6);

        assertEquals(6, solution.getValue());
        assertReflectionEquals(
                asList(new Thing(5, 5), new Thing(1, 1)),
                solution.things,
                ReflectionComparatorMode.LENIENT_ORDER
        );
    }
}
