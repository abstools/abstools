package org.abs_models.frontend.typechecker.nullable;

import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.parser.Main;
import org.abs_models.frontend.typechecker.locationtypes.LocationTypeExtension;
import org.abs_models.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension;

import java.io.IOException;
import java.io.StringReader;
import java.util.Arrays;

public class Benchmark {
    private static final int RUNS = 5000;

    private static class BenchmarkResult {
        public final double avg;
        public final double median;
        public final long[] times;

        public BenchmarkResult(long[] times) {
            this.times = times;
            avg = Arrays.stream(times).asDoubleStream().average().orElse(0.0);
            Arrays.sort(times);
            if (times.length % 2 == 0)
                median = ((double) times[times.length / 2] + (double) times[times.length / 2 - 1]) / 2;
            else
                median = (double) times[times.length / 2];
        }

        @Override
        public String toString() {
            return "BenchmarkResult{" +
                "avg=" + avg +
                ", median=" + median +
                '}';
        }
    }

    public static void main(String[] args) {
        BenchmarkResult oldLocTypesNormal = benchmark(() -> {
            Model m = parse("module M; {}");
            assert m != null;
            m.registerTypeSystemExtension(new LocationTypeExtension(m));
            m.typeCheck();
        });
        BenchmarkResult oldLocTypesInfer = benchmark(() -> {
            Model m = parse("module M; {}");
            assert m != null;
            m.registerTypeSystemExtension(new LocationTypeInferrerExtension(m));
            m.typeCheck();
        });

        System.out.println("Old Loc w/o infer " + oldLocTypesNormal);
        System.out.println("Old loc w/ infer " + oldLocTypesInfer);
    }

    private static BenchmarkResult benchmark(Runnable r) {
        return new BenchmarkResult(times(r));
    }

    private static long[] times(Runnable r) {
        long[] times = new long[RUNS];

        for (int i = 0; i < RUNS; i++) {
            System.out.println("Run " + (i + 1) + " of " + RUNS);
            times[i] = timedRun(r);
        }

        return times;
    }

    private static long timedRun(Runnable r) {
        long start = System.currentTimeMillis();
        r.run();
        long end = System.currentTimeMillis();
        return end - start;
    }

    private static Model parse(String s) {
        try {
            return Main.parse(null, new StringReader(s));
        } catch (IOException | InternalBackendException e) {
            e.printStackTrace();
            return null;
        }
    }
}
