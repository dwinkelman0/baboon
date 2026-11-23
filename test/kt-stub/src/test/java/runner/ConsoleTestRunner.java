package runner;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Test entry point for Bazel java_test targets running JUnit 5 via the
 * standalone console launcher.
 *
 * <p>{@code --scan-classpath} without an explicit {@code --class-path} finds
 * no tests under Bazel (the launcher does not scan the ambient JVM
 * classpath), while the ambient classpath is exactly right. This main
 * forwards {@code java.class.path} explicitly and delegates to the console
 * launcher, so no per-target file lists are needed.
 */
public final class ConsoleTestRunner {
  private ConsoleTestRunner() {}

  public static void main(String[] args) {
    List<String> full =
        new ArrayList<>(Arrays.asList("--scan-classpath", "--class-path=" + System.getProperty("java.class.path")));
    full.addAll(Arrays.asList(args));
    org.junit.platform.console.ConsoleLauncher.main(full.toArray(new String[0]));
  }
}
