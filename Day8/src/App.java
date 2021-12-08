import java.util.List;
import java.util.stream.Stream;
import java.nio.file.Files;
import java.nio.file.Paths;

public class App {
    public static long partOne(List<String> input) {
        // Java is a dynamically typed functional language
        return input.stream()
            .map(s -> s.split("\\|"))
            .map(s -> s[1].trim().split(" "))
            .flatMap(Stream::of)
            .filter(s -> s.length() == 2 || s.length() == 3 || s.length() == 4 || s.length() == 7)
            .count();
    }

    public static void main(String[] args) throws Exception {
        List<String> input = Files.readAllLines(Paths.get("input.txt"));
        
        System.out.println("Part 1: " + partOne(input));
    }
}
