package hh.test;

// Java
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class Test1 {

    private static Optional<List<Integer>> twoSum(List<Integer> list, int X) {
        int left = 0;
        int right = list.size() - 1;
        while (left < right) {
            if(list.get(left) + list.get(right) > X){
                right--;
            }else if (list.get(left) + list.get(right) < X){
                left++;
            }else{
                return Optional.of(Arrays.asList(list.get(left), list.get(right)));
            }
        }
        return Optional.empty();
    }

    public static void main(String[] args) throws IOException {
        Optional<List<Integer>> result =null;
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            int n = Integer.parseInt(reader.readLine().strip());
            List<Integer> items = Arrays.asList(reader.readLine().strip().split(" "))
                    .stream()
                    .map(Integer::parseInt)
                    .collect(Collectors.toList());
            int k = Integer.parseInt(reader.readLine().strip());

            result =twoSum(items, k);
        }
        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out));
        result.ifPresentOrElse((resultItems)->{
            resultItems.forEach((t)->{
                try{
                    writer.write(String.valueOf(t));
                    writer.write(" ");
                }catch (IOException e){

                }
            });
        }, ()->{
            try {
                writer.write("None");
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });

        writer.flush();
    }

}