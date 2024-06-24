package com.lazyrequest;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.function.Function;

public class Console<T> {
    private final T value;
    public Console(T value) {
        this.value = value;
    }
    public Console() {
        this.value = null;
    }

    public <R> Console<R> map(Function<T, R> mapper) {
        return new Console<>(mapper.apply(value));
    }

    public <R> Console<R> flatMap(Function<T, Console<R>> mapper) {
        return mapper.apply(value);
    }

    public Console<String> readLine() {
        try {
            // Enter data using BufferReader
            BufferedReader reader = new BufferedReader(
                    new InputStreamReader(System.in));

            // Reading data using readLine
            String text = reader.readLine();

            return new Console<String>(text);
        }catch (Exception err){
            return new Console<String>(err.getMessage());
        }
    }

    public Console<Void> println(String prefix) {
        if(value !=null)
            System.out.println(prefix +" " +value.toString());
        else
            System.out.println(prefix);
        return new Console<Void>(null);
    }
}
