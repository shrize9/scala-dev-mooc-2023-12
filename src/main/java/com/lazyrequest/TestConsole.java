package com.lazyrequest;

import fansi.Str;
import org.graalvm.collections.Pair;

public class TestConsole {
    public static void main(String[] args) {
        Console<String> readName =((Console<String>)(new Console().println("enter name:").readLine())).map((t)->t.toUpperCase());
        Console<Integer> readAge = ((Console<String>)(new Console().println("enter age:").readLine())).map((t)->Integer.decode(t));

        readName.flatMap((name)->readAge.map((age)-> Pair.create(name,age))).println("you entered ");
    }
}
