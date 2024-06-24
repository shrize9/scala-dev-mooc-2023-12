package com.lazyrequest;

public class TestRequest {
    public static void main(String[] args) {
        RequestContainer result =new RequestContainer(Response.setData("apiKey is full")).mapError(throwable->System.out.println(((Response<Throwable>)throwable).getResult()));
        result.forEach((value)->System.out.println(value));

        LazyRequestContainer result1 =new LazyRequestContainer(Response.setData("test")).map(resp1->Response.setData (((Response<String>)resp1).getResult() +" test"));
        LazyRequestContainer<String> value = result1.run();
        value.forEach(System.out::println);

        value.map(resp1->Response.setData (((Response<String>)resp1).getResult() +" test")).run().forEach(System.out::println);
    }
}
