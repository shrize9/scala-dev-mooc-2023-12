package com.lazyrequest;

public class Response<T> {
    private T result =null;

    private Response(T result) {
        this.result = result;
    }

    public boolean isError(){
        return result instanceof Throwable;
    }

    public T getResult() {
        return result;
    }

    public static Response setError(Throwable error){return new Response<Throwable>(error);}
    public static<T> Response<T> setData(T response){
        return new Response<T>(response);
    }
}
