package com.lazyrequest;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;

public class RequestContainer<T> {
    private final Response<T> value;
    public RequestContainer(Response<T> value) {
        this.value = value;
    }

    public <R> RequestContainer<R> map(Function<Response<T>, Response<R>> mapper) {
        if(value.isError()){
            return new RequestContainer<R>((Response<R>) value);
        }
        return new RequestContainer<R>(mapper.apply(value));
    }

    public <R> RequestContainer<R> flatMap(Function<Response<T>, RequestContainer<R>> mapper) {
        if(value.isError()){
            return new RequestContainer<R>((Response<R>) value);
        }
        return mapper.apply(value);
    }

    public RequestContainer<T> mapError(Consumer<Response<Throwable>> mapper) {
        if(value.isError())
            mapper.accept((Response<Throwable>)value);
        return this;
    }

    public RequestContainer<T> filter(Function<T, Boolean> condition){
        if(!value.isError())
            if(condition.apply(value.getResult()) ==true) {
                return this;
            }else{
                return new RequestContainer<>(Response.setError(new Exception("filter condition false")));
            }
        return this;
    }

    public void forEach(Consumer<T> mapper) {
        if(!value.isError())
            mapper.accept(value.getResult());
    }

    public T get() throws RuntimeException{
        if(value ==null)
            throw new RuntimeException("value is null");
        return value.getResult();
    }

    public Optional<T> getSafe(){
        if(value ==null)
            return Optional.empty();
        if(value.isError())
            return Optional.empty();

        return Optional.of(value.getResult());
    }
}