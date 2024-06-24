package com.lazyrequest;

import java.util.LinkedList;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Function;

public class LazyRequestContainer<T> {
    private final Response<T> value;

    private LinkedList<Object> mappers=new LinkedList<>();

    public LazyRequestContainer(Response<T> value) {
        this.value = value;
    }

    public <R> LazyRequestContainer<R> run() {
        if(mappers.size() ==0)
            return (LazyRequestContainer<R>) this;

        AtomicReference<LazyRequestContainer> valueAccum = new AtomicReference<>(this);
        mappers.stream().forEach((mapperMon)->{
            if(mapperMon instanceof LazyRequestContainer.LazyMapRequestContainer<?,?>){
                System.out.println(mapperMon);
                System.out.println(valueAccum.get());
                valueAccum.set(
                        new LazyRequestContainer ((Response) ((LazyMapRequestContainer) mapperMon).mapper.apply(valueAccum.get().value))
                );
            }
            if(mapperMon instanceof LazyRequestContainer.LazyFlatMapRequestContainer<?,?>){
                valueAccum.set((LazyRequestContainer) ((LazyMapRequestContainer) mapperMon).mapper.apply(valueAccum.get().value));
            }
        });
        return (LazyRequestContainer<R>)valueAccum.get();
    }

    public <R> LazyRequestContainer<T> map(Function<Response<T>, Response<R>> mapper) {
        if(!value.isError()){
            mappers.add(new LazyMapRequestContainer(mapper));
        }
        return this;
    }

    public <R> LazyRequestContainer<T> flatMap(Function<Response<T>, LazyRequestContainer<R>> mapper) {
        if(!value.isError()){
            mappers.add(new LazyFlatMapRequestContainer(mapper));
        }
        return this;
    }

    public void forEach(Consumer<T> mapper) {
        if(!value.isError())
            mapper.accept(value.getResult());
    }

    private static class LazyMapRequestContainer<T,R> {
        private Function<Response<T>, Response<R>> mapper;
        LazyMapRequestContainer(Function<Response<T>, Response<R>> mapper) {
            this.mapper = mapper;
        }
    }

    private static class LazyFlatMapRequestContainer<T,R> {
        private Function<Response<T>, RequestContainer<R>> mapper;
        LazyFlatMapRequestContainer(Function<Response<T>, RequestContainer<R>> mapper) {
            this.mapper = mapper;
        }
    }
}
