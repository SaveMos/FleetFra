package it.unipi.dsmt.CaveDowny.DTO;

import java.util.List;

public class PageDTO <T>{
    private List<T> entries;
    private int counter;

    public List<T> getEntries() {
        return entries;
    }

    public void setEntries(List<T> entries) {
        this.entries = entries;
    }

    public int getCounter() {
        return counter;
    }

    public void setCounter(int counter) {
        this.counter = counter;
    }

}
