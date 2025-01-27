package it.unipi.dsmt.CaveDowny.DTO;

import java.sql.Timestamp;

public class MatchDTO {
    private int id;
    private String user1;
    private String user2;
    private Timestamp timestamp;
    private int winner;


    public MatchDTO(){}

    public MatchDTO(int id, String user1, String user2, Timestamp timestamp, int winner) {
        this.id = id;
        this.user1 = user1;
        this.user2 = user2;
        this.timestamp = timestamp;
        this.winner= winner;
    }

    public MatchDTO(String user1, String user2, Timestamp timestamp, int winner) {
        this.user1 = user1;
        this.user2 = user2;
        this.timestamp = timestamp;
        this.winner= winner;

    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getUser1() {
        return user1;
    }

    public void setUser1(String user1) {
        this.user1 = user1;
    }

    public String getUser2() {
        return user2;
    }

    public void setUser2(String user2) {
        this.user2 = user2;
    }


    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }

    public int getWinner() {
        return winner;
    }

    public void setWinner(int winner) {
        this.winner= winner;
    }
}
