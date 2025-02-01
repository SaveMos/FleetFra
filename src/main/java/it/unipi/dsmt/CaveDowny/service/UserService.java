package it.unipi.dsmt.CaveDowny.service;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.atomic.AtomicLong;

public class UserService {
    private static final AtomicLong counter = new AtomicLong(0);


    public static String generateUniqueId() {
        // Combine the timestamp with an atomic counter to generate a unique ID
        long timestamp = System.currentTimeMillis();
        long count = counter.getAndIncrement();
        return timestamp + "-" + count;
    }
    public ArrayList<String> handleGame(String request, HashMap<String, Matchmaking> waitingQueue) {
        // Check if there is already a player waiting in the queue
        Matchmaking match;
        boolean matchFound = false;
        synchronized (waitingQueue) {
            matchFound = !waitingQueue.isEmpty();
        }
        if (matchFound) {
            // Match the requester with the waiting player
            synchronized (waitingQueue) {
                match = waitingQueue.values().iterator().next();
            }


            match.player2 = request;

            // Notify the thread associated with the first player
            synchronized (match) {
                match.notify();
            }


            // Return the match details
            ArrayList<String> ret = new ArrayList<>();
            synchronized (waitingQueue) {
                ret.add(match.matchId);
                ret.add(match.player1);
            }

            return ret;

        } else {
            // If there is no one waiting, add the requester to the queue
            String matchId = generateUniqueId();
            synchronized (waitingQueue) {
                waitingQueue.put(matchId, new Matchmaking(request, null, matchId));
                match = waitingQueue.get(matchId);
            }


            System.out.println("Added to waiting queue: " + request);


            // Wait for the second player to arrive and start the game
            synchronized (match) {
                        try {
                            match.wait();
                        } catch (InterruptedException e) {
                            Thread.currentThread().interrupt();
                        }
           }


            ArrayList<String> ret = new ArrayList<>();
            synchronized (waitingQueue) {
                ret.add(match.matchId);
                ret.add(match.player2);
            }

            return ret; // No match found yet, still waiting
        }
    }


}
