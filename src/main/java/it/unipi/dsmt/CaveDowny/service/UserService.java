package it.unipi.dsmt.CaveDowny.service;


import java.util.LinkedList;
import java.util.Queue;
import java.util.concurrent.atomic.AtomicLong;

public class UserService {
    private static final AtomicLong counter = new AtomicLong(0);
    // Thread-safe queue to store players waiting for a match
    private final Queue<String> waitingQueue = new LinkedList<>();

    public static String generateUniqueId() {
        // Combine the timestamp with an atomic counter to generate a unique ID
        long timestamp = System.currentTimeMillis();
        long count = counter.getAndIncrement();
        return timestamp + "-" + count;
    }

    public synchronized String handleGame(String request) {
        // Check if there is already a player waiting in the queue
        if (!waitingQueue.isEmpty()) {
            // Match the requester with the waiting player
            String waitingPlayer = waitingQueue.poll();
            String matchId = generateUniqueId();

            System.out.println("Match found!");
            System.out.println("Player 1: " + waitingPlayer);
            System.out.println("Player 2: " + request);

            // Return the match ID
            return matchId;

        } else {
            // If there is no one waiting, add the requester to the queue
            waitingQueue.add(request);
            System.out.println("Added to waiting queue: " + request);
            return null; // No match found
        }
    }
}
