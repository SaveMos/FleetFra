package it.unipi.dsmt.CaveDowny.service;


import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicLong;

public class UserService {
    private static final AtomicLong counter = new AtomicLong(0);
    // Thread-safe queue to store players waiting for a match
    private final ConcurrentLinkedQueue<String> waitingQueue = new ConcurrentLinkedQueue<>();
    // Thread-safe map to store player threads
    private Map<String, Thread> playerThreads = new ConcurrentHashMap<>();


    public static String generateUniqueId() {
        // Combine the timestamp with an atomic counter to generate a unique ID
        long timestamp = System.currentTimeMillis();
        long count = counter.getAndIncrement();
        return timestamp + "-" + count;
    }
    public synchronized ArrayList<String> handleGame(String request) {
        // Check if there is already a player waiting in the queue
        if (!waitingQueue.isEmpty()) {
            // Match the requester with the waiting player
            String waitingPlayer = waitingQueue.poll();  // This is the first player
            String matchId = generateUniqueId();

            System.out.println("Match found!");
            System.out.println("Player 1: " + waitingPlayer);
            System.out.println("Player 2: " + request);

            // Notify the thread associated with the first player
            notifyPlayer(waitingPlayer);

            // Return the match details
            ArrayList<String> ret = new ArrayList<>();
            ret.add(matchId);
            ret.add(waitingPlayer);
            ret.add(request);  // Adding second player (the requester)
            return ret;

        } else {
            // If there is no one waiting, add the requester to the queue
            waitingQueue.add(request);
            System.out.println("Added to waiting queue: " + request);

            // Add the current player's thread to the map
            playerThreads.put(request, Thread.currentThread());

            // Wait for the second player to arrive and start the game
            try {
                wait();  // The first player waits until notified by the second player
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }

            return null; // No match found yet, still waiting
        }
    }

    // Notifies the thread associated with the first player
    private synchronized void notifyPlayer(String player) {
        // Get the thread associated with the player
        Thread playerThread = playerThreads.get(player);

        if (playerThread != null) {
            System.out.println("Notifying player: " + player);

            // Notify the player's thread
            synchronized (playerThread) {
                playerThread.notify();  // Notify the player's thread
            }
        }
    }
}
