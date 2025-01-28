
# FleetFra: A Web-Based Battleship Game Server in Erlang

## Overview
FleetFra is a simple, concurrent web-based battleship game server written in Erlang. The server is designed to handle multiple simultaneous battles between players, with real-time communication happening through HTTP requests. The game logic, communication with clients, and management of game sessions are all handled by Erlang processes. The server interacts with a frontend web GUI and a remote Spring-based backend (Java) via JSON messages over HTTP.

## Technologies Used

### Erlang
Erlang is a functional programming language and runtime designed for building highly concurrent, distributed, and fault-tolerant systems. It is well-suited for applications like this one, which require managing multiple independent game sessions concurrently while maintaining high availability and reliability.

- **Concurrency**: Erlang's lightweight process model allows each game session to be handled by a separate process, making it easy to scale and manage many games simultaneously.
- **Fault-tolerance**: The built-in supervision tree ensures that the server can recover from failures of individual game processes, ensuring high availability.

### Rebar3
Rebar3 is a build tool for Erlang that simplifies managing dependencies, compiling code, and running tests. It is used to handle the dependencies in this project, including the `jsx` library for JSON parsing.

### JSX (JSON Parsing)
JSX is a JSON parser for Erlang that allows the server to easily encode and decode JSON messages. The server receives JSON requests from the frontend and other systems (like the Spring backend), processes them, and sends back JSON responses.

### HTTP Server (Erlang's `inets` library)
The server listens for HTTP requests from external clients (the frontend and the Java-based backend). The `inets` library in Erlang provides the HTTP server functionality, which allows the server to receive and respond to HTTP requests.

### Java Spring Backend (External System)
The Erlang server communicates with a remote Spring-based backend (written in Java) via HTTP. The backend sends game moves or session initiation requests as JSON, which are processed by the Erlang server.

## File Structure
```text
fleetfra_simple/
├── README.md
├── src/
│   ├── fleetfra_web_server.erl
│   ├── fleetfra_game_registry.erl
│   ├── config.erl
│   └── rebar.config
├── LICENSE
└── .gitignore
```

## How It Works

### 1. Game Initialization
- When a request with `type_request = "start_game"` is received, the server generates a unique `GameId` by concatenating the player usernames (sorted alphabetically) and the current timestamp.
- A new game session is started by spawning an Erlang process dedicated to the game, and the game process is registered with the `GameId`.

### 2. Handling Moves
- During the game, the Spring backend sends move requests (in JSON format) to the Erlang server. These requests are handled by the server, which forwards the moves to the appropriate game process based on the `GameId`.
- Each game process handles the game logic for the respective game and returns the result to the server, which in turn sends the outcome back to the Spring backend.

### 3. JSON Communication
- The server parses incoming JSON requests using the `jsx` library.
- Responses are also generated as JSON to maintain a consistent format for communication with the Spring backend.

### 4. Concurrency and Fault Tolerance
- Erlang's lightweight process model allows multiple games to be handled concurrently without blocking each other.
- If a game process fails or crashes, the supervision tree ensures the failure is detected, and the process is restarted, maintaining the availability of the system.

## How to Run

### 1. Clone the repository

```bash
git clone https://github.com/yourusername/fleetfra.git
cd fleetfra
```

### 2. Install Dependencies

This project uses **Rebar3** to manage Erlang dependencies. First, install Rebar3 if you don't have it already:

```bash
# Install Rebar3 (if not installed)
curl -sSL https://github.com/erlang/rebar3/releases/download/3.16.0/rebar3-3.16.0.tar.gz | tar xz
sudo mv rebar3 /usr/local/bin
```

### 3. Compile the Project

Run the following command to compile the Erlang code and fetch the dependencies:

```bash
rebar3 compile
```

### 4. Run the Application

To start the Erlang application and launch the web server, use the following command:

```bash
rebar3 shell
```

In the Erlang shell, you can start the web server by calling:

```erlang
fleetfra_web_server:start().
```

The server will start listening on port 8080 for incoming HTTP requests.

### 5. Interacting with the Server

- **Start a new game**: Send a POST request to `http://localhost:8080/start_game` with a JSON body containing the usernames of the two players.
- **Make a move**: Send a POST request to `http://localhost:8080/make_move` with a JSON body containing the move data.

## Future Improvements

- **Authentication**: Add user authentication to ensure that players are verified before starting a game.
- **Game State Persistence**: Store game states to allow players to resume unfinished games.
- **Real-time WebSocket Support**: Use WebSockets to push real-time updates to the frontend, improving the user experience.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.