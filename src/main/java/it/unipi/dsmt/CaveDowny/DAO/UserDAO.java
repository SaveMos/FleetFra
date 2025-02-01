package it.unipi.dsmt.CaveDowny.DAO;

import it.unipi.dsmt.CaveDowny.DTO.*;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

public class UserDAO extends BaseDAO {

    // DAO method for signing up a user: it returns an integer value to indicate the result of the operation
    public int signup(UserDTO user) {
        // SQL query to check if the username already exists
        String checkExistingUserSQL = "SELECT * FROM FleetFra.user WHERE Username = ?";
        String registerUserSQL = "INSERT INTO FleetFra.user" +
                "(Username, Name, Surname, Password, Email)" +
                "VALUES" +
                "(?,?,?,?,?);";

        try (Connection connection = getConnection()) {
            connection.setAutoCommit(false);

            // Check if the username already exists
            try (PreparedStatement checkStatement = connection.prepareStatement(checkExistingUserSQL)) {
                checkStatement.setString(1, user.getUsername());

                try (ResultSet resultSet = checkStatement.executeQuery()) {
                    if (resultSet.next()) {
                        // Username already exists, return an error
                        return 0;
                    }
                }
            } catch (SQLException ex) {
                ex.printStackTrace();
                return -1;
            }


            // Insert the new user if the username doesn't exist
            try (PreparedStatement preparedStatement = connection.prepareStatement(registerUserSQL)) {
                preparedStatement.setString(1, user.getUsername());
                preparedStatement.setString(2, user.getFirstName());
                preparedStatement.setString(3, user.getLastName());
                preparedStatement.setString(4, user.getPassword());
                preparedStatement.setString(5, user.getEmail());


                if (preparedStatement.executeUpdate() == 0) {
                    connection.rollback();
                    return -1;
                }

                connection.commit();
            } catch (SQLException ex) {
                connection.rollback();
                ex.printStackTrace();
                return -1;
            }
        } catch (SQLException ex) {
            ex.printStackTrace();
            return -1;
        }

        return 1;
    }

    // DAO method for logging in a user: it returns a UserDTO object containing the user's information
    public UserDTO login(String username, String password) {
        String loginQuery = "SELECT * FROM FleetFra.user WHERE username = ? AND password = ?";

        try (Connection connection = getConnection(); PreparedStatement preparedStatement = connection.prepareStatement(loginQuery)) {
            // Set the username and password parameters in the query
            preparedStatement.setString(1, username);
            preparedStatement.setString(2, password);

            // Execute the query and return the UserDTO object if the user exists
            try (ResultSet resultSet = preparedStatement.executeQuery()) {
                if (resultSet.next()) {
                    UserDTO userDTO = new UserDTO();
                    userDTO.setUsername(resultSet.getString("Username"));
                    return userDTO;
                } else {
                    return null;
                }
            }

        } catch (SQLException ex) {
            ex.printStackTrace();
            return null;
        }
    }

    // DAO method for removing a user from the database
    public boolean removeUser(String username) {
        // SQL query to remove a user from the database
        String removeQuery = "DELETE FROM FleetFra.user WHERE username = ?";

        try (Connection connection = getConnection(); PreparedStatement preparedStatement = connection.prepareStatement(removeQuery)) {

            // Set the username parameter in the query
            preparedStatement.setString(1, username);

            return preparedStatement.executeUpdate() > 0;

        } catch (SQLException ex) {
            ex.printStackTrace();
            return false;
        }
    }

    // DAO method for viewing users: it returns a PageDTO object containing a list of UserDTO objects
    // and the number of users in the list
    public PageDTO<UserDTO> viewUsers() {
        // Create a new PageDTO object to be returned
        PageDTO<UserDTO> pageDTO = new PageDTO<>();
        List<UserDTO> entries = new ArrayList<>();
        // SQL query to retrieve all users from the database
        String browseUsersSQL = "SELECT Username, Name, Surname FROM FleetFra.user";

        try (Connection connection = getConnection(); PreparedStatement preparedStatement = connection.prepareStatement(browseUsersSQL)) {

            ResultSet resultSet = preparedStatement.executeQuery();
            // Iterate over the result set and create a new UserDTO object for each user
            while (resultSet.next()) {
                UserDTO userDTO = new UserDTO();
                userDTO.setUsername(resultSet.getString("Username"));
                userDTO.setFirstName(resultSet.getString("Name"));
                userDTO.setLastName(resultSet.getString("Surname"));
                entries.add(userDTO);
            }

            // Set the list of users and the number of users in the PageDTO object
            pageDTO.setEntries(entries);
            pageDTO.setCounter(entries.size());
        } catch (SQLException e) {
            e.printStackTrace();
            return null;
        }

        return pageDTO;
    }


}

