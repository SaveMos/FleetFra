package it.unipi.dsmt.CaveDowny.DAO;

// browse matches
// insert match

import it.unipi.dsmt.CaveDowny.DTO.*;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

public class MatchDAO extends  BaseDAO{

    // browse matches
    public PageDTO<MatchDTO> browseGames(String username){
        //pageDTO contains the list of games to be displayed
        PageDTO<MatchDTO> pageDTO = new PageDTO<>();
        List<MatchDTO> entries = new ArrayList<>();
        String browseMatchSQL = "SELECT * FROM FleetFra.match WHERE user1 = ? OR user2 = ?";
        String browseMatchAdmin = "SELECT * FROM FleetFra.match";

        // if username is null, the request is from an admin
        if(username == null){
            try (Connection connection = getConnection();
                 PreparedStatement preparedStatement = connection.prepareStatement(browseMatchAdmin, PreparedStatement.RETURN_GENERATED_KEYS)) {

                ResultSet resultSet = preparedStatement.executeQuery();
                while (resultSet.next()){
                    // create a new MatchDTO object and fill it with the data from the database
                    MatchDTO matchDTO = new MatchDTO();
                    matchDTO.setId(resultSet.getInt(1));
                    matchDTO.setUser1(resultSet.getString(2));
                    matchDTO.setUser2(resultSet.getString(3));
                    matchDTO.setTimestamp(resultSet.getTimestamp(4));
                    matchDTO.setWinner(resultSet.getInt(5));
                    entries.add(matchDTO);
                }
                // set the entries and the counter in the pageDTO object
                pageDTO.setEntries(entries);
                pageDTO.setCounter(entries.size());
            } catch (SQLException e){
                e.printStackTrace();
                return null;
            }
        }
        else{
            try (Connection connection = getConnection();
                 PreparedStatement preparedStatement = connection.prepareStatement(browseMatchSQL, PreparedStatement.RETURN_GENERATED_KEYS)) {
                //search for matches where the user is involved as user1 or user2
                preparedStatement.setString(1, username);
                preparedStatement.setString(2, username);

                ResultSet resultSet = preparedStatement.executeQuery();
                while (resultSet.next()){
                    // create a new MatchDTO object and fill it with the data from the database
                    MatchDTO matchDTO = new MatchDTO();
                    matchDTO.setId(resultSet.getInt(1));
                    matchDTO.setUser1(resultSet.getString(2));
                    matchDTO.setUser2(resultSet.getString(3));
                    matchDTO.setTimestamp(resultSet.getTimestamp(4));
                    matchDTO.setWinner(resultSet.getInt(5));
                    entries.add(matchDTO);
                }
                // set the entries and the counter in the pageDTO object
                pageDTO.setEntries(entries);
                pageDTO.setCounter(entries.size());
            } catch (SQLException e){
                e.printStackTrace();
                return null;
            }
        }

        return pageDTO;
    }

    // insert match
    public boolean insert(MatchDTO match) {
        String insertQuery = "INSERT INTO FleetFra.match" +
                "(User1, User2, Timestamp, Winner) " +
                "VALUES (?, ?, ?, ?)";

        try (Connection connection = getConnection();
            PreparedStatement preparedStatement = connection.prepareStatement(insertQuery, PreparedStatement.RETURN_GENERATED_KEYS)) {
            preparedStatement.setString(1, match.getUser1());
            preparedStatement.setString(2, match.getUser2());
            preparedStatement.setTimestamp(3, match.getTimestamp());
            preparedStatement.setInt(4, match.getWinner());

            int rowsAffected = preparedStatement.executeUpdate();

            if (rowsAffected > 0) {
                // Get the automatically generated ID
                try (ResultSet generatedKeys = preparedStatement.getGeneratedKeys()) {
                    if (generatedKeys.next()) {
                        int generatedId = generatedKeys.getInt(1);
                        match.setId(generatedId);
                    }
                }
                return true;
            } else {
                return false;
            }
        } catch (SQLException ex) {
            ex.printStackTrace();
            return false;
        }
    }

}
