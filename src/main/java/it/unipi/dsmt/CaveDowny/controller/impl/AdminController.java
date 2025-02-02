//Implement the methods of the AdminControllerInterface interface
package it.unipi.dsmt.CaveDowny.controller.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.unipi.dsmt.CaveDowny.DAO.*;

import it.unipi.dsmt.CaveDowny.controller.AdminControllerInterface;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import it.unipi.dsmt.CaveDowny.DTO.*;

import java.util.List;


//this class manages http requests and every response is a json object
@RestController
public class AdminController implements AdminControllerInterface {

    //DAO object for database user interaction
    private UserDAO userDAO = new UserDAO();

    //endpoint API for user visualization
    @PostMapping("/viewDetailedUsers")
    @Override
    public ResponseEntity<String> viewUsers(@RequestBody ViewUsersRequestDTO request) {
        //pageDTO contains the list of users to be displayed
        List<UserDTO> list = userDAO.viewUsers(request);
        //ObjectMapper is used to convert the pageDTO object into a JSON object
        ObjectMapper objectMapper = new ObjectMapper();

        try {
            String jsonResult = objectMapper.writeValueAsString(list);
            //return HTTP response with JSON object with Code 200
            return new ResponseEntity<>(jsonResult, HttpStatus.OK);

        } catch (JsonProcessingException e) {
            e.printStackTrace();
            return new ResponseEntity<>("Error during JSON serialization", HttpStatus.BAD_REQUEST);
        }
    }

    //endpoint API for user removal
    @PostMapping("/viewUsers")
    @Override
    public ResponseEntity<String> removeUser(@RequestBody String username) {
        if(!userDAO.removeUser(username)){
            return new ResponseEntity<>("Error during the operation", HttpStatus.BAD_REQUEST);
        }

        return new ResponseEntity<>("User '"+username+"' removed", HttpStatus.OK);
    }

    //endpoint API for game visualization
    @PostMapping("/browseGamesAdmin")
    @Override
    // return a list of games
    public ResponseEntity<String> browseGames() {
        //DAO object for database match interaction
        MatchDAO matchDAO = new MatchDAO();
        //ObjectMapper is used to convert the pageDTO object into a JSON object
        ObjectMapper objectMapper = new ObjectMapper();

        //pageDTO contains the list of games to be displayed
        PageDTO<MatchDTO> pageDTO = matchDAO.browseGames(null);

        try {
            String jsonResult = objectMapper.writeValueAsString(pageDTO);
            //return HTTP response with JSON object with Code 200
            return new ResponseEntity<>(jsonResult, HttpStatus.OK);

        } catch (JsonProcessingException e) {
            e.printStackTrace();
            return new ResponseEntity<>("Error during JSON serialization", HttpStatus.BAD_REQUEST);
        }
    }

}

