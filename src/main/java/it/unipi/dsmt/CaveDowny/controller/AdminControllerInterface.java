package it.unipi.dsmt.CaveDowny.controller;

import it.unipi.dsmt.CaveDowny.DTO.ViewUsersRequestDTO;
import org.springframework.http.ResponseEntity;

public interface AdminControllerInterface {

    ResponseEntity<String> viewUsers(ViewUsersRequestDTO request);

    ResponseEntity<String> removeUser(String username);

    ResponseEntity<String> browseGames();

}