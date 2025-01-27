package it.unipi.dsmt.CaveDowny.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import it.unipi.dsmt.CaveDowny.DTO.*;

public interface UserControllerInterface {
    ResponseEntity<String> signUp(UserDTO UserSignUp);
    ResponseEntity<String> game(@RequestBody String request);
}