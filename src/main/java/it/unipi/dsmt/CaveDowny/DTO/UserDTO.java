package it.unipi.dsmt.CaveDowny.DTO;

import com.fasterxml.jackson.annotation.JsonInclude;

//null values (in class attributes) are not included in the json object
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserDTO {
    private String firstName;
    private String lastName;
    private String username;
    private String password;

    public UserDTO(){}

    //constructor
    public UserDTO(String firstName, String lastName, String username, String password) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.username= username;
        this.password = password;
    }
    //getters and setters
    public String getFirstName() {
        return firstName;
    }
    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }
    public String getLastName() {
        return lastName;
    }
    public void setLastName(String lastName) {
        this.lastName = lastName;
    }
    public String getUsername() {
        return username;
    }
    public void setUsername(String username) {
        this.username = username;
    }
    public String getPassword() {
        return password;
    }
    public void setPassword(String password) {
        this.password = password;
    }
}
