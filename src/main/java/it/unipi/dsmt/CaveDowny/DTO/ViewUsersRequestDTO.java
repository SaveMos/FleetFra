package it.unipi.dsmt.CaveDowny.DTO;

public class ViewUsersRequestDTO {
    private String username;
    private int page;

    public ViewUsersRequestDTO(){}

    public ViewUsersRequestDTO(String username, int page) {
        this.username = username;
        this.page = page;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public int getPage() {
        return page;
    }

    public void setPage(int page) {
        this.page = page;
    }
}
