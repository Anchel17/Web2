package com.ufrn.imdMarket.enums;


public enum RoleEnum {
    ADMIN ("ADMIN"),
    USER  ("USER");
    
    private String role;
    
    RoleEnum(String role){
        this.role = role;
    }
    
    public String getRole() {
        return this.role;
    }
}
