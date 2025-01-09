package com.ufrn.imdMarket.dto;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class LoginDTO {
    
    @NotNull
    @NotEmpty
    private String login;
    
    @NotNull
    @NotEmpty
    private String password;
}
