package com.ufrn.imdMarket.dto;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import com.ufrn.imdMarket.enums.RoleEnum;

import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class RegisterDTO {
    @NotNull
    @NotEmpty
    private String login;
    
    @NotNull
    @NotEmpty
    private String password;
    
    @NotNull
    private RoleEnum role;
}
