package com.ufrn.imdMarket.dto;

import java.time.LocalDate;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import org.hibernate.validator.constraints.br.CPF;

import com.ufrn.imdMarket.enums.GeneroClienteEnum;

import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class ClienteDTO {
    @NotNull
    @NotEmpty
    private String nome;
    
    @NotNull
    @NotEmpty
    @CPF
    private String cpf;
    
    @NotNull
    private GeneroClienteEnum genero;
    
    @NotNull
    private LocalDate dataNascimento;
}
