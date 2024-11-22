package com.ufrn.imdMarket.dto;

import java.time.LocalDate;

import com.ufrn.imdMarket.enums.GeneroClienteEnum;

import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class ClienteDTO {
    private String nome;
    private String cpf;
    private GeneroClienteEnum genero;
    private LocalDate dataNascimento;
}
