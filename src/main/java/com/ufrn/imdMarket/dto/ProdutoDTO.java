package com.ufrn.imdMarket.dto;

import java.time.LocalDate;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import com.ufrn.imdMarket.enums.GeneroProdutoEnum;

import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class ProdutoDTO {
    
    @NotNull
    @NotEmpty
    private String nomeProduto;
    
    @NotNull
    @NotEmpty
    private String marca;
    
    @NotNull    
    private LocalDate dataFabricacao;
    
    @NotNull
    private LocalDate dataValidade;
    
    @NotNull
    private GeneroProdutoEnum genero;
    
    @NotNull
    @NotEmpty
    private String lote;
}
