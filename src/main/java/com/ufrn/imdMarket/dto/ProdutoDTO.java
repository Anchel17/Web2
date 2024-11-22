package com.ufrn.imdMarket.dto;

import java.time.LocalDate;

import com.ufrn.imdMarket.enums.GeneroProdutoEnum;

import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class ProdutoDTO {
    private String nomeProduto;
    private String marca;
    private LocalDate dataFabricacao;
    private LocalDate dataValidade;
    private GeneroProdutoEnum genero;    
    private String lote;
}
