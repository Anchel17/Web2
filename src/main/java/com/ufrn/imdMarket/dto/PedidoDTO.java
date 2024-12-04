package com.ufrn.imdMarket.dto;

import java.util.List;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PedidoDTO {
    
    @NotNull
    @NotEmpty
    private String codigo;

    @NotNull
    private Long idCliente;
    
    @NotNull
    private List<ProdutoDTO> produtos;
    
}
