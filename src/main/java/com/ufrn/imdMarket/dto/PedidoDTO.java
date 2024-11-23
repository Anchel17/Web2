package com.ufrn.imdMarket.dto;

import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PedidoDTO {   
    private String codigo;
    private List<ProdutoDTO> produtos;
    private ClienteDTO cliente;
}
