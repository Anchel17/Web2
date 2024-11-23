package com.ufrn.imdMarket.entity;

import java.time.LocalDate;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.ufrn.imdMarket.enums.GeneroProdutoEnum;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name="PRODUTO")
public class ProdutoEntity {
    
    @Id
    @GeneratedValue(strategy=GenerationType.SEQUENCE)
    @Column(name="ID_PRODUTO", nullable=false)
    private Long id;
    
    @Column(name="NOME_PRODUTO")
    private String nomeProduto;
    
    @Column(name="MARCA_PRODUTO")
    private String marca;
    
    @Column(name="DATA_FABRICACAO")
    private LocalDate dataFabricacao;
    
    @Column(name="DATA_VALIDADE")
    private LocalDate dataValidade;
    
    @Column(name="GENERO_PRODUTO")
    private GeneroProdutoEnum genero;
    
    @Column(name="LOTE_PRODUTO")
    private String lote;
    
    @ManyToOne(cascade=CascadeType.ALL)
    @JoinColumn(name="ID_PEDIDO")
    private PedidoEntity pedido;
    
    @Column(name="DELETED")
    private Boolean produtoDeletado;
}
