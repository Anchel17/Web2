package com.ufrn.imdMarket.entity;

import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Table(name="PEDIDOS")
public class PedidoEntity {

    @Id
    @GeneratedValue(strategy=GenerationType.SEQUENCE)
    @Column(name="ID_PEDIDO", nullable=false)
    private Long id;
    
    @Column(name="CODIGO")
    private String codigo;
    
    @OneToMany(mappedBy="pedido", fetch = FetchType.LAZY)
    private List<ProdutoEntity> produtos;
    
    @ManyToOne
    @JsonIgnore
    @JoinColumn(name="ID_CLIENTE")
    private ClienteEntity cliente;
    
    @Column(name="DELETED")
    private Boolean pedidoDeleted;
}
