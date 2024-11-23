package com.ufrn.imdMarket.entity;

import java.time.LocalDate;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.ufrn.imdMarket.enums.GeneroClienteEnum;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Table(name="CLIENTE")
public class ClienteEntity {
    
    @Id
    @GeneratedValue(strategy=GenerationType.SEQUENCE)
    @Column(name="ID_CLIENTE", nullable=false)
    private Long id;
    
    @Column(name="NOME")
    private String nome;
    
    @Column(name="CPF")
    private String cpf;
    
    @Column(name="GENERO_CLIENTE")
    private GeneroClienteEnum genero;
    
    @Column(name="DATA_NASCIMENTO")
    private LocalDate dataNascimento;
    
    @OneToMany(mappedBy="cliente")
    @JsonBackReference
    private List<PedidoEntity> pedidos;
    
    @Column(name="DELETED")
    private Boolean clienteDeleted;
}
