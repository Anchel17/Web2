package com.ufrn.imdMarket.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.ufrn.imdMarket.entity.ProdutoEntity;

public interface ProdutoRepository extends JpaRepository<ProdutoEntity, Long> {

}
