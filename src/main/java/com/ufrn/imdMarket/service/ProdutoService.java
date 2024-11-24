package com.ufrn.imdMarket.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.ufrn.imdMarket.dto.ProdutoDTO;
import com.ufrn.imdMarket.entity.ProdutoEntity;
import com.ufrn.imdMarket.repository.ProdutoRepository;

@Service
public class ProdutoService {
    @Autowired
    private ProdutoRepository produtoRepository;
    
    public List<ProdutoEntity> getAllProdutos(){
        var produtos = produtoRepository.findAll();
        
        List<ProdutoEntity> listaFinalProdutos = new ArrayList<>();
        
        produtos.forEach(p -> {
           if(Boolean.FALSE.equals(p.getProdutoDeletado())) {
               listaFinalProdutos.add(p);
           }
        });
        
        return listaFinalProdutos;
    }
    
    public Optional<ProdutoEntity> getProduto(Long idProduto){
        return produtoRepository.findById(idProduto);
    }
    
    public ProdutoEntity cadastrarProduto(ProdutoDTO produtoDTO) {
        var produto = new ProdutoEntity();
        
        produto.setNomeProduto(produtoDTO.getNomeProduto());
        produto.setMarca(produtoDTO.getMarca());
        produto.setGenero(produtoDTO.getGenero());
        produto.setLote(produtoDTO.getLote());
        produto.setDataFabricacao(produtoDTO.getDataFabricacao());
        produto.setDataValidade(produtoDTO.getDataValidade());
        produto.setProdutoDeletado(false);
        
        return produtoRepository.save(produto);
    }
    
    public Optional<ProdutoEntity> atualizarProduto(Long idProduto, ProdutoDTO produtoDTO){
        var optProduto = produtoRepository.findById(idProduto);
        
        if(optProduto.isPresent()) {
            var produto = optProduto.get();
            
            produto.setId(idProduto);
            produto.setNomeProduto(produtoDTO.getNomeProduto());
            produto.setMarca(produtoDTO.getMarca());
            produto.setGenero(produtoDTO.getGenero());
            produto.setLote(produtoDTO.getLote());
            produto.setDataFabricacao(produtoDTO.getDataFabricacao());
            produto.setDataValidade(produtoDTO.getDataValidade());
            produto.setProdutoDeletado(false);
            
            return Optional.of(produtoRepository.save(produto));
        }
        
        return Optional.empty();
    }
    
    public Boolean deleteProduto(Long idProduto) {
        var optProduto = produtoRepository.findById(idProduto);
        
        if(optProduto.isPresent()) {
            produtoRepository.deleteById(idProduto);
            return Boolean.TRUE;
        }

        return Boolean.FALSE;
    }
    
    public Boolean deleteLogicProduto(Long idProduto) {
        var optProduto = produtoRepository.findById(idProduto);
        
        if(optProduto.isPresent()) {
            var produto = optProduto.get();
            produto.setProdutoDeletado(true);
            
            produtoRepository.save(produto);
            return Boolean.TRUE;
        }

        return Boolean.FALSE;
    }
}
