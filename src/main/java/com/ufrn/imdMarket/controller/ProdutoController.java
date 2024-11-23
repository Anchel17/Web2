package com.ufrn.imdMarket.controller;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.ufrn.imdMarket.dto.ProdutoDTO;
import com.ufrn.imdMarket.entity.ClienteEntity;
import com.ufrn.imdMarket.entity.ProdutoEntity;
import com.ufrn.imdMarket.repository.ProdutoRepository;

@RestController
@RequestMapping("/produtos")
public class ProdutoController {
    @Autowired
    private ProdutoRepository produtoRepository;
    
    @GetMapping(value="/getAll", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<ProdutoEntity>> getAllProdutos(){
        var produtos = produtoRepository.findAll();
        
        List<ProdutoEntity> listaFinalProdutos = new ArrayList<>();
        
        produtos.forEach(p -> {
           if(Boolean.FALSE.equals(p.getProdutoDeletado())) {
               listaFinalProdutos.add(p);
           }
        });
        
        return ResponseEntity.ok().body(listaFinalProdutos);
    }
    
    @GetMapping(value="/get/{idProduto}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Optional<ProdutoEntity>> getById(@PathVariable Long idProduto){
        var produto = produtoRepository.findById(idProduto);
        
        if(produto.isPresent()) {
            if(Boolean.FALSE.equals(produto.get().getProdutoDeletado())) {
                return ResponseEntity.ok().body(produto);
            }
        }
        
        return ResponseEntity.notFound().build();
    }
    
    @PostMapping(value="/postProduto", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ProdutoEntity> postProduto(@RequestBody @Valid ProdutoDTO produtoDTO){
        var produto = new ProdutoEntity();
        
        produto.setNomeProduto(produtoDTO.getNomeProduto());
        produto.setMarca(produtoDTO.getMarca());
        produto.setGenero(produtoDTO.getGenero());
        produto.setLote(produtoDTO.getLote());
        produto.setDataFabricacao(produtoDTO.getDataFabricacao());
        produto.setDataValidade(produtoDTO.getDataValidade());
        produto.setProdutoDeletado(false);
        
        return ResponseEntity.ok().body(produtoRepository.save(produto));
    }
    
    @PutMapping(value="/putProduto/{idProduto}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ProdutoEntity> putProduto(@PathVariable Long idProduto, 
            @RequestBody @Valid ProdutoDTO produtoDTO){
        var produto = new ProdutoEntity();
        
        produto.setId(idProduto);
        produto.setNomeProduto(produtoDTO.getNomeProduto());
        produto.setMarca(produtoDTO.getMarca());
        produto.setGenero(produtoDTO.getGenero());
        produto.setLote(produtoDTO.getLote());
        produto.setDataFabricacao(produtoDTO.getDataFabricacao());
        produto.setDataValidade(produtoDTO.getDataValidade());
        produto.setProdutoDeletado(false);
        
        return ResponseEntity.ok().body(produtoRepository.save(produto));
    }
    
    @DeleteMapping(value="/deleteProduto/{idProduto}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ProdutoEntity> deleteProduto(@PathVariable Long idProduto){
        produtoRepository.deleteById(idProduto);
        
        return ResponseEntity.ok().build();
    }
    
    @DeleteMapping(value="/deleteProduto/logic/{idProduto}")
    public ResponseEntity<ProdutoEntity> deleteLogic(@PathVariable Long idProduto){
        var produto = produtoRepository.findById(idProduto);
        
        produto.ifPresent(p -> {
            p.setProdutoDeletado(true);
            produtoRepository.save(p);
        });
        
        return ResponseEntity.ok().build();
    }
}
