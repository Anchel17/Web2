package com.ufrn.imdMarket.controller;

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
import com.ufrn.imdMarket.entity.ProdutoEntity;
import com.ufrn.imdMarket.repository.ProdutoRepository;
import com.ufrn.imdMarket.service.ProdutoService;

@RestController
@RequestMapping("/produtos")
public class ProdutoController {
    @Autowired
    private ProdutoRepository produtoRepository;
    
    @Autowired
    private ProdutoService produtoService;
    
    @GetMapping(value="/getAll", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<ProdutoEntity>> getAllProdutos(){
        return ResponseEntity.ok().body(produtoService.getAllProdutos());
    }
    
    @GetMapping(value="/get/{idProduto}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Optional<ProdutoEntity>> getById(@PathVariable Long idProduto){
        var optProduto = produtoService.getProduto(idProduto);
        
        if(optProduto.isPresent() && Boolean.FALSE.equals(optProduto.get().getProdutoDeletado())) {
            return ResponseEntity.ok().body(optProduto);
        }
        
        return ResponseEntity.notFound().build();
    }
    
    @PostMapping(value="/postProduto", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ProdutoEntity> postProduto(@RequestBody @Valid ProdutoDTO produtoDTO){
        return ResponseEntity.ok().body(produtoRepository.save(produtoService.cadastrarProduto(produtoDTO)));
    }
    
    @PutMapping(value="/putProduto/{idProduto}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ProdutoEntity> putProduto(@PathVariable Long idProduto, 
            @RequestBody @Valid ProdutoDTO produtoDTO){
        var optProduto = produtoService.atualizarProduto(idProduto, produtoDTO);
       
        if(optProduto.isPresent()) {
            return ResponseEntity.ok().body(optProduto.get());
        }
        
        return ResponseEntity.notFound().build();
    }
    
    @DeleteMapping(value="/deleteProduto/{idProduto}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ProdutoEntity> deleteProduto(@PathVariable Long idProduto){
        var isProdutoDeleted = produtoService.deleteProduto(idProduto);
        
        return isProdutoDeleted ? ResponseEntity.ok().build() : ResponseEntity.notFound().build();
    }
    
    @DeleteMapping(value="/deleteProduto/logic/{idProduto}")
    public ResponseEntity<ProdutoEntity> deleteLogic(@PathVariable Long idProduto){
        var isProdutoLogicDeleted = produtoService.deleteLogicProduto(idProduto);
        
        return isProdutoLogicDeleted ? ResponseEntity.ok().build() : ResponseEntity.notFound().build();
    }
}
