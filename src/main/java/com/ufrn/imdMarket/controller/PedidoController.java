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

import com.ufrn.imdMarket.dto.PedidoDTO;
import com.ufrn.imdMarket.dto.ProdutoDTO;
import com.ufrn.imdMarket.entity.PedidoEntity;
import com.ufrn.imdMarket.repository.PedidoRepository;
import com.ufrn.imdMarket.service.PedidoService;

@RestController
@RequestMapping("/pedidos")
public class PedidoController {
    @Autowired
    private PedidoRepository pedidoRepository;
    
    @Autowired
    private PedidoService pedidoService;
    
    @GetMapping(value="/getAll", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<PedidoEntity>> getAllPedidos(){
        return ResponseEntity.ok(pedidoService.getAllPedidos());
    }
    
    @GetMapping(value="/get/{idPedido}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Optional<PedidoEntity>> getById(@PathVariable Long idPedido){
        var optPedido = pedidoService.getPedido(idPedido);
        if(optPedido.isPresent() && Boolean.FALSE.equals(optPedido.get().getPedidoDeleted())) {
                return ResponseEntity.ok().body(optPedido);                
        }
        
        return ResponseEntity.notFound().build();
    }
    
    @PostMapping(value="/postPedido", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PedidoEntity> postPedido(@RequestBody @Valid PedidoDTO pedidoDTO) throws IllegalArgumentException{
        return ResponseEntity.ok().body(pedidoService.cadastrarPedido(pedidoDTO));
    }
    
    
    @PutMapping(value="/putPedido/{idPedido}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PedidoEntity> putPedido(@PathVariable Long idPedido,
            @RequestBody PedidoDTO pedidoDTO){
        var optPedido = pedidoService.atualizarPedido(idPedido, pedidoDTO);

        if(optPedido.isEmpty()) {
            return ResponseEntity.notFound().build();
        }

        return ResponseEntity.ok().body(pedidoRepository.save(optPedido.get()));
    }
    
    @PutMapping(value="/adicionarProduto/{idPedido}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PedidoEntity> adicionarProdutoAoPedido(@PathVariable Long idPedido,
            @RequestBody @Valid ProdutoDTO produtoDTO){
        var optPedido = pedidoService.adicionarProdutoAoPedido(idPedido, produtoDTO);
        
        return optPedido.isPresent() ? 
                ResponseEntity.ok().body(optPedido.get()) : ResponseEntity.notFound().build();
    }
    
    @PutMapping(value="/removerProduto/{idPedido}/{idProduto}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PedidoEntity> removerProdutoDoPedido(@PathVariable Long idPedido, @PathVariable Long idProduto){
        var optPedido = pedidoService.removerProdutoDoPedido(idPedido, idProduto);
        
        return optPedido.isPresent() ? 
                ResponseEntity.ok().body(optPedido.get()) : ResponseEntity.notFound().build();
    }
    
    @DeleteMapping(value="/deletePedido/{idPedido}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PedidoEntity> deletePedido(@PathVariable Long idPedido){
        var isPedidoDeleted = pedidoService.deletePedido(idPedido);
        
        return Boolean.TRUE.equals(isPedidoDeleted) ? 
                ResponseEntity.ok().build() : ResponseEntity.notFound().build();
    }
    
    @DeleteMapping(value="/deletePedido/logic/{idPedido}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PedidoEntity> deletePedidoLogic(@PathVariable Long idPedido){
        var isPedidoDeletedLogic = pedidoService.deleteLogicPedido(idPedido);
        
        return Boolean.TRUE.equals(isPedidoDeletedLogic) ?
                ResponseEntity.ok().build() : ResponseEntity.notFound().build();
    }
    
}
