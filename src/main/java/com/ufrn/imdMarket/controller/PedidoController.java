package com.ufrn.imdMarket.controller;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

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

import com.ufrn.imdMarket.dto.ClienteDTO;
import com.ufrn.imdMarket.dto.PedidoDTO;
import com.ufrn.imdMarket.dto.ProdutoDTO;
import com.ufrn.imdMarket.entity.ClienteEntity;
import com.ufrn.imdMarket.entity.PedidoEntity;
import com.ufrn.imdMarket.entity.ProdutoEntity;
import com.ufrn.imdMarket.repository.ClienteRepository;
import com.ufrn.imdMarket.repository.PedidoRepository;
import com.ufrn.imdMarket.repository.ProdutoRepository;

@RestController
@RequestMapping("/pedidos")
public class PedidoController {
    @Autowired
    private PedidoRepository pedidoRepository;
    
    @Autowired
    private ClienteRepository clienteRepository;
    
    @Autowired
    private ProdutoRepository produtoRepository;
    
    @GetMapping(value="/getAll", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<PedidoEntity>> getAllPedidos(){
        return ResponseEntity.ok(pedidoRepository.findAll());
    }
    
    @GetMapping(value="/get/{idPedido}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Optional<PedidoEntity>> getById(@PathVariable Long idPedido){
        var pedido = pedidoRepository.findById(idPedido);
        if(pedido.isPresent()) {
            if(Boolean.FALSE.equals(pedido.get().getPedidoDeleted())) {
                return ResponseEntity.ok().body(pedido);                
            }
        }
        
        return ResponseEntity.notFound().build();
    }
    
    @PostMapping(value="/postPedido", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PedidoEntity> postPedido(@RequestBody PedidoDTO pedidoDTO){
        var pedido = new PedidoEntity();
        
        pedido.setCodigo(pedidoDTO.getCodigo());
        pedido.setCliente(buildClienteEntity(pedidoDTO.getCliente()));
        pedido.setPedidoDeleted(false);
        pedido.setProdutos(buildListProdutos(pedidoDTO.getProdutos()));
        associarProdutosAPedido(pedido, pedido.getProdutos());
        
        return ResponseEntity.ok().body(pedidoRepository.save(pedido));
    }
    
    
    @PutMapping(value="/putPedido/{idPedido}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PedidoEntity> putPedido(@PathVariable Long idPedido, @RequestBody PedidoDTO pedidoDTO){
        var optPedido = pedidoRepository.findById(idPedido);
        
        if(optPedido.isEmpty()) {
            return ResponseEntity.notFound().build();
        }
        
        var pedido = optPedido.get();
        pedido.setCodigo(pedidoDTO.getCodigo());
        pedido.setCliente(buildClienteEntity(pedidoDTO.getCliente()));
        pedido.setProdutos(buildListProdutos(pedidoDTO.getProdutos()));
        pedido.setPedidoDeleted(false);
        
        return ResponseEntity.ok().body(pedidoRepository.save(pedido));
    }
    
    @PutMapping(value="/adicionarProduto/{idPedido}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PedidoEntity> adicionarProdutoAoPedido(@PathVariable Long idPedido, @RequestBody ProdutoDTO produtoDTO){
        var optPedido = pedidoRepository.findById(idPedido);
        
        optPedido.ifPresent(pedido ->{
            var produto = new ProdutoEntity();
            
            produto.setNomeProduto(produtoDTO.getNomeProduto());
            produto.setMarca(produtoDTO.getMarca());
            produto.setGenero(produtoDTO.getGenero());
            produto.setLote(produtoDTO.getLote());
            produto.setDataFabricacao(produtoDTO.getDataFabricacao());
            produto.setDataValidade(produtoDTO.getDataValidade());
            produto.setProdutoDeletado(false);
            
            pedido.getProdutos().add(produto);
            
            pedidoRepository.save(pedido);
        });
        
        return ResponseEntity.ok().build();
    }
    
    @PutMapping(value="/removerProduto/{idPedido}/{idProduto}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PedidoEntity> removerProdutoDoPedido(@PathVariable Long idPedido, @PathVariable Long idProduto){
        var optPedido = pedidoRepository.findById(idPedido);
        
        optPedido.ifPresent(pedido -> {
           var indice = 0;
           var achou = Boolean.FALSE;
           for(var i = 0; i < pedido.getProdutos().size(); i++) {
               if(pedido.getProdutos().get(i).getId().equals(idProduto)) {
                   achou = Boolean.TRUE;
                   indice = i;
                   break;
               }
           }
           
           if(Boolean.TRUE.equals(achou)) {
               pedido.getProdutos().remove(indice);
               
               pedidoRepository.save(pedido);
           }
        });
        
        return ResponseEntity.ok().build();
    }
    
    @DeleteMapping(value="/deletePedido/{idPedido}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PedidoEntity> deletePedido(@PathVariable Long idPedido){
        pedidoRepository.deleteById(idPedido);
        
        return ResponseEntity.ok().build();
    }
    
    @DeleteMapping(value="/deletePedido/logic/{idPedido}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PedidoEntity> deletePedidoLogic(@PathVariable Long idPedido){
        var pedido = pedidoRepository.findById(idPedido);
        
        pedido.ifPresent(p ->{
           p.setPedidoDeleted(true);
           pedidoRepository.save(p);
        });
        
        return ResponseEntity.ok().build();
    }
    
    private ClienteEntity buildClienteEntity(ClienteDTO clienteDTO) {
        var cliente = new ClienteEntity();
        
        cliente.setCpf(clienteDTO.getCpf());
        cliente.setNome(clienteDTO.getNome());
        cliente.setGenero(clienteDTO.getGenero());
        cliente.setDataNascimento(clienteDTO.getDataNascimento());
        cliente.setClienteDeleted(false);
        
        clienteRepository.save(cliente);
        
        return cliente;
    }
    
    private List<ProdutoEntity> buildListProdutos(List<ProdutoDTO> produtosDTO){
        List<ProdutoEntity> produtos = new ArrayList<>();
        
        produtosDTO.forEach(p -> {
            var produto = new ProdutoEntity();
            
            produto.setNomeProduto(p.getNomeProduto());
            produto.setMarca(p.getMarca());
            produto.setGenero(p.getGenero());
            produto.setLote(p.getLote());
            produto.setDataFabricacao(p.getDataFabricacao());
            produto.setDataValidade(p.getDataValidade());
            produto.setProdutoDeletado(false);
            
            produtoRepository.save(produto);
            
            produtos.add(produto);
        });
        
        return produtos;
    }
    
    private void associarProdutosAPedido(PedidoEntity pedido, List<ProdutoEntity> produtos) {
        produtos.forEach(produto ->{
            produto.setPedido(pedido);
            
            produtoRepository.save(produto);
        });
    }
}
