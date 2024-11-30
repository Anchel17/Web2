package com.ufrn.imdMarket.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.ufrn.imdMarket.dto.PedidoDTO;
import com.ufrn.imdMarket.dto.ProdutoDTO;
import com.ufrn.imdMarket.entity.PedidoEntity;
import com.ufrn.imdMarket.entity.ProdutoEntity;
import com.ufrn.imdMarket.repository.ClienteRepository;
import com.ufrn.imdMarket.repository.PedidoRepository;
import com.ufrn.imdMarket.repository.ProdutoRepository;

@Service
public class PedidoService {
    @Autowired
    private PedidoRepository pedidoRepository;
    
    @Autowired
    private ProdutoRepository produtoRepository;
    
    @Autowired
    private ClienteRepository clienteRepository;
    
    public List<PedidoEntity> getAllPedidos(){
        var pedidos = pedidoRepository.findAll();
        List<PedidoEntity> listaFinalPedidos = new ArrayList<>();
        
        pedidos.forEach(pedido -> {
            if(Boolean.FALSE.equals(pedido.getPedidoDeleted())) {
                listaFinalPedidos.add(pedido);
            }
        });
        
        return listaFinalPedidos;
    }
    
    public Optional<PedidoEntity> getPedido(Long idPedido) {
        return pedidoRepository.findById(idPedido);
    }
    
    @Transactional
    public PedidoEntity cadastrarPedido(PedidoDTO pedidoDTO) throws IllegalArgumentException {
        var pedido = new PedidoEntity();
        var optCliente = clienteRepository.findById(pedidoDTO.getIdCliente());
        
        if(optCliente.isEmpty()) {
            throw new IllegalArgumentException("Cliente não encontrado na base de dados");
        }
        
        pedido.setCodigo(pedidoDTO.getCodigo());
        pedido.setCliente(optCliente.get());
        pedido.setPedidoDeleted(false);
        pedido.setProdutos(buildListProdutos(pedidoDTO.getProdutos()));
        associarProdutosAPedido(pedido, pedido.getProdutos());
        
        return pedidoRepository.save(pedido);
    }
    
    public Optional<PedidoEntity> atualizarPedido(Long idPedido, PedidoDTO pedidoDTO){
        var optPedido = pedidoRepository.findById(idPedido);
        var optCliente = clienteRepository.findById(pedidoDTO.getIdCliente());
        
        if(optCliente.isEmpty()) {
            throw new IllegalArgumentException("Cliente não encontrado na base de dados");
        }
        
        if(optPedido.isPresent()) {
            var pedido = optPedido.get();
            pedido.setCodigo(pedidoDTO.getCodigo());
            pedido.setCliente(optCliente.get());
            pedido.setProdutos(buildListProdutos(pedidoDTO.getProdutos()));
            pedido.setPedidoDeleted(false);
            
            return Optional.of(pedido);
        }
        
        return Optional.empty();   
    }
    
    public Optional<PedidoEntity> adicionarProdutoAoPedido(Long idPedido, ProdutoDTO produtoDTO){
        var optPedido = pedidoRepository.findById(idPedido);
        
        if(optPedido.isPresent()) {
            var pedido = optPedido.get();
            var produto = new ProdutoEntity();
            
            produto.setNomeProduto(produtoDTO.getNomeProduto());
            produto.setMarca(produtoDTO.getMarca());
            produto.setGenero(produtoDTO.getGenero());
            produto.setLote(produtoDTO.getLote());
            produto.setDataFabricacao(produtoDTO.getDataFabricacao());
            produto.setDataValidade(produtoDTO.getDataValidade());
            produto.setProdutoDeletado(false);
            pedido.getProdutos().add(produto);
            
            return Optional.of(pedidoRepository.save(pedido));
        }
        
        return Optional.empty();
    }
    
    public Optional<PedidoEntity> removerProdutoDoPedido(Long idPedido, Long idProduto){
        var optPedido = pedidoRepository.findById(idPedido);
        
        if(optPedido.isPresent()) {
            var pedido = optPedido.get();
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
                
                return Optional.of(pedidoRepository.save(pedido));
            }
        }
        
        return Optional.empty();
    }
    
    //TODO
    //Implementar a deleção das relações na tabela (deletar pedido do cliente, por ex)
    public Boolean deletePedido(Long idPedido) {
        var optPedido = pedidoRepository.findById(idPedido);
        
        if(optPedido.isPresent()) {
            pedidoRepository.deleteById(idPedido);
            
            return Boolean.TRUE;
        }
        
        return Boolean.FALSE;
    }
    
    //TODO
    //Implementar a deleção das relações na tabela (deletar pedido do cliente, por ex)
    public Boolean deleteLogicPedido(Long idPedido) {
        var optPedido = pedidoRepository.findById(idPedido);
        
        if(optPedido.isPresent()) {
            var pedido = optPedido.get();
            
            pedido.setPedidoDeleted(Boolean.TRUE);
            pedidoRepository.save(pedido);
            return Boolean.TRUE;
        }
        
        return Boolean.FALSE;
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
