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
        
        if(Boolean.FALSE.equals(optCliente.get().getId().equals(optPedido.get().getCliente().getId()))) {
            throw new IllegalArgumentException("Operação não permitida, não pode passar seus pedidos para outro cliente.");
        }

        if(optPedido.isPresent()) {
            var pedido = optPedido.get();
            pedido.setCodigo(pedidoDTO.getCodigo());
            pedido.setCliente(optCliente.get());
            pedido.setProdutos(buildListProdutos(pedidoDTO.getProdutos()));
            pedido.setPedidoDeleted(false);
            associarProdutosAPedido(pedido, pedido.getProdutos());
            
            return Optional.of(pedido);
        }
        
        return Optional.empty();   
    }
    
    @Transactional
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
            produto.setPedido(pedido);
            pedido.getProdutos().add(produto);
            
            produtoRepository.save(produto);
            return Optional.of(pedidoRepository.save(pedido));
        }
        
        return Optional.empty();
    }
    
    @Transactional
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
                var produto = pedido.getProdutos().get(indice);
                produto.setPedido(null);
                pedido.getProdutos().remove(indice);
                
                produtoRepository.save(produto);
                return Optional.of(pedidoRepository.save(pedido));
            }
        }
        
        return Optional.empty();
    }
    
    @Transactional
    public Boolean deletePedido(Long idPedido) {
        var optPedido = pedidoRepository.findById(idPedido);
        
        if(optPedido.isPresent()) {
            excluirRelacoesComPedido(optPedido.get());
            pedidoRepository.deleteById(idPedido);
            
            return Boolean.TRUE;
        }
        
        return Boolean.FALSE;
    }
    
    @Transactional
    public Boolean deleteLogicPedido(Long idPedido) {
        var optPedido = pedidoRepository.findById(idPedido);
        
        if(optPedido.isPresent()) {
            var pedido = optPedido.get();
            excluirRelacoesComPedido(optPedido.get());
            
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
    
    private void excluirRelacoesComPedido(PedidoEntity pedido) {
        excluirPedidoDeCliente(pedido);
        excluirPedidoDeProduto(pedido);
    }
    
    private void excluirPedidoDeCliente(PedidoEntity pedido) {
        var cliente = pedido.getCliente();  
        var indice = 0;
        var achou = Boolean.FALSE;
        
        for(var i = 0; i < cliente.getPedidos().size(); i++) {
            if(cliente.getPedidos().get(i).getId().equals(pedido.getId())) {
                indice = i;
                achou = Boolean.TRUE;
                break;
            }
        }
        
        if(Boolean.TRUE.equals(achou)) {
            cliente.getPedidos().remove(indice);
            clienteRepository.save(cliente);
        }
    }
    
    private void excluirPedidoDeProduto(PedidoEntity pedido) {
        var produtos = pedido.getProdutos();
        produtos.forEach(produto -> {
            produto.setPedido(null);
        });
        
        produtoRepository.saveAll(produtos);
    }
}
