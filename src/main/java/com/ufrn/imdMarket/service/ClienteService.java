package com.ufrn.imdMarket.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.stereotype.Service;

import com.ufrn.imdMarket.dto.ClienteDTO;
import com.ufrn.imdMarket.entity.ClienteEntity;
import com.ufrn.imdMarket.repository.ClienteRepository;

@Service
public class ClienteService {
    private ClienteRepository clienteRepository;
    
    public List<ClienteEntity> getAllClientes() {
        var clientes = clienteRepository.findAll();
        List<ClienteEntity> listaFinalClientes = new ArrayList<>();
        
        clientes.forEach(c -> {
           if(Boolean.FALSE.equals(c.getClienteDeleted())) {
               listaFinalClientes.add(c);
           }
        });
        
        return listaFinalClientes;
    }
    
    public Optional<ClienteEntity> getCliente(Long idCliente) {
        return clienteRepository.findById(idCliente);
    }
    
    public ClienteEntity cadastrarCliente(ClienteDTO clienteDTO) {
        var cliente = new ClienteEntity();
        
        cliente.setCpf(clienteDTO.getCpf());
        cliente.setNome(clienteDTO.getNome());
        cliente.setGenero(clienteDTO.getGenero());
        cliente.setDataNascimento(clienteDTO.getDataNascimento());
        cliente.setClienteDeleted(false);
        
        return clienteRepository.save(cliente);
    }
    
    public Optional<ClienteEntity> atualizarCliente(Long idCliente, ClienteDTO clienteDTO) {
        
        var optCliente = clienteRepository.findById(idCliente);
        
        if(optCliente.isPresent()) {
            var cliente = optCliente.get();
            cliente.setCpf(clienteDTO.getCpf());
            cliente.setNome(clienteDTO.getNome());
            cliente.setGenero(clienteDTO.getGenero());
            cliente.setDataNascimento(clienteDTO.getDataNascimento());
            cliente.setClienteDeleted(false);
            
            return Optional.of(clienteRepository.save(cliente));
        }
        
        return Optional.empty();
    }
    
    public Boolean deleteCliente(Long idCliente) {
        if(clienteRepository.findById(idCliente).isPresent()) {
            clienteRepository.deleteById(idCliente);
            return Boolean.TRUE;
        }
        
        return Boolean.FALSE;
    }
    
    public Boolean deleteLogicCliente(Long idCliente) {
        var optCliente = clienteRepository.findById(idCliente); 
        
        if(optCliente.isPresent()) {
            var cliente = optCliente.get();
            
            cliente.setClienteDeleted(true);
            clienteRepository.save(cliente);
            
            return true;
        }
        
        return false;
    }
}
