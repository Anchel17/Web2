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
import com.ufrn.imdMarket.entity.ClienteEntity;
import com.ufrn.imdMarket.repository.ClienteRepository;

@RestController
@RequestMapping("/clientes")
public class ClienteController {
    @Autowired
    private ClienteRepository clienteRepository;
    
    @GetMapping(value="/getAll", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<ClienteEntity>> getAllclientes(){
        var clientes = clienteRepository.findAll();
        List<ClienteEntity> listaFinalClientes = new ArrayList<>();
        
        clientes.forEach(c -> {
           if(Boolean.FALSE.equals(c.getClienteDeleted())) {
               listaFinalClientes.add(c);
           }
        });
        
        return ResponseEntity.ok().body(listaFinalClientes);
    }
    
    @GetMapping(value="/get/{idCliente}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Optional<ClienteEntity>> getById(@PathVariable Long idCliente){
        var cliente = clienteRepository.findById(idCliente);
        
        if(cliente.isPresent()) {
            if(Boolean.FALSE.equals(cliente.get().getClienteDeleted())) {
                return ResponseEntity.ok().body(cliente);
            }
        }
        
        return ResponseEntity.notFound().build();
    }
    
    @PostMapping(value="/postCliente", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ClienteEntity> postCliente(@RequestBody ClienteDTO clienteDTO){
        var cliente = new ClienteEntity();
        
        cliente.setCpf(clienteDTO.getCpf());
        cliente.setNome(clienteDTO.getNome());
        cliente.setGenero(clienteDTO.getGenero());
        cliente.setDataNascimento(clienteDTO.getDataNascimento());
        cliente.setClienteDeleted(false);
        
        return ResponseEntity.ok().body(clienteRepository.save(cliente));
    }
    
    @PutMapping(value="/putCliente/{idCliente}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ClienteEntity> putCliente(@PathVariable Long idCliente, @RequestBody ClienteDTO clienteDTO){
        var cliente = new ClienteEntity();
        
        cliente.setId(idCliente);
        cliente.setCpf(clienteDTO.getCpf());
        cliente.setNome(clienteDTO.getNome());
        cliente.setGenero(clienteDTO.getGenero());
        cliente.setDataNascimento(clienteDTO.getDataNascimento());
        cliente.setClienteDeleted(false);
        
        return ResponseEntity.ok().body(clienteRepository.save(cliente));
    }
    
    @DeleteMapping(value="/deleteCliente/{idCliente}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ClienteEntity> deleteProduto(@PathVariable Long idCliente){
        clienteRepository.deleteById(idCliente);
        
        return ResponseEntity.ok().build();
    }
    
    @DeleteMapping(value="/deleteCliente/logic/{idCliente}")
    public ResponseEntity<ClienteEntity> deleteLogic(@PathVariable Long idCliente){
        var cliente = clienteRepository.findById(idCliente);
        
        cliente.ifPresent(c -> {
            c.setClienteDeleted(true);
            clienteRepository.save(c);
        });
        
        return ResponseEntity.ok().build();
    }
}
