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

import com.ufrn.imdMarket.dto.ClienteDTO;
import com.ufrn.imdMarket.entity.ClienteEntity;
import com.ufrn.imdMarket.service.ClienteService;

@RestController
@RequestMapping("/clientes")
public class ClienteController {    
    @Autowired
    private ClienteService clienteService;
    
    @GetMapping(value="/getAll", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<ClienteEntity>> getAllclientes(){
        return ResponseEntity.ok().body(clienteService.getAllClientes());
    }
    
    @GetMapping(value="/get/{idCliente}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Optional<ClienteEntity>> getById(@PathVariable Long idCliente){
        var cliente = clienteService.getCliente(idCliente);
        
        if(cliente.isPresent() && Boolean.FALSE.equals(cliente.get().getClienteDeleted())) {
            return ResponseEntity.ok().body(cliente);
        }
        
        return ResponseEntity.notFound().build();
    }
    
    @PostMapping(value="/postCliente", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ClienteEntity> postCliente(@RequestBody @Valid ClienteDTO clienteDTO){
        return ResponseEntity.ok().body(clienteService.cadastrarCliente(clienteDTO));
    }
    
    @PutMapping(value="/putCliente/{idCliente}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ClienteEntity> putCliente(@PathVariable Long idCliente, 
            @RequestBody @Valid ClienteDTO clienteDTO){
        var cliente = clienteService.atualizarCliente(idCliente, clienteDTO);
        
        if(cliente.isPresent()) {
            return ResponseEntity.ok().body(cliente.get());
        }
        
        return ResponseEntity.notFound().build();
    }
    
    @DeleteMapping(value="/deleteCliente/{idCliente}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ClienteEntity> deleteProduto(@PathVariable Long idCliente){
        var isClientDeleted = clienteService.deleteCliente(idCliente);
        
        return isClientDeleted ? ResponseEntity.ok().build() : ResponseEntity.notFound().build(); 
    }
    
    @DeleteMapping(value="/deleteCliente/logic/{idCliente}")
    public ResponseEntity<ClienteEntity> deleteLogic(@PathVariable Long idCliente){
        var isClienteLogicDeleted = clienteService.deleteLogicCliente(idCliente);

        return isClienteLogicDeleted ? ResponseEntity.ok().build() : ResponseEntity.notFound().build();
    }
}
