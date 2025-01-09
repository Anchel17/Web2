package com.ufrn.imdMarket.controller;

import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.ufrn.imdMarket.dto.LoginDTO;
import com.ufrn.imdMarket.dto.RegisterDTO;
import com.ufrn.imdMarket.entity.UserEntity;
import com.ufrn.imdMarket.repository.UserRepository;
import com.ufrn.imdMarket.security.TokenService;

@RestController
@RequestMapping("/auth")
public class AuthenticationController {
    
    @Autowired
    private AuthenticationManager authenticationManager;
    
    @Autowired
    private UserRepository userRepository;
    
    @Autowired
    private TokenService tokenService;
    
    @PostMapping("/login")
    public ResponseEntity login(@RequestBody @Valid LoginDTO loginDTO) {
        var usernamePassword = new UsernamePasswordAuthenticationToken(loginDTO.getLogin(), loginDTO.getPassword());
        
        var auth = authenticationManager.authenticate(usernamePassword);
        
        var token = tokenService.generateTokern((UserEntity) auth.getPrincipal()); 
        
        return ResponseEntity.ok(token);
    }
    
    @PostMapping("/register")
    public ResponseEntity register(@RequestBody @Valid RegisterDTO registerDTO) {
        if(this.userRepository.findByLogin(registerDTO.getLogin()) != null) {
            return ResponseEntity.badRequest().build();
        }
        
        var encryptedPassword = new BCryptPasswordEncoder().encode(registerDTO.getPassword());
        
        var user = new UserEntity();
        user.setLogin(registerDTO.getLogin());
        user.setPassword(encryptedPassword);
        user.setRole(registerDTO.getRole());
        
        this.userRepository.save(user);
        
        return ResponseEntity.ok().build();
    }
}
