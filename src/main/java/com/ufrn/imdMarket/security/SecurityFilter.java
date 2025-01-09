package com.ufrn.imdMarket.security;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import com.ufrn.imdMarket.repository.UserRepository;

@Component
public class SecurityFilter extends OncePerRequestFilter{
    @Autowired
    private TokenService tokenService;
    
    @Autowired
    private UserRepository userRepository;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {
        var token = this.recoverToken(request);
        
        if(token != null) {
            var login = tokenService.validateToken(token);
            
            var userDetails = userRepository.findByLogin(login);
            
            var authentication = new UsernamePasswordAuthenticationToken(userDetails, null, userDetails.getAuthorities());
            SecurityContextHolder.getContext().setAuthentication(authentication);
        }
        
        filterChain.doFilter(request, response);
    }
    
    private String recoverToken(HttpServletRequest request) {
        var authHeader = request.getHeader("Authorization");
        
        if(authHeader == null) {
            return null;
        }
        
        return authHeader.replace("Bearer ", "");
    }
}
