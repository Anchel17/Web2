package com.ufrn.imdMarket.security;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Date;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.auth0.jwt.JWT;
import com.auth0.jwt.algorithms.Algorithm;
import com.auth0.jwt.exceptions.JWTCreationException;
import com.auth0.jwt.exceptions.JWTVerificationException;
import com.ufrn.imdMarket.entity.UserEntity;

@Service
public class TokenService {
    
    @Value ("${api.security.token.secret}")
    private String secret;
    
    public String generateTokern(UserEntity user) {        
        try {
            var algorithm = Algorithm.HMAC256(secret);
            
            return  JWT.create()
                                    .withIssuer("imd-market")
                                    .withSubject(user.getLogin())
                                    .withExpiresAt(Date.from( this.generateExpirationDate()))
                                    .sign(algorithm);
            
        }
        catch(JWTCreationException e) {
            throw new RuntimeException("Erro ao gerar token JWT", e);
        }
    }
    
    public String validateToken(String token) {
        try {
            var algorithm = Algorithm.HMAC256(secret);
            
            return JWT.require(algorithm)
                            .withIssuer("imd-market")
                            .build()
                            .verify(token)
                            .getSubject();
        }
        catch(JWTVerificationException e) {
            return "";
        }
    }
    
    private Instant generateExpirationDate(){
        return LocalDateTime.now().plusHours(2).toInstant(ZoneOffset.of("-03:00"));
    }
}
