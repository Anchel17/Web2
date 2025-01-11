package com.ufrn.imdMarket.security;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

import static org.springframework.boot.autoconfigure.security.servlet.PathRequest.toH2Console;


/*A Anotação EnableWebSecurity diz para o Spring Security habilitar o Web Security
 * que vai ser configurado nessa classe, retirando as configurações padrões
 *  */
@Configuration
@EnableWebSecurity
public class SecurityConfig {
    
    @Autowired
    private SecurityFilter securityFilter;
    
    /*
     * O authorizeResquest ficou diferente por causa da versão do Spring
     * */
    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity httpSecurity) throws Exception {
        return httpSecurity
                        .csrf(csrf -> csrf.disable())
                        .sessionManagement(session -> session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                        .headers(headers -> headers.frameOptions().disable())
                        .authorizeRequests()
                            .requestMatchers(toH2Console()).permitAll()
                            .antMatchers(HttpMethod.POST, "/auth/register").permitAll()
                            .antMatchers(HttpMethod.POST, "/auth/login").permitAll()
                            .antMatchers(HttpMethod.POST).hasRole("ADMIN")
                            .antMatchers(HttpMethod.PUT).hasRole("ADMIN")
                            .antMatchers(HttpMethod.DELETE).hasRole("ADMIN")
                            .antMatchers(HttpMethod.GET).hasRole("ADMIN")
                            .antMatchers(HttpMethod.GET).hasRole("USER")
                            .anyRequest().authenticated()
                            .and()
                        .addFilterBefore(securityFilter, UsernamePasswordAuthenticationFilter.class)
                        .build();
    }
    
    /*
     * No AuthenticationConfiguration o Spring Security vai utilizar desse Manager para 
     * fazer a autenticação
     * */
    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration authenticationConfiguration) throws Exception {
        return authenticationConfiguration.getAuthenticationManager();
    }
    
    /*
     * Criptografa e descriptograva as senhas para salvar ou resgatar do banco
     * */
    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }
}
