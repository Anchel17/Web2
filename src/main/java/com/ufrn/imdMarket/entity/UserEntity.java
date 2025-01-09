package com.ufrn.imdMarket.entity;

import java.util.Collection;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import com.ufrn.imdMarket.enums.RoleEnum;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Table(name="USER_ENTITY")
public class UserEntity  implements UserDetails{
    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy=GenerationType.SEQUENCE)
    @Column(name="ID_USER", nullable=false)
    private Long id;
    
    @Column(name="LOGIN", nullable=false)
    private String login;
    
    @Column(name="PASSWORD", nullable=false)
    private String password;
    
    @Column(name="ROLE", nullable=false)
    private RoleEnum role;
    
    
    /*
     * O SpringSecurity consulta nossa entidade para ver quais são as Roles que esse usuário tem
     * O Spring tem suas Roles, então precisamos fazer um de-para das nossas Roles para as do Spring
     * Um usuário que tem permissão de admin, também tem de usuário normal, por isso precisamos retornar uma
     * coleção de Roles para o Spring
     * */
    @Override
    public Collection<? extends GrantedAuthority> getAuthorities(){
        if(role == RoleEnum.ADMIN) {
            return List.of(new SimpleGrantedAuthority("ROLE_ADMIN"), new SimpleGrantedAuthority("ROLE_USER"));
        }
        
        return List.of(new SimpleGrantedAuthority("ROLE_USER"));
    }

    @Override
    public String getUsername() {
        return this.login;
    }
    
    @Override
    public String getPassword() {
        return this.password;
    }

    @Override
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    public boolean isAccountNonLocked() {        
        return true;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    public boolean isEnabled() {
        return true;
    }
    
    
    
}
