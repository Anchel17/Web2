package com.ufrn.imdMarket.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.security.core.userdetails.UserDetails;

import com.ufrn.imdMarket.entity.UserEntity;

public interface UserRepository extends JpaRepository<UserEntity, Long>{
    UserDetails findByLogin(String login);
}
