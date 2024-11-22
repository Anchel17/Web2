package com.ufrn.imdMarket.enums;

public enum GeneroProdutoEnum {
    COSMETICO   ("COSMETICO"),
    ALIMENTICIO ("ALIMENTICIO"),
    HIGIENE     ("HIGIENE"),
    PESSOAL     ("PESSOAL"),
    LIMPEZA     ("LIMPEZA");
    
    private String descricao;
    
    GeneroProdutoEnum(String genero){
        this.descricao = genero;
    }
    
    public String getDescricao() {
        return this.descricao;
    }
}
