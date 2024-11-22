package com.ufrn.imdMarket.enums;

public enum GeneroClienteEnum {
    
    MASCULINO   ("MASCULINO"),
    FEMININO    ("FEMININO"),
    NAO_APLICA  ("NAO_APLICA");
    
    private String genero;
    
    GeneroClienteEnum(String genero){
        this.genero = genero;
    }
    
    public String getGenero() {
        return this.genero;
    }
}
