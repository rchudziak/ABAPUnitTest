interface ZIF_MATERIAL_DB_SEL
  public .


  methods GET_MATERIAL_STOCK
    importing
      !I_MATERIAL type FIP_T_BAL_MAT_ID_RANGE
    returning
      value(R_STOCK) type ZMAT_STOCK_TT
    raising
      ZCX_NO_DATA_ERR .
endinterface.
