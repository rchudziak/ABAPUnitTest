interface ZIF_MATERIAL_CALCULATIONS
  public .


  methods CALCULATE_UNRESTRICTED_STOCK
    importing
      !I_MATERIAL type FIP_T_BAL_MAT_ID_RANGE
    returning
      value(R_CALCULATED_STOCK) type ZMAT_UNRESTR_TT
    raising
      ZCX_NO_DATA_ERR .
  methods CALCULATE_AVG_STOCK
    importing
      !I_MATERIAL type FIP_T_BAL_MAT_ID_RANGE
    returning
      value(R_AVG_STOCK) type ZMAT_AVG_STOCK_TT
    raising
      ZCX_NO_DATA_ERR .
endinterface.
