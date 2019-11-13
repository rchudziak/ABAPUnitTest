class ZCL_MATERIAL_CALCULATIONS definition
  public
  create public .

public section.

  interfaces ZIF_MATERIAL_CALCULATIONS .

  methods CONSTRUCTOR
    importing
      !IO_MATERIAL_DB_SEL type ref to ZIF_MATERIAL_DB_SEL optional .
protected section.
private section.

  data GO_MATERIAL_DB_SEL type ref to ZIF_MATERIAL_DB_SEL .
ENDCLASS.



CLASS ZCL_MATERIAL_CALCULATIONS IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    IF io_material_db_sel IS SUPPLIED.
      go_material_db_sel = io_material_db_sel.
    ELSE.
      go_material_db_sel = NEW zcl_material_db_sel( ).
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_MATERIAL_CALCULATIONS~CALCULATE_AVG_STOCK.

    DATA ls_avg_stock TYPE zmat_avg_stock.
    DATA lv_total TYPE mchb-clabs.
    DATA lv_matnr TYPE mara-matnr.
    DATA lt_unique_plants TYPE zmat_stock_tt.

    DATA(lt_stock) = go_material_db_sel->get_material_stock( i_material ).

    IF lt_stock IS INITIAL.
      RAISE EXCEPTION TYPE zcx_no_data_err
        EXPORTING
          textid = VALUE #( msgid = 'ZKMM' msgno = '999' ).
    ENDIF.

    LOOP AT lt_stock ASSIGNING FIELD-SYMBOL(<fs_material>).
      AT NEW matnr.
        CLEAR lv_total.
        ls_avg_stock-material = <fs_material>-matnr.
        ls_avg_stock-uom = <fs_material>-uom.
      ENDAT.

      lv_total = lv_total + <fs_material>-clabs.

      AT END OF matnr.

* Get a number of unique plants
        lt_unique_plants = VALUE #( FOR stock IN lt_stock WHERE ( matnr = <fs_material>-matnr )
        ( CORRESPONDING #( stock MAPPING werks = werks ) ) ).
        SORT lt_unique_plants BY werks.
        DELETE ADJACENT DUPLICATES FROM lt_unique_plants COMPARING werks.

* Calculate the acerage stock
        ls_avg_stock-average_stock = lv_total / lines( lt_unique_plants ).
        APPEND ls_avg_stock TO r_avg_stock.
        CLEAR ls_avg_stock.
        REFRESH lt_unique_plants.
      ENDAT.
    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_MATERIAL_CALCULATIONS~CALCULATE_UNRESTRICTED_STOCK.
    DATA ls_stock TYPE zmat_unrestr.
    DATA(lt_stock) = go_material_db_sel->get_material_stock( i_material ).

    IF lt_stock IS INITIAL.
      RAISE EXCEPTION TYPE zcx_no_data_err
        EXPORTING
          textid = VALUE #( msgid = 'ZKMM' msgno = '999' ).
    ENDIF.

*   Calculate Unrestricted stock
    LOOP AT lt_stock ASSIGNING FIELD-SYMBOL(<fs_material>).

      AT NEW matnr.
        ls_stock-material = <fs_material>-matnr.
        ls_stock-uom = <fs_material>-uom.
      ENDAT.

      ls_stock-unrestr_stock = ls_stock-unrestr_stock + <fs_material>-clabs.

      AT END OF matnr.
        APPEND ls_stock TO r_calculated_stock.
        CLEAR ls_stock.
      ENDAT.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
