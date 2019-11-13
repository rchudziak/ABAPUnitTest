CLASS ltd_material_db_sel  DEFINITION FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_material_db_sel PARTIALLY IMPLEMENTED.

ENDCLASS.

*CLASS ltd_material_db_sel IMPLEMENTATION.
*  METHOD zif_material_db_sel~get_material_stock.
*
*    DATA(lt_data_table) = VALUE zmat_stock_tt( ( matnr = '000000000010037637' werks = 'EU33' lgort = 'EU33' charg = 'FC-NEW' clabs = '2.000' uom = 'PC' )
*                                               ( matnr = '000000000010037637' werks = 'NG01' lgort = 'NG01' charg = 'FC-NEW' clabs = '2.000' uom = 'PC' )
*                                               ( matnr = '000000000010055771' werks = 'DZ01' lgort = 'C04'  charg = 'FC-NEW' clabs = '35.000' uom = 'PC' )
*                                               ( matnr = '000000000010055771'  werks = 'EU34' lgort = 'EU34' charg = 'FC-NEW' clabs = '4.000' uom = 'PC' )
*                                               ( matnr = '000000000010055771'  werks = 'EU36' lgort = 'EU36' charg = 'FC-NEW' clabs = '20.000' uom = 'PC' )
*                                               ( matnr = '000000000010055771'  werks = 'EU41' lgort = 'EU41' charg = 'FC-NEW' clabs = '40.000' uom = 'PC' )
*                                               ( matnr = '000000000010055771'  werks = 'EU52' lgort = 'EU52' charg = 'FC-NEW' clabs = '17.000' uom = 'PC' )
*                                               ( matnr = '000000000010055771' werks = 'IQ12' lgort = 'C01'  charg = 'FC-NEW' clabs = '38.000' uom = 'PC' )
*                                               ( matnr = '000000000010055771' werks = 'KZ10' lgort = 'C02'  charg = 'FC-NEW' clabs = '3.000' uom = 'PC' )
*                                               ( matnr = '000000000010055771'  werks = 'NG01' lgort = 'NG01' charg = 'FC-NEW' clabs = '15.000' uom = 'PC' )
*                                               ( matnr = '000000000010055771' werks = 'OM12' lgort = 'C01'  charg = 'FC-NEW' clabs = '10.000' uom = 'PC' )
*                                               ( matnr = '000000000010055771' werks = 'OM17' lgort = 'C01'  charg = 'FC-NEW' clabs = '57.000' uom = 'PC' )
*                                               ( matnr = '000000000010055771' werks = 'RU80' lgort = 'C03'  charg = 'FC-NEW' clabs = '25.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'DZ01' lgort = 'DZ03'  charg = 'FC-NEW' clabs = '8.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'EU13' lgort = 'C02'  charg = 'FC-NEW' clabs = '10.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'EU34' lgort = 'EU34'  charg = 'FC-NEW' clabs = '4.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'EU52' lgort = 'EU52'  charg = 'FC-NEW' clabs = '8.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'IQ11' lgort = 'C03'  charg = 'FC-NEW' clabs = '24.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'KZ10' lgort = 'C02'  charg = 'FC-NEW' clabs = '6.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'NG01' lgort = 'NG01'  charg = 'FC-NEW' clabs = '6.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'OM15' lgort = 'C01'  charg = 'FC-NEW' clabs = '8.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'OM17' lgort = 'C01'  charg = 'FC-NEW' clabs = '1.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'OM20' lgort = 'C01'  charg = 'FC-NEW' clabs = '1.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'OM23' lgort = 'C01'  charg = 'FC-NEW' clabs = '5.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'OM23' lgort = 'C02'  charg = 'FC-NEW' clabs = '13.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'PK10' lgort = 'C05'  charg = 'FC-NEW' clabs = '5.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'PK11' lgort = 'C08'  charg = 'FC-NEW' clabs = '4.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'RU80' lgort = 'C03'  charg = 'FC-NEW' clabs = '8.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'RU85' lgort = 'C04'  charg = 'FC-NEW' clabs = '5.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'RU86' lgort = 'C03'  charg = 'FC-NEW' clabs = '7.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'RU89' lgort = 'C02'  charg = 'FC-NEW' clabs = '3.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'RU91' lgort = 'C03'  charg = 'FC-NEW' clabs = '5.000' uom = 'PC' )
*                                               ( matnr = '000000000010055691' werks = 'RU96' lgort = 'C03'  charg = 'FC-NEW' clabs = '17.000' uom = 'PC' ) ).
*
*
*    LOOP AT lt_data_table ASSIGNING FIELD-SYMBOL(<fs_data_table>) WHERE matnr IN i_material.
*      APPEND <fs_data_table> TO r_stock.
*    ENDLOOP.
*  ENDMETHOD.
*ENDCLASS.

CLASS ltd_material_db_sel IMPLEMENTATION.
  METHOD zif_material_db_sel~get_material_stock.

    DATA lo_ml      TYPE REF TO zcl_mockup_loader.
    DATA: BEGIN OF lt_where,
            matnr TYPE RANGE OF matnr,
          END OF lt_where.

* Mockup loader instance is created and bind to the source of mockups
    lo_ml = zcl_mockup_loader=>create(
    i_type = 'MIME'
    i_path = 'ZMOCKDATA_STOCK' ). " <YOUR MOCKUP>

* Build a filter for specified materials
    lt_where-matnr = i_material.

    lo_ml->load_data( " Load test data (table) from mockup
     EXPORTING
       i_obj       = 'EXAMPLE/stock'
       i_strict    = abap_false
       i_where     = lt_where
     IMPORTING
       e_container = r_stock ).

  ENDMETHOD.
ENDCLASS.

CLASS ltc_material_calculations DEFINITION FOR TESTING
                                DURATION SHORT
                                RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_values,
             matnr TYPE matnr,
             value TYPE labst,
           END OF ty_values.
    TYPES tt_values TYPE  TABLE OF ty_values WITH EMPTY KEY.
    DATA:
      f_cut          TYPE REF TO zcl_material_calculations,
      gt_exp_tot_val TYPE tt_values,
      gt_exp_avg_val TYPE tt_values.  "class under test


    METHODS: setup.
    METHODS: teardown.
    METHODS: verify_success_total_single FOR TESTING.
    METHODS: verify_success_total_ranges FOR TESTING.
    METHODS: verify_exceptions FOR TESTING.
    METHODS: verify_success_avg_single FOR TESTING.
    METHODS: verify_success_avg_ranges FOR TESTING.
    METHODS: add_user_input IMPORTING i_material TYPE matnr
                            CHANGING  c_input    TYPE fip_t_bal_mat_id_range.
    METHODS: calc_stock
      IMPORTING i_material TYPE fip_t_bal_mat_id_range
                i_msg      TYPE csequence.
    METHODS: calc_stock_error
      IMPORTING i_material TYPE fip_t_bal_mat_id_range
                i_msg      TYPE csequence.
    METHODS: calc_avg_stock
      IMPORTING i_material TYPE fip_t_bal_mat_id_range
                i_msg      TYPE csequence.


ENDCLASS.       "ltc_Material_Calculations


CLASS ltc_material_calculations IMPLEMENTATION.

  METHOD setup.

* Expected values for total material testing
    gt_exp_tot_val =  VALUE #( ( matnr = '000000000010037637' value = 4 )
                               ( matnr = '000000000010055771' value = 264 )
                               ( matnr = '000000000010055691' value = 148 ) ).

* Expected values for average material testing
    gt_exp_avg_val =  VALUE #( ( matnr = '000000000010037637' value = 2 )
                               ( matnr = '000000000010055771' value = 24 )
                               ( matnr = '000000000010055691' value = '7.789' ) ).

*   Constructor injection of fake DB access class
    f_cut = NEW #( NEW ltd_material_db_sel( ) ).

  ENDMETHOD.

  METHOD teardown.
    FREE f_cut.
  ENDMETHOD.

  METHOD add_user_input.
    DATA lv_matnr TYPE matnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_material
      IMPORTING
        output = lv_matnr.

    APPEND VALUE #(  sign = 'I' option = 'EQ' low = lv_matnr high = ''   ) TO c_input.

  ENDMETHOD.

********Testing total stock methods***********
  METHOD calc_stock.
    DATA lt_calculated_stock TYPE zmat_unrestr_tt.

    lt_calculated_stock = f_cut->zif_material_calculations~calculate_unrestricted_stock( i_material ).

    LOOP AT lt_calculated_stock ASSIGNING FIELD-SYMBOL(<fs_calculated_stock>).

      cl_abap_unit_assert=>assert_equals(
        act   = <fs_calculated_stock>-unrestr_stock
        exp   = gt_exp_tot_val[ matnr = <fs_calculated_stock>-material ]-value
        msg   = i_msg ).

    ENDLOOP.
  ENDMETHOD.

  METHOD verify_success_total_single.

    DATA lt_10037637 TYPE fip_t_bal_mat_id_range.
    DATA lt_10055771 TYPE fip_t_bal_mat_id_range.
    DATA lt_10055691 TYPE fip_t_bal_mat_id_range.


    add_user_input( EXPORTING i_material = '10037637'
                    CHANGING  c_input = lt_10037637 ).
    calc_stock( i_material = lt_10037637 i_msg = |Testing stock for single material 10037637| ).


    add_user_input( EXPORTING i_material = '10055771'
                    CHANGING  c_input = lt_10037637 ).
    calc_stock( i_material = lt_10037637 i_msg = |Testing stock for single material 10055771| ).

    add_user_input( EXPORTING i_material = '10055691'
                    CHANGING  c_input = lt_10055691 ).
    calc_stock( i_material = lt_10037637 i_msg = |Testing stock for single material 10055691| ).

  ENDMETHOD.

  METHOD verify_success_total_ranges.
    DATA(lr_range) = VALUE fip_t_bal_mat_id_range( ( sign = 'I' option = 'EQ' low = '000000000010037637' high = ''  )
                                                   ( sign = 'I' option = 'EQ' low = '000000000010055771' high = ''  )
                                                   ( sign = 'I' option = 'EQ' low = '000000000010055691' high = '' ) ).

    calc_stock( i_material = lr_range  i_msg = |Testing stock for range of materials| ).
  ENDMETHOD.

  METHOD calc_stock_error.
    DATA lt_calculated_stock TYPE zmat_unrestr_tt.

    TRY.
        lt_calculated_stock = f_cut->zif_material_calculations~calculate_unrestricted_stock( i_material ).

        cl_abap_unit_assert=>fail( msg = i_msg ).

      CATCH zcx_no_data_err.
    ENDTRY.

  ENDMETHOD.

  METHOD verify_exceptions.
    DATA(lr_10055692) = VALUE fip_t_bal_mat_id_range( ( sign = 'I' option = 'EQ' low = '000000000010055692' high = ''  ) ).

    calc_stock_error( i_material = lr_10055692  i_msg = |ZCX_NO_DATA_ERR not raised| ).
  ENDMETHOD.


********Testing avg stock methods***********
  METHOD calc_avg_stock.
    DATA lt_avg_stock TYPE zmat_avg_stock_tt.

    lt_avg_stock = f_cut->zif_material_calculations~calculate_avg_stock( i_material ).

    LOOP AT lt_avg_stock ASSIGNING FIELD-SYMBOL(<fs_avg_stock>).

      cl_abap_unit_assert=>assert_equals(
        act   = <fs_avg_stock>-average_stock
        exp   = gt_exp_avg_val[ matnr = <fs_avg_stock>-material ]-value
        msg   = i_msg ).

    ENDLOOP.
  ENDMETHOD.

  METHOD verify_success_avg_single.

    DATA(lr_10037637) = VALUE fip_t_bal_mat_id_range( ( sign = 'I' option = 'EQ' low = '000000000010037637' high = ''  ) ).
    calc_avg_stock( i_material = lr_10037637  i_msg = '10037637 avg stock incorrect' ).

    DATA(lr_10055771) = VALUE fip_t_bal_mat_id_range( ( sign = 'I' option = 'EQ' low = '000000000010055771' high = ''  ) ).
    calc_avg_stock( i_material = lr_10055771  i_msg = '10055771 avg stock incorrect' ).

    DATA(lr_10055691) = VALUE fip_t_bal_mat_id_range( ( sign = 'I' option = 'EQ' low = '000000000010055691' high = ''  ) ).
    calc_avg_stock( i_material = lr_10055691  i_msg = '10055691 avg stock incorrect' ).
  ENDMETHOD.


  METHOD verify_success_avg_ranges.

    DATA(lr_ranges) = VALUE fip_t_bal_mat_id_range( ( sign = 'I' option = 'EQ' low = '000000000010037637' high = ''  )
                                                    ( sign = 'I' option = 'EQ' low = '000000000010055771' high = ''  )
                                                    ( sign = 'I' option = 'EQ' low = '000000000010055691' high = ''  ) ).

    calc_avg_stock( i_material = lr_ranges  i_msg = 'Testing avg stock ranges' ).
  ENDMETHOD.

ENDCLASS.
