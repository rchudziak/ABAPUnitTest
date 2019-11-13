*&---------------------------------------------------------------------*
*& Report  ZMATERIAL_CALC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmaterial_calc.
TABLES mara.

SELECT-OPTIONS so_matnr FOR mara-matnr.

SELECTION-SCREEN : BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) text-001.
PARAMETERS: p_radio1 RADIOBUTTON GROUP rad1 DEFAULT 'X'.
SELECTION-SCREEN : END OF LINE.

SELECTION-SCREEN : BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) text-002.
PARAMETERS: p_radio2 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN : END OF LINE.


START-OF-SELECTION.

  DATA(material_calc) = NEW zcl_material_calculations( ).
  DATA: alv TYPE REF TO cl_salv_table.

  IF p_radio1 = abap_true.

    TRY.
        DATA(total_stock) = material_calc->zif_material_calculations~calculate_unrestricted_stock( so_matnr[] ).
      CATCH zcx_no_data_err.
    ENDTRY.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = alv
                                CHANGING  t_table      = total_stock ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
    ENDTRY.
  ELSEIF p_radio2 = abap_true.
    TRY.
        DATA(avg_stock) = material_calc->zif_material_calculations~calculate_avg_stock( so_matnr[] ).
      CATCH zcx_no_data_err.
    ENDTRY.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = alv
                                CHANGING  t_table      = avg_stock ).

      CATCH cx_salv_msg INTO lx_msg.
    ENDTRY.
  ENDIF.

  alv->display( ).
