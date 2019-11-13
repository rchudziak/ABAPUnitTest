class ZCL_MATERIAL_DB_SEL definition
  public
  create public .

public section.

  interfaces ZIF_MATERIAL_DB_SEL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MATERIAL_DB_SEL IMPLEMENTATION.


  METHOD zif_material_db_sel~get_material_stock.

    SELECT a~matnr,
           a~werks,
           a~lgort,
           a~charg,
           a~clabs,
           b~meins
      INTO TABLE @r_stock
      FROM mchb AS a INNER JOIN mara AS b ON a~matnr = b~matnr
      WHERE a~matnr IN @i_material
      AND   a~clabs > 0.

  ENDMETHOD.
ENDCLASS.
