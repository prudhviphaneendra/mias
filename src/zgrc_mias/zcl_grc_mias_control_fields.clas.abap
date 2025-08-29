CLASS zcl_grc_mias_control_fields DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

 PUBLIC SECTION.
  INTERFACES if_sadl_exit .
    INTERFACES if_sadl_exit_calc_element_read .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GRC_MIAS_CONTROL_FIELDS IMPLEMENTATION.


 METHOD if_sadl_exit_calc_element_read~calculate.

    IF it_requested_calc_elements IS INITIAL.
      EXIT.
    ENDIF.

    DATA : lt_original_data   TYPE STANDARD TABLE OF zc_grc_mi_claimreg WITH DEFAULT KEY,
           lt_fieldname_range TYPE RANGE OF ztgrc_scr_contrl-fieldname.


      lt_original_data = CORRESPONDING #( it_original_data ).
      DATA(lv_record_type) =  VALUE #(   lt_original_data[ 1 ]-recordtype OPTIONAL ).


    SELECT *
    FROM ztgrc_scr_contrl
    WHERE record_type = @lv_record_type
    INTO TABLE @DATA(lt_scr_control).
    IF sy-subrc IS INITIAL.

      LOOP AT lt_scr_control ASSIGNING FIELD-SYMBOL(<lfs_control>)
      WHERE control = 'H'.

        APPEND VALUE #(  sign = 'I'
                         option = 'EQ'
                         low = 'HIDE' && to_upper(  <lfs_control>-fieldname ) )
        TO lt_fieldname_range.

      ENDLOOP.

    ENDIF.



       LOOP AT lt_original_data ASSIGNING FIELD-SYMBOL(<ls_original_data>).
        LOOP AT it_requested_calc_elements ASSIGNING FIELD-SYMBOL(<ls_req_calc_elements>) .
          IF NOT line_exists(  lt_fieldname_range[  low  = <ls_req_calc_elements> ] ).
            CONTINUE.
          ENDIF.
          ASSIGN COMPONENT <ls_req_calc_elements> OF STRUCTURE <ls_original_data>
      TO FIELD-SYMBOL(<field_value>).
          IF sy-subrc IS INITIAL AND <field_value> IS ASSIGNED.
            <field_value> = abap_true.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      ct_calculated_data = CORRESPONDING #( lt_original_data ).


  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
  ENDMETHOD.
ENDCLASS.
