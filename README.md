# Advance-Shipment-Notification
Advance Shipment Notification Integration ( Inbound )
It is an Inbound Flow from Sharepoint to SAP

  METHOD if_rest_resource~post.

*-----------------------------Data Declarations For Function Module inputs ---------------------------

    DATA : lt_xkomdlgn TYPE TABLE OF komdlgn,
           ls_xkomdlgn TYPE komdlgn,
           lt_xvbfs    TYPE TABLE OF vbfs,
           ls_xvbfs    TYPE vbfs,
           lt_xvbls    TYPE TABLE OF vbls,
           ls_xvbls    TYPE vbls,
           lt_xxlips   TYPE TABLE OF lips,
           ls_xxlips   TYPE lips,
           ls_vbsk_i   TYPE vbsk,
           asn_data    TYPE zst_asn.

    DATA: lt_ret       TYPE TABLE OF bapiret2,
          ls_header    TYPE bapiibdlvhdrchg,
          ls_control   TYPE bapiibdlvhdrctrlchg,
          lv_return    TYPE bapiret2,
          lt_item      TYPE TABLE OF bapiibdlvitemchg,
          ls_item      TYPE bapiibdlvitemchg,
          lt_control_i TYPE TABLE OF bapiibdlvitemctrlchg,
          ls_control_i TYPE bapiibdlvitemctrlchg.

*-----------------------------Data Declarations For Conversion ---------------------------
    DATA : lv_pono    TYPE ebeln,
           lv_asn(10) TYPE c,
           lv_vendor  TYPE lifnr,
           lv_date    TYPE datum,
           lv_date1   TYPE datum,
           lv_date2   TYPE datum,
           lv_msg(80) TYPE c.

    TYPES : BEGIN OF ty_output,
              lv_msg(40) TYPE c,
              lv_vbelnib TYPE vbeln,
            END OF ty_output.

    DATA : ls_output TYPE ty_output,
           lt_output TYPE TABLE OF ty_output.

*-----------------------------Data Declarations For ASN details ---------------------------
    DATA :ls_data          TYPE zst_asn,
          ls_asn_data      TYPE zst_asn,
          lt_asn_data      TYPE TABLE OF zst_asn,
          ls_asn_converted TYPE zst_asn,
          lt_asn_converted TYPE TABLE OF zst_asn,
          ls_asn_final     TYPE zasn_create,
          lt_asn_final     TYPE TABLE OF zasn_create.

    DATA(lo_entity) = mo_request->get_entity( ).
    DATA(lt_asn_rawdata)  = io_entity->get_string_data( ).      " get data from Portal


    CALL METHOD /ui2/cl_json=>deserialize(                     "Convert Raw data to Internal table
      EXPORTING
        json        = lt_asn_rawdata
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = lt_asn_data   "ls_data
    ).

*-----------------------------Fetch data from EKPO for function module inputs  ---------------------------

    SELECT ebeln,
           ebelp,
           matnr,
           menge,
           werks,
           lgort,
           inco1,
           inco2,
           bwtar
          FROM ekpo INTO TABLE @DATA(lt_fmdet)
          FOR ALL ENTRIES IN @lt_asn_data
          WHERE ebeln = @lt_asn_data-pono
          AND ebelp = @lt_asn_data-poline.

    SORT lt_fmdet BY ebeln ebelp.

*--------------------------------------------------Loop for ASN creation inputs from Internal table--------------------------------------------------------------

    LOOP AT lt_asn_data INTO ls_asn_data.

      CLEAR:ls_xkomdlgn,ls_asn_converted.

      MOVE-CORRESPONDING ls_asn_data TO ls_asn_converted.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'          "FM for add leading zero
        EXPORTING
          input  = ls_asn_data-pono
        IMPORTING
          output = ls_asn_converted-pono.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'          "FM for add leading zero
        EXPORTING
          input  = ls_asn_data-vendor
        IMPORTING
          output = ls_asn_converted-vendor.


      CALL FUNCTION 'CONVERSION_EXIT_IDATE_INPUT'    "FM for convert ASN date format
        EXPORTING
          input  = ls_asn_data-asndate
        IMPORTING
          output = ls_asn_converted-asndate.


      CALL FUNCTION 'CONVERSION_EXIT_IDATE_INPUT'    "FM for convert PO date format
        EXPORTING
          input  = ls_asn_data-podate
        IMPORTING
          output = ls_asn_converted-podate.

      CALL FUNCTION 'CONVERSION_EXIT_IDATE_INPUT'   "FM for convert Supplier date format
        EXPORTING
          input  = ls_asn_data-supplier_date
        IMPORTING
          output = ls_asn_converted-supplier_date.

      READ TABLE lt_fmdet INTO DATA(ls_fmdet) WITH KEY ebeln = ls_asn_converted-pono
                                                       ebelp = ls_asn_converted-poline
                                                       matnr = ls_asn_converted-materialno
                                                       werks = ls_asn_converted-plant
                                                       BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        IF ls_fmdet-menge LE ls_asn_converted-qty.

          lv_msg = TEXT-112.

        ELSE.

          ls_xkomdlgn-lfart  = 'EL'.     "select  delivery type likp-lfart
          ls_xkomdlgn-matnr  = ls_asn_converted-materialno.
          ls_xkomdlgn-werks  = ls_asn_converted-plant.
          ls_xkomdlgn-lfdat  = sy-datum.
          ls_xkomdlgn-lfimg  = ls_asn_converted-qty.    "select  delivery qty lips-lfimg
          ls_xkomdlgn-umvkz  = '1'.    "select  Numerator (factor) for conversion of sales quantity into SKU vbap-umvkz
          ls_xkomdlgn-umvkn  = '1'.    "select  Denominator (Divisor) for Conversion of Sales Qty into SKU vbap-umvkn
          ls_xkomdlgn-vrkme  = ls_asn_converted-uom."ls_validate-lv_uom.     "select UOM from EKPO-meins
          ls_xkomdlgn-meins  = ls_asn_converted-uom."ls_validate-lv_uom.     "select UOM from EKPO-meins
          ls_xkomdlgn-vgbel  = ls_asn_converted-pono.   "ls_asn_data-pono.
          ls_xkomdlgn-vgpos  = ls_asn_converted-poline.
          ls_xkomdlgn-lifnr  = ls_asn_converted-vendor ."ls_asn_data-vendor.
          ls_xkomdlgn-lifex  = ls_asn_converted-supplier_inv.
          ls_xkomdlgn-verur  = ls_asn_converted-supplier_inv.
*      ls_xkomdlgn-inco1  = ls_fmdet-inco1.    " 'CIF'.        "select incoterms from ekpo-inco1
*      ls_xkomdlgn-inco2  = ls_fmdet-inco2.    "'VELLORE'.    "select incoterms 2 from ekpo-inco1
          ls_xkomdlgn-lgort  = ls_fmdet-lgort.    "'AUC'.        "select storage location from ekpo-lgort
          ls_xkomdlgn-uebto  = '0'.         "select Overdelivery Tolerance from ekpo-uebto
          ls_xkomdlgn-untto  = '0'.       "select Overdelivery Tolerance from ekpo-untto
          ls_xkomdlgn-bwtar  = ls_fmdet-bwtar.           "select valuation type from ekpo-bwtar
*            ls_xkomdlgn-prctr  = 'MCPDCOMMH1'. "select profit center from ekpo-prctr
          ls_xkomdlgn-bestq  = 'Q'.      "select Stock Category in the Warehouse Management System from mseg-bestq
          ls_xkomdlgn-insmk  = 'X'.      "select Stock Type from ekpo-insmk
          ls_xkomdlgn-vgtyp  = 'V'.      "select SD Document Category from vbak-vgtyp
          ls_xkomdlgn-kzazu  = 'X'.      "select Order Combination Indicator from
*      ls_xkomdlgn-charg  = ls_asn_converted-asnno.      "select Order Combination Indicator from

          APPEND ls_xkomdlgn TO lt_xkomdlgn.

          APPEND ls_asn_converted TO lt_asn_converted.
        ENDIF.

      ELSE.

        lv_msg = TEXT-000.

      ENDIF.
      CLEAR ls_asn_data.

    ENDLOOP.


    IF lt_xkomdlgn IS NOT INITIAL.

      ls_vbsk_i-ernam = sy-uname.
      ls_vbsk_i-erdat = sy-datum.
*----------------------------------------FM for Inbound delivery creation-------------------------------

      CALL FUNCTION 'GN_DELIVERY_CREATE'
        EXPORTING
          vbsk_i   = ls_vbsk_i
        TABLES
          xkomdlgn = lt_xkomdlgn
          xvbfs    = lt_xvbfs
          xvbls    = lt_xvbls
          xxlips   = lt_xxlips.

*--------------------------------------------------------------------------------------------------

      SORT lt_xxlips BY vgbel vgpos.

      LOOP AT lt_asn_converted INTO ls_asn_converted.

        MOVE-CORRESPONDING ls_asn_converted TO ls_asn_final.

        READ TABLE lt_xxlips INTO ls_xxlips WITH KEY vgbel = ls_asn_converted-pono
                                                     vgpos = ls_asn_converted-poline
                                                     BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          ls_asn_final-delivery = ls_xxlips-vbeln.
          ls_asn_final-delivery_item = ls_xxlips-posnr.
          ls_output-lv_vbelnib = ls_xxlips-vbeln.

        ELSE.

          EXIT.
        ENDIF.

        APPEND ls_asn_final TO lt_asn_final.
        lv_msg = TEXT-001.

        CLEAR ls_asn_converted.
        CLEAR ls_xxlips.

      ENDLOOP.

      MODIFY zasn_create FROM TABLE lt_asn_final.

*--------------------------------------------------ASN Change-------------------------------------------------------------------
*    ELSE.
*
*      SELECT asnno,
*             delivery,
*             delivery_item,
*             pono,
*             poline,
*             supplier_inv,
*             qty
*             FROM zasn_create
*             INTO TABLE @DATA(lt_change)
*             FOR ALL ENTRIES IN @lt_asn_data
*             WHERE asnno = @lt_asn_data-asnno
*             AND  pono = @lt_asn_data-pono
*             AND poline = @lt_asn_data-poline.
*
*
*      LOOP AT lt_asn_converted INTO ls_asn_converted.
*
*        CLEAR ls_asn_final.
*
*        MOVE-CORRESPONDING ls_asn_converted TO ls_asn_final.
*
*        ls_header-extdelv_no = ls_asn_converted-supplier_inv.
*
*        READ TABLE lt_change INTO DATA(ls_change) WITH KEY asnno = ls_asn_final-asnno
*                                                           pono = ls_asn_converted-pono
*                                                           poline = ls_asn_converted-poline.
*
*        IF sy-subrc IS INITIAL.
*
*          ls_asn_final-delivery = ls_change-delivery.
*          ls_asn_final-delivery_item = ls_change-delivery_item.
*          ls_output-lv_vbelnib = ls_change-delivery.
*
*          ls_header-deliv_numb = ls_change-delivery.
*          ls_header-extdelv_no = ls_asn_converted-supplier_inv.
*
*          ls_control-deliv_numb = ls_change-delivery.
*          ls_control-dlv_extid_flg = 'X'.
*
*          ls_item-deliv_numb = ls_change-delivery.
*          ls_item-deliv_item = ls_change-delivery_item.
*          ls_item-dlv_qty = ls_asn_converted-qty.
*          ls_item-dlv_qty_imunit = ls_asn_converted-qty.
*          ls_item-fact_unit_nom = '1'.
*          ls_item-fact_unit_denom = '1'.
*          ls_control_i-deliv_numb = ls_change-delivery.
*          ls_control_i-deliv_item = ls_change-delivery_item.
*
*
*          APPEND ls_asn_final TO lt_asn_final.
*
*        ENDIF.
*
*        IF ls_asn_converted-chang_ind = 'X'.
*
*          ls_control_i-del_item = 'X'.
**          ls_control-dlv_del = 'X'.
**          DATA(lv_del) = ls_asn_converted-asnno.
*          DATA(lv_vbeln) = ls_change-delivery.
*          DATA(lv_posnr) = ls_change-delivery_item.
*          lv_msg = TEXT-201.
*
*        ELSE.
*          ls_control_i-chg_delqty = 'X'.
*          lv_msg = TEXT-200.
*        ENDIF.
*
*        APPEND ls_item TO lt_item.
*        APPEND ls_control_i TO lt_control_i.
*
*        CLEAR ls_asn_converted.
*
*      ENDLOOP.
*
*      CALL FUNCTION 'BAPI_INB_DELIVERY_CHANGE'
*        EXPORTING
*          header_data    = ls_header
*          header_control = ls_control
*          delivery       = ls_output-lv_vbelnib
**         TECHN_CONTROL  =
*        TABLES
**         HEADER_PARTNER =
**         HEADER_PARTNER_ADDR        =
**         HEADER_DEADLINES           =
*          item_data      = lt_item
*          item_control   = lt_control_i
**         ITEM_SERIAL_NO =
**         EXTENSION1     =
**         EXTENSION2     =
*          return         = lt_ret
**         TOKENREFERENCE =
**         HANDLING_UNIT_HEADER       =
**         HANDLING_UNIT_ITEM         =
**         PARTIAL_GR_OBJECTS         =
**         CWM_ITEM_DATA  =
*        .
*
*
*      IF sy-subrc IS INITIAL.
*
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
*
*      ENDIF.
*      DELETE FROM zasn_create WHERE delivery = lv_vbeln AND
*                                    delivery_item = lv_posnr.
*      DELETE FROM zasn_create WHERE asnno = lv_del.
**      IF lv_vbeln IS INITIAL.
*        MODIFY zasn_create FROM TABLE lt_asn_final.
*      ENDIF.
    ENDIF.

*------------------------------------------------------------ Update Z table -------------------------------------

    IF ls_asn_final-delivery IS INITIAL.

      lv_msg = TEXT-111.

    ENDIF.
*------------------------------------------Return message---------------------------------------------------------
    ls_output-lv_msg = lv_msg.
*    ls_output-lv_vbelnib = lv_ibno.

    APPEND ls_output TO lt_output.

*--------------------------------------------Convert into JSON format--------------------------------------------
    /ui2/cl_json=>serialize(
      EXPORTING
        data             =  ls_output                 " Data to serialize
      RECEIVING
        r_json           = DATA(ls_json)                 " JSON string
    ).

    mo_response->create_entity( )->set_string_data( iv_data = ls_json ).

  ENDMETHOD.
