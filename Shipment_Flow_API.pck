CREATE OR REPLACE PACKAGE Shipment_Flow_API IS

module_  CONSTANT VARCHAR2(25) := 'ORDER';
lu_name_ CONSTANT VARCHAR2(25) := 'ShipmentFlow';
lu_type_ CONSTANT VARCHAR2(25) := 'Utility';

-----------------------------------------------------------------------------
-------------------- PUBLIC DECLARATIONS ------------------------------------
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-------------------- LU SPECIFIC PUBLIC METHODS -----------------------------
-----------------------------------------------------------------------------
-- Get_Pick_Lists_For_Shipments
--   Returns unpicked pick lists for a given shipment set.
--
-- Start_Shipment_Flow
--
-- Process_Optional_Events
--
-- Pack_Acc_To_Packing_Instr
--   Include the logic to execute the Pack According to Packing Instruction optional event.
--
-- Pack_Into_Handling_Unit
--   Include the logic to execute the Pack into Handling Unit optional event.
--
-- Get_Next_Step
--
-- Blocked_Orders_Exist
--   Checks whether blocked orders exist after certain steps in the shipment flow
--   Bug 126454, start
--
-- Check_All_License_Connected
--   Check if export license is connected.
--
-- All_Lines_Expctr
--   all_lines_expctr_ will be true if all lines of the specified shipment is export controlled.
--   connected_ will be true if at least one of those lines is license connected.
-----------------------------------------------------------------------------

--@PoReadOnly(Get_Pick_Lists_For_Shipments)
FUNCTION Get_Pick_Lists_For_Shipments (
   attr_ IN VARCHAR2 ) RETURN VARCHAR2;

PROCEDURE Start_Shipment_Flow (
   shipment_id_        IN NUMBER,
   event_no_           IN NUMBER, 
   rental_transfer_db_ IN VARCHAR2 DEFAULT Fnd_Boolean_API.db_false);

PROCEDURE Process_Optional_Events (
   shipment_id_   IN NUMBER,
   shipment_type_ IN VARCHAR2,
   event_         IN NUMBER );

PROCEDURE Pack_Acc_To_Packing_Instr (
   shipment_id_ IN NUMBER );

PROCEDURE Pack_Into_Handling_Unit (
   shipment_id_ IN NUMBER );

--@PoReadOnly(Get_Next_Step)
FUNCTION Get_Next_Step (
   shipment_id_ IN NUMBER ) RETURN VARCHAR2;

PROCEDURE Blocked_Orders_Exist(
   blocked_orders_exist_ OUT BOOLEAN,
   shipment_id_          IN  NUMBER,
   operation_            IN  VARCHAR2);

PROCEDURE Check_All_License_Connected (
   display_info_  IN OUT NOCOPY NUMBER,
   shipment_id_   IN     NUMBER);

PROCEDURE All_Lines_Expctr(
   all_lines_expctr_ OUT VARCHAR2,
   connected_        OUT VARCHAR2,
   shipment_id_      IN  VARCHAR2 );

-----------------------------------------------------------------------------
-------------------- LU SPECIFIC PRIVATE METHODS ----------------------------
-----------------------------------------------------------------------------
-- Start_Automatic_Shipment__
--   Excute the automatic shipment flow called to Finalize the shipment
--
-- Process_Automatic_Shipment__
--   Process the shipment automatically through the following flows.
--   1. Reserve the shipment
--   2. Create pick lists
--   3. Report picking
--   4. Complete the shipment
--   5. Deliver the shipment
--   6. Close the shipment
--
-- Lock_Shipment__
--   Lock the specified shipment while it is beeing processed.
--
-- Get_Allowed_Ship_Operations__
--   Returns a string used to determine which operations should be allowed
--   for the specified shipment.
--
-- Deliver_Shipment_Allowed__
--   Checks if a shipment is allowed to be delivered.
--
-- Pick_Plan_Shipment__
--   Starts pick planning of all order lines connected to a Shipment
--
-- Plan_Pick_Shipment_Allowed__
--   Return TRUE (1) if the Plan Picking (Reserve) operation is allowed for the
--   specified  shipment
--
-- Pick_Report_Shipment_Allowed__
--   Return TRUE (1) if the Report Picking operation is allowed for the
--   specified  shipment
--
-- Print_Shipment_Pick_List__
--   Print the Consolidated Pick Lists for the shipment.
--   There may be several consolidated pick list per shipment.
--
-- Print_Shipment_Delivery_Note__
--   Print the Delivery Note for the shipment.
--
-- Print_Pick_List_Allowed__
--   Check if the Pick List for the shipment can be printed.
--
-- Print_Delivery_Note_Allowed__
--   Check if the delivery Note for the shipment can be printed.
--
-- Print_Proforma_Ivc_Allowed__
--   Check if the proforma invoice for the shipment can be printed.
--
-- Get_Pick_Lists_For_Shipment__
--   Returns unpicked pick lists for a shipment.
--
-- Create_Pick_List_Allowed__
--   Checks if a pick list can be created for a shipment.
--
-- Create_Shipment_Pick_Lists__
--   Create pick list for the shipment.
--
-- Create_Ship_Invoice_Allowed__
--   Checks if an invoice can be created for a shipment.
--
-- Print_Invoice_Allowed__
--   Return TRUE (1) if the Print Invoice operation is allowed for the
--   specified  order
--
-- Start_Print_Consol_Pl__
--   This method starts printing of the consolidated pick lists.
--
-- Start_Print_Ship_Consol_Pl__
--   This method starts printing of the shipment consolidated pick lists.
--
-- Print_Ship_Consol_Pick_List__
--
-- Contains_Dangerous_Goods__
--   Return TRUE if the given shipment contains hazardous goods.
--
-- Finalize_Shipment_Allowed__
--   Return TRUE (1) if the Finalizing Shipment operation is allowed for the
--   specified  shipment
--
-- Process_Mandatory_Events__
--   Process mandatory events in one shipment.
--   The attribute string passed as a parameter should contain the parameters
--   needed for the processing.
--
-- Process_All_Shipments__
--
-- Start_Reserve_Shipment__
--   Start reserving customer orders connected to the shipment.
--
-- Start_Create_Pick_List__
--   Start creation of pick list for the shipment.
--
-- Start_Print_Pick_List__
--   Start printing pick list for the shipment.
--
-- Start_Print_Consignment_Note__
--   Start printing consignment note for the shipment.
--
-- Start_Print_Bill_Of_Lading__
--   Start printing bill Of lading for the shipment.
--
-- Start_Print_Packing_List__
--   Start printing packing list for the shipment.
--
-- Start_Print_Address_Label__
--   Start printing address label for the shipment.
--
-- Start_Print_Proforma_Inv__
--   Start printing proforma invoice for the shipment.
--
-- Start_Print_Shipment_Delnote__
--   Start printing shipment delivery note for the shipment.
--
-- Start_Pick_Report_Shipment__
--   Start report picking for the shipment.
--
-- Start_Complete_Shipment__
--   Start completing the shipment.
--
-- Start_Deliver_Shipment__
--   Start Deliver the shipment.
--
-- Start_Close_Shipment__
--   Start Closing the Shipment.
--
-- Start_Create_Ship_Invoice__
--   Start creating Shipment Invoices.
--
-- Start_Reopen_Shipment__
--   Start reopening the Shipment and go back to Preliminary state.
--
-- Start_Cancel_Shipment__
--   Start cancelling shipment and go back to Cancel state.
--
-- Start_Send_Dispatch_Advice__
--   Start send dispatch advices for a shipment set.
--
-- Start_Pack_Into_Handl_Unit__
--   Start packing into lowest level handling units for a shipment set.
--
-- Start_Pack_Acc_Packing_Instr__
--   Start packing according to the packing instruction added on every line for a shipment set.
--
-- Check_Pick_List_Use_Ship_Inv__
-----------------------------------------------------------------------------

PROCEDURE Start_Automatic_Shipment__ (
   shipment_id_            IN NUMBER,
   finalize_on_picked_qty_ IN VARCHAR2,
   consider_reserved_qty_  IN VARCHAR2,
   disconnect_released_    IN VARCHAR2,
   disconnect_not_picked_  IN VARCHAR2 );

PROCEDURE Process_Automatic_Shipment__ (
   attr_ IN VARCHAR2 );

PROCEDURE Lock_Shipment__ (
   shipment_id_ IN NUMBER );

FUNCTION Get_Allowed_Ship_Operations__ (
   shipment_id_ IN NUMBER ) RETURN VARCHAR2;

--@PoReadOnly(Deliver_Shipment_Allowed__)
FUNCTION Deliver_Shipment_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER;

PROCEDURE Pick_Plan_Shipment__ (
   shipment_id_ IN NUMBER );

--@PoReadOnly(Plan_Pick_Shipment_Allowed__)
FUNCTION Plan_Pick_Shipment_Allowed__ (
   shipment_id_ IN NUMBER,
   finalize_    IN NUMBER ) RETURN NUMBER;

--@PoReadOnly(Pick_Report_Shipment_Allowed__)
FUNCTION Pick_Report_Shipment_Allowed__ (
   shipment_id_               IN NUMBER,
   report_pick_from_co_lines_ IN VARCHAR2 DEFAULT 'FALSE' ) RETURN NUMBER;

PROCEDURE Print_Shipment_Pick_List__ (
   shipment_id_           IN NUMBER,
   process_in_background_ IN VARCHAR2 );

PROCEDURE Print_Shipment_Delivery_Note__ (
   shipment_id_ IN NUMBER );

--@PoReadOnly(Print_Pick_List_Allowed__)
FUNCTION Print_Pick_List_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER;

--@PoReadOnly(Print_Delivery_Note_Allowed__)
FUNCTION Print_Delivery_Note_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER;

--@PoReadOnly(Print_Proforma_Ivc_Allowed__)
FUNCTION Print_Proforma_Ivc_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER;

--@PoReadOnly(Get_Pick_Lists_For_Shipment__)
FUNCTION Get_Pick_Lists_For_Shipment__ (
   shipment_id_  IN NUMBER,
   printed_flag_ IN VARCHAR2 ) RETURN VARCHAR2;

--@PoReadOnly(Create_Pick_List_Allowed__)
FUNCTION Create_Pick_List_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER;

PROCEDURE Create_Shipment_Pick_Lists__ (
   shipment_id_ IN NUMBER );

--@PoReadOnly(Create_Ship_Invoice_Allowed__)
FUNCTION Create_Ship_Invoice_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER;

--@PoReadOnly(Print_Invoice_Allowed__)
FUNCTION Print_Invoice_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER;

PROCEDURE Start_Print_Consol_Pl__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Print_Ship_Consol_Pl__ (
   attr_ IN OUT NOCOPY VARCHAR2 );

PROCEDURE Print_Ship_Consol_Pick_List__ (
   attr_ IN OUT NOCOPY VARCHAR2 );

--@PoReadOnly(Contains_Dangerous_Goods__)
FUNCTION Contains_Dangerous_Goods__ (
   shipment_id_ IN NUMBER ) RETURN VARCHAR2;

--@PoReadOnly(Finalize_Shipment_Allowed__)
FUNCTION Finalize_Shipment_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER;

PROCEDURE Process_Mandatory_Events__ (
   attr_ IN VARCHAR2 );

PROCEDURE Process_All_Shipments__ (
   attr_            IN VARCHAR2,
   mandatory_event_ IN BOOLEAN );

PROCEDURE Start_Reserve_Shipment__ (
   info_ IN OUT NOCOPY VARCHAR2,
   attr_ IN     VARCHAR2 );

PROCEDURE Start_Create_Pick_List__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Print_Pick_List__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Print_Consignment_Note__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Print_Bill_Of_Lading__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Print_Packing_List__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Print_Address_Label__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Print_Proforma_Inv__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Print_Shipment_Delnote__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Pick_Report_Shipment__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Complete_Shipment__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Deliver_Shipment__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Close_Shipment__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Create_Ship_Invoice__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Reopen_Shipment__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Cancel_Shipment__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Send_Dispatch_Advice__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Pack_Into_Handl_Unit__ (
   attr_ IN VARCHAR2 );

PROCEDURE Start_Pack_Acc_Packing_Instr__ (
   attr_ IN VARCHAR2 );

--@PoReadOnly(Check_Pick_List_Use_Ship_Inv__)
FUNCTION Check_Pick_List_Use_Ship_Inv__ (
   shipment_id_ IN NUMBER ) RETURN VARCHAR2;

-----------------------------------------------------------------------------
-------------------- FOUNDATION1 METHODS ------------------------------------
-----------------------------------------------------------------------------
-- Init
--   Framework method that initializes this package.
-----------------------------------------------------------------------------

--@PoReadOnly(Init)
PROCEDURE Init;

END Shipment_Flow_API;
/
CREATE OR REPLACE PACKAGE BODY Shipment_Flow_API IS

-----------------------------------------------------------------------------
-------------------- PRIVATE DECLARATIONS -----------------------------------
-----------------------------------------------------------------------------

db_true_                     CONSTANT VARCHAR2(4)  := Fnd_Boolean_API.db_true;

db_false_                    CONSTANT VARCHAR2(5)  := Fnd_Boolean_API.db_false;


-----------------------------------------------------------------------------
-------------------- IMPLEMENTATION METHOD DECLARATIONS ---------------------
-----------------------------------------------------------------------------

PROCEDURE Reserve_Shipment___ (
   shipment_id_            IN NUMBER,
   finalize_on_picked_qty_ IN VARCHAR2,
   consider_reserved_qty_  IN VARCHAR2,
   discon_not_picked_      IN VARCHAR2 );

PROCEDURE Create_Shipment_Pick_Lists___ (
   shipment_id_ IN NUMBER );

PROCEDURE Report_Shipment_Pick_Lists___ (
   shipment_id_ IN NUMBER );

PROCEDURE Report_Shipment_Pick_Lists___ (
   shipment_id_ IN NUMBER,
   location_no_ IN VARCHAR2);

PROCEDURE Deliver_Shipment___ (
   shipment_id_        IN NUMBER,
   deliver_through_cs_ IN VARCHAR2 );

FUNCTION Get_Next_Event___ (
   event_ IN VARCHAR2 ) RETURN NUMBER;

PROCEDURE Print_Goods_Declaration___ (
   shipment_id_ IN NUMBER );

PROCEDURE Process_Optional_Events___ (
   shipment_id_   IN NUMBER,
   shipment_type_ IN VARCHAR2,
   event_         IN NUMBER );

PROCEDURE Print_Shipment_Doc___ (
   report_id_                  IN VARCHAR2,
   shipment_id_                IN NUMBER,
   handling_unit_id_           IN NUMBER,
   no_of_handling_unit_labels_ IN NUMBER  );

PROCEDURE Print_Consignment_Note___ (
   shipment_id_ IN NUMBER );

PROCEDURE Print_Bill_Of_Lading___ (
   shipment_id_ IN NUMBER );

PROCEDURE Print_Address_Label___ (
   shipment_id_ IN NUMBER );

PROCEDURE Print_Pro_Forma_Invoice___ (
   shipment_id_ IN NUMBER );

PROCEDURE Print_Packing_List___ (
   shipment_id_ IN NUMBER );

PROCEDURE Create_Sssc___ (
   shipment_id_ IN NUMBER );

PROCEDURE Print_Handling_Unit_Labels___ (
   shipment_id_ IN NUMBER );

PROCEDURE Process_Shipment___(
  mandatory_event_ IN BOOLEAN,
  shipment_type_   IN VARCHAR2,
  shiment_attr_    IN VARCHAR2,
  start_event_     IN NUMBER);

FUNCTION Plan_Pick_Shipment_Allowed___ (
   shipment_id_ IN NUMBER,
   finalize_    IN NUMBER ) RETURN NUMBER;

FUNCTION Create_Pick_List_Allowed___ (
   shipment_id_ IN NUMBER ) RETURN NUMBER;

FUNCTION Deliver_Shipment_Allowed___ (
   shipment_id_ IN NUMBER ) RETURN NUMBER;

FUNCTION Pick_Report_Ship_Allowed___ (
   shipment_id_ IN NUMBER,
   report_pick_from_co_lines_ IN VARCHAR2 DEFAULT 'FALSE') RETURN NUMBER;

FUNCTION Get_Pick_Lists_For_Shipment___ (
   shipment_id_  IN NUMBER,
   printed_flag_ IN VARCHAR2 ) RETURN VARCHAR2;

FUNCTION Print_Pick_List_Allowed___ (
   shipment_id_ IN NUMBER ) RETURN NUMBER;

FUNCTION Print_Proforma_Ivc_Allowed___ (
   shipment_id_ IN NUMBER ) RETURN NUMBER;

PROCEDURE Print_Ship_Consol_Pl___ (
   pick_list_no_ IN VARCHAR2 );

PROCEDURE Process_Nonmandatory_Events___ (
   attr_ IN VARCHAR2 );

PROCEDURE Check_Manual_Tax_Lia_Date___ (
   invoice_id_  IN NUMBER,
   shipment_id_ IN NUMBER );

PROCEDURE Print_Invoice___ (
   invoice_id_      IN VARCHAR2,
   media_code_      IN VARCHAR2,
   cust_email_addr_ IN VARCHAR2,
   email_invoice_   IN VARCHAR2 );

PROCEDURE Check_All_License_Connected___ (
   display_info_ IN OUT NOCOPY NUMBER,
   shipment_id_  IN     NUMBER );

-----------------------------------------------------------------------------
-------------------- LU SPECIFIC PUBLIC METHODS -----------------------------
-----------------------------------------------------------------------------

--@IgnoreMissingSysinit
FUNCTION Get_Pick_Lists_For_Shipments (
   attr_ IN VARCHAR2 ) RETURN VARCHAR2
IS
   
   FUNCTION Core (
      attr_ IN VARCHAR2 ) RETURN VARCHAR2
   IS
      pick_list_no_list_ VARCHAR2(32000);
      ptr_               NUMBER;
      name_              VARCHAR2(30);
      value_             VARCHAR2(2000);
      shipment_id_       NUMBER; 
   BEGIN
      pick_list_no_list_ := NULL;
      WHILE (Client_SYS.Get_Next_From_Attr(attr_, ptr_, name_, value_)) LOOP
         IF (name_ = 'SHIPMENT_ID') THEN
            shipment_id_       := Client_SYS.Attr_Value_To_Number(value_);
            pick_list_no_list_ := pick_list_no_list_ || Get_Pick_Lists_For_Shipment__(shipment_id_, NULL);
         END IF;
      END LOOP;
      RETURN pick_list_no_list_;
   END Core;

BEGIN
   RETURN Core(attr_);
END Get_Pick_Lists_For_Shipments;


PROCEDURE Start_Shipment_Flow (
   shipment_id_        IN NUMBER,
   event_no_           IN NUMBER, 
   rental_transfer_db_ IN VARCHAR2 DEFAULT Fnd_Boolean_API.db_false)
IS
   
   PROCEDURE Core (
      shipment_id_        IN NUMBER,
      event_no_           IN NUMBER, 
      rental_transfer_db_ IN VARCHAR2 DEFAULT Fnd_Boolean_API.db_false)
   IS
      shipment_type_ shipment_tab.shipment_type%TYPE;
      ship_attr_     VARCHAR2(32000);
      start_event_   VARCHAR2(10);
   BEGIN
      shipment_type_ := Shipment_API.Get_Shipment_Type(shipment_id_);
      IF (event_no_ = 10) THEN
         Process_Optional_Events___(shipment_id_, shipment_type_, 10);
         --@ApproveTransactionStatement(2014-08-06,mahplk)
         COMMIT;
         start_event_ := '20';     
      ELSIF (event_no_ = 20) THEN
         Process_Optional_Events___(shipment_id_, shipment_type_, 20);
         --@ApproveTransactionStatement(2014-08-06,mahplk)
         COMMIT;
         start_event_ := '30';
      ELSIF (event_no_ = 40) THEN
         Process_Optional_Events___(shipment_id_, shipment_type_, 40);
         --@ApproveTransactionStatement(2014-08-06,mahplk)
         COMMIT;
         start_event_ := '50';
      END IF;
      IF (Shipment_Type_Event_API.Get_Next_Event(shipment_type_, event_no_, rental_transfer_db_) IS NOT NULL) THEN
         Client_SYS.Clear_Attr(ship_attr_);
         Client_SYS.Add_To_Attr('START_EVENT', start_event_, ship_attr_);
         Client_SYS.Add_To_Attr('SHIPMENT_ID', shipment_id_, ship_attr_);
         Client_SYS.Add_To_Attr('SHIPMENT_TYPE', shipment_type_, ship_attr_);
         Client_SYS.Add_To_Attr('LOCATION_NO', Shipment_API.Get_Ship_Inventory_Location_No(shipment_id_), ship_attr_);
         Client_SYS.Add_To_Attr('USE_SHIP_INVENTORY', Customer_Order_Pick_List_API.Check_Pick_List_Use_Ship_Inv(shipment_id_), ship_attr_);
         Client_SYS.Add_To_Attr('RENTAL_TRANSFER_DB', rental_transfer_db_, ship_attr_);
         Client_SYS.Add_To_Attr('END', '', ship_attr_);
         IF (start_event_ = 20) THEN
            Shipment_Flow_API.Start_Create_Pick_List__(ship_attr_);
         ELSIF (start_event_ = 30) THEN
            Shipment_Flow_API.Start_Print_Pick_List__(ship_attr_);
         ELSIF (start_event_ = 50) THEN
            Shipment_Flow_API.Start_Complete_Shipment__(ship_attr_);
         END IF;
      END IF;
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Shipment_Flow');
   Core(shipment_id_, event_no_, rental_transfer_db_);
END Start_Shipment_Flow;


PROCEDURE Process_Optional_Events (
   shipment_id_   IN NUMBER,
   shipment_type_ IN VARCHAR2,
   event_         IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_   IN NUMBER,
      shipment_type_ IN VARCHAR2,
      event_         IN NUMBER )
   IS
   BEGIN
      Process_Optional_Events___(shipment_id_, shipment_type_, event_);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Process_Optional_Events');
   Core(shipment_id_, shipment_type_, event_);
END Process_Optional_Events;


PROCEDURE Pack_Acc_To_Packing_Instr (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
      shipment_order_line_tab_ Shipment_Order_Line_API.Line_Tab;
      CURSOR get_lines IS
         SELECT shipment_id, order_no, line_no, rel_no, line_item_no
         FROM shipment_connectable_line
         WHERE shipment_id            = shipment_id_
         AND   packing_instruction_id IS NOT NULL
         AND   remaining_parcel_qty   > 0;
   BEGIN
      OPEN get_lines;
      FETCH get_lines BULK COLLECT INTO shipment_order_line_tab_;
      CLOSE get_lines;
      
      IF (shipment_order_line_tab_.COUNT > 0) THEN 
         Shipment_Auto_Packing_Util_API.Auto_Pack_Shipment_Lines(shipment_order_line_tab_); 
      END IF;
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Pack_Acc_To_Packing_Instr');
   Core(shipment_id_);
END Pack_Acc_To_Packing_Instr;


PROCEDURE Pack_Into_Handling_Unit (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
      CURSOR get_lines IS
         SELECT order_no, line_no, rel_no, line_item_no, handling_unit_type_id, remaining_parcel_qty
         FROM shipment_connectable_line
         WHERE shipment_id           = shipment_id_
         AND   handling_unit_type_id IS NOT NULL
         AND   remaining_parcel_qty  > 0;
   BEGIN
      FOR rec_ IN get_lines LOOP 
         Shipment_Order_Line_API.Connect_To_New_Handling_Units(shipment_id_,
                                                               rec_.order_no,
                                                               rec_.line_no,
                                                               rec_.rel_no,
                                                               rec_.line_item_no,
                                                               rec_.handling_unit_type_id,
                                                               rec_.remaining_parcel_qty);
      END LOOP;
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Pack_Into_Handling_Unit');
   Core(shipment_id_);
END Pack_Into_Handling_Unit;


--@IgnoreMissingSysinit
FUNCTION Get_Next_Step (
   shipment_id_ IN NUMBER ) RETURN VARCHAR2
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN VARCHAR2
   IS 
      allowed_operations_   VARCHAR2(20);
      delim_                VARCHAR2(2);
      next_step_list_       VARCHAR2(1400);
      allowed_operation_    VARCHAR2(2);
      next_step_            VARCHAR2(200);
   BEGIN
      allowed_operations_ := Get_Allowed_Ship_Operations__(shipment_id_);
      FOR position_ IN 1..8 LOOP
         allowed_operation_ := SUBSTR(allowed_operations_, position_, 1);
         IF (position_ = 2) AND (allowed_operation_ != '1') THEN
            allowed_operation_ := SUBSTR(allowed_operations_, 14, 1);
         END IF;
         IF allowed_operation_ NOT IN ('*', '4') THEN
            next_step_ := NULL; 
            CASE allowed_operation_
               WHEN '0' THEN
                  next_step_ := Shipment_Flow_Activities_API.Decode(Shipment_Flow_Activities_API.DB_RESERVE);
               WHEN ('1') THEN
                  next_step_ := Shipment_Flow_Activities_API.Decode(Shipment_Flow_Activities_API.DB_REPORT_PICKING);
               WHEN ('R') THEN
                  next_step_ := Shipment_Flow_Activities_API.Decode(Shipment_Flow_Activities_API.DB_REPORT_PICKING);
               WHEN '2' THEN
                  next_step_ := Shipment_Flow_Activities_API.Decode(Shipment_Flow_Activities_API.DB_DELIVER);
               WHEN '3' THEN
                  next_step_ := Shipment_Flow_Activities_API.Decode(Shipment_Flow_Activities_API.DB_COMPLETE);
               WHEN '5' THEN
                  next_step_ := Shipment_Flow_Activities_API.Decode(Shipment_Flow_Activities_API.DB_CLOSE);
               WHEN '6' THEN
                  next_step_ := Shipment_Flow_Activities_API.Decode(Shipment_Flow_Activities_API.DB_CREATE_PICK_LIST);
               WHEN '7' THEN
                  next_step_ := Shipment_Flow_Activities_API.Decode(Shipment_Flow_Activities_API.DB_PRINT_PICK_LIST);
               ELSE
                  NULL;
            END CASE;
            next_step_list_ := next_step_list_||delim_||next_step_;
            delim_ := ', ';
         END IF;
      END LOOP;
      RETURN next_step_list_;
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Get_Next_Step;


PROCEDURE Blocked_Orders_Exist(
   blocked_orders_exist_ OUT BOOLEAN,
   shipment_id_          IN  NUMBER,
   operation_            IN  VARCHAR2)
IS
   
   PROCEDURE Core(
      blocked_orders_exist_ OUT BOOLEAN,
      shipment_id_          IN  NUMBER,
      operation_            IN  VARCHAR2)
   IS
      CURSOR get_orders_for_delivery IS
         SELECT DISTINCT order_no
         FROM shipment_order_line_tab
         WHERE shipment_id  = shipment_id_
         AND   line_item_no >= 0
         AND  (qty_picked   > 0
         OR    qty_to_ship  > 0);
   
      CURSOR get_orders_for_picklist IS
         SELECT DISTINCT order_no
         FROM customer_order_reservation_tab
         WHERE pick_list_no = '*'
         AND   shipment_id  = shipment_id_;
         
      CURSOR get_orders_for_reservation IS
         SELECT DISTINCT order_no
         FROM shipment_order_line_tab
         WHERE shipment_id = shipment_id_;
   BEGIN
      blocked_orders_exist_ := FALSE;
      IF (operation_ = 'DELIVER') THEN
         FOR rec_ IN get_orders_for_delivery LOOP
            IF (Customer_Order_API.Get_Objstate(rec_.order_no) = 'CreditBlocked') THEN
               blocked_orders_exist_ := TRUE;
               EXIT;
            END IF;
         END LOOP;
      ELSIF (operation_ = 'CREATE_PICKLIST') THEN
         FOR rec_ IN get_orders_for_picklist LOOP
            IF (Customer_Order_API.Get_Objstate(rec_.order_no) = 'CreditBlocked') THEN
               blocked_orders_exist_ := TRUE;
               EXIT;
            END IF;
         END LOOP;
      ELSIF (operation_ = 'RESERVE') THEN
         FOR rec_ IN get_orders_for_reservation LOOP
            IF (Customer_Order_API.Get_Objstate(rec_.order_no) = 'CreditBlocked') THEN
               blocked_orders_exist_ := TRUE;
               EXIT;
            END IF;
         END LOOP;
      END IF;
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Blocked_Orders_Exist');
   Core(blocked_orders_exist_, shipment_id_, operation_);
END Blocked_Orders_Exist;


PROCEDURE Check_All_License_Connected (
   display_info_  IN OUT NOCOPY NUMBER,
   shipment_id_   IN     NUMBER)
IS
   
   PROCEDURE Core (
      display_info_  IN OUT NOCOPY NUMBER,
      shipment_id_   IN     NUMBER)
   IS
   BEGIN  
      Check_All_License_Connected___(display_info_, shipment_id_);  
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Check_All_License_Connected');
   Core(display_info_, shipment_id_);
END Check_All_License_Connected;


PROCEDURE All_Lines_Expctr(
   all_lines_expctr_ OUT VARCHAR2,
   connected_        OUT VARCHAR2,
   shipment_id_      IN  VARCHAR2 )
IS
   
   PROCEDURE Core(
      all_lines_expctr_ OUT VARCHAR2,
      connected_        OUT VARCHAR2,
      shipment_id_      IN  VARCHAR2 )
   IS
      exp_license_connect_id_  NUMBER;
      licensed_order_type_     VARCHAR2(25);
      line_count_              NUMBER := 0;
      CURSOR get_ship_order_lines IS
         SELECT sol.order_no, sol.line_no, sol.rel_no, sol.line_item_no, 
                col.demand_code, col.demand_order_ref1, 
                col.demand_order_ref2, col.demand_order_ref3
         FROM   shipment_order_line_tab sol, customer_order_line_tab col
         WHERE  shipment_id = shipment_id_
         AND    sol.order_no = col.order_no
         AND    sol.line_no = col.line_no
         AND    sol.rel_no = col.rel_no
         AND    sol.line_item_no = col.line_item_no
         AND    col.rowstate IN ('Released', 'Reserved');   
   BEGIN
      $IF Component_Expctr_SYS.INSTALLED $THEN
         line_count_       := 1;
         all_lines_expctr_ := 'TRUE';
         connected_        := 'FALSE';
         FOR rec_ IN get_ship_order_lines LOOP
            licensed_order_type_ := Customer_Order_Line_API.Get_Expctr_License_Order_Type(rec_.demand_code, rec_.demand_order_ref1, rec_.demand_order_ref2, rec_.demand_order_ref3);
            exp_license_connect_id_ := Exp_License_Connect_Head_API.Get_Connect_Id_From_Ref(licensed_order_type_, rec_.order_no, rec_.line_no, rec_.rel_no, rec_.line_item_no);
            IF exp_license_connect_id_ IS NULL THEN
               all_lines_expctr_ := 'FALSE';
               EXIT;
            END IF;
            line_count_ := line_count_ + 1;
         END LOOP;
         IF line_count_ > 1 THEN
            FOR rec_ IN get_ship_order_lines LOOP
               IF Exp_License_Connect_Head_API.Get_State_By_Ref(rec_.order_no, rec_.line_no, rec_.rel_no, rec_.line_item_no, licensed_order_type_) = 'Approved' THEN 
                  connected_ := 'TRUE';
                  EXIT;
               END IF;
            END LOOP;
         END IF;
      $ELSE
         NULL;
      $END   
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'All_Lines_Expctr');
   Core(all_lines_expctr_, connected_, shipment_id_);
END All_Lines_Expctr;

-----------------------------------------------------------------------------
-------------------- LU SPECIFIC PRIVATE METHODS ----------------------------
-----------------------------------------------------------------------------

PROCEDURE Start_Automatic_Shipment__ (
   shipment_id_            IN NUMBER,
   finalize_on_picked_qty_ IN VARCHAR2,
   consider_reserved_qty_  IN VARCHAR2,
   disconnect_released_    IN VARCHAR2,
   disconnect_not_picked_  IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      shipment_id_            IN NUMBER,
      finalize_on_picked_qty_ IN VARCHAR2,
      consider_reserved_qty_  IN VARCHAR2,
      disconnect_released_    IN VARCHAR2,
      disconnect_not_picked_  IN VARCHAR2 )
   IS
      description_ VARCHAR2(250);
      attr_        VARCHAR2(2000):= NULL;
   BEGIN
      description_ := Language_SYS.Translate_Constant(lu_name_, 'FROM_SHIPMENT: Automatic Shipment Process');
      Client_SYS.Add_To_Attr('SHIPMENT_ID',            shipment_id_,            attr_);
      Client_SYS.Add_To_Attr('FINALIZE_ON_PICKED_QTY', finalize_on_picked_qty_, attr_);
      Client_SYS.Add_To_Attr('CONSIDER_RESERVED_QTY',  consider_reserved_qty_,  attr_);
      Client_SYS.Add_To_Attr('DISCON_NO_RESERVATIONS', disconnect_released_,    attr_);
      Client_SYS.Add_To_Attr('DISCON_NOT_PICKED',      disconnect_not_picked_,  attr_);
      Transaction_SYS.Deferred_Call('SHIPMENT_FLOW_API.Process_Automatic_Shipment__', attr_, description_);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Automatic_Shipment__');
   Core(shipment_id_, finalize_on_picked_qty_, consider_reserved_qty_, disconnect_released_, disconnect_not_picked_);
END Start_Automatic_Shipment__;


PROCEDURE Process_Automatic_Shipment__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
      error_message_          VARCHAR2(2000);
      info_                   VARCHAR2(2000);
      shipment_id_            NUMBER;
      next_event_             NUMBER;
   
      finalize_on_picked_qty_ VARCHAR2(5);
      consider_reserved_qty_  VARCHAR2(5);
      discon_no_reservations_ VARCHAR2(5);
      order_no_               VARCHAR2(12);
      line_no_                VARCHAR2(4);
      rel_no_                 VARCHAR2(4);
      exist_                  NUMBER := 0;
      discon_not_picked_      VARCHAR2(5);
      shipment_state_         VARCHAR2(20);
      blocked_orders_exist_   BOOLEAN := FALSE;
      
      -- Removed the state checking of COLine and considered the shipment order line quantities
      -- Added col.rowstate to SELECT list and modified WHERE clause.
      CURSOR get_non_reserved_lines IS
         SELECT sol.order_no, sol.line_no, sol.rel_no, sol.line_item_no, col.rowstate
           FROM shipment_order_line_tab sol, customer_order_line_tab col
          WHERE sol.shipment_id   = shipment_id_
            AND sol.order_no      = col.order_no
            AND sol.line_no       = col.line_no
            AND sol.rel_no        = col.rel_no
            AND sol.line_item_no  = col.line_item_no
            AND col.catalog_type != 'NON'
            AND sol.line_item_no <= 0
            AND sol.qty_assigned  = 0;
   
      -- Considered the shipment order line quantities
      CURSOR get_component_reservation IS
         SELECT 1
         FROM  shipment_order_line_tab sol
         WHERE sol.shipment_id   = shipment_id_
         AND   sol.order_no      = order_no_
         AND   sol.line_no       = line_no_
         AND   sol.rel_no        = rel_no_
         AND   sol.line_item_no  > 0
         AND   sol.qty_assigned != 0;
   
      -- Considered the shipment order line quantities
      CURSOR get_component_not_reserved IS
         SELECT sol.line_item_no
           FROM shipment_order_line_tab sol
          WHERE sol.order_no      = order_no_
            AND sol.line_no       = line_no_
            AND sol.rel_no        = rel_no_
            AND sol.line_item_no  > 0
            AND sol.qty_assigned = 0
            AND sol.shipment_id   = shipment_id_;
       
      -- Removed the state checking of COLine and considered the shipment order line quantities
      CURSOR get_non_picked_lines IS
         SELECT sol.order_no, sol.line_no, sol.rel_no, sol.line_item_no
           FROM shipment_order_line_tab sol, customer_order_line_tab col
          WHERE sol.shipment_id   = shipment_id_
            AND sol.order_no      = col.order_no
            AND sol.line_no       = col.line_no
            AND sol.rel_no        = col.rel_no
            AND sol.line_item_no  = col.line_item_no
            AND col.catalog_type != 'NON'
            AND sol.qty_picked    = 0
            AND sol.line_item_no <= 0;
      
      -- Considered the shipment order line quantities
      CURSOR get_component_picked IS
         SELECT 1
           FROM shipment_order_line_tab sol
          WHERE sol.order_no      = order_no_
            AND sol.line_no       = line_no_
            AND sol.rel_no        = rel_no_
            AND sol.line_item_no  > 0
            AND sol.qty_picked  != 0
            AND sol.shipment_id   = shipment_id_; 
   BEGIN
      shipment_id_            := Client_SYS.Attr_Value_To_Number(Client_SYS.Get_Item_Value('SHIPMENT_ID', attr_));
   
      finalize_on_picked_qty_ := Client_SYS.Get_Item_Value('FINALIZE_ON_PICKED_QTY', attr_);
      consider_reserved_qty_  := Client_SYS.Get_Item_Value('CONSIDER_RESERVED_QTY', attr_);
      discon_no_reservations_ := Client_SYS.Get_Item_Value('DISCON_NO_RESERVATIONS', attr_);
      discon_not_picked_      := Client_SYS.Get_Item_Value('DISCON_NOT_PICKED', attr_);
      shipment_state_         := Shipment_API.Get_State(shipment_id_);
   
      IF (discon_no_reservations_ = 'TRUE' AND shipment_state_ = 'Preliminary') THEN
         FOR lines_ IN get_non_reserved_lines LOOP
            IF (lines_.line_item_no = 0) THEN
               Shipment_Order_Line_API.Remove_Shipment_Line(lines_.order_no, lines_.line_no, lines_.rel_no, lines_.line_item_no, shipment_id_);
            ELSE
               order_no_ := lines_.order_no;
               line_no_  := lines_.line_no;
               rel_no_   := lines_.rel_no;
               OPEN get_component_reservation;
               FETCH get_component_reservation INTO exist_;
               IF (get_component_reservation%NOTFOUND) THEN
                  exist_ :=0;
               END IF;
               CLOSE get_component_reservation;
   
               IF (exist_ = 0) THEN
                  Shipment_Order_Line_API.Remove_Shipment_Line(lines_.order_no, lines_.line_no, lines_.rel_no, lines_.line_item_no, shipment_id_);
               ELSE
                  FOR comp_ IN get_component_not_reserved LOOP
                     Shipment_Order_Line_API.Remove_Shipment_Line(lines_.order_no, lines_.line_no, lines_.rel_no, comp_.line_item_no, shipment_id_);  
                  END LOOP;
               END IF;
            END IF;
         END LOOP;
      END IF;
   
      IF (discon_not_picked_ = 'TRUE' AND shipment_state_ = 'Preliminary') THEN
         FOR lines_ IN get_non_picked_lines LOOP
            IF (Customer_Order_Reservation_API.Pick_List_Exist(lines_.order_no, lines_.line_no, lines_.rel_no, lines_.line_item_no, shipment_id_) = 0) THEN
               IF (lines_.line_item_no = 0) THEN
                  Shipment_Order_Line_API.Remove_Shipment_Line(lines_.order_no, lines_.line_no, lines_.rel_no, lines_.line_item_no, shipment_id_);
               ELSE
                  order_no_ := lines_.order_no;
                  line_no_  := lines_.line_no;
                  rel_no_   := lines_.rel_no;
   
                  OPEN get_component_picked;
                  FETCH get_component_picked INTO exist_;
                  IF (get_component_picked%NOTFOUND) THEN
                     exist_ := 0;
                  END IF;
                  CLOSE get_component_picked;
   
                  IF (exist_ = 0) THEN
                     Shipment_Order_Line_API.Remove_Shipment_Line(lines_.order_no, lines_.line_no, lines_.rel_no, lines_.line_item_no, shipment_id_);
                  END IF;
               END IF;
            END IF;
         END LOOP;
      END IF;
   
      --@ApproveTransactionStatement(2012-01-24,GanNLK)
      SAVEPOINT step_processed;
   
      next_event_ := 10;
   
      IF (Shipment_Order_Line_API.Connected_Lines_Exist(shipment_id_) = 1) THEN
         WHILE (next_event_ IS NOT NULL) LOOP
            Lock_Shipment__(shipment_id_);
         
            IF (next_event_ = 10) THEN
               IF (Plan_Pick_Shipment_Allowed__(shipment_id_, 1) = 1) THEN
                  Reserve_Shipment___(shipment_id_, finalize_on_picked_qty_, consider_reserved_qty_, discon_not_picked_);
                  -- Bug 134220, start
                  IF (consider_reserved_qty_ = 'TRUE' OR finalize_on_picked_qty_ = 'TRUE') THEN
                     Shipment_Handling_Utility_API.Release_Not_Reserved_Qty__(shipment_id_); 
                  END IF;
                  -- Bug 134220, end
               ELSIF ((finalize_on_picked_qty_ = 'TRUE' AND consider_reserved_qty_ = 'FALSE') OR (discon_not_picked_ = 'TRUE')) THEN
                  Reserve_Customer_Order_API.Clear_Unpicked_Shipment_Res__(shipment_id_, consider_reserved_qty_, discon_not_picked_);
                  -- Bug 134220, start
                  Shipment_Handling_Utility_API.Release_Not_Reserved_Qty__(shipment_id_);
                  -- Bug 134220, end
               END IF;         
            ELSIF (next_event_ = 20) THEN
               IF (Create_Pick_List_Allowed__(shipment_id_) = 1) THEN
                  Create_Shipment_Pick_Lists___(shipment_id_);
               END IF;
            ELSIF (next_event_ = 30) THEN
               IF (Print_Pick_List_Allowed__(shipment_id_) = 1) THEN
                  BEGIN
                     Print_Shipment_Pick_List__(shipment_id_, 'FALSE');
                  EXCEPTION
                     WHEN OTHERS THEN
                        Transaction_SYS.Log_Status_Info(SQLERRM, 'WARNING');
                  END;
               END IF;
            ELSIF (next_event_ = 40) THEN
               IF (Pick_Report_Ship_Allowed___(shipment_id_) = 1) THEN
                  Report_Shipment_Pick_Lists___(shipment_id_);
               END IF;
            ELSIF (next_event_ = 50) THEN
               IF (Shipment_API.Complete_Shipment_Allowed__(shipment_id_) = 1) THEN
                  Shipment_API.Complete(shipment_id_);
               END IF;
            ELSIF (next_event_ = 70) THEN
               IF (Deliver_Shipment_Allowed__(shipment_id_) = 1) THEN
                  Deliver_Shipment___(shipment_id_, 'FALSE');
               END IF;
            ELSIF (next_event_ = 60) THEN
               IF (Print_Delivery_Note_Allowed__(shipment_id_) = 1) THEN
                  Print_Shipment_Delivery_Note__(shipment_id_);
               END IF;
            ELSIF (next_event_ = 80) THEN
               IF (Shipment_API.Close_Shipment_Allowed__(shipment_id_) = 1) THEN
                  Shipment_API.Close(shipment_id_);
               END IF;
            END IF;
            -- Commit to avoid long transactions blocking the system
            --@ApproveTransactionStatement(2012-01-24,GanNLK)
            COMMIT;
            -- Set savepoint after each step succesfully processed
            --@ApproveTransactionStatement(2012-01-24,GanNLK)
            SAVEPOINT step_processed;
   
            IF (next_event_ = 20) THEN
               Blocked_Orders_Exist(blocked_orders_exist_, shipment_id_, 'CREATE_PICKLIST');
            ELSIF (next_event_ = 70) THEN
               Blocked_Orders_Exist(blocked_orders_exist_, shipment_id_, 'DELIVER');
            END IF;
            IF (blocked_orders_exist_) THEN
               Error_SYS.Record_General(lu_name_, 'ORDERSBLOCKED: Connected order(s) in shipment :P1 are credit blocked.', shipment_id_);
            END IF;
   
            -- Get the next event to process
            next_event_ := Get_Next_Event___(next_event_);
         END LOOP;
      END IF;
   EXCEPTION
      WHEN others THEN
         error_message_ := sqlerrm;
         -- Note: Rollback to the last savepoint
         --@ApproveTransactionStatement(2012-01-24,GanNLK)
         ROLLBACK to step_processed;
         -- Note: Logg the error
         info_ := Language_SYS.Translate_Constant(lu_name_, 'SHIPMENTERR: Shipment: :P1   :P2',
                                                  NULL, shipment_id_, error_message_);
   
         Transaction_SYS.Set_Status_Info(info_);
         Cust_Order_Event_Creation_API.Shipment_Processing_Error(shipment_id_, error_message_);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Process_Automatic_Shipment__');
   Core(attr_);
END Process_Automatic_Shipment__;


PROCEDURE Lock_Shipment__ (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
   BEGIN
      -- Lock the order while it is being processed
      Shipment_API.Lock_By_Keys__(shipment_id_);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Lock_Shipment__');
   Core(shipment_id_);
END Lock_Shipment__;


FUNCTION Get_Allowed_Ship_Operations__ (
   shipment_id_ IN NUMBER ) RETURN VARCHAR2
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN VARCHAR2
   IS
      operations_ VARCHAR2(20) := NULL;
   BEGIN
      -- Plan picking
      IF (Plan_Pick_Shipment_Allowed__(shipment_id_, 0)  = 1) THEN
         operations_ := operations_ || '0';
      ELSE
         operations_ := operations_ || '*';
      END IF;
   
      -- Report picking
      IF (Pick_Report_Shipment_Allowed__(shipment_id_) = 1) THEN
         operations_ := operations_ || '1';
      ELSE
         operations_ := operations_ || '*';
      END IF;
   
      -- Deliver shipment
      IF (Deliver_Shipment_Allowed__(shipment_id_) = 1) THEN
         operations_ := operations_ || '2';
      ELSE
         operations_ := operations_ || '*';
      END IF;
   
      -- Complete shipment
      IF (Shipment_API.Complete_Shipment_Allowed__(shipment_id_) = 1) THEN
         operations_ := operations_ || '3';
      ELSE
         operations_ := operations_ || '*';
      END IF;
   
      -- Reopen shipment
      IF (Shipment_API.Reopen_Shipment_Allowed__(shipment_id_) = 1) THEN
         operations_ := operations_ || '4';
      ELSE
         operations_ := operations_ || '*';
      END IF;
   
      -- Close shipment
      IF (Shipment_API.Close_Shipment_Allowed__(shipment_id_) = 1) THEN
         operations_ := operations_ || '5';
      ELSE
         operations_ := operations_ || '*';
      END IF;
   
      -- Create Pick List
      IF (Create_Pick_List_Allowed__(shipment_id_) = 1) THEN
         operations_ := operations_ || '6';
      ELSE
         operations_ := operations_ || '*';
      END IF;
   
      -- Print Pick List
      IF (Print_Pick_List_Allowed__(shipment_id_) = 1) THEN
         operations_ := operations_ || '7';
      ELSE
         operations_ := operations_ || '*';
      END IF;
   
      -- Cancel Shipment
      IF (Shipment_API.Cancel_Shipment_Allowed__(shipment_id_) = 1) THEN
         operations_ := operations_ || '8';
      ELSE
         operations_ := operations_ || '*';
      END IF;
   
      -- Print Shipment Pro Forma Invoice
      IF (Print_Proforma_Ivc_Allowed__(shipment_id_) = 1) THEN
         operations_ := operations_ || '9';
      ELSE
         operations_ := operations_ || '*';
      END IF;
      
      -- Finalize Shipment
      IF (Finalize_Shipment_Allowed__(shipment_id_) = 1) THEN
         operations_ := operations_ || 'A';
       ELSE
         operations_ := operations_ || '*';
      END IF;
      
      -- Pack Into Handling Unit or Pack according to Packing Instruction allowed
      IF ((Shipment_API.Pack_Into_Handl_Unit_Allowed__(shipment_id_) = 1) OR (Shipment_API.Pack_Acc_Pack_Instr_Allowed__(shipment_id_) = 1)) THEN
         operations_ := operations_ || 'G';
      ELSE
         operations_ := operations_ || '*';
      END IF;
   
      -- Send dispatch advice
      IF (Shipment_API.Send_Disadv_Allowed__(shipment_id_) = 1) THEN
         operations_ := operations_ || 'S';
      ELSE
         operations_ := operations_ || '*';
      END IF;
   
      -- Report Picking of Customer Order Lines
      IF (Pick_Report_Shipment_Allowed__(shipment_id_, 'TRUE') = 1) THEN
         operations_ := operations_ || 'R';
      ELSE
         operations_ := operations_ || '*';
      END IF;
      
      -- Approve Shipment
      IF (Shipment_API.Approve_Shipment_Allowed__(shipment_id_) = 1) THEN
         operations_ := operations_ || 'P';
      ELSE
         operations_ := operations_ || '*';
      END IF;   
      
      RETURN operations_;
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Get_Allowed_Ship_Operations__');
   RETURN Core(shipment_id_);
END Get_Allowed_Ship_Operations__;


--@IgnoreMissingSysinit
FUNCTION Deliver_Shipment_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN NUMBER
   IS
      allowed_ NUMBER := 0;
   
      CURSOR get_connected_shipments IS
         SELECT shipment_id, rowstate
         FROM   shipment_tab
         WHERE  parent_consol_shipment_id = shipment_id_
         AND    rowstate IN ('Completed', 'Preliminary');
   BEGIN
      IF (Shipment_API.Get_Shipment_Category_Db(shipment_id_) = 'NORMAL') THEN
         allowed_ := Deliver_Shipment_Allowed___(shipment_id_);
      ELSE     
         FOR rec_ IN get_connected_shipments LOOP
            IF (Shipment_Handling_Utility_API.All_Lines_Delivered(rec_.shipment_id) = 'FALSE') THEN                    
               IF (Deliver_Shipment_Allowed___(rec_.shipment_id) = 1) THEN
                  allowed_ := 1; 
               ELSE
                  allowed_ := 0;
                  EXIT;
               END IF;
            END IF;
         END LOOP;
      END IF;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Deliver_Shipment_Allowed__;


PROCEDURE Pick_Plan_Shipment__ (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
      contract_      VARCHAR2(5);
      row_count_     NUMBER;
      pick_ship_tab_ Customer_Order_Flow_API.Pick_Shipment_Table;
      order_blocked_ BOOLEAN := FALSE;
   
      CURSOR get_shipment_lines IS
         SELECT sol.order_no, sol.line_no, sol.rel_no, sol.line_item_no, col.part_no
         FROM customer_order_line_tab col, shipment_order_line_tab sol, shipment_tab s
         WHERE s.shipment_id    = shipment_id_
         AND   s.rowstate       = 'Preliminary'
         AND   sol.shipment_id  = s.shipment_id
         AND   sol.order_no     = col.order_no
         AND   sol.line_no      = col.line_no
         AND   sol.rel_no       = col.rel_no
         AND   sol.line_item_no <= 0
         AND   sol.line_item_no = col.line_item_no
         AND   supply_code IN ('IO', 'PS', 'PI', 'PRD', 'NO', 'PKG', 'PT', 'IPT', 'SO')      
         AND   col.shipment_connected = 'TRUE'
         AND   ((sol.revised_qty_due - sol.qty_assigned - sol.qty_to_ship - sol.qty_shipped) > 0)
         AND   (col.revised_qty_due - col.qty_assigned - col.qty_to_ship - col.qty_on_order - col.qty_shipped + col.qty_shipdiff > 0);
   
      CURSOR get_orders IS
         SELECT sol.order_no
         FROM shipment_order_line_tab sol, shipment_tab s
         WHERE s.shipment_id    = shipment_id_
         AND   s.rowstate       = 'Preliminary'
         AND   sol.shipment_id  = s.shipment_id
         AND   sol.line_item_no <= 0
         GROUP BY sol.order_no;
   BEGIN
      contract_  := Shipment_API.Get_Contract(shipment_id_);
      row_count_ := pick_ship_tab_.COUNT;
   
      FOR linerec_ IN get_shipment_lines LOOP
         pick_ship_tab_(row_count_).order_no     := linerec_.order_no;
         pick_ship_tab_(row_count_).line_no      := linerec_.line_no;
         pick_ship_tab_(row_count_).rel_no       := linerec_.rel_no;
         pick_ship_tab_(row_count_).line_item_no := linerec_.line_item_no;
         pick_ship_tab_(row_count_).contract     := contract_;
         pick_ship_tab_(row_count_).part_no      := linerec_.part_no;   
         row_count_ := row_count_ + 1;
      END LOOP;
      
      IF (pick_ship_tab_.COUNT > 0) THEN
         FOR rec_ IN get_orders LOOP
            Customer_Order_Flow_API.Credit_Check_Order(rec_.order_no, 'PICK_PROPOSAL');
            IF (Customer_Order_API.Get_Objstate(rec_.order_no) = 'CreditBlocked') THEN
               order_blocked_ := TRUE;
            END if;
         END LOOP;
         
         IF (NOT order_blocked_) THEN
            Customer_Order_Flow_API.Start_Plan_Picking__(pick_ship_tab_, shipment_id_);
         END IF;
      END IF;
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Pick_Plan_Shipment__');
   Core(shipment_id_);
END Pick_Plan_Shipment__;


--@IgnoreMissingSysinit
FUNCTION Plan_Pick_Shipment_Allowed__ (
   shipment_id_ IN NUMBER,
   finalize_    IN NUMBER ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER,
      finalize_    IN NUMBER ) RETURN NUMBER
   IS
      allowed_  NUMBER;
      CURSOR get_connected_shipments IS
         SELECT shipment_id
         FROM   shipment_tab
         WHERE  parent_consol_shipment_id = shipment_id_
         AND    rowstate = 'Preliminary'; 
   BEGIN
      IF (Shipment_API.Get_Shipment_Category_Db(shipment_id_) = 'NORMAL') THEN
         allowed_ := Plan_Pick_Shipment_Allowed___(shipment_id_, finalize_);
      ELSE
         allowed_ := 0;
         FOR rec_ IN get_connected_shipments LOOP
            IF (Plan_Pick_Shipment_Allowed___(rec_.shipment_id, finalize_) = 1) THEN
               allowed_ := 1;
               EXIT;
            END IF;
         END LOOP;
      END IF;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_, finalize_);
END Plan_Pick_Shipment_Allowed__;


--@IgnoreMissingSysinit
FUNCTION Pick_Report_Shipment_Allowed__ (
   shipment_id_               IN NUMBER,
   report_pick_from_co_lines_ IN VARCHAR2 DEFAULT 'FALSE' ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_               IN NUMBER,
      report_pick_from_co_lines_ IN VARCHAR2 DEFAULT 'FALSE' ) RETURN NUMBER
   IS
      allowed_ NUMBER := 0;
      CURSOR get_connected_shipments IS
         SELECT shipment_id
         FROM   shipment_tab
         WHERE  parent_consol_shipment_id = shipment_id_
         AND    rowstate = 'Preliminary'; 
   BEGIN
      IF (Shipment_API.Get_Shipment_Category_Db(shipment_id_) = 'NORMAL') THEN
         -- Bug 130619, Passed report_pick_from_co_lines_ into the Pick_Report_Ship_Allowed___() 
         IF (Pick_Report_Ship_Allowed___(shipment_id_,report_pick_from_co_lines_) = 1) THEN
            allowed_ := Pick_Customer_Order_API.Is_Ship_Pick_Report_Allowed(shipment_id_, report_pick_from_co_lines_);
         END IF;
      ELSE    
         FOR rec_ IN get_connected_shipments LOOP
            IF (Pick_Report_Ship_Allowed___(rec_.shipment_id,report_pick_from_co_lines_) = 1) THEN
               allowed_ := Pick_Customer_Order_API.Is_Ship_Pick_Report_Allowed(rec_.shipment_id, report_pick_from_co_lines_);            
            END IF;
            IF (allowed_ = 1) THEN
               EXIT;
            END IF;
         END LOOP;
      END IF;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_, report_pick_from_co_lines_);
END Pick_Report_Shipment_Allowed__;


PROCEDURE Print_Shipment_Pick_List__ (
   shipment_id_           IN NUMBER,
   process_in_background_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      shipment_id_           IN NUMBER,
      process_in_background_ IN VARCHAR2 )
   IS
      description_  VARCHAR2(2000);
      attr_         VARCHAR2(2000);
   BEGIN
      Client_SYS.Clear_Attr(attr_);
      Client_SYS.Add_To_Attr('SHIPMENT_ID', shipment_id_, attr_);
   
      IF (process_in_background_ = 'TRUE') THEN
         description_ := Language_SYS.Translate_Constant(lu_name_, 'PRINT_CONSOL_SHIP: Print Consolidated Pick List');
         Transaction_SYS.Deferred_Call('SHIPMENT_FLOW_API.Start_Print_Consol_Pl__', attr_, description_);
      ELSE
         Start_Print_Consol_Pl__(attr_);
      END IF;
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Print_Shipment_Pick_List__');
   Core(shipment_id_, process_in_background_);
END Print_Shipment_Pick_List__;


PROCEDURE Print_Shipment_Delivery_Note__ (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
      delnote_no_           VARCHAR2(15);    
      print_job_id_         NUMBER;
      parameter_attr_       VARCHAR2(32000);      
      no_of_delnote_copies_ NUMBER;
      customer_no_          VARCHAR2(20);
      alt_delnote_no_       VARCHAR2(50);
      -- Bug 123054, start
      result_               NUMBER;
      -- Bug 123054, end
      
      -- Bug 137151, added the condition to avoid selecting of delnote nos where rowstate is 'Invalid'.
      CURSOR get_delivery_note IS
         SELECT delnote_no, alt_delnote_no
         FROM   customer_order_deliv_note_tab
         WHERE  shipment_id = shipment_id_ 
         AND    rowstate != 'Invalid'; 
   
      CURSOR get_customer_no IS
         SELECT deliver_to_customer_no 
         FROM   shipment_tab
         WHERE  shipment_id = shipment_id_;
   BEGIN
      OPEN  get_delivery_note;
      FETCH get_delivery_note INTO delnote_no_, alt_delnote_no_;
      CLOSE get_delivery_note;
   
      OPEN  get_customer_no;
      FETCH get_customer_no INTO customer_no_;
      CLOSE get_customer_no;
   
      IF (delnote_no_ IS NULL) THEN
         Create_Order_Delivery_Note_API.Create_Shipment_Deliv_Note(delnote_no_, shipment_id_);
      END IF;
      
      no_of_delnote_copies_ := NVL(Cust_Ord_Customer_API.Get_No_Delnote_Copies(customer_no_), 0);  
   
      FOR delnote_copy_no_ IN 0..no_of_delnote_copies_ LOOP
         Client_SYS.Clear_Attr(parameter_attr_);
         Client_SYS.Add_To_Attr('SHIPMENT_ID' ,    shipment_id_,     parameter_attr_);
         Client_SYS.Add_To_Attr('DELNOTE_NO' ,     delnote_no_,      parameter_attr_);
         Client_SYS.Add_To_Attr('DELNOTE_COPY_NO', delnote_copy_no_, parameter_attr_);
         Client_SYS.Add_To_Attr('ALT_DELIV_NOTE',  alt_delnote_no_,  parameter_attr_);     
         -- Bug 123054, Passed result_ as a parameter.
         -- Create one print job for original report and attach print job instances to same print job if there are no of copies 
         -- Bug 131342, start
         result_ := NULL;
         -- Bug 131342, end
         Customer_Order_Flow_API.Create_Print_Jobs(print_job_id_, result_, 'SHIPMENT_DELIVERY_NOTE_REP', parameter_attr_);
      END LOOP;
      
      -- Only one job will be created for a particular report. So this method will be called per print job.
      Customer_Order_Flow_API.Printing_Print_Jobs(print_job_id_);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Print_Shipment_Delivery_Note__');
   Core(shipment_id_);
END Print_Shipment_Delivery_Note__;


--@IgnoreMissingSysinit
FUNCTION Print_Pick_List_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN NUMBER
   IS
      allowed_ NUMBER := 0;
      CURSOR get_connected_shipments IS
         SELECT shipment_id
         FROM   shipment_tab
         WHERE  parent_consol_shipment_id = shipment_id_
         AND    rowstate != 'Cancelled'; 
   BEGIN
      IF (Shipment_API.Get_Shipment_Category_Db(shipment_id_) = 'NORMAL') THEN
         -- Bug 124537, Used Print_Pick_List_Allowed___() call instead of Pick_Customer_Order_API.Is_Ship_Pick_Report_Allowed.
         allowed_ := Print_Pick_List_Allowed___(shipment_id_);
      ELSE    
         FOR rec_ IN get_connected_shipments LOOP
            IF (Print_Pick_List_Allowed___(rec_.shipment_id) = 1) THEN
               -- Bug 124537, Set the value as TRUE instead of fetching it through Pick_Customer_Order_API.Is_Ship_Pick_Report_Allowed.
               allowed_ := 1;
               EXIT;
            END IF;
         END LOOP;
      END IF;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Print_Pick_List_Allowed__;


--@IgnoreMissingSysinit
FUNCTION Print_Delivery_Note_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN NUMBER
   IS
      allowed_ NUMBER;
      CURSOR get_del_note IS
         SELECT 1
         FROM   customer_order_deliv_note_tab
         WHERE  shipment_id = shipment_id_ 
         AND    rowstate NOT IN ('Preliminary', 'Printed');      
   BEGIN
      OPEN get_del_note;
      FETCH get_del_note INTO allowed_;
      IF (get_del_note%NOTFOUND) THEN
         allowed_ := 0;
      END IF;
      CLOSE get_del_note;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Print_Delivery_Note_Allowed__;


--@IgnoreMissingSysinit
FUNCTION Print_Proforma_Ivc_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN NUMBER
   IS
      allowed_ NUMBER;
      CURSOR get_connected_shipments IS
         SELECT shipment_id
         FROM shipment_tab
         WHERE parent_consol_shipment_id = shipment_id_
         AND   rowstate IN ('Completed','Closed'); 
   BEGIN
      IF (Shipment_API.Get_Shipment_Category_Db(shipment_id_) = 'NORMAL') THEN
         IF (Print_Proforma_Ivc_Allowed___(shipment_id_) = 1) THEN
            allowed_ := 1;
         END IF;
      ELSE
         allowed_ := 0;    
         FOR rec_ IN get_connected_shipments LOOP
            IF (Print_Proforma_Ivc_Allowed___(rec_.shipment_id) = 1) THEN
               allowed_ := 1;
               EXIT;
            END IF;
         END LOOP;
      END IF;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Print_Proforma_Ivc_Allowed__;


--@IgnoreMissingSysinit
FUNCTION Get_Pick_Lists_For_Shipment__ (
   shipment_id_  IN NUMBER,
   printed_flag_ IN VARCHAR2 ) RETURN VARCHAR2
IS
   
   FUNCTION Core (
      shipment_id_  IN NUMBER,
      printed_flag_ IN VARCHAR2 ) RETURN VARCHAR2
   IS
      pick_list_no_list_ VARCHAR2(32000) := NULL; 
      CURSOR get_connected_shipments IS
         SELECT shipment_id
         FROM   SHIPMENT_TAB
         WHERE  parent_consol_shipment_id =  shipment_id_
         AND    rowstate = 'Preliminary';
   BEGIN   
      IF (Shipment_API.Get_Shipment_Category_Db(shipment_id_) = 'NORMAL') THEN
         pick_list_no_list_ := Get_Pick_Lists_For_Shipment___(shipment_id_, printed_flag_);     
      ELSE      
         FOR rec_ IN get_connected_shipments LOOP
            IF (Pick_Report_Shipment_Allowed__(rec_.shipment_id) = 1) THEN
               pick_list_no_list_ := pick_list_no_list_ || Get_Pick_Lists_For_Shipment___(rec_.shipment_id, printed_flag_) ||Client_SYS.field_separator_;      
            END IF;
         END LOOP;
      END IF;
      RETURN pick_list_no_list_;
   END Core;

BEGIN
   RETURN Core(shipment_id_, printed_flag_);
END Get_Pick_Lists_For_Shipment__;


--@IgnoreMissingSysinit
FUNCTION Create_Pick_List_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN NUMBER
   IS
      allowed_   NUMBER;
      CURSOR get_connected_shipments IS
         SELECT shipment_id
         FROM   shipment_tab
         WHERE  parent_consol_shipment_id = shipment_id_
         AND    rowstate = 'Preliminary';
   BEGIN
      IF (Shipment_API.Get_Shipment_Category_Db(shipment_id_) = 'NORMAL') THEN
         allowed_ := Create_Pick_List_Allowed___(shipment_id_);
      ELSE
         allowed_ := 0;
         FOR rec_ IN get_connected_shipments LOOP
            IF (Create_Pick_List_Allowed___(rec_.shipment_id) = 1) THEN
               allowed_ := 1;
               EXIT;
            END IF;
         END LOOP;
      END IF;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Create_Pick_List_Allowed__;


PROCEDURE Create_Shipment_Pick_Lists__ (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
      CURSOR get_shipment_orders IS
         SELECT order_no, line_no, rel_no, line_item_no 
         FROM SHIPMENT_ORDER_LINE_TAB
         WHERE shipment_id = shipment_id_;
   BEGIN
      Lock_Shipment__(shipment_id_);
      IF (Create_Pick_List_Allowed__(shipment_id_) = 1) THEN
         FOR order_rec_ IN get_shipment_orders LOOP
            IF (Order_Config_Util_API.Check_Ord_Line_Config_Mismatch(order_rec_.order_no, order_rec_.line_no, order_rec_.rel_no, order_rec_.line_item_no) = 'TRUE') THEN                
               Error_SYS.Record_General(lu_name_, 'CONFIGMISMATCH: Pick list creation is not allowed since supply site configuration is different from demand site configuration in the connected order no :P1, line no :P2, release no :P3.', order_rec_.order_no, order_rec_.line_no, order_rec_.rel_no);
            END IF;
         END LOOP;
         Create_Shipment_Pick_Lists___(shipment_id_);
      END IF;
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Create_Shipment_Pick_Lists__');
   Core(shipment_id_);
END Create_Shipment_Pick_Lists__;


--@IgnoreMissingSysinit
FUNCTION Create_Ship_Invoice_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN NUMBER
   IS
      allowed_ NUMBER := 0;
   BEGIN
      IF Invoice_Customer_Order_API.Is_Shipment_Invoiceable(shipment_id_) THEN
         allowed_  := 1;
      END IF;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Create_Ship_Invoice_Allowed__;


--@IgnoreMissingSysinit
FUNCTION Print_Invoice_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN NUMBER
   IS
   BEGIN
      IF (Customer_Order_Inv_Head_API.Allow_Print_Shipment_Invoice(shipment_id_) = 1) THEN
         RETURN 1;
      ELSE
         RETURN 0;
      END IF;
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Print_Invoice_Allowed__;


PROCEDURE Start_Print_Consol_Pl__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
      shipment_id_ NUMBER;
   
      CURSOR get_pick_list IS
         SELECT pick_list_no
         FROM   customer_order_pick_list_tab
         WHERE  shipment_id       = shipment_id_
         AND    printed_flag      = 'N'
         AND    picking_confirmed = 'UNPICKED';
   BEGIN
      shipment_id_ := Client_SYS.Attr_Value_To_Number(Client_SYS.Get_Item_Value('SHIPMENT_ID', attr_));
      FOR rec_ IN get_pick_list LOOP
         Customer_Order_Flow_API.Print_Pick_List(rec_.pick_list_no);
      END LOOP;
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Print_Consol_Pl__');
   Core(attr_);
END Start_Print_Consol_Pl__;


PROCEDURE Start_Print_Ship_Consol_Pl__ (
   attr_ IN OUT NOCOPY VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN OUT NOCOPY VARCHAR2 )
   IS
      description_  VARCHAR2(200);
   BEGIN
      Trace_SYS.Field('attr_', attr_);
      description_ := Language_SYS.Translate_Constant(lu_name_, 'PRINTSHIPCONSPL: Print Shipment Consolidated Pick List');
      Transaction_SYS.Deferred_Call('SHIPMENT_FLOW_API.Print_Ship_Consol_Pick_List__', attr_, description_);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Print_Ship_Consol_Pl__');
   Core(attr_);
END Start_Print_Ship_Consol_Pl__;


PROCEDURE Print_Ship_Consol_Pick_List__ (
   attr_ IN OUT NOCOPY VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN OUT NOCOPY VARCHAR2 )
   IS
      ptr_          NUMBER;
      name_         VARCHAR2(30);
      value_        VARCHAR2(2000);
      pick_list_no_ VARCHAR2(15);
   BEGIN
      WHILE (CLIENT_SYS.Get_Next_From_Attr(attr_, ptr_, name_, value_)) LOOP
         IF (name_ = 'PICK_LIST_NO') THEN
            pick_list_no_ := value_;
            Print_Ship_Consol_Pl___(pick_list_no_);
            Trace_SYS.Field('pick_list_no_', pick_list_no_);
         END IF;
      END LOOP;
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Print_Ship_Consol_Pick_List__');
   Core(attr_);
END Print_Ship_Consol_Pick_List__;


--@IgnoreMissingSysinit
FUNCTION Contains_Dangerous_Goods__ (
   shipment_id_ IN NUMBER ) RETURN VARCHAR2
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN VARCHAR2
   IS
      dummy_           NUMBER;
      dangerous_goods_ VARCHAR2(5) := 'TRUE';
      CURSOR find_dangerous_goods IS
         SELECT 1
         FROM   shipment_order_line_tab
         WHERE  shipment_id = shipment_id_
         AND    Part_Catalog_Invent_Attrib_API.Get_Adr_Rid_Class_Id(
                   Customer_Order_Line_API.Get_Catalog_No(order_no, line_no, rel_no, line_item_no)) IS NOT NULL;
   BEGIN
      OPEN  find_dangerous_goods;
      FETCH find_dangerous_goods INTO dummy_;
      IF (find_dangerous_goods%NOTFOUND) THEN
         dangerous_goods_ := 'FALSE';
      END IF;
      CLOSE find_dangerous_goods;
      RETURN dangerous_goods_;
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Contains_Dangerous_Goods__;


--@IgnoreMissingSysinit
FUNCTION Finalize_Shipment_Allowed__ (
   shipment_id_ IN NUMBER ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN NUMBER
   IS
      allowed_     NUMBER;
      CURSOR finalize_shipment IS
         SELECT 1
         FROM   SHIPMENT_ORDER_LINE_TAB sol, CUSTOMER_ORDER_TAB co, SHIPMENT_TAB s
         WHERE  sol.shipment_id  = shipment_id_
         AND    s.shipment_id    = sol.shipment_id
         AND    co.order_no      = sol.order_no
         AND    co.rowstate     != 'CreditBlocked'
         AND    s.rowstate      != 'Closed';
   BEGIN
      OPEN finalize_shipment;
      FETCH finalize_shipment INTO allowed_;
      IF (finalize_shipment%NOTFOUND) THEN
         allowed_ := 0;
      END IF;
      CLOSE finalize_shipment;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Finalize_Shipment_Allowed__;


PROCEDURE Process_Mandatory_Events__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
      start_event_          NUMBER;
      next_event_           NUMBER;
      ptr_                  NUMBER;
      name_                 VARCHAR2(30);
      value_                VARCHAR2(2000);
      shipment_id_          NUMBER; 
      shipment_type_        SHIPMENT_TYPE_TAB.shipment_type%TYPE;   
      error_message_        VARCHAR2(2000);
      info_                 VARCHAR2(2000);
      location_no_          VARCHAR2(35);
      use_ship_inventory_   VARCHAR2(5);
      invoice_id_           NUMBER;
      media_code_           VARCHAR2(30) := NULL;
      shipment_rec_         Shipment_API.Public_Rec;
      company_              VARCHAR2(20);
      identity_             VARCHAR2(20);
      your_reference_       VARCHAR2(30);
      inv_addr_id_          VARCHAR2(50);
      cust_email_addr_      VARCHAR2(200);   
      email_invoice_        VARCHAR2(5) := NULL;
      deliver_through_cs_   VARCHAR2(5) := 'FALSE';
      dummy_attr_           VARCHAR2(2000);
      blocked_orders_exist_ BOOLEAN := FALSE;
      blocked_orders_found  EXCEPTION;
      rental_transfer_db_   VARCHAR2(5) := db_false_;
      
      CURSOR get_inv_details(invoice_id_ IN VARCHAR2, company_ IN VARCHAR2) IS
         SELECT identity, your_reference, invoice_address_id
         FROM   customer_order_inv_head
         WHERE  invoice_id = invoice_id_
         AND    company    = company_;  
   BEGIN
      --@ApproveTransactionStatement(2012-07-04,MaEelk)
      SAVEPOINT event_processed;
      
      -- Retrieve parameters from the attribute string
      WHILE (Client_SYS.Get_Next_From_Attr(attr_, ptr_, name_, value_)) LOOP
         IF (name_ = 'START_EVENT') THEN
            start_event_ := Client_SYS.Attr_Value_To_Number(value_);
         ELSIF (name_ = 'SHIPMENT_ID') THEN
            shipment_id_ := value_;
         ELSIF (name_ = 'SHIPMENT_TYPE') THEN
            shipment_type_ := value_;
         ELSIF (name_ = 'LOCATION_NO') THEN   
            location_no_ := value_;  
         ELSIF (name_ = 'USE_SHIP_INVENTORY') THEN
            use_ship_inventory_ := value_;
         ELSIF (name_ = 'DELIVER_THROUGH_CS') THEN
            deliver_through_cs_ := value_;
         ELSIF (name_ = 'RENTAL_TRANSFER_DB') THEN
            rental_transfer_db_ := value_;      
         END IF;
      END LOOP;
      
      shipment_rec_ := Shipment_API.Get(shipment_id_);
      company_      := Site_API.Get_Company(shipment_rec_.contract);
      
      next_event_ := start_event_;
      WHILE (next_event_ IS NOT NULL ) LOOP
         -- Lock the current shipment for processing
         -- The order needs to be relocked before each step because each step ends with a COMMIT
         Lock_Shipment__(shipment_id_);
         
         -- If the next step is to reserve the shipment
         IF (next_event_ = 10) THEN
            IF (Plan_Pick_Shipment_Allowed__(shipment_id_, 1) = 1) THEN
               Pick_Plan_Shipment__(shipment_id_);
               --@ApproveTransactionStatement(2014-12-16,jelise)
               COMMIT;
               Blocked_Orders_Exist(blocked_orders_exist_, shipment_id_, 'RESERVE');
               IF (blocked_orders_exist_) THEN
                  RAISE blocked_orders_found;
               END IF;
               Process_Optional_Events___(shipment_id_, shipment_type_, 10);
            END IF;
         -- If the next step is to create the pick list         
         ELSIF (next_event_ = 20) THEN
            IF (Create_Pick_List_Allowed__(shipment_id_) = 1) THEN
               Create_Shipment_Pick_Lists___(shipment_id_);
               --@ApproveTransactionStatement(2014-12-16,jelise)
               COMMIT;
               Blocked_Orders_Exist(blocked_orders_exist_, shipment_id_, 'CREATE_PICKLIST');
               IF (blocked_orders_exist_) THEN
                  RAISE blocked_orders_found;
               END IF;
               Process_Optional_Events___(shipment_id_, shipment_type_, 20);
            END IF;
         -- If the next step is to print the pick list                  
         ELSIF (next_event_ = 30) THEN
            IF (Print_Pick_List_Allowed__(shipment_id_) = 1) THEN
               Print_Shipment_Pick_List__(shipment_id_, 'FALSE');
               --@ApproveTransactionStatement(2014-12-16,jelise)
               COMMIT;
               Process_Optional_Events___(shipment_id_, shipment_type_, 30);
            END IF;
         -- If the next step is report picking
         ELSIF (next_event_ = 40) THEN
            IF (Pick_Report_Shipment_Allowed__(shipment_id_) = 1) THEN
               IF (use_ship_inventory_ = 'TRUE') THEN
                  Report_Shipment_Pick_Lists___(shipment_id_, location_no_);
               ELSE
                  Report_Shipment_Pick_Lists___(shipment_id_);
               END IF;               
               --@ApproveTransactionStatement(2014-12-16,jelise)
               COMMIT;
               Process_Optional_Events___(shipment_id_, shipment_type_, 40);
            END IF;
         -- If the next step is to complete the shipment
         ELSIF (next_event_ = 50) THEN
            IF (Shipment_API.Complete_Shipment_Allowed__(shipment_id_) = 1) THEN
               Shipment_API.Complete_Shipment__(shipment_id_);
               --@ApproveTransactionStatement(2014-12-16,jelise)
               COMMIT;
               Process_Optional_Events___(shipment_id_, shipment_type_, 50);
            END IF;
         -- If the next step is to deliver the shipment         
         ELSIF (next_event_ = 60) THEN
            IF (Deliver_Shipment_Allowed__(shipment_id_) = 1) THEN
               Deliver_Shipment___(shipment_id_, deliver_through_cs_);
               --@ApproveTransactionStatement(2014-12-16,jelise)
               COMMIT;
               Blocked_Orders_Exist(blocked_orders_exist_, shipment_id_, 'DELIVER');
               IF (blocked_orders_exist_) THEN
                  RAISE blocked_orders_found;
               END IF;
               Process_Optional_Events___(shipment_id_, shipment_type_, 60);
            END IF;
         -- If the next step is to close the shipment
         ELSIF (next_event_ = 70) THEN
            IF (Shipment_API.Close_Shipment_Allowed__(shipment_id_) = 1) THEN
               Shipment_API.Close(shipment_id_);
               --@ApproveTransactionStatement(2014-12-16,jelise)
               COMMIT;
               Process_Optional_Events___(shipment_id_, shipment_type_, 70);
            END IF;
         -- If the next step is to create the invoice
         ELSIF (next_event_ = 80) THEN
            IF (Create_Ship_Invoice_Allowed__(shipment_id_) = 1) THEN
               dummy_attr_ := NULL;
               Invoice_Customer_Order_API.Make_Shipment_Invoice__(invoice_id_, dummy_attr_, shipment_id_);
               --@ApproveTransactionStatement(2014-12-16,jelise)
               COMMIT;
               Process_Optional_Events___(shipment_id_, shipment_type_, 80);
            END IF;         
         -- If the next step is to print the invoice
         ELSIF (next_event_ = 90) THEN
            IF ((invoice_id_ IS NOT NULL) AND (Print_Invoice_Allowed__(shipment_id_) = 1)) THEN
               media_code_ := Cust_Ord_Customer_API.Get_Default_Media_Code(shipment_rec_.deliver_to_customer_no, 'INVOIC', company_);
               
               Check_Manual_Tax_Lia_Date___(invoice_id_, shipment_id_);
               
               OPEN get_inv_details(invoice_id_, company_);
               FETCH get_inv_details INTO identity_, your_reference_, inv_addr_id_; 
               CLOSE get_inv_details;
               
               cust_email_addr_ := Cust_Ord_Customer_Address_API.Get_Email(identity_, your_reference_, inv_addr_id_); 
               email_invoice_   := Cust_Ord_Customer_API.Get_Email_Invoice_Db(identity_);
                           
               Print_Invoice___(invoice_id_, media_code_, cust_email_addr_, email_invoice_);
               --@ApproveTransactionStatement(2014-12-16,jelise)
               COMMIT;
               Process_Optional_Events___(shipment_id_, shipment_type_, 90);
            END IF;
         END IF;
         -- Commit changes made so far to avoid long transactions for fast order types
         --@ApproveTransactionStatement(2012-07-04,MaEelk)
         COMMIT;
         -- Set before processing next event
         --@ApproveTransactionStatement(2012-07-04,MaEelk)
         SAVEPOINT event_processed;
   
         next_event_ := Shipment_Type_Event_Api.Get_Next_Event(shipment_type_, next_event_, rental_transfer_db_);
      END LOOP;
   EXCEPTION
      WHEN blocked_orders_found THEN
         IF (Transaction_SYS.Is_Session_Deferred) THEN
            error_message_ :=  Language_SYS.Translate_Constant(lu_name_, 'ORDERSBLOCKED: Connected order(s) in shipment :P1 are credit blocked.', NULL, shipment_id_);
            Transaction_SYS.Set_Status_Info(error_message_);
            Cust_Order_Event_Creation_API.Order_Processing_Error(shipment_id_, error_message_);
         ELSE
            Error_SYS.Record_General(lu_name_, 'ORDERSBLOCKED: Connected order(s) in shipment :P1 are credit blocked.', shipment_id_);
         END IF;
      WHEN others THEN
         error_message_ := sqlerrm;
         -- Rollback to the last savepoint
         --@ApproveTransactionStatement(2012-07-04,MaEelk)
         ROLLBACK to event_processed;
         IF (Transaction_SYS.Is_Session_Deferred) THEN
            -- Logg the error
            info_ := Language_SYS.Translate_Constant(lu_name_, 'SHIPMENTERROR: Shipment :P1   :P2',
                                                     NULL, shipment_id_, error_message_);
            Transaction_SYS.Set_Status_Info(info_);
            Cust_Order_Event_Creation_API.Order_Processing_Error(shipment_id_, error_message_);
         ELSE
            -- Raise the error
            RAISE;
         END IF;
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Process_Mandatory_Events__');
   Core(attr_);
END Process_Mandatory_Events__;


PROCEDURE Process_All_Shipments__ (
   attr_            IN VARCHAR2,
   mandatory_event_ IN BOOLEAN )
IS
   
   PROCEDURE Core (
      attr_            IN VARCHAR2,
      mandatory_event_ IN BOOLEAN )
   IS
      start_event_        NUMBER;
      ptr_                NUMBER;
      name_               VARCHAR2(30);
      value_              VARCHAR2(2000);
      shiment_attr_       VARCHAR2(2000);
      shipment_type_      VARCHAR2(3);
      shipment_id_        NUMBER;   
      location_no_        VARCHAR2(35);
      allowed_operation_  BOOLEAN;     
      ascending_order_    VARCHAR2(5);
      rental_transfer_db_ VARCHAR2(5) := db_false_;
      -- Bug 135579, start
      dummy_                 NUMBER;
      execute_shipment_flow_ BOOLEAN := FALSE;
      -- Bug 135579, end
      
      CURSOR get_connected_shipments IS
         SELECT shipment_id, shipment_type
         FROM   shipment_tab
         WHERE  parent_consol_shipment_id = shipment_id_
         AND    rowstate != 'Cancelled'
         ORDER BY DECODE(ascending_order_, 'TRUE', load_sequence_no, (load_sequence_no * -1));
         
      -- Bug 135579, start   
       CURSOR check_non_deliverying_shipment IS
         SELECT 1
         FROM   shipment_tab st, shipment_type_event_tab stet
         WHERE  st.parent_consol_shipment_id = shipment_id_
         AND    st.shipment_type = stet.shipment_type
         AND    stet.event = 50
         AND    (st.rowstate != 'Preliminary' OR stet.stop_flag = 'TRUE' OR st.approve_before_delivery = 'TRUE');
      -- Bug 135579, end
   BEGIN
      -- Retrieve all the shipments to be processed and process the shipments one by one
      Client_SYS.Clear_Attr(shiment_attr_);
      WHILE (Client_SYS.Get_Next_From_Attr(attr_, ptr_, name_, value_)) LOOP
         -- 'END' should be the last parameter passed for each order
         IF (name_ = 'END') THEN
            IF (Shipment_API.Get_Shipment_Category_Db(shipment_id_) = 'NORMAL') THEN
               IF (start_event_ = 1400) THEN
                  Print_Consignment_Note___(shipment_id_);  
               ELSIF (start_event_ = 1500) THEN
                  Print_Bill_Of_Lading___(shipment_id_);
               ELSIF (start_event_ = 1600) THEN
                  Print_Packing_List___(shipment_id_);
               ELSIF (start_event_ = 1700) THEN
                  Print_Shipment_Delivery_Note__(shipment_id_);
               ELSIF (start_event_ = 1800) THEN
                  Print_Pro_Forma_Invoice___(shipment_id_);
               ELSIF (start_event_ = 1900) THEN
                  Print_Address_Label___(shipment_id_); 
               ELSE
                  Process_Shipment___(mandatory_event_, shipment_type_, shiment_attr_, start_event_);            
               END IF;
               Client_SYS.Clear_Attr(shiment_attr_);
            ELSE
               IF (Shipment_API.Shipments_Connected__(shipment_id_) = 0) THEN
                  IF (start_event_ = 1100) THEN
                     Shipment_API.Cancel_Shipment__(shipment_id_);
                  END IF;
               ELSE
                  IF (start_event_ IN (1400, 1500, 1700, 1800)) THEN
                     ascending_order_ := 'FALSE';
                  ELSE
                     ascending_order_ := 'TRUE';
                  END IF;
                  
                  -- Bug 135579, start
                  OPEN  check_non_deliverying_shipment;
                  FETCH check_non_deliverying_shipment INTO dummy_;
                  IF (check_non_deliverying_shipment%NOTFOUND) THEN
                     execute_shipment_flow_ := TRUE;
                  END IF;
                  CLOSE check_non_deliverying_shipment;
                  -- Bug 135579, end
                     
                  FOR rec_ IN get_connected_shipments LOOP
                     Client_SYS.Clear_Attr(shiment_attr_);
                     Client_SYS.Add_To_Attr('START_EVENT', start_event_, shiment_attr_); 
                     Client_SYS.Add_To_Attr('SHIPMENT_ID', rec_.shipment_id, shiment_attr_);
                     Client_SYS.Add_To_Attr('SHIPMENT_TYPE', rec_.shipment_type, shiment_attr_);
                     Client_SYS.Add_To_Attr('RENTAL_TRANSFER_DB', rental_transfer_db_, shiment_attr_);
                     allowed_operation_ := FALSE;
   
                     CASE start_event_
                        WHEN 10 THEN
                           allowed_operation_     := (Plan_Pick_Shipment_Allowed__(rec_.shipment_id, 0)= 1); 
                        WHEN 20 THEN
                           allowed_operation_     := (Create_Pick_List_Allowed__(rec_.shipment_id)= 1);
                        WHEN 30 THEN
                           allowed_operation_     := (Print_Pick_List_Allowed__(rec_.shipment_id)= 1);
                        WHEN 50 THEN
                           allowed_operation_     := (Shipment_API.Complete_Shipment_Allowed__(rec_.shipment_id)= 1);
                           -- Bug 135579, start
                           IF (allowed_operation_) THEN     
                              IF (execute_shipment_flow_) THEN
                                 Client_SYS.Add_To_Attr('DELIVER_THROUGH_CS', 'TRUE', shiment_attr_); 
                              END IF;        
                           END IF;
                           -- Bug 135579, end
                        WHEN 60 THEN
                           allowed_operation_     := (Deliver_Shipment_Allowed__(rec_.shipment_id)= 1);
                           Client_SYS.Add_To_Attr('DELIVER_THROUGH_CS', 'TRUE', shiment_attr_); 
                        WHEN 70 THEN
                           allowed_operation_     := (Shipment_API.Close_Shipment_Allowed__(rec_.shipment_id) = 1);
                        WHEN 1000 THEN
                           allowed_operation_     := (Shipment_API.Reopen_Shipment_Allowed__(rec_.shipment_id) = 1);
                        WHEN 1100 THEN
                           allowed_operation_     := (Shipment_API.Cancel_Shipment_Allowed__(rec_.shipment_id) = 1);
                        WHEN 1200 THEN
                           allowed_operation_     := (Shipment_API.Pack_Acc_Pack_Instr_Allowed__(rec_.shipment_id) = 1);
                        WHEN 1250 THEN 
                           allowed_operation_     := (Shipment_API.Pack_Into_Handl_Unit_Allowed__(rec_.shipment_id) = 1);
                        WHEN 1300 THEN
                           allowed_operation_     := (Shipment_API.Send_Disadv_Allowed__(rec_.shipment_id) = 1);
                           Client_SYS.Add_To_Attr('DELIVERY_NOTE_NO', Customer_Order_Deliv_Note_API.Get_Delnote_No_For_Shipment(rec_.shipment_id), shiment_attr_);
                           Client_SYS.Add_To_Attr('MEDIA_CODE', Cust_Ord_Customer_API.Get_Default_Media_Code(Shipment_API.Get_Deliver_To_Customer_No(rec_.shipment_id),'DESADV'), shiment_attr_);  
                        WHEN 1400 THEN
                           Print_Consignment_Note___(rec_.shipment_id);  
                        WHEN 1500 THEN
                           Print_Bill_Of_Lading___(rec_.shipment_id);
                        WHEN 1600 THEN
                           Print_Packing_List___(rec_.shipment_id);
                        WHEN 1700 THEN
                           Print_Shipment_Delivery_Note__(rec_.shipment_id);
                        WHEN 1800 THEN
                           IF (Print_Proforma_Ivc_Allowed__(rec_.shipment_id) = 1) THEN
                              Print_Pro_Forma_Invoice___(rec_.shipment_id);
                           END IF;
                        WHEN 1900 THEN
                           Print_Address_Label___(rec_.shipment_id);
                        ELSE
                           NULL;
                     END CASE;
                     IF (allowed_operation_) THEN
                        Process_Shipment___(mandatory_event_, rec_.shipment_type, shiment_attr_, start_event_);               
                     END IF;
                   END LOOP;            
               END IF;
            END IF;  
         ELSE
            Client_SYS.Add_To_Attr(name_, value_, shiment_attr_);
            IF (name_ = 'START_EVENT') THEN
               start_event_ := Client_SYS.Attr_Value_To_Number(value_); 
            ELSIF (name_ = 'SHIPMENT_ID') THEN
               shipment_id_ := Client_SYS.Attr_Value_To_Number(value_);     
            ELSIF (name_ = 'SHIPMENT_TYPE') THEN
               shipment_type_ := value_;        
            ELSIF (name_ = 'LOCATION_NO') THEN
               location_no_ := value_;  
            ELSIF (name_ = 'RENTAL_TRANSFER_DB') THEN
               rental_transfer_db_ := value_;
            END IF;
         END IF;
      END LOOP;
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Process_All_Shipments__');
   Core(attr_, mandatory_event_);
END Process_All_Shipments__;


PROCEDURE Start_Reserve_Shipment__ (
   info_ IN OUT NOCOPY VARCHAR2,
   attr_ IN     VARCHAR2 )
IS
   
   PROCEDURE Core (
      info_ IN OUT NOCOPY VARCHAR2,
      attr_ IN     VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, TRUE);
      info_ := Customer_Order_Line_API.Get_Current_Info || Client_SYS.Get_All_Info;   
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Reserve_Shipment__');
   Core(info_, attr_);
END Start_Reserve_Shipment__;


PROCEDURE Start_Create_Pick_List__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, TRUE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Create_Pick_List__');
   Core(attr_);
END Start_Create_Pick_List__;


PROCEDURE Start_Print_Pick_List__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, TRUE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Print_Pick_List__');
   Core(attr_);
END Start_Print_Pick_List__;


PROCEDURE Start_Print_Consignment_Note__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, FALSE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Print_Consignment_Note__');
   Core(attr_);
END Start_Print_Consignment_Note__;


PROCEDURE Start_Print_Bill_Of_Lading__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, FALSE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Print_Bill_Of_Lading__');
   Core(attr_);
END Start_Print_Bill_Of_Lading__;


PROCEDURE Start_Print_Packing_List__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, FALSE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Print_Packing_List__');
   Core(attr_);
END Start_Print_Packing_List__;


PROCEDURE Start_Print_Address_Label__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, FALSE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Print_Address_Label__');
   Core(attr_);
END Start_Print_Address_Label__;


PROCEDURE Start_Print_Proforma_Inv__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, FALSE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Print_Proforma_Inv__');
   Core(attr_);
END Start_Print_Proforma_Inv__;


PROCEDURE Start_Print_Shipment_Delnote__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, FALSE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Print_Shipment_Delnote__');
   Core(attr_);
END Start_Print_Shipment_Delnote__;


PROCEDURE Start_Pick_Report_Shipment__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, TRUE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Pick_Report_Shipment__');
   Core(attr_);
END Start_Pick_Report_Shipment__;


PROCEDURE Start_Complete_Shipment__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, TRUE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Complete_Shipment__');
   Core(attr_);
END Start_Complete_Shipment__;


PROCEDURE Start_Deliver_Shipment__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, TRUE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Deliver_Shipment__');
   Core(attr_);
END Start_Deliver_Shipment__;


PROCEDURE Start_Close_Shipment__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, TRUE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Close_Shipment__');
   Core(attr_);
END Start_Close_Shipment__;


PROCEDURE Start_Create_Ship_Invoice__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, TRUE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Create_Ship_Invoice__');
   Core(attr_);
END Start_Create_Ship_Invoice__;


PROCEDURE Start_Reopen_Shipment__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, FALSE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Reopen_Shipment__');
   Core(attr_);
END Start_Reopen_Shipment__;


PROCEDURE Start_Cancel_Shipment__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, FALSE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Cancel_Shipment__');
   Core(attr_);
END Start_Cancel_Shipment__;


PROCEDURE Start_Send_Dispatch_Advice__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, FALSE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Send_Dispatch_Advice__');
   Core(attr_);
END Start_Send_Dispatch_Advice__;


PROCEDURE Start_Pack_Into_Handl_Unit__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, FALSE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Pack_Into_Handl_Unit__');
   Core(attr_);
END Start_Pack_Into_Handl_Unit__;


PROCEDURE Start_Pack_Acc_Packing_Instr__ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
   BEGIN
      Process_All_Shipments__(attr_, FALSE);
   END Core;

BEGIN
   General_SYS.Init_Method(Shipment_Flow_API.lu_name_, 'Shipment_Flow_API', 'Start_Pack_Acc_Packing_Instr__');
   Core(attr_);
END Start_Pack_Acc_Packing_Instr__;


--@IgnoreMissingSysinit
FUNCTION Check_Pick_List_Use_Ship_Inv__ (
   shipment_id_ IN NUMBER ) RETURN VARCHAR2
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN VARCHAR2
   IS
      use_ship_invent_ VARCHAR2(5) := 'FALSE';
   
      CURSOR get_connected_shipments IS
         SELECT shipment_id, rowstate
         FROM   shipment_tab
         WHERE  parent_consol_shipment_id = shipment_id_
         AND    rowstate = 'Preliminary';
   BEGIN
      FOR rec_ IN get_connected_shipments LOOP
         IF (Pick_Report_Shipment_Allowed__(rec_.shipment_id) = 1) THEN      
            use_ship_invent_ := Customer_Order_Pick_List_API.Check_Pick_List_Use_Ship_Inv(rec_.shipment_id);
            IF (use_ship_invent_ = 'TRUE') THEN
               EXIT;
            END IF;
         END IF;
      END LOOP;
      RETURN use_ship_invent_;    
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Check_Pick_List_Use_Ship_Inv__;

-----------------------------------------------------------------------------
-------------------- LU SPECIFIC IMPLEMENTATION METHODS ---------------------
-----------------------------------------------------------------------------

PROCEDURE Reserve_Shipment___ (
   shipment_id_            IN NUMBER,
   finalize_on_picked_qty_ IN VARCHAR2,
   consider_reserved_qty_  IN VARCHAR2,
   discon_not_picked_      IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      shipment_id_            IN NUMBER,
      finalize_on_picked_qty_ IN VARCHAR2,
      consider_reserved_qty_  IN VARCHAR2,
      discon_not_picked_      IN VARCHAR2 )
   IS
   BEGIN
      Reserve_Customer_Order_API.Reserve_Shipment_Lines__(shipment_id_, finalize_on_picked_qty_, consider_reserved_qty_, discon_not_picked_);
   END Core;

BEGIN
   Core(shipment_id_, finalize_on_picked_qty_, consider_reserved_qty_, discon_not_picked_);
END Reserve_Shipment___;


PROCEDURE Create_Shipment_Pick_Lists___ (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
      pick_list_no_list_ Create_Pick_List_API.Pick_List_Table;
   BEGIN   
      Create_Pick_List_API.Create_Shipment_Pick_Lists__(pick_list_no_list_, shipment_id_, NULL, TRUE);
   END Core;

BEGIN
   Core(shipment_id_);
END Create_Shipment_Pick_Lists___;


PROCEDURE Report_Shipment_Pick_Lists___ (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
   BEGIN
      Pick_Customer_Order_API.Pick_Report_Shipment__(shipment_id_);
   END Core;

BEGIN
   Core(shipment_id_);
END Report_Shipment_Pick_Lists___;


PROCEDURE Report_Shipment_Pick_Lists___ (
   shipment_id_ IN NUMBER,
   location_no_ IN VARCHAR2)
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER,
      location_no_ IN VARCHAR2)
   IS
   BEGIN
      Pick_Customer_Order_API.Pick_Report_Shipment__(shipment_id_, location_no_);
   END Core;

BEGIN
   Core(shipment_id_, location_no_);
END Report_Shipment_Pick_Lists___;


PROCEDURE Deliver_Shipment___ (
   shipment_id_        IN NUMBER,
   deliver_through_cs_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      shipment_id_        IN NUMBER,
      deliver_through_cs_ IN VARCHAR2 )
   IS
      info_               VARCHAR2(2000);
      parent_shipment_id_ NUMBER;
   BEGIN
      parent_shipment_id_ := Shipment_API.Get_Parent_Consol_Shipment_Id(shipment_id_);
      IF (deliver_through_cs_ = 'FALSE') AND (parent_shipment_id_ IS NOT NULL) THEN
         Error_SYS.Record_General(lu_name_, 'DELIVERYNOTALLOWED: Shipment :P1 must be delivered via the consolidated shipment :P2.', shipment_id_, parent_shipment_id_);
      END IF;
      
      IF Shipment_API.Get_Approve_Before_Delivery_Db(shipment_id_) = 'FALSE' THEN
      Deliver_Customer_Order_API.Deliver_Shipment__(shipment_id_);
      ELSE
         IF (Transaction_SYS.Is_Session_Deferred) THEN         
            info_ := Language_SYS.Translate_Constant(lu_name_, 'SHIPMENTAPPROVE: The shipment(s) should have been approved to be delivered.');
            Transaction_SYS.Set_Status_Info(info_);
         ELSE
            Error_SYS.Appl_General(lu_name_,'SHIPMENTAPPROVE: The shipment(s) should have been approved to be delivered.');
         END IF;
      END IF;   
   END Core;

BEGIN
   Core(shipment_id_, deliver_through_cs_);
END Deliver_Shipment___;


FUNCTION Get_Next_Event___ (
   event_ IN VARCHAR2 ) RETURN NUMBER
IS
   
   FUNCTION Core (
      event_ IN VARCHAR2 ) RETURN NUMBER
   IS
      next_event_ NUMBER;
   BEGIN
      next_event_ := CASE event_
         WHEN 10 THEN 20
         WHEN 20 THEN 30
         WHEN 30 THEN 40
         WHEN 40 THEN 50
         WHEN 50 THEN 70
         WHEN 70 THEN 60
         WHEN 60 THEN 80
         ELSE NULL
      END;
   
      RETURN next_event_;
   END Core;

BEGIN
   RETURN Core(event_);
END Get_Next_Event___;


PROCEDURE Print_Goods_Declaration___ (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
   BEGIN
      Print_Shipment_Doc___('GOODS_DECLARATION_REP', shipment_id_, NULL, NULL);
   END Core;

BEGIN
   Core(shipment_id_);
END Print_Goods_Declaration___;


PROCEDURE Process_Optional_Events___ (
   shipment_id_   IN NUMBER,
   shipment_type_ IN VARCHAR2,
   event_         IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_   IN NUMBER,
      shipment_type_ IN VARCHAR2,
      event_         IN NUMBER )
   IS
      shipment_rec_         Shipment_API.Public_Rec;
      handling_unit_exists_ BOOLEAN := FALSE;
   
      CURSOR get_optional_events IS
         SELECT optional_event
         FROM SHIPMENT_TYPE_OPT_EVENT_TAB 
         WHERE shipment_type = shipment_type_
         AND   event         = event_
         ORDER BY CASE 
                     WHEN optional_event = 'RELEASE_QTY_NOT_RESERVED' THEN '0'
                     WHEN optional_event = 'PACK_ACC_TO_PACKING_INSTR' THEN '1'
                     WHEN optional_event = 'PACK_INTO_HANDLING_UNIT' THEN '2'
                     WHEN optional_event = 'CREATE_SSCC' THEN '3'
                     WHEN optional_event = 'PRINT_HANDLING_UNIT_LABELS' THEN '4'
                     ELSE optional_event 
                  END;
   
      -- Bug 129130, Modified WHERE clause to correctly get the top level handling units.
      CURSOR get_top_level_hus IS
         SELECT handling_unit_id
         FROM handling_unit_tab 
         WHERE shipment_id = shipment_id_
         AND parent_handling_unit_id IS NULL;
   BEGIN
      --@ApproveTransactionStatement(2014-12-16,jelise)
      SAVEPOINT event_processed;
      
      FOR optional_event_rec_ IN get_optional_events LOOP
         IF (optional_event_rec_.optional_event = 'RELEASE_QTY_NOT_RESERVED') THEN
            Shipment_Handling_Utility_API.Release_Not_Reserved_Qty__(shipment_id_);
            --@ApproveTransactionStatement(2014-12-16,jelise)
            COMMIT;
         ELSIF (optional_event_rec_.optional_event = 'PACK_ACC_TO_PACKING_INSTR') THEN
            Pack_Acc_To_Packing_Instr(shipment_id_);
            --@ApproveTransactionStatement(2014-12-16,jelise)
            COMMIT; 
         ELSIF (optional_event_rec_.optional_event = 'PACK_INTO_HANDLING_UNIT') THEN
            Pack_Into_Handling_Unit(shipment_id_);
            --@ApproveTransactionStatement(2014-12-16,jelise)
            COMMIT; 
         ELSIF (optional_event_rec_.optional_event = 'PRINT_PACKING_LIST') THEN
            Print_Packing_List___(shipment_id_); 
            --@ApproveTransactionStatement(2014-12-16,jelise)
            COMMIT;
         ELSIF (optional_event_rec_.optional_event = 'PRINT_HANDLING_UNIT_LABELS') THEN
            Print_Handling_Unit_Labels___(shipment_id_);  
            --@ApproveTransactionStatement(2014-12-16,jelise)
            COMMIT;
         ELSIF (optional_event_rec_.optional_event = 'PRINT_ADDRESS_LABEL') THEN
            FOR rec_ IN get_top_level_hus LOOP
               Print_Shipment_Doc___('ADDRESS_LABEL_REP', shipment_id_, rec_.handling_unit_id, NULL);
               handling_unit_exists_ := TRUE;
            END LOOP; 
            IF NOT (handling_unit_exists_) THEN
               Print_Address_Label___(shipment_id_);
            END IF;
            --@ApproveTransactionStatement(2014-12-16,jelise)
            COMMIT;
         ELSIF (optional_event_rec_.optional_event = 'PRINT_CONSIGNMENT_NOTE') THEN
            Print_Consignment_Note___(shipment_id_); 
            --@ApproveTransactionStatement(2014-12-16,jelise)
            COMMIT;
         ELSIF (optional_event_rec_.optional_event = 'PRINT_BILL_OF_LADING') THEN
            Print_Bill_Of_Lading___(shipment_id_);
            --@ApproveTransactionStatement(2014-12-16,jelise)
            COMMIT;
         ELSIF (optional_event_rec_.optional_event = 'CREATE_SSCC') THEN
            Create_Sssc___(shipment_id_); 
            --@ApproveTransactionStatement(2014-12-16,jelise)
            COMMIT;
         ELSIF (optional_event_rec_.optional_event = 'PRINT_PRO_FORMA_INVOICE') THEN
            Print_Pro_Forma_Invoice___(shipment_id_);
            --@ApproveTransactionStatement(2014-12-16,jelise)
            COMMIT;
         ELSIF (optional_event_rec_.optional_event = 'PRINT_SHIP_DELIVERY_NOTE') THEN
            Print_Shipment_Delivery_Note__(shipment_id_);
            --@ApproveTransactionStatement(2014-12-16,jelise)
            COMMIT; 
         ELSIF (optional_event_rec_.optional_event = 'BLOCK_AUTO_CONNECTION') THEN
            shipment_rec_ := Shipment_API.Get(shipment_id_);
            IF ((shipment_rec_.auto_connection_blocked = 'FALSE') AND (shipment_rec_.shipment_category = 'NORMAL')) THEN
               Shipment_API.Modify_Auto_Connect_Blocked__(shipment_id_, 'TRUE');
               --@ApproveTransactionStatement(2014-12-16,jelise)
               COMMIT;
            END IF;
         END IF;  
   
         --@ApproveTransactionStatement(2014-12-16,jelise)
         SAVEPOINT event_processed;
      END LOOP;
   END Core;

BEGIN
   Core(shipment_id_, shipment_type_, event_);
END Process_Optional_Events___;


PROCEDURE Print_Shipment_Doc___ (
   report_id_                  IN VARCHAR2,
   shipment_id_                IN NUMBER,
   handling_unit_id_           IN NUMBER,
   no_of_handling_unit_labels_ IN NUMBER  )
IS
   
   PROCEDURE Core (
      report_id_                  IN VARCHAR2,
      shipment_id_                IN NUMBER,
      handling_unit_id_           IN NUMBER,
      no_of_handling_unit_labels_ IN NUMBER  )
   IS
      report_attr_     VARCHAR2(2000);
      parameter_attr_  VARCHAR2(2000);
      result_key_      NUMBER;
      printer_id_      VARCHAR2(100);
      print_job_id_    NUMBER;
      printer_id_list_ VARCHAR2(32000);
      result_key_app_  NUMBER;
      attr_            VARCHAR2(2000);
   BEGIN
      -- Retrive default printer
      Client_SYS.Clear_Attr(report_attr_);
      printer_id_ := Printer_Connection_API.Get_Default_Printer(Fnd_Session_API.Get_Fnd_User, report_id_);
      Client_SYS.Add_To_Attr('PRINTER_ID', printer_id_, report_attr_);
      Print_Job_API.New(print_job_id_, report_attr_);
   
      IF (report_id_ != 'TRANSPORT_PACKAGE_LABEL_REP') THEN     
         Client_SYS.Clear_Attr(report_attr_);
         Client_SYS.Add_To_Attr('REPORT_ID', report_id_, report_attr_);   
         Client_SYS.Clear_Attr(parameter_attr_);
         Client_SYS.Add_To_Attr('SHIPMENT_ID', shipment_id_, parameter_attr_);
         IF (handling_unit_id_ IS NOT NULL) THEN
            Client_SYS.Add_To_Attr('HANDLING_UNIT_ID', handling_unit_id_, parameter_attr_);
         END IF;         
         Archive_API.New_Instance(result_key_, report_attr_, parameter_attr_);
   
         -- Add the report to the print job
         Client_SYS.Clear_Attr(report_attr_);
         Client_SYS.Add_To_Attr('PRINT_JOB_ID', print_job_id_, report_attr_);
         Client_SYS.Add_To_Attr('RESULT_KEY',   result_key_,   report_attr_);
         -- Bug 123092, start
         Client_SYS.Add_To_Attr('OPTIONS',      'COPIES(1)',   report_attr_);
         -- Bug 123092, end
         -- Bug 129130, start
         Client_SYS.Add_To_Attr('LAYOUT_NAME', Report_Layout_Definition_API.Get_Default_Layout(report_id_), report_attr_);
         -- Bug 129130, end
         Print_Job_Contents_API.New_Instance(report_attr_);
      ELSE
         IF no_of_handling_unit_labels_ IS NOT NULL THEN
            FOR i_ IN 1 .. no_of_handling_unit_labels_ LOOP          
               Client_SYS.Clear_Attr(report_attr_);
               Client_SYS.Add_To_Attr('REPORT_ID', report_id_, report_attr_);   
               Client_SYS.Clear_Attr(parameter_attr_);
               Client_SYS.Add_To_Attr('SHIPMENT_ID', shipment_id_, parameter_attr_);
               IF (handling_unit_id_ IS NOT NULL) THEN
                  Client_SYS.Add_To_Attr('HANDLING_UNIT_ID', handling_unit_id_, parameter_attr_);
               END IF;         
               Archive_API.New_Instance(result_key_app_, report_attr_, parameter_attr_);
   
               -- Add the report to the print job    
               Client_SYS.Clear_Attr(attr_);
               Client_SYS.Add_To_Attr('PRINT_JOB_ID', print_job_id_, attr_);     
               Client_SYS.Set_Item_Value('RESULT_KEY',   result_key_app_,   attr_);
               -- Bug 123092, start
               Client_SYS.Add_To_Attr('OPTIONS', 'COPIES(1)', attr_);
               -- Bug 123092, end
               -- Bug 129130, start
               Client_SYS.Add_To_Attr('LAYOUT_NAME', Report_Layout_Definition_API.Get_Default_Layout(report_id_), attr_);
               -- Bug 129130, end
               Print_Job_Contents_API.New_Instance(attr_);
            END LOOP;
         END IF;
      END IF;
   
      Logical_Printer_API.Enumerate_Printer_Id(printer_id_list_);
      IF (printer_id_list_ IS NOT NULL) THEN
         IF (print_job_id_ IS NOT NULL) THEN
            Print_Job_API.Print(print_job_id_);
         END IF;
      END IF;
   END Core;

BEGIN
   Core(report_id_, shipment_id_, handling_unit_id_, no_of_handling_unit_labels_);
END Print_Shipment_Doc___;


PROCEDURE Print_Consignment_Note___ (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
   BEGIN
      Print_Shipment_Doc___('SHIPMENT_HANDLING_UTILITY_REP', shipment_id_, NULL, NULL); 
      IF (Contains_Dangerous_Goods__(shipment_id_) = 'TRUE') THEN
         Print_Goods_Declaration___(shipment_id_);
      END IF;   
   END Core;

BEGIN
   Core(shipment_id_);
END Print_Consignment_Note___;


PROCEDURE Print_Bill_Of_Lading___ (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
   BEGIN
      Print_Shipment_Doc___('SHIPMENT_BILL_OF_LADING_REP', shipment_id_, NULL, NULL); 
      IF (Contains_Dangerous_Goods__(shipment_id_) = 'TRUE') THEN
         Print_Goods_Declaration___(shipment_id_);
      END IF;   
   END Core;

BEGIN
   Core(shipment_id_);
END Print_Bill_Of_Lading___;


PROCEDURE Print_Address_Label___ (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
   BEGIN
      Print_Shipment_Doc___('ADDRESS_LABEL_REP', shipment_id_, NULL, NULL);  
   END Core;

BEGIN
   Core(shipment_id_);
END Print_Address_Label___;


PROCEDURE Print_Pro_Forma_Invoice___ (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
   BEGIN
      Print_Shipment_Doc___('SHIPMENT_PROFORMA_INVOICE_REP', shipment_id_, NULL, NULL);  
   END Core;

BEGIN
   Core(shipment_id_);
END Print_Pro_Forma_Invoice___;


PROCEDURE Print_Packing_List___ (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
   BEGIN
      Print_Shipment_Doc___('SHIPMENT_ORDER_LINE_REP', shipment_id_, NULL, NULL);  
   END Core;

BEGIN
   Core(shipment_id_);
END Print_Packing_List___;


PROCEDURE Create_Sssc___ (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
   BEGIN
      IF (Shipment_API.All_Lines_Delivered__(shipment_id_) = 0) THEN
         Handling_Unit_API.Connect_Sscc(shipment_id_      => shipment_id_,
                                        handling_unit_id_ => NULL);
      END IF;
   END Core;

BEGIN
   Core(shipment_id_);
END Create_Sssc___;


PROCEDURE Print_Handling_Unit_Labels___ (
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      shipment_id_ IN NUMBER )
   IS
      CURSOR get_print_label_hus(handling_unit_id_ NUMBER) IS
         SELECT handling_unit_id, no_of_handling_unit_labels
         FROM handling_unit_pub 
         WHERE print_label = Fnd_Boolean_API.DB_TRUE
         CONNECT BY PRIOR handling_unit_id = parent_handling_unit_id   
         START WITH       handling_unit_id = handling_unit_id_;
      
      -- Bug 129130, Modified WHERE clause to correctly get the top level handling units.
      CURSOR get_top_level_hus IS
         SELECT handling_unit_id
         FROM handling_unit_pub 
         WHERE shipment_id = shipment_id_
         AND parent_handling_unit_id IS NULL;        
   BEGIN
      IF (Shipment_Order_Line_API.Connected_Lines_Exist(shipment_id_) = 0) THEN
         RETURN;
      END IF;
      
      FOR top_level_hu_ IN get_top_level_hus LOOP
         FOR rec_ IN get_print_label_hus(top_level_hu_.handling_unit_id) LOOP
            Print_Shipment_Doc___('TRANSPORT_PACKAGE_LABEL_REP', shipment_id_, rec_.handling_unit_id, rec_.no_of_handling_unit_labels);
         END LOOP; 
      END LOOP;  
   END Core;

BEGIN
   Core(shipment_id_);
END Print_Handling_Unit_Labels___;


PROCEDURE Process_Shipment___(
  mandatory_event_ IN BOOLEAN,
  shipment_type_   IN VARCHAR2,
  shiment_attr_    IN VARCHAR2,
  start_event_     IN NUMBER)
IS
   
   PROCEDURE Core(
     mandatory_event_ IN BOOLEAN,
     shipment_type_   IN VARCHAR2,
     shiment_attr_    IN VARCHAR2,
     start_event_     IN NUMBER)
   IS
     description_       VARCHAR2(200);
     online_processing_ VARCHAR2(5);
   BEGIN
      IF (mandatory_event_) THEN 
         online_processing_ := Shipment_Type_API.Get_Online_Processing_Db(shipment_type_);
         -- for rental transfers set the online_processing_ to TRUE to rollback the process if any errors appear
         IF (online_processing_ = db_false_ AND Client_SYS.Get_Item_Value('RENTAL_TRANSFER_DB', shiment_attr_) = db_true_) THEN
            online_processing_ := db_true_;
         END IF;   
         IF (Transaction_SYS.Is_Session_Deferred OR online_processing_ = db_true_) THEN
            -- don't start another background job when already in a background job.
            Process_Mandatory_Events__(shiment_attr_);
         ELSE
            CASE start_event_ 
               WHEN 10 THEN
                  description_ := Language_SYS.Translate_Constant(lu_name_, 'RESERVE_SHIPMENT: Reserve Shipment');
               WHEN 20 THEN
                  description_ := Language_SYS.Translate_Constant(lu_name_, 'CREATE_PICKLIST: Create Pick List');
               WHEN 30 THEN
                  description_ := Language_SYS.Translate_Constant(lu_name_, 'PRINT_PICKLIST: Print Pick List');
               WHEN 40 THEN
                  description_ := Language_SYS.Translate_Constant(lu_name_, 'REPORT_PICKING: Report Picking');
               WHEN 50 THEN
                  description_ := Language_SYS.Translate_Constant(lu_name_, 'COMPLETE: Complete Shipment');
               WHEN 60 THEN
                  description_ := Language_SYS.Translate_Constant(lu_name_, 'DELIVER: Deliver the Shipment');
               WHEN 70 THEN
                  description_ := Language_SYS.Translate_Constant(lu_name_, 'CLOSE_SHIPMENT: Close Shipment');
               WHEN 80 THEN
                  description_ := Language_SYS.Translate_Constant(lu_name_, 'CREATE_INVOICE: Create Shipment Invoice');
               WHEN 90 THEN
                  description_ := Language_SYS.Translate_Constant(lu_name_, 'PRINT_INVOICE: Print Invoice');
               ELSE
                  description_ := NULL;
               END CASE;
            Transaction_SYS.Deferred_Call('SHIPMENT_FLOW_API.Process_Mandatory_Events__', shiment_attr_, description_);
         END IF;
      ELSE
         Process_Nonmandatory_Events___(shiment_attr_);
      END IF;
   END Core;

BEGIN
   Core(mandatory_event_, shipment_type_, shiment_attr_, start_event_);
END Process_Shipment___;


FUNCTION Plan_Pick_Shipment_Allowed___ (
   shipment_id_ IN NUMBER,
   finalize_    IN NUMBER ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER,
      finalize_    IN NUMBER ) RETURN NUMBER
   IS
      allowed_ NUMBER;
   
      CURSOR plan_pick IS
         SELECT 1
         FROM   SHIPMENT_ORDER_LINE_TAB sol, CUSTOMER_ORDER_LINE_TAB col, SHIPMENT_TAB s, CUSTOMER_ORDER_TAB co
         WHERE  sol.shipment_id  = shipment_id_
         AND    s.shipment_id    = shipment_id_
         AND    col.order_no     = sol.order_no
         AND    col.line_no      = sol.line_no
         AND    col.rel_no       = sol.rel_no
         AND    col.line_item_no = sol.line_item_no
         AND    col.rowstate IN ('Released', 'Reserved', 'Picked', 'PartiallyDelivered')
         AND    col.line_item_no >= 0
         AND    ((col.revised_qty_due - col.qty_assigned - col.qty_to_ship - col.qty_on_order - col.qty_shipped + col.qty_shipdiff > 0) AND
                (sol.revised_qty_due - sol.qty_assigned - sol.qty_to_ship - sol.qty_shipped > 0) AND
                (supply_code IN ('IO', 'PS', 'PI', 'PRD', 'NO', 'PRJ', 'PT', 'IPT', 'SO')))     
         AND    s.rowstate       = 'Preliminary' 
         AND    co.order_no      = sol.order_no
         AND    ((finalize_ = 1) OR (co.rowstate!= 'CreditBlocked'));
   BEGIN
      OPEN plan_pick;
      FETCH plan_pick INTO allowed_;
      IF (plan_pick%NOTFOUND) THEN
         allowed_ := 0;
      END IF;
      CLOSE plan_pick;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_, finalize_);
END Plan_Pick_Shipment_Allowed___;


FUNCTION Create_Pick_List_Allowed___ (
   shipment_id_ IN NUMBER ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN NUMBER
   IS
      allowed_ NUMBER;
   
      -- No checks on pallet_id were done so that pallets could be processed.
      CURSOR create_pick_list IS
         SELECT 1
         FROM   CREATE_PICK_LIST_JOIN_MAIN
         WHERE  pick_list_no  = '*'
         AND    shipment_id   = shipment_id_;
   BEGIN
      OPEN create_pick_list;
      FETCH create_pick_list INTO allowed_;
      IF (create_pick_list%NOTFOUND) THEN
         allowed_ := 0;
      END IF;
      CLOSE create_pick_list;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Create_Pick_List_Allowed___;


FUNCTION Deliver_Shipment_Allowed___ (
   shipment_id_ IN NUMBER ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN NUMBER
   IS
      allowed_ NUMBER := 0;
   BEGIN
      IF (Shipment_API.Get_State(shipment_id_) = 'Completed') THEN
         IF (Shipment_Handling_Utility_API.All_Lines_Picked(shipment_id_) = 'TRUE') THEN
            allowed_ := 1;
         END IF;
      END IF;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Deliver_Shipment_Allowed___;


FUNCTION Pick_Report_Ship_Allowed___ (
   shipment_id_ IN NUMBER,
   report_pick_from_co_lines_ IN VARCHAR2 DEFAULT 'FALSE') RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER,
      report_pick_from_co_lines_ IN VARCHAR2 DEFAULT 'FALSE') RETURN NUMBER
   IS
      allowed_           NUMBER;
      pick_list_count_   NUMBER := 0;
      separator_         VARCHAR2(1) := Client_SYS.text_separator_;
   
      -- Bug 130619, added a condition to check the value of report_pick_from_co_lines_ in order to differentiate the value of Report picking and Report picking of pick list lines 
      -- Report picking is allowed if qty_assigned > qty_picked
      CURSOR report_picking IS
         SELECT 1
         FROM   CUSTOMER_ORDER_RESERVATION_TAB cor, SHIPMENT_TAB s
         WHERE  s.shipment_id  = shipment_id_
         AND    s.rowstate != 'Completed'
         AND    cor.shipment_id  = s.shipment_id
         AND    cor.qty_assigned > cor.qty_picked
         AND    EXISTS (SELECT 1
                        FROM   CUSTOMER_ORDER_PICK_LIST_TAB cop
                        WHERE  ((cop.shipment_id  = shipment_id_) OR 
                                ((NVL(INSTR(cop.shipments_consolidated, shipment_id_) ,0) != 0) AND 
                                (NVL(INSTR(cop.shipments_consolidated, separator_) ,0) = 0) AND ((report_pick_from_co_lines_ = 'TRUE' AND pick_list_count_ >= 1)
                                                                                                  OR (report_pick_from_co_lines_ = 'FALSE' AND pick_list_count_ = 1))))
                        AND    picking_confirmed =  'UNPICKED');
      
      CURSOR get_pick_list_count IS
         SELECT COUNT(DISTINCT con.pick_list_no)
         FROM consolidated_orders_tab con
         WHERE con.shipment_id  = shipment_id_;
   BEGIN
      OPEN get_pick_list_count;
      FETCH get_pick_list_count INTO pick_list_count_;
      CLOSE get_pick_list_count;
      
      OPEN report_picking;
      FETCH report_picking INTO allowed_;
      IF (report_picking%NOTFOUND) THEN
         allowed_ := 0;
      END IF;
      CLOSE report_picking;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_, report_pick_from_co_lines_);
END Pick_Report_Ship_Allowed___;


FUNCTION Get_Pick_Lists_For_Shipment___ (
   shipment_id_  IN NUMBER,
   printed_flag_ IN VARCHAR2 ) RETURN VARCHAR2
IS
   
   FUNCTION Core (
      shipment_id_  IN NUMBER,
      printed_flag_ IN VARCHAR2 ) RETURN VARCHAR2
   IS
      pick_list_no_list_ VARCHAR2(32000);
      -- Bug 130619, start
      shipment_count_    NUMBER;
      -- Bug 130619, end
      
      CURSOR get_pick_lists IS
            SELECT DISTINCT cop.pick_list_no
            FROM CUSTOMER_ORDER_PICK_LIST_TAB cop, consolidated_orders_tab con
            WHERE con.shipment_id       = shipment_id_
               AND con.pick_list_no      = cop.pick_list_no
               AND (cop.printed_flag     =  printed_flag_ OR printed_flag_ IS NULL)
               AND cop.picking_confirmed = 'UNPICKED';
               
      -- Bug 130619, start
      CURSOR get_shipment_count(pick_list_no_ IN NUMBER) IS
            SELECT COUNT (DISTINCT con.shipment_id)
            FROM consolidated_orders_tab con
            WHERE con.pick_list_no = pick_list_no_ ;
      -- Bug 130619, end
      
   BEGIN
      pick_list_no_list_ := NULL;
      
      -- Bug 130619, start
      FOR rec_ IN get_pick_lists LOOP      
         OPEN get_shipment_count(rec_.pick_list_no);
         FETCH get_shipment_count INTO shipment_count_;
         CLOSE get_shipment_count;
         
         IF (shipment_count_ =  1) THEN
            -- Add pick list to list
            pick_list_no_list_ := pick_list_no_list_ || rec_.pick_list_no || Client_SYS.field_separator_;
         END IF;
      END LOOP;
      -- Bug 130619, end
      RETURN pick_list_no_list_;
   END Core;

BEGIN
   RETURN Core(shipment_id_, printed_flag_);
END Get_Pick_Lists_For_Shipment___;


FUNCTION Print_Pick_List_Allowed___ (
   shipment_id_ IN NUMBER ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN NUMBER
   IS
      allowed_ NUMBER;
      CURSOR get_pick_list IS
         SELECT 1
         FROM   CUSTOMER_ORDER_PICK_LIST_TAB
         WHERE  shipment_id = shipment_id_
         AND    printed_flag      = 'N'
         AND    picking_confirmed = 'UNPICKED';
   BEGIN
      OPEN get_pick_list;
      FETCH get_pick_list INTO allowed_;
      IF (get_pick_list%NOTFOUND) THEN
         allowed_ := 0;
      END IF;
      CLOSE get_pick_list;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Print_Pick_List_Allowed___;


FUNCTION Print_Proforma_Ivc_Allowed___ (
   shipment_id_ IN NUMBER ) RETURN NUMBER
IS
   
   FUNCTION Core (
      shipment_id_ IN NUMBER ) RETURN NUMBER
   IS
      allowed_ NUMBER;
      CURSOR get_shipment IS
         SELECT 1
         FROM shipment_tab
         WHERE shipment_id = shipment_id_ 
         AND   rowstate IN ('Completed', 'Closed');
   BEGIN
      OPEN get_shipment;
      FETCH get_shipment INTO allowed_;
      IF (get_shipment%NOTFOUND) THEN
         allowed_ := 0;
      END IF;
      CLOSE get_shipment;
      RETURN allowed_;
   END Core;

BEGIN
   RETURN Core(shipment_id_);
END Print_Proforma_Ivc_Allowed___;


PROCEDURE Print_Ship_Consol_Pl___ (
   pick_list_no_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      pick_list_no_ IN VARCHAR2 )
   IS
      printer_id_         VARCHAR2(100);
      attr_               VARCHAR2(200);
      report_attr_        VARCHAR2(2000);
      parameter_attr_     VARCHAR2(2000);
      result_key_         NUMBER;
      print_job_id_       NUMBER;
      printer_id_list_    VARCHAR2(32000);
      msg_                VARCHAR2(200);
      order_no_           VARCHAR2(12);
      result_key_app_     NUMBER;
      work_order_no_      NUMBER;
      purchase_order_no_  VARCHAR2(12);   
      nopart_lines_exist_ NUMBER;
   
      CURSOR line_details IS
         SELECT DISTINCT line_no, rel_no, line_item_no
         FROM create_pick_list_join_new
         WHERE order_no     = order_no_
         AND   pick_list_no = pick_list_no_;
   
      CURSOR get_mro_lines IS
         SELECT col.demand_order_ref1
         FROM customer_order_line_tab col, create_pick_list_join_new cpl
         WHERE cpl.order_no     = order_no_
         AND   col.order_no     = cpl.order_no
         AND   col.line_no      = cpl.line_no
         AND   col.rel_no       = cpl.rel_no
         AND   col.line_item_no = cpl.line_item_no
         AND   col.supply_code  = 'MRO';
   
     CURSOR get_con_orders IS
         SELECT order_no
         FROM consolidated_orders_tab
         WHERE pick_list_no = pick_list_no_;
   BEGIN
      order_no_   := Customer_Order_Pick_List_API.Get_Order_No(pick_list_no_);
       
      -- Generate a new print job id
      printer_id_ := Printer_Connection_API.Get_Default_Printer(Fnd_Session_API.Get_Fnd_User,
                                                                'SHIPMENT_CONSOL_PICK_LIST_REP');
      Client_SYS.Clear_Attr(attr_);
      Client_SYS.Add_To_Attr('PRINTER_ID', printer_id_, attr_);
      Print_Job_API.New(print_job_id_, attr_);
      -- Create the Consolidated report
      Client_SYS.Clear_Attr(report_attr_);
      Client_SYS.Add_To_Attr('REPORT_ID', 'SHIPMENT_CONSOL_PICK_LIST_REP', report_attr_);
      Client_SYS.Clear_Attr(parameter_attr_);
      Client_SYS.Add_To_Attr('PICK_LIST_NO', pick_list_no_, parameter_attr_);
      Archive_API.New_Instance(result_key_, report_attr_, parameter_attr_);
   
      -- Connect the created report to a print job id
      Client_SYS.Clear_Attr(attr_);
      Client_SYS.Add_To_Attr('PRINT_JOB_ID', print_job_id_, attr_);
      Client_SYS.Add_To_Attr('RESULT_KEY', result_key_, attr_);
      Print_Job_Contents_API.New_Instance(attr_);
      
      $IF (Component_Wo_SYS.INSTALLED AND Component_Purch_SYS.INSTALLED) $THEN
         -- Note: Get MRO order lines for Miscellaneous Part Report.
         FOR mro_line_ IN get_mro_lines LOOP
            work_order_no_      := TO_NUMBER(mro_line_.demand_order_ref1);      
            purchase_order_no_  := Active_Work_Order_API.Get_Receive_Order_No(work_order_no_);
            nopart_lines_exist_ := Purchase_Order_API.Check_Nopart_Lines_Exist(purchase_order_no_);
   
            IF nopart_lines_exist_ != 0 THEN
               -- Note: Create the Pick_List Appendix report
               Client_SYS.Clear_Attr(report_attr_);
               Client_SYS.Add_To_Attr('REPORT_ID', 'PURCH_MISCELLANEOUS_PART_REP', report_attr_);
               Client_SYS.Clear_Attr(parameter_attr_);
               Client_SYS.Add_To_Attr('ORDER_NO', purchase_order_no_, parameter_attr_);
               Client_SYS.Add_To_Attr('REPORT_TYPE', 'PICK LIST', parameter_attr_);
               Archive_API.New_Instance(result_key_app_, report_attr_, parameter_attr_);
   
               -- Note: Connect the Pick_List Appendix report to the same print job id
               Client_SYS.Set_Item_Value('RESULT_KEY', result_key_app_, attr_);
               Print_Job_Contents_API.New_Instance(attr_);
            END IF;           
         END LOOP;
      $END 
   
      -- Send the print job to the printer.
      Logical_Printer_API.Enumerate_Printer_Id(printer_id_list_);
      IF (printer_id_list_ IS NOT NULL) THEN
         IF (print_job_id_ IS NOT NULL) THEN
            Print_Job_API.Print(print_job_id_);
         END IF;
      END IF;
   
      -- Add a new record in customer order(s) history.    
      msg_ := Language_SYS.Translate_Constant(lu_name_, 'PICKPRINT: Picklist :P1 printed', NULL, pick_list_no_);
      FOR ord_ IN get_con_orders LOOP
         Customer_Order_History_API.New(ord_.order_no, msg_);
         order_no_ := ord_.order_no;
         FOR linerec_ IN line_details LOOP
             --Add a new record in Customer Order Line History
             Customer_Order_Line_Hist_API.New(order_no_,linerec_.line_no,linerec_.rel_no,linerec_.line_item_no,msg_);
         END LOOP;
      END LOOP;
   END Core;

BEGIN
   Core(pick_list_no_);
END Print_Ship_Consol_Pl___;


PROCEDURE Process_Nonmandatory_Events___ (
   attr_ IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      attr_ IN VARCHAR2 )
   IS
      start_event_            NUMBER;
      ptr_                    NUMBER;
      name_                   VARCHAR2(30);
      value_                  VARCHAR2(2000);
      shipment_id_            NUMBER; 
      error_message_          VARCHAR2(2000);
      delivery_note_no_       VARCHAR2(30);
      media_code_             VARCHAR2(30);
      show_shipment_id_       VARCHAR2(5);
   BEGIN
      --@ApproveTransactionStatement(2012-08-09,MaEelk)
      SAVEPOINT event_processed;
      
      -- Retrieve parameters from the attribute string
      WHILE (Client_SYS.Get_Next_From_Attr(attr_, ptr_, name_, value_)) LOOP
         IF (name_ = 'START_EVENT') THEN
            start_event_ := Client_SYS.Attr_Value_To_Number(value_);
         ELSIF (name_ = 'SHIPMENT_ID') THEN
            shipment_id_ := value_;
         ELSIF (name_ = 'DELIVERY_NOTE_NO') THEN
            delivery_note_no_ :=  value_;
         ELSIF (name_ = 'MEDIA_CODE') THEN
            media_code_ := value_;
         ELSIF (name_ = 'SHOW_SHIPMENT_ID') THEN
            show_shipment_id_ := value_;         
         END IF;
      END LOOP;
      
      CASE start_event_ 
         WHEN 1000 THEN
            Shipment_API.Re_Open(shipment_id_);
         WHEN 1100 THEN
            Shipment_API.Cancel_Shipment__(shipment_id_);      
         WHEN 1200 THEN
            Pack_Acc_To_Packing_Instr(shipment_id_); 
         WHEN 1250 THEN
            Pack_Into_Handling_Unit(shipment_id_); 
         WHEN 1300 THEN
            Customer_Order_Transfer_API.Send_Dispatch_Advice(delivery_note_no_, media_code_);       
         ELSE
            NULL;
      END CASE;        
      --@ApproveTransactionStatement(2014-12-16,jelise)
      COMMIT;   
   EXCEPTION
      WHEN others THEN
         error_message_ := sqlerrm;
         -- Rollback to the last savepoint
         --@ApproveTransactionStatement(2012-08-09,MaEelk)
         ROLLBACK to event_processed;
         -- Raise the error
         RAISE;
   END Core;

BEGIN
   Core(attr_);
END Process_Nonmandatory_Events___;


PROCEDURE Check_Manual_Tax_Lia_Date___ (
   invoice_id_  IN NUMBER,
   shipment_id_ IN NUMBER )
IS
   
   PROCEDURE Core (
      invoice_id_  IN NUMBER,
      shipment_id_ IN NUMBER )
   IS
      invoice_type_           CUSTOMER_ORDER_INV_HEAD.invoice_type%TYPE;
      company_                CUSTOMER_ORDER_INV_HEAD.company%TYPE;
      invoice_series_id_      CUSTOMER_ORDER_INV_HEAD.series_id%TYPE;
      has_man_tax_liab_lines_ VARCHAR2(5);
      info_                   VARCHAR2(2000);
   
      CURSOR get_item_rec IS
         SELECT item_id,man_tax_liability_date
         FROM   cust_invoice_pub_util_item
         WHERE  company    = company_
         AND    invoice_id = invoice_id_;
   BEGIN
      company_      := Site_API.Get_Company(Shipment_API.Get_Contract(shipment_id_));
      invoice_type_ := Customer_Order_Inv_Head_API.Get_Invoice_Type(company_, invoice_id_);
   
      FOR item_rec_ IN get_item_rec LOOP
         IF (item_rec_.man_tax_liability_date IS NULL) THEN
            has_man_tax_liab_lines_ := Customer_Order_Inv_Item_API.Has_Manual_Tax_Liablty_Lines(company_,invoice_id_,item_rec_.item_id,invoice_type_);
            IF (has_man_tax_liab_lines_ = 'TRUE') THEN
               invoice_series_id_ := Customer_Order_Inv_Head_API.Get_Series_Id(company_,NULL, NULL, invoice_id_);
               info_              := Language_SYS.Translate_Constant(lu_name_, 'NOMANTAXLIADATE: This invoice has a tax code defined with Tax Liability Date type as Manual. But no tax liability date specified on invoice/tax lines for invoice :P1 :P2.', NULL, invoice_series_id_, invoice_id_);
               Transaction_SYS.Set_Status_Info(info_);
            END IF;
         END IF;
      END LOOP;
   END Core;

BEGIN
   Core(invoice_id_, shipment_id_);
END Check_Manual_Tax_Lia_Date___;


PROCEDURE Print_Invoice___ (
   invoice_id_      IN VARCHAR2,
   media_code_      IN VARCHAR2,
   cust_email_addr_ IN VARCHAR2,
   email_invoice_   IN VARCHAR2 )
IS
   
   PROCEDURE Core (
      invoice_id_      IN VARCHAR2,
      media_code_      IN VARCHAR2,
      cust_email_addr_ IN VARCHAR2,
      email_invoice_   IN VARCHAR2 )
   IS
      attr_ VARCHAR2(2000);
   BEGIN
      -- Print the invoice
      Client_SYS.Clear_Attr(attr_);
      Client_SYS.Add_To_Attr('INVOICE_ID', invoice_id_, attr_);
      -- Add media code if not null. The invoice is then sent instead of printed.
      IF (media_code_ IS NOT NULL) THEN
         Client_SYS.Add_To_Attr('MEDIA_CODE', media_code_, attr_);
         -- The connected objects value should be TRUE for media code E-Invoice
         -- in the automatic customer order flow. Otherwise FALSE.
         IF (media_code_ = 'E-INVOICE') THEN
            Client_SYS.Add_To_Attr('CONNECTED_OBJECTS', 'TRUE', attr_);
         ELSE
            Client_SYS.Add_To_Attr('CONNECTED_OBJECTS', 'FALSE', attr_);
         END IF;
      END IF;
      IF (cust_email_addr_ IS NOT NULL AND email_invoice_ = 'TRUE') THEN
         Client_SYS.Add_To_Attr('EMAIL_ADDR', cust_email_addr_, attr_);
      END IF;
   
      Customer_Order_Inv_Head_API.Print_Invoices(attr_);
   END Core;

BEGIN
   Core(invoice_id_, media_code_, cust_email_addr_, email_invoice_);
END Print_Invoice___;


PROCEDURE Check_All_License_Connected___ (
   display_info_ IN OUT NOCOPY NUMBER,
   shipment_id_  IN     NUMBER )
IS
   
   PROCEDURE Core (
      display_info_ IN OUT NOCOPY NUMBER,
      shipment_id_  IN     NUMBER )
   IS   
   BEGIN
         $IF Component_Expctr_SYS.INSTALLED $THEN         
            DECLARE
               shipment_rec_              Shipment_API.Public_Rec;
               stop_after_reserve_        NUMBER := NULL;
               all_license_connected_     VARCHAR2(10):= 'TRUE';
               raise_message_             VARCHAR2(5) := 'FALSE';
               all_lines_expctr_          VARCHAR2(5) := 'TRUE';
               connected_                 VARCHAR2(5) := 'FALSE';
               CURSOR get_ship_order_lines IS
                  SELECT order_no, line_no, rel_no, line_item_no
                  FROM   shipment_order_line_tab
                  WHERE  shipment_id = shipment_id_;
            BEGIN            
               shipment_rec_              := Shipment_API.Get(shipment_id_);
               IF Expctr_System_Parameter_API.Validate_Export_License('INTERACT_CUST_ORD', shipment_rec_.contract) = 'TRUE' THEN    
                  stop_after_reserve_ := Shipment_Type_Event_API.Get_Next_Event(shipment_rec_.shipment_type, 10);   
                  IF (display_info_ = 0 AND stop_after_reserve_ IS NOT NULL) OR display_info_ = 1 THEN 
                     -- Reset display_info_ to get the new value.
                     display_info_ := 0;
                     FOR line_rec_ IN get_ship_order_lines LOOP
                        Exp_License_Connect_Util_API.Get_Order_License_Connect_Info(all_license_connected_, raise_message_, line_rec_.order_no, line_rec_.line_no, line_rec_.rel_no, line_rec_.line_item_no);
                           IF raise_message_ = 'TRUE' THEN 
                              IF all_license_connected_ = 'TRUE' THEN
                                 -- License are not connected but user has override license connection rights.
                                 display_info_ := 1;
                              ELSE
                                 -- License are not connected and user does not have override license connection rights.
                                 display_info_ := 2;
                              END IF;
                           END IF;
                     END LOOP;   
                  END IF;
                  All_Lines_Expctr(all_lines_expctr_, connected_, shipment_id_);   
                  IF all_lines_expctr_ = 'TRUE' AND display_info_ = 2 THEN
                     IF connected_ = 'TRUE' THEN
                        -- All lines of the shipment is export controlled and user does not have override license connection rights.
                        -- But some are license connected. Those should proceed the flow.
                        display_info_ := 2;
                     ELSE 
                        -- All lines of the shipment is export controlled and user does not have override license connection rights.
                        display_info_ := 3;
                     END IF;
                  END IF;
               END IF;
            END;
         $ELSE
            -- Bug 129487, start
            display_info_ := 0;
            -- Bug 129487, end
         $END
   END Core;

BEGIN
   Core(display_info_, shipment_id_);
END Check_All_License_Connected___;

-----------------------------------------------------------------------------
-------------------- FOUNDATION1 METHODS ------------------------------------
-----------------------------------------------------------------------------


--@IgnoreMissingSysinit
PROCEDURE Init
IS
   
   PROCEDURE Base
   IS
   BEGIN
      NULL;
   END Base;

BEGIN
   Base;
END Init;

BEGIN
   Init;
END Shipment_Flow_API;
/
