--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Implementation of the SPI master for the STM32F401 controller. It use DMA
--  for data transfer.

--  pragma Restrictions (No_Elaboration_Code);

with A0B.Callbacks;
with A0B.SPI;
with A0B.STM32F401.DMA;
with A0B.STM32F401.GPIO;
with A0B.STM32F401.SVD.SPI;

package A0B.STM32F401.SPI
  with Preelaborate
is

   type Controller_Number is range 1 .. 4;

   type Master_Controller
     (Peripheral       : not null access A0B.STM32F401.SVD.SPI.SPI_Peripheral;
      Controller       : Controller_Number;
      Interrupt        : A0B.STM32F401.Interrupt_Number;
      Transmit_Stream  : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
      Transmit_Channel : A0B.STM32F401.DMA.Channel_Number;
      Receive_Stream   : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
      Receive_Channel  : A0B.STM32F401.DMA.Channel_Number;
      MOSI_Pin         : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
      MOSI_Line        :
        not null access constant A0B.STM32F401.Function_Line_Descriptor;
      MISO_Pin         : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
      MISO_Line        :
        not null access constant A0B.STM32F401.Function_Line_Descriptor;
      SCK_Pin          : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
      SCK_Line         :
        not null access constant A0B.STM32F401.Function_Line_Descriptor;
      NSS_Pin          : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
      NSS_Line         :
        not null access constant A0B.STM32F401.Function_Line_Descriptor) is
          limited new A0B.SPI.SPI_Slave_Device with private
          with Preelaborable_Initialization,
               Static_Predicate =>
                 A0B.STM32F401.GPIO.Is_Supported
                  (Master_Controller.MOSI_Pin.all,
                   Master_Controller.MOSI_Line.all)
                 and A0B.STM32F401.GPIO.Is_Supported
                       (Master_Controller.MISO_Pin.all,
                        Master_Controller.MISO_Line.all)
                 and A0B.STM32F401.GPIO.Is_Supported
                       (Master_Controller.SCK_Pin.all,
                        Master_Controller.SCK_Line.all)
                 and A0B.STM32F401.GPIO.Is_Supported
                       (Master_Controller.NSS_Pin.all,
                        Master_Controller.NSS_Line.all);

   procedure Configure (Self : in out Master_Controller'Class);

private

   --  package Device_Locks is
   --
   --     type Lock is limited private with Preelaborable_Initialization;
   --
   --     procedure Acquire
   --       (Self    : in out Lock;
   --        Device  : not null I2C_Device_Driver_Access;
   --        Success : in out Boolean);
   --
   --     procedure Release
   --       (Self    : in out Lock;
   --        Device  : not null I2C_Device_Driver_Access;
   --        Success : in out Boolean);
   --
   --     function Device (Self : Lock) return I2C_Device_Driver_Access;
   --
   --  private
   --
   --     type Lock is limited record
   --        Device : I2C_Device_Driver_Access;
   --     end record;
   --
   --     function Device (Self : Lock) return I2C_Device_Driver_Access is
   --       (Self.Device);
   --
   --  end Device_Locks;
   --
   --  type Operation_Kind is (None, Read, Write, Write_Done);

   type Master_Controller
     (Peripheral       : not null access A0B.STM32F401.SVD.SPI.SPI_Peripheral;
      Controller       : Controller_Number;
      Interrupt        : A0B.STM32F401.Interrupt_Number;
      Transmit_Stream  : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
      Transmit_Channel : A0B.STM32F401.DMA.Channel_Number;
      Receive_Stream   : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
      Receive_Channel  : A0B.STM32F401.DMA.Channel_Number;
      MOSI_Pin         : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
      MOSI_Line        :
        not null access constant A0B.STM32F401.Function_Line_Descriptor;
      MISO_Pin         : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
      MISO_Line        :
        not null access constant A0B.STM32F401.Function_Line_Descriptor;
      SCK_Pin          : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
      SCK_Line         :
        not null access constant A0B.STM32F401.Function_Line_Descriptor;
      NSS_Pin          : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
      NSS_Line         :
        not null access constant A0B.STM32F401.Function_Line_Descriptor) is
          limited new A0B.SPI.SPI_Slave_Device with record
   --  limited new I2C_Bus_Master with record
   --     Device_Lock    : Device_Locks.Lock;

      Transmit_Buffers   : access A0B.SPI.Buffer_Descriptor_Array;
      Transmit_Active    : A0B.Types.Unsigned_32;
      --  Transmit_Done   : Boolean;
      Receive_Buffers    : access A0B.SPI.Buffer_Descriptor_Array;
      Receive_Active     : A0B.Types.Unsigned_32;
      --  Receive_Done    : Boolean;
      Finished           : A0B.Callbacks.Callback;
      Placeholder_Buffer : aliased A0B.Types.Unsigned_8;
      Placeholder_Count  : A0B.Types.Unsigned_32;

      --  Buffers        : access Buffer_Descriptor_Array;
      --  Active         : A0B.Types.Unsigned_32;
   --     Stream         : access A0B.STM32F401.DMA.DMA_Stream'Class;
   --
   --     Device_Address : A0B.I2C.Device_Address;
   --     Operation      : Operation_Kind := None;
   --
   --     Stop           : Boolean;
   --     --  Send of STOP condition is requested after completion of the current
   --     --  operation. This flag is used to reject erroneous Read/Write request
   --     --  after completion of the transfer and before release of the bus.
   end record;

   overriding procedure Transfer
     (Self              : in out Master_Controller;
      Transmit_Buffer   : aliased A0B.Types.Unsigned_8;
      Receive_Buffer    : aliased out A0B.Types.Unsigned_8;
      Finished_Callback : A0B.Callbacks.Callback;
      Success           : in out Boolean);

   overriding procedure Transmit
     (Self              : in out Master_Controller;
      Transmit_Buffer   : aliased A0B.Types.Unsigned_8;
      Finished_Callback : A0B.Callbacks.Callback;
      Success           : in out Boolean);

   overriding procedure Select_Device (Self : in out Master_Controller);

   overriding procedure Release_Device (Self : in out Master_Controller);

   overriding procedure Transmit
     (Self             : in out Master_Controller;
      Transmit_Buffers : A0B.SPI.Buffer_Descriptor_Array;
      On_Finished      : A0B.Callbacks.Callback;
      Success          : in out Boolean);

   overriding procedure Receive
     (Self            : in out Master_Controller;
      --  Transmit_Placeholder : A0B.Types.Unsigned_8;
      Receive_Buffers : A0B.SPI.Buffer_Descriptor_Array;
      On_Finished     : A0B.Callbacks.Callback;
      Success         : in out Boolean);

   --  overriding procedure Start
   --    (Self    : in out Master_Controller;
   --     Device  : not null I2C_Device_Driver_Access;
   --     Success : in out Boolean);
   --
   --  overriding procedure Write
   --    (Self    : in out Master_Controller;
   --     Device  : not null I2C_Device_Driver_Access;
   --     Buffers : in out Buffer_Descriptor_Array;
   --     Stop    : Boolean;
   --     Success : in out Boolean);
   --
   --  overriding procedure Read
   --    (Self    : in out Master_Controller;
   --     Device  : not null I2C_Device_Driver_Access;
   --     Buffers : in out Buffer_Descriptor_Array;
   --     Stop    : Boolean;
   --     Success : in out Boolean);
   --
   --  overriding procedure Stop
   --    (Self    : in out Master_Controller;
   --     Device  : not null I2C_Device_Driver_Access;
   --     Success : in out Boolean);

   procedure On_Interrupt (Self : in out Master_Controller'Class);
   --  Interrupt handler for all interrupts (SPI events, DMA streams).

end A0B.STM32F401.SPI;
