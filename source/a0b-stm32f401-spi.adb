--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  pragma Restrictions (No_Elaboration_Code);
pragma Ada_2022;

with A0B.ARMv7M.NVIC_Utilities;
with A0B.Callbacks.Generic_Non_Dispatching;
with A0B.STM32F401.SVD.DMA;
with A0B.STM32F401.SVD.RCC;

package body A0B.STM32F401.SPI is

   use type A0B.Types.Unsigned_32;

   procedure Enable_Clock (Self : in out Master_Controller'Class);

   package On_Interrupt_Callbacks is
     new A0B.Callbacks.Generic_Non_Dispatching
           (Master_Controller, On_Interrupt);

   type Event_Kind is (None, Transmit, Receive, Interrupt);

   type Event (Kind : Event_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Transmit =>
            null;

         when Receive =>
            null;

         when Interrupt =>
            SPI_SR    : A0B.STM32F401.SVD.SPI.SR_Register;
            SPI_CR2   : A0B.STM32F401.SVD.SPI.CR2_Register;
            DMA_LISR  : A0B.STM32F401.SVD.DMA.LISR_Register;
            DMA_S2CR  : A0B.STM32F401.SVD.DMA.S2CR_Register;
            DMA_S2NDT : A0B.STM32F401.SVD.DMA.S2NDTR_NDT_Field;
            DMA_S3CR  : A0B.STM32F401.SVD.DMA.S3CR_Register;
            DMA_S3NDT : A0B.STM32F401.SVD.DMA.S3NDTR_NDT_Field;
      end case;
   end record;

   Event_Log  : array (Positive range 1 .. 100) of Event;
   Event_Last : Natural := 0;

   ---------------
   -- Configure --
   ---------------

   procedure Configure (Self : in out Master_Controller'Class) is
   begin
      Self.MOSI_Pin.Configure_Alternative_Function
        (Line  => Self.MOSI_Line.all,
         Mode  => A0B.STM32F401.GPIO.Push_Pull,
         Speed => A0B.STM32F401.GPIO.Very_High,
         Pull  => A0B.STM32F401.GPIO.Pull_Up);
      Self.MISO_Pin.Configure_Alternative_Function
        (Line  => Self.MISO_Line.all,
         Mode  => A0B.STM32F401.GPIO.Push_Pull,
         Speed => A0B.STM32F401.GPIO.Very_High,
         Pull  => A0B.STM32F401.GPIO.Pull_Up);
      Self.SCK_Pin.Configure_Alternative_Function
        (Line  => Self.SCK_Line.all,
         Mode  => A0B.STM32F401.GPIO.Push_Pull,
         Speed => A0B.STM32F401.GPIO.Very_High,
         Pull  => A0B.STM32F401.GPIO.Pull_Up);
      Self.NSS_Pin.Configure_Alternative_Function
        (Line  => Self.NSS_Line.all,
         Mode  => A0B.STM32F401.GPIO.Push_Pull,
         Speed => A0B.STM32F401.GPIO.Very_High,
         Pull  => A0B.STM32F401.GPIO.Pull_Up);

      Self.Enable_Clock;

      Self.Peripheral.CR1 :=
        (CPHA     => False,
         --  The first clock transition is the first data capture edge
         CPOL     => False,   --  CK to 0 when idle
         MSTR     => True,    --  Master configuration
         BR       => 2#001#,  --  fPCLK/4
         --  BR       => 2#111#,  --  fPCLK/256
         SPE      => False,   --  Peripheral disabled
         LSBFIRST => False,   --  MSB transmitted first
         SSI      => <>,
         SSM      => False,   --  Software slave management disabled
         RXONLY   => False,   --  Full duplex (Transmit and receive)
         DFF      => False,
         --  8-bit data frame format is selected for transmission/reception
         CRCNEXT  => False,   --  Data phase (no CRC phase)
         CRCEN    => False,   --  CRC calculation disabled
         BIDIOE   => <>,
         BIDIMODE => False,   --  2-line unidirectional data mode selected
         others   => <>);

      declare
         Aux : A0B.STM32F401.SVD.SPI.CR2_Register := Self.Peripheral.CR2;

      begin
         Aux.RXDMAEN := True;   --  Rx buffer DMA enabled
         Aux.TXDMAEN := True;   --  Tx buffer DMA enabled
         Aux.SSOE    := True;
         --  SS output is enabled in master mode and when the cell is enabled.
         --  The cell cannot work in a multimaster environment.
         Aux.FRF     := False;  --  SPI Motorola mode
         Aux.ERRIE   := False;  --  Error interrupt is masked
         Aux.RXNEIE  := False;
         --  Aux.RXNEIE  := True;
         --  RXNE interrupt not masked. Used to generate an interrupt request
         --  when the RXNE flag is set.
         Aux.TXEIE   := False;
         --  Aux.TXEIE   := True;
         --  TXE interrupt not masked. Used to generate an interrupt request
         --  when the TXE flag is set.

         Self.Peripheral.CR2 := Aux;
      end;

      --  Clear pending and enable NVIC interrupts

      A0B.ARMv7M.NVIC_Utilities.Clear_Pending (Self.Interrupt);
      A0B.ARMv7M.NVIC_Utilities.Enable_Interrupt (Self.Interrupt);

      --  Configure DMA stream for data transmit

      Self.Transmit_Stream.Configure_Memory_To_Peripheral
        (Channel    => Self.Transmit_Channel,
         Peripheral => Self.Peripheral.DR'Address);
      Self.Transmit_Stream.Enable_Transfer_Complete_Interrupt;
      Self.Transmit_Stream.Set_Interrupt_Callback
        (On_Interrupt_Callbacks.Create_Callback (Self));

      --  Configure DMA stream for data receive

      Self.Receive_Stream.Configure_Peripheral_To_Memory
        (Channel    => Self.Receive_Channel,
         Peripheral => Self.Peripheral.DR'Address);
      Self.Receive_Stream.Enable_Transfer_Complete_Interrupt;
      Self.Receive_Stream.Set_Interrupt_Callback
        (On_Interrupt_Callbacks.Create_Callback (Self));
   end Configure;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (Self : in out Master_Controller'Class) is
   begin
      case Self.Controller is
         when 1 =>
            A0B.STM32F401.SVD.RCC.RCC_Periph.APB2ENR.SPI1EN := True;

         when 2 =>
            A0B.STM32F401.SVD.RCC.RCC_Periph.APB1ENR.SPI2EN := True;

         when 3 =>
            A0B.STM32F401.SVD.RCC.RCC_Periph.APB1ENR.SPI3EN := True;

         when 4 =>
            A0B.STM32F401.SVD.RCC.RCC_Periph.APB2ENR.Reserved_13_13 := 1;
            --  SVD file doesn't define SPI4EN flag.
      end case;
   end Enable_Clock;

   ------------------
   -- On_Interrupt --
   ------------------

   procedure On_Interrupt (Self : in out Master_Controller'Class) is
   begin
      Event_Last := Natural'Min (@ + 1, Event_Log'Last);
      Event_Log (Event_Last) :=
        (Kind      => Interrupt,
         SPI_SR    => Self.Peripheral.SR,
         SPI_CR2   => Self.Peripheral.CR2,
         DMA_LISR  => A0B.STM32F401.SVD.DMA.DMA2_Periph.LISR,
         DMA_S2CR  => A0B.STM32F401.SVD.DMA.DMA2_Periph.S2CR,
         DMA_S2NDT => A0B.STM32F401.SVD.DMA.DMA2_Periph.S2NDTR.NDT,
         DMA_S3CR  => A0B.STM32F401.SVD.DMA.DMA2_Periph.S3CR,
         DMA_S3NDT => A0B.STM32F401.SVD.DMA.DMA2_Periph.S3NDTR.NDT);

      if Self.Transmit_Stream.Get_Masked_And_Clear_Transfer_Completed then
         --  Self.Transmit_Stream.Disable;
         --  --  Disable DMA stream

         if Self.Transmit_Buffers /= null then
            Self.Transmit_Buffers (Self.Transmit_Active).Transferred :=
              Self.Transmit_Buffers (Self.Transmit_Active).Size
              - A0B.Types.Unsigned_32 (Self.Transmit_Stream.Remaining_Items);
            --  XXX Isn't remainging is 0 here always?
            Self.Transmit_Buffers (Self.Transmit_Active).State       := Success;

            if Self.Transmit_Active /= Self.Transmit_Buffers'Last then
               --  Setup data transfer for the next buffer.

               Self.Transmit_Active := @ + 1;
               Self.Transmit_Stream.Set_Memory_Buffer
                 (Self.Transmit_Buffers (Self.Transmit_Active).Address,
                  A0B.Types.Unsigned_16
                    (Self.Transmit_Buffers (Self.Transmit_Active).Size));
               Self.Transmit_Stream.Clear_Interrupt_Status;
               Self.Transmit_Stream.Enable;

            else
               Self.Peripheral.CR2.TXEIE := True;

               --  while not Self.Peripheral.SR.TXE loop
               --     null;
               --  end loop;
               --
               --  while Self.Peripheral.SR.BSY loop
               --     null;
               --  end loop;
               --  declare
               --     DR : constant A0B.STM32F401.SVD.spi.DR_DR_Field :=
               --       Self.Peripheral.DR.DR
               --         with Unreferenced;
               --  begin
               --     Self.Peripheral.CR2.RXNEIE := True;
               --  end;
            end if;

            if Self.Placeholder_Count /= 0 then
               raise Program_Error;

               --  Self.Transmit_Stream.Set_Memory_Buffer
               --    (Memory    => Self.Placeholder_Buffer'Address,
               --     Count     => A0B.Types.Unsigned_16 (Self.Placeholder_Count),
               --     Increment => False);
               --  Self.Placeholder_Count := 0;
               --  Self.Transmit_Stream.Clear_Interrupt_Status;
               --  Self.Transmit_Stream.Enable;
            end if;
         end if;
      end if;

      if Self.Receive_Stream.Get_Masked_And_Clear_Transfer_Completed then
         --  if Self.Transmit_Buffers /= null then
         Self.Receive_Buffers (Self.Receive_Active).Transferred :=
           Self.Receive_Buffers (Self.Receive_Active).Size
             - A0B.Types.Unsigned_32 (Self.Receive_Stream.Remaining_Items);
            --  XXX Isn't remainging is 0 here always?
         Self.Receive_Buffers (Self.Receive_Active).State       := Success;

         if Self.Receive_Active /= Self.Receive_Buffers'Last then
            --  Setup data transfer for the next buffer.

            Self.Receive_Active := @ + 1;
            Self.Receive_Stream.Set_Memory_Buffer
              (Self.Receive_Buffers (Self.Receive_Active).Address,
               A0B.Types.Unsigned_16
                 (Self.Receive_Buffers (Self.Receive_Active).Size));
            Self.Receive_Stream.Clear_Interrupt_Status;
            Self.Receive_Stream.Enable;

         else
            A0B.Callbacks.Emit_Once (Self.Finished);
         end if;

         --     if Self.Placeholder_Count /= 0 then
         --        Self.Transmit_Stream.Set_Memory_Buffer
         --          (Memory    => Self.Placeholder_Buffer'Address,
         --           Count     => A0B.Types.Unsigned_16 (Self.Placeholder_Count),
         --           Increment => False);
         --        Self.Placeholder_Count := 0;
         --        Self.Transmit_Stream.Clear_Interrupt_Status;
         --        Self.Transmit_Stream.Enable;
         --     end if;
         --  end if;
      end if;

      if Self.Peripheral.SR.TXE and Self.Peripheral.CR2.TXEIE then
         declare
            DR : constant A0B.STM32F401.SVD.SPI.DR_DR_Field :=
              Self.Peripheral.DR.DR
                with Unreferenced;
         begin
            Self.Peripheral.CR2.RXNEIE := True;
         end;

         Self.Peripheral.CR2.TXEIE := False;
      end if;

      if Self.Peripheral.SR.RXNE and Self.Peripheral.CR2.RXNEIE then
         Self.Peripheral.CR2.RXNEIE := False;
         A0B.Callbacks.Emit_Once (Self.Finished);
      end if;
   end On_Interrupt;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Self            : in out Master_Controller;
      --  Transmit_Placeholder : A0B.Types.Unsigned_8;
      Receive_Buffers : A0B.SPI.Buffer_Descriptor_Array;
      On_Finished     : A0B.Callbacks.Callback;
      Success         : in out Boolean) is
   begin
      if not Success then
         return;
      end if;

      Event_Last := Natural'Min (@ + 1, Event_Log'Last);
      Event_Log (Event_Last) := (Kind => Receive);

      Self.Placeholder_Buffer := 16#DC#;

      Self.Transmit_Buffers  := null;
      Self.Transmit_Active   := 0;
      Self.Receive_Buffers   := Receive_Buffers'Unrestricted_Access;
      Self.Receive_Active    := Receive_Buffers'First;
      Self.Finished          := On_Finished;
      Self.Placeholder_Count := 0;

      if Self.Receive_Buffers /= null then
         for Buffer of Self.Receive_Buffers.all loop
            Buffer.Transferred := 0;
            Buffer.State       := Active;
         end loop;
      end if;

      if Self.Receive_Buffers /= null then
         for Buffer of Self.Receive_Buffers.all loop
            Self.Placeholder_Count := @ + Buffer.Size;
         end loop;
      end if;

      if Self.Receive_Buffers /= null then
         declare
            Aux : constant A0B.Types.Unsigned_16 := Self.Peripheral.DR.DR
              with Unreferenced;

         begin
            null;
         end;

         Self.Receive_Stream.Set_Memory_Buffer
           (Self.Receive_Buffers (Self.Receive_Active).Address,
            A0B.Types.Unsigned_16
              (Self.Receive_Buffers (Self.Receive_Active).Size));
         Self.Receive_Stream.Clear_Interrupt_Status;
         Self.Receive_Stream.Enable;
      end if;

      if Self.Transmit_Buffers /= null then
         Self.Transmit_Stream.Set_Memory_Buffer
           (Self.Transmit_Buffers (Self.Transmit_Active).Address,
            A0B.Types.Unsigned_16
              (Self.Transmit_Buffers (Self.Transmit_Active).Size));
         Self.Transmit_Stream.Clear_Interrupt_Status;
         Self.Transmit_Stream.Enable;

      else
         Self.Transmit_Stream.Set_Memory_Buffer
           (Memory    => Self.Placeholder_Buffer'Address,
            Count     => A0B.Types.Unsigned_16 (Self.Placeholder_Count),
            Increment => False);
         Self.Placeholder_Count := 0;
         Self.Transmit_Stream.Clear_Interrupt_Status;
         Self.Transmit_Stream.Enable;
      end if;

      Self.Peripheral.CR1.SPE := True;
   end Receive;

   --------------------
   -- Release_Device --
   --------------------

   overriding procedure Release_Device (Self : in out Master_Controller) is
   begin
      if Self.Peripheral.SR.BSY then
         raise Program_Error;
      end if;

      Self.Peripheral.CR1.SPE := False;
      --  raise Program_Error;
   end Release_Device;

   -------------------
   -- Select_Device --
   -------------------

   overriding procedure Select_Device (Self : in out Master_Controller) is
   begin
      raise Program_Error;
   end Select_Device;

   --------------
   -- Transfer --
   --------------

   overriding procedure Transfer
     (Self              : in out Master_Controller;
      Transmit_Buffer   : aliased A0B.Types.Unsigned_8;
      Receive_Buffer    : aliased out A0B.Types.Unsigned_8;
      Finished_Callback : A0B.Callbacks.Callback;
      Success           : in out Boolean) is
   begin
      Success := False;

      raise Program_Error;
   end Transfer;

   --------------
   -- Transmit --
   --------------

   overriding procedure Transmit
     (Self              : in out Master_Controller;
      Transmit_Buffer   : aliased A0B.Types.Unsigned_8;
      Finished_Callback : A0B.Callbacks.Callback;
      Success           : in out Boolean) is
   begin
      Success := False;

      raise Program_Error;
   end Transmit;

   --------------
   -- Transmit --
   --------------

   overriding procedure Transmit
     (Self             : in out Master_Controller;
      Transmit_Buffers : A0B.SPI.Buffer_Descriptor_Array;
      On_Finished      : A0B.Callbacks.Callback;
      Success          : in out Boolean) is
   begin
      if not Success then
         return;
      end if;

      Event_Last := Natural'Min (@ + 1, Event_Log'Last);
      Event_Log (Event_Last) := (Kind => Transmit);

      Self.Transmit_Buffers  := Transmit_Buffers'Unrestricted_Access;
      Self.Transmit_Active   := Transmit_Buffers'First;
      Self.Receive_Buffers   := null;
      Self.Receive_Active    := 0;
      Self.Finished          := On_Finished;
      Self.Placeholder_Count := 0;

      if Self.Transmit_Buffers /= null then
         Self.Transmit_Stream.Set_Memory_Buffer
           (Self.Transmit_Buffers (Self.Transmit_Active).Address,
            A0B.Types.Unsigned_16
              (Self.Transmit_Buffers (Self.Transmit_Active).Size));
         Self.Transmit_Stream.Clear_Interrupt_Status;
         Self.Transmit_Stream.Enable;
      end if;

      if Self.Receive_Buffers /= null then
         Self.Receive_Stream.Set_Memory_Buffer
           (Self.Receive_Buffers (Self.Receive_Active).Address,
            A0B.Types.Unsigned_16
              (Self.Receive_Buffers (Self.Receive_Active).Size));
         Self.Receive_Stream.Clear_Interrupt_Status;
         Self.Receive_Stream.Enable;
      end if;

      Self.Peripheral.CR1.SPE := True;
   end Transmit;

end A0B.STM32F401.SPI;
