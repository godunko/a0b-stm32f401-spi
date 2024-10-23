--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

with A0B.STM32F401.SPI_Function_Lines;

generic
   Transmit_Stream : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
   Receive_Stream  : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
   MOSI_Pin        : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
   MISO_Pin        : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
   SCK_Pin         : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
   NSS_Pin         : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;

package A0B.STM32F401.SPI.Generic_SPI1
  with Preelaborate
is

   pragma Elaborate_Body;

   SPI1 : aliased A0B.STM32F401.SPI.Master_Controller
     (Peripheral       => A0B.STM32F401.SVD.SPI.SPI1_Periph'Access,
      Controller       => 1,
      Interrupt        => A0B.STM32F401.SPI1,
      Transmit_Stream  => Transmit_Stream,
      Transmit_Channel => 3,
      Receive_Stream   => Receive_Stream,
      Receive_Channel  => 3,
      MOSI_Pin         => MOSI_Pin,
      MOSI_Line        => A0B.STM32F401.SPI_Function_Lines.SPI1_MOSI'Access,
      MISO_Pin         => MISO_Pin,
      MISO_Line        => A0B.STM32F401.SPI_Function_Lines.SPI1_MISO'Access,
      SCK_Pin          => SCK_Pin,
      SCK_Line         => A0B.STM32F401.SPI_Function_Lines.SPI1_SCK'Access,
      NSS_Pin          => NSS_Pin,
      NSS_Line         => A0B.STM32F401.SPI_Function_Lines.SPI1_NSS'Access);

end A0B.STM32F401.SPI.Generic_SPI1;
