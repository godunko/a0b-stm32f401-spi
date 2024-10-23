--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

package body A0B.STM32F401.SPI.Generic_SPI1 is

   pragma Warnings
     (Off, "ll instances of ""*"" will have the same external name");
   --  It is by design to prevent multiple instanses of the package.

   procedure SPI1_Handler
     with Export, Convention => C, External_Name => "SPI1_Handler";

   ------------------
   -- SPI1_Handler --
   ------------------

   procedure SPI1_Handler is
   begin
      SPI1.On_Interrupt;
   end SPI1_Handler;

end A0B.STM32F401.SPI.Generic_SPI1;
