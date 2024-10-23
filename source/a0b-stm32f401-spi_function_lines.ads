--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  STM32F401 SPI function line descriptors

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2022;

package A0B.STM32F401.SPI_Function_Lines
  with Preelaborate
is

   SPI1_MOSI : aliased constant Function_Line_Descriptor;
   SPI1_MISO : aliased constant Function_Line_Descriptor;
   SPI1_SCK  : aliased constant Function_Line_Descriptor;
   SPI1_NSS  : aliased constant Function_Line_Descriptor;
   SPI2_MOSI : aliased constant Function_Line_Descriptor;
   SPI2_MISO : aliased constant Function_Line_Descriptor;
   SPI2_SCK  : aliased constant Function_Line_Descriptor;
   SPI2_NSS  : aliased constant Function_Line_Descriptor;
   SPI3_MOSI : aliased constant Function_Line_Descriptor;
   SPI3_MISO : aliased constant Function_Line_Descriptor;
   SPI3_SCK  : aliased constant Function_Line_Descriptor;
   SPI3_NSS  : aliased constant Function_Line_Descriptor;
   SPI4_MOSI : aliased constant Function_Line_Descriptor;
   SPI4_MISO : aliased constant Function_Line_Descriptor;
   SPI4_SCK  : aliased constant Function_Line_Descriptor;
   SPI4_NSS  : aliased constant Function_Line_Descriptor;

private

   SPI1_MOSI : aliased constant Function_Line_Descriptor :=
     [(A, 7, 5), (B, 5, 5)];
   SPI1_MISO : aliased constant Function_Line_Descriptor :=
     [(A, 6, 5), (B, 4, 5)];
   SPI1_SCK  : aliased constant Function_Line_Descriptor :=
     [(A, 5, 5), (B, 3, 5)];
   SPI1_NSS  : aliased constant Function_Line_Descriptor :=
     [(A, 4, 5), (A, 15, 5)];
   SPI2_MOSI : aliased constant Function_Line_Descriptor :=
     [(B, 15, 5), (C, 3, 5)];
   SPI2_MISO : aliased constant Function_Line_Descriptor :=
     [(B, 14, 5), (C, 2, 5)];
   SPI2_SCK  : aliased constant Function_Line_Descriptor :=
     [(B, 10, 5), (B, 13, 5), (D, 3, 5)];
   SPI2_NSS  : aliased constant Function_Line_Descriptor :=
     [(B, 9, 5), (B, 12, 5)];
   SPI3_MOSI : aliased constant Function_Line_Descriptor :=
     [(B, 5, 6), (C, 12, 6), (D, 6, 5)];
   SPI3_MISO : aliased constant Function_Line_Descriptor :=
     [(B, 4, 6), (C, 11, 6)];
   SPI3_SCK  : aliased constant Function_Line_Descriptor :=
     [(B, 3, 6), (C, 10, 6)];
   SPI3_NSS  : aliased constant Function_Line_Descriptor :=
     [(A, 4, 6), (A, 15, 6)];
   SPI4_MOSI : aliased constant Function_Line_Descriptor :=
     [(E, 6, 5), (E, 14, 5)];
   SPI4_MISO : aliased constant Function_Line_Descriptor :=
     [(E, 5, 5), (E, 13, 5)];
   SPI4_SCK  : aliased constant Function_Line_Descriptor :=
     [(E, 2, 5), (E, 12, 5)];
   SPI4_NSS  : aliased constant Function_Line_Descriptor :=
     [(E, 4, 5), (E, 11, 5)];

end A0B.STM32F401.SPI_Function_Lines;
