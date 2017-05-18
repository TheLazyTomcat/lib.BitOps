{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  BitOps - Binary operations

  ©František Milt 2016-12-18

  Version 1.5.1

  Dependencies:
    AuxTypes    - github.com/ncs-sniper/Lib.AuxTypes
  * SimpleCPUID - github.com/ncs-sniper/Lib.SimpleCPUID

  Marked (*) libraries are required only when AllowASMExtensions symbol is
  defined and PurePascal symbol is not defined.

===============================================================================}
unit BitOps;

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE 64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL '16bit CPU not supported'}
{$ELSE}
  {$DEFINE 32bit}
{$IFEND}

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$IFEND}

{$IFDEF FPC}
  {$MODE Delphi}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
{$ELSE}
  {$IFNDEF x64}
    {
      When defined, some ASM instructions are inserted into stream directly as
      a machine code. It is there because not all compilers supports, and
      therefore can compile, such instructions.
      As I am not able to tell which 32bit delphi compilers do support them,
      I am assuming none of them do. I am also assuming that all 64bit delphi
      compilers and current FPCs are supporting the instructions.
      Has no meaning when PurePascal is defined.
    }
    {$DEFINE ASM_MachineCode}
  {$ENDIF}
{$ENDIF}

{
  When defined, PopCount functions will, in their PurePascal version, use
  lookup table instead of testing each bit in a passed value.
}
{$DEFINE UseLookupTable}

{
  Allows use of x86(-64) instruction extensions in ASM.
  When defined, availability of each extension is tested at unit initialization.
  The instructions are used only when proper extension is supported.

  Currently used extensions:

    POPCNT (SSE 4.2)    - PopCount functions
}
{$DEFINE AllowASMExtensions}

interface

uses
  AuxTypes;

{------------------------------------------------------------------------------}
{==============================================================================}
{                  Integer number <-> Bit string conversions                   }
{==============================================================================}
{------------------------------------------------------------------------------}

Function NumberToBits(Number: UInt8): String; overload;
Function NumberToBits(Number: UInt16): String; overload;
Function NumberToBits(Number: UInt32): String; overload;
Function NumberToBits(Number: UInt64): String; overload;

Function BitsToNumber(const BitString: String): UInt64;

{------------------------------------------------------------------------------}
{==============================================================================}
{                               Rotate left (ROL)                              }
{==============================================================================}
{------------------------------------------------------------------------------}

Function ROL(Value: UInt8; Shift: UInt8): UInt8; overload;
Function ROL(Value: UInt16; Shift: UInt8): UInt16; overload;
Function ROL(Value: UInt32; Shift: UInt8): UInt32; overload;
Function ROL(Value: UInt64; Shift: UInt8): UInt64; overload;

procedure ROLValue(var Value: UInt8; Shift: UInt8); overload;
procedure ROLValue(var Value: UInt16; Shift: UInt8); overload;
procedure ROLValue(var Value: UInt32; Shift: UInt8); overload;
procedure ROLValue(var Value: UInt64; Shift: UInt8); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                              Rotate right (ROR)                              }
{==============================================================================}
{------------------------------------------------------------------------------}

Function ROR(Value: UInt8; Shift: UInt8): UInt8; overload;
Function ROR(Value: UInt16; Shift: UInt8): UInt16; overload;
Function ROR(Value: UInt32; Shift: UInt8): UInt32; overload;
Function ROR(Value: UInt64; Shift: UInt8): UInt64; overload;

procedure RORValue(var Value: UInt8; Shift: UInt8); overload;
procedure RORValue(var Value: UInt16; Shift: UInt8); overload;
procedure RORValue(var Value: UInt32; Shift: UInt8); overload;
procedure RORValue(var Value: UInt64; Shift: UInt8); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Rotate left with carry (RCL)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function RCLCarry(Value: UInt8; Shift: UInt8; var CF: ByteBool): UInt8; overload;
Function RCLCarry(Value: UInt16; Shift: UInt8; var CF: ByteBool): UInt16; overload;
Function RCLCarry(Value: UInt32; Shift: UInt8; var CF: ByteBool): UInt32; overload;
Function RCLCarry(Value: UInt64; Shift: UInt8; var CF: ByteBool): UInt64; overload;

Function RCL(Value: UInt8; Shift: UInt8; CF: ByteBool = False): UInt8; overload;
Function RCL(Value: UInt16; Shift: UInt8; CF: ByteBool = False): UInt16; overload;
Function RCL(Value: UInt32; Shift: UInt8; CF: ByteBool = False): UInt32; overload;
Function RCL(Value: UInt64; Shift: UInt8; CF: ByteBool = False): UInt64; overload;

procedure RCLValueCarry(var Value: UInt8; Shift: UInt8; var CF: ByteBool); overload;
procedure RCLValueCarry(var Value: UInt16; Shift: UInt8; var CF: ByteBool); overload;
procedure RCLValueCarry(var Value: UInt32; Shift: UInt8; var CF: ByteBool); overload;
procedure RCLValueCarry(var Value: UInt64; Shift: UInt8; var CF: ByteBool); overload;

procedure RCLValue(var Value: UInt8; Shift: UInt8; CF: ByteBool = False); overload;
procedure RCLValue(var Value: UInt16; Shift: UInt8; CF: ByteBool = False); overload;
procedure RCLValue(var Value: UInt32; Shift: UInt8; CF: ByteBool = False); overload;
procedure RCLValue(var Value: UInt64; Shift: UInt8; CF: ByteBool = False); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                        Rotate right with carry (RCR)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function RCRCarry(Value: UInt8; Shift: UInt8; var CF: ByteBool): UInt8; overload;
Function RCRCarry(Value: UInt16; Shift: UInt8; var CF: ByteBool): UInt16; overload;
Function RCRCarry(Value: UInt32; Shift: UInt8; var CF: ByteBool): UInt32; overload;
Function RCRCarry(Value: UInt64; Shift: UInt8; var CF: ByteBool): UInt64; overload;

Function RCR(Value: UInt8; Shift: UInt8; CF: ByteBool = False): UInt8; overload;
Function RCR(Value: UInt16; Shift: UInt8; CF: ByteBool = False): UInt16; overload;
Function RCR(Value: UInt32; Shift: UInt8; CF: ByteBool = False): UInt32; overload;
Function RCR(Value: UInt64; Shift: UInt8; CF: ByteBool = False): UInt64; overload;

procedure RCRValueCarry(var Value: UInt8; Shift: UInt8; var CF: ByteBool); overload;
procedure RCRValueCarry(var Value: UInt16; Shift: UInt8; var CF: ByteBool); overload;
procedure RCRValueCarry(var Value: UInt32; Shift: UInt8; var CF: ByteBool); overload;
procedure RCRValueCarry(var Value: UInt64; Shift: UInt8; var CF: ByteBool); overload;

procedure RCRValue(var Value: UInt8; Shift: UInt8; CF: ByteBool = False); overload;
procedure RCRValue(var Value: UInt16; Shift: UInt8; CF: ByteBool = False); overload;
procedure RCRValue(var Value: UInt32; Shift: UInt8; CF: ByteBool = False); overload;
procedure RCRValue(var Value: UInt64; Shift: UInt8; CF: ByteBool = False); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Arithmetic left shift (SAL)                          }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SAL(Value: UInt8; Shift: UInt8): UInt8; overload;
Function SAL(Value: UInt16; Shift: UInt8): UInt16; overload;
Function SAL(Value: UInt32; Shift: UInt8): UInt32; overload;
Function SAL(Value: UInt64; Shift: UInt8): UInt64; overload;

procedure SALValue(var Value: UInt8; Shift: UInt8); overload;
procedure SALValue(var Value: UInt16; Shift: UInt8); overload;
procedure SALValue(var Value: UInt32; Shift: UInt8); overload;
procedure SALValue(var Value: UInt64; Shift: UInt8); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Arithmetic right shift (SAR)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SAR(Value: UInt8; Shift: UInt8): UInt8; overload;
Function SAR(Value: UInt16; Shift: UInt8): UInt16; overload;
Function SAR(Value: UInt32; Shift: UInt8): UInt32; overload;
Function SAR(Value: UInt64; Shift: UInt8): UInt64; overload;

procedure SARValue(var Value: UInt8; Shift: UInt8); overload;
procedure SARValue(var Value: UInt16; Shift: UInt8); overload;
procedure SARValue(var Value: UInt32; Shift: UInt8); overload;
procedure SARValue(var Value: UInt64; Shift: UInt8); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                Endianity swap                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function EndianSwap(Value: UInt16): UInt16; overload;
Function EndianSwap(Value: UInt32): UInt32; overload;
Function EndianSwap(Value: UInt64): UInt64; overload;

procedure EndianSwapValue(var Value: UInt16); overload;
procedure EndianSwapValue(var Value: UInt32); overload;
procedure EndianSwapValue(var Value: UInt64); overload;

procedure EndianSwap(var Buffer; Size: TMemSize); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                 Bit test (BT)                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BT(Value: UInt8; Bit: UInt8): ByteBool; overload;
Function BT(Value: UInt16; Bit: UInt8): ByteBool; overload;
Function BT(Value: UInt32; Bit: UInt8): ByteBool; overload;
Function BT(Value: UInt64; Bit: UInt8): ByteBool; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit test and set (BTS)                            }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTS(var Value: UInt8; Bit: UInt8): ByteBool; overload;
Function BTS(var Value: UInt16; Bit: UInt8): ByteBool; overload;
Function BTS(var Value: UInt32; Bit: UInt8): ByteBool; overload;
Function BTS(var Value: UInt64; Bit: UInt8): ByteBool; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                           Bit test and reset (BTR)                           }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTR(var Value: UInt8; Bit: UInt8): ByteBool; overload;
Function BTR(var Value: UInt16; Bit: UInt8): ByteBool; overload;
Function BTR(var Value: UInt32; Bit: UInt8): ByteBool; overload;
Function BTR(var Value: UInt64; Bit: UInt8): ByteBool; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Bit test and complement (BTC)                        }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTC(var Value: UInt8; Bit: UInt8): ByteBool; overload;
Function BTC(var Value: UInt16; Bit: UInt8): ByteBool; overload;
Function BTC(var Value: UInt32; Bit: UInt8): ByteBool; overload;
Function BTC(var Value: UInt64; Bit: UInt8): ByteBool; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                      Bit test and set to a given value                       }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BitSetTo(var Value: UInt8; Bit: UInt8; NewValue: ByteBool): ByteBool; overload;
Function BitSetTo(var Value: UInt16; Bit: UInt8; NewValue: ByteBool): ByteBool; overload;
Function BitSetTo(var Value: UInt32; Bit: UInt8; NewValue: ByteBool): ByteBool; overload;
Function BitSetTo(var Value: UInt64; Bit: UInt8; NewValue: ByteBool): ByteBool; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit scan forward (BSF)                            }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BSF(Value: UInt8): Int32; overload;
Function BSF(Value: UInt16): Int32; overload;
Function BSF(Value: UInt32): Int32; overload;
Function BSF(Value: UInt64): Int32; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit scan reversed (BSR)                           }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BSR(Value: UInt8): Int32; overload;
Function BSR(Value: UInt16): Int32; overload;
Function BSR(Value: UInt32): Int32; overload;
Function BSR(Value: UInt64): Int32; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                               Population count                               }
{==============================================================================}
{------------------------------------------------------------------------------}

Function PopCount(Value: UInt8): Int32; overload;
Function PopCount(Value: UInt16): Int32; overload;
Function PopCount(Value: UInt32): Int32; overload;
Function PopCount(Value: UInt64): Int32; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                              Nibble manipulation                             }
{==============================================================================}
{------------------------------------------------------------------------------}

Function GetHighNibble(Value: UInt8): TNibble;
Function GetLowNibble(Value: UInt8): TNibble;

Function SetHighNibble(Value: UInt8; SetTo: TNibble): UInt8;
Function SetLowNibble(Value: UInt8; SetTo: TNibble): UInt8;

procedure SetHighNibbleValue(var Value: UInt8; SetTo: TNibble);
procedure SetLowNibbleValue(var Value: UInt8; SetTo: TNibble);

{------------------------------------------------------------------------------}
{==============================================================================}
{                                Get flag state                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function GetFlagState(Value,FlagBitmask: UInt8; ExactMatch: Boolean = False): Boolean; overload;
Function GetFlagState(Value,FlagBitmask: UInt16; ExactMatch: Boolean = False): Boolean; overload;
Function GetFlagState(Value,FlagBitmask: UInt32; ExactMatch: Boolean = False): Boolean; overload;
Function GetFlagState(Value,FlagBitmask: UInt64; ExactMatch: Boolean = False): Boolean; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                   Set flag                                   }
{==============================================================================}
{------------------------------------------------------------------------------}
{
  Functions with bits noted in name (*_8, *_16, ...) are there mainly for older
  versions of Delphi (up to Delphi 2007), because they are not able to
  distinguish what overloaded function to call (some problem with open array
  parameter parsing).
}

Function SetFlag(Value,FlagBitmask: UInt8): UInt8; overload;
Function SetFlag(Value,FlagBitmask: UInt16): UInt16; overload;
Function SetFlag(Value,FlagBitmask: UInt32): UInt32; overload;
Function SetFlag(Value,FlagBitmask: UInt64): UInt64; overload;

procedure SetFlagValue(var Value: UInt8; FlagBitmask: UInt8); overload;
procedure SetFlagValue(var Value: UInt16; FlagBitmask: UInt16); overload;
procedure SetFlagValue(var Value: UInt32; FlagBitmask: UInt32); overload;
procedure SetFlagValue(var Value: UInt64; FlagBitmask: UInt64); overload;

Function SetFlags_8(Value: UInt8; Flags: array of UInt8): UInt8;
Function SetFlags_16(Value: UInt16; Flags: array of UInt16): UInt16;
Function SetFlags_32(Value: UInt32; Flags: array of UInt32): UInt32;
Function SetFlags_64(Value: UInt64; Flags: array of UInt64): UInt64;

Function SetFlags(Value: UInt8; Flags: array of UInt8): UInt8; overload;
Function SetFlags(Value: UInt16; Flags: array of UInt16): UInt16; overload;
Function SetFlags(Value: UInt32; Flags: array of UInt32): UInt32; overload;
Function SetFlags(Value: UInt64; Flags: array of UInt64): UInt64; overload;

procedure SetFlagsValue_8(var Value: UInt8; Flags: array of UInt8);
procedure SetFlagsValue_16(var Value: UInt16; Flags: array of UInt16);
procedure SetFlagsValue_32(var Value: UInt32; Flags: array of UInt32);
procedure SetFlagsValue_64(var Value: UInt64; Flags: array of UInt64);

procedure SetFlagsValue(var Value: UInt8; Flags: array of UInt8); overload;
procedure SetFlagsValue(var Value: UInt16; Flags: array of UInt16); overload;
procedure SetFlagsValue(var Value: UInt32; Flags: array of UInt32); overload;
procedure SetFlagsValue(var Value: UInt64; Flags: array of UInt64); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                  Reset flag                                  }
{==============================================================================}
{------------------------------------------------------------------------------}
{
  Functions with bits noted in name (*_8, *_16, ...) are there mainly for older
  versions of Delphi (up to Delphi 2007), because they are not able to
  distinguish what overloaded function to call (some problem with open array
  parameter parsing).
}

Function ResetFlag(Value,FlagBitmask: UInt8): UInt8; overload;
Function ResetFlag(Value,FlagBitmask: UInt16): UInt16; overload;
Function ResetFlag(Value,FlagBitmask: UInt32): UInt32; overload;
Function ResetFlag(Value,FlagBitmask: UInt64): UInt64; overload;

procedure ResetFlagValue(var Value: UInt8; FlagBitmask: UInt8); overload;
procedure ResetFlagValue(var Value: UInt16; FlagBitmask: UInt16); overload;
procedure ResetFlagValue(var Value: UInt32; FlagBitmask: UInt32); overload;
procedure ResetFlagValue(var Value: UInt64; FlagBitmask: UInt64); overload;

Function ResetFlags_8(Value: UInt8; Flags: array of UInt8): UInt8;
Function ResetFlags_16(Value: UInt16; Flags: array of UInt16): UInt16;
Function ResetFlags_32(Value: UInt32; Flags: array of UInt32): UInt32;
Function ResetFlags_64(Value: UInt64; Flags: array of UInt64): UInt64;

Function ResetFlags(Value: UInt8; Flags: array of UInt8): UInt8; overload;
Function ResetFlags(Value: UInt16; Flags: array of UInt16): UInt16; overload;
Function ResetFlags(Value: UInt32; Flags: array of UInt32): UInt32; overload;
Function ResetFlags(Value: UInt64; Flags: array of UInt64): UInt64; overload;

procedure ResetFlagsValue_8(var Value: UInt8; Flags: array of UInt8);
procedure ResetFlagsValue_16(var Value: UInt16; Flags: array of UInt16);
procedure ResetFlagsValue_32(var Value: UInt32; Flags: array of UInt32);
procedure ResetFlagsValue_64(var Value: UInt64; Flags: array of UInt64);

procedure ResetFlagsValue(var Value: UInt8; Flags: array of UInt8); overload;
procedure ResetFlagsValue(var Value: UInt16; Flags: array of UInt16); overload;
procedure ResetFlagsValue(var Value: UInt32; Flags: array of UInt32); overload;
procedure ResetFlagsValue(var Value: UInt64; Flags: array of UInt64); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                Set flag state                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SetFlagState(Value,FlagBitmask: UInt8; NewState: Boolean): UInt8; overload;
Function SetFlagState(Value,FlagBitmask: UInt16; NewState: Boolean): UInt16; overload;
Function SetFlagState(Value,FlagBitmask: UInt32; NewState: Boolean): UInt32; overload;
Function SetFlagState(Value,FlagBitmask: UInt64; NewState: Boolean): UInt64; overload;

procedure SetFlagStateValue(var Value: UInt8; FlagBitmask: UInt8; NewState: Boolean); overload;
procedure SetFlagStateValue(var Value: UInt16; FlagBitmask: UInt16; NewState: Boolean); overload;
procedure SetFlagStateValue(var Value: UInt32; FlagBitmask: UInt32; NewState: Boolean); overload;
procedure SetFlagStateValue(var Value: UInt64; FlagBitmask: UInt64; NewState: Boolean); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                   Get bits                                   }
{==============================================================================}
{------------------------------------------------------------------------------}
{
  Returns contiguous segment of bits from passed Value, selected by a bit range.
}
Function GetBits(Value: UInt8; FromBit,ToBit: Int32; ShiftDown: Boolean = True): UInt8; overload;
Function GetBits(Value: UInt16; FromBit,ToBit: Int32; ShiftDown: Boolean = True): UInt16; overload;
Function GetBits(Value: UInt32; FromBit,ToBit: Int32; ShiftDown: Boolean = True): UInt32; overload;
Function GetBits(Value: UInt64; FromBit,ToBit: Int32; ShiftDown: Boolean = True): UInt64; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                   Set bits                                   }
{==============================================================================}
{------------------------------------------------------------------------------}
{
  Replaces contiguous segment of bits in Value by corresponding bits from
  NewBits.
}
Function SetBits(Value,NewBits: UInt8; FromBit,ToBit: Int32): UInt8; overload;
Function SetBits(Value,NewBits: UInt16; FromBit,ToBit: Int32): UInt16; overload;
Function SetBits(Value,NewBits: UInt32; FromBit,ToBit: Int32): UInt32; overload;
Function SetBits(Value,NewBits: UInt64; FromBit,ToBit: Int32): UInt64; overload;

procedure SetBitsValue(var Value: UInt8; NewBits: UInt8; FromBit,ToBit: Int32); overload;
procedure SetBitsValue(var Value: UInt16; NewBits: UInt16; FromBit,ToBit: Int32); overload;
procedure SetBitsValue(var Value: UInt32; NewBits: UInt32; FromBit,ToBit: Int32); overload;
procedure SetBitsValue(var Value: UInt64; NewBits: UInt64; FromBit,ToBit: Int32); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                 Reverse bits                                 }
{==============================================================================}
{------------------------------------------------------------------------------}

Function ReverseBits(Value: UInt8): UInt8; overload;
Function ReverseBits(Value: UInt16): UInt16; overload;
Function ReverseBits(Value: UInt32): UInt32; overload;
Function ReverseBits(Value: UInt64): UInt64; overload;

procedure ReverseBitsValue(var Value: UInt8); overload;
procedure ReverseBitsValue(var Value: UInt16); overload;
procedure ReverseBitsValue(var Value: UInt32); overload;
procedure ReverseBitsValue(var Value: UInt64); overload;

implementation

uses
  SysUtils, Math
{$IF Defined(AllowASMExtensions) and not Defined(PurePascal)}
  , SimpleCPUID
{$IFEND};

{------------------------------------------------------------------------------}
{==============================================================================}
{                  Integer number <-> Bit string conversions                   }
{==============================================================================}
{------------------------------------------------------------------------------}

Function _NumberToBits(Number: UInt64; Bits: UInt8): String;
var
  i:  Integer;
begin
Result := StringOfChar('0',Bits);
For i := Bits downto 1 do
  begin
    If (Number and 1) <> 0 then Result[i] := '1';
    Number := Number shr 1;
  end;
end;

//------------------------------------------------------------------------------

Function NumberToBits(Number: UInt8): String;
begin
Result := _NumberToBits(Number,8);
end;

//------------------------------------------------------------------------------

Function NumberToBits(Number: UInt16): String;
begin
Result := _NumberToBits(Number,16);
end;

//------------------------------------------------------------------------------

Function NumberToBits(Number: UInt32): String;
begin
Result := _NumberToBits(Number,32);
end;

//------------------------------------------------------------------------------

Function NumberToBits(Number: UInt64): String;
begin
Result := _NumberToBits(Number,64);
end;

//==============================================================================

Function BitsToNumber(const BitString: String): UInt64;
var
  i:  Integer;
begin
Result := 0;
For i := 1 to Min(Length(BitString),64) do
  begin
    Result := Result shl 1;
    If BitString[i] <> '0' then Result := Result or 1;
  end;
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                              Rotate left (ROL)                               }
{==============================================================================}
{------------------------------------------------------------------------------}

Function ROL(Value: UInt8; Shift: UInt8): UInt8;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   AL,   Value
{$ENDIF}
    MOV   CL,   Shift
    ROL   AL,   CL
end;
{$ELSE}
begin
Shift := Shift and $07;
Result := UInt8((Value shl Shift) or (Value shr (8 - Shift)));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROL(Value: UInt16; Shift: UInt8): UInt16;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   AX,   Value
{$ENDIF}
    MOV   CL,   Shift
    ROL   AX,   CL
end;
{$ELSE}
begin
Shift := Shift and $0F;
Result := UInt16((Value shl Shift) or (Value shr (16 - Shift)));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROL(Value: UInt32; Shift: UInt8): UInt32;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   EAX,  Value
{$ENDIF}
    MOV   CL,   Shift
    ROL   EAX,  CL
end;
{$ELSE}
begin
Shift := Shift and $1F;
Result := UInt32((Value shl Shift) or (Value shr (32 - Shift)));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROL(Value: UInt64; Shift: UInt8): UInt64;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   RAX,  Value
    MOV   CL,   Shift
    ROL   RAX,  CL
{$ELSE}
    MOV   ECX,  EAX
    AND   ECX,  $3F
    CMP   ECX,  32

    JAE   @Above31

  @Below32:
    MOV   EAX,  dword ptr [Value]
    MOV   EDX,  dword ptr [Value + 4]
    CMP   ECX,  0
    JE    @FuncEnd

    MOV   dword ptr [Value],  EDX
    JMP   @Rotate

  @Above31:
    MOV   EDX,  dword ptr [Value]
    MOV   EAX,  dword ptr [Value + 4]
    JE    @FuncEnd

    AND   ECX,  $1F

  @Rotate:
    SHLD  EDX,  EAX, CL
    SHL   EAX,  CL
    PUSH  EAX
    MOV   EAX,  dword ptr [Value]
    XOR   CL,   31
    INC   CL
    SHR   EAX,  CL
    POP   ECX
    OR    EAX,  ECX

  @FuncEnd:
{$ENDIF}
end;
{$ELSE}
begin
Shift := Shift and $3F;
Result := UInt64((Value shl Shift) or (Value shr (64 - Shift)));
end;
{$ENDIF}

//==============================================================================

procedure ROLValue(var Value: UInt8; Shift: UInt8);
begin
Value := ROL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure ROLValue(var Value: UInt16; Shift: UInt8);
begin
Value := ROL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure ROLValue(var Value: UInt32; Shift: UInt8);
begin
Value := ROL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure ROLValue(var Value: UInt64; Shift: UInt8);
begin
Value := ROL(Value,Shift);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                              Rotate right (ROR)                              }
{==============================================================================}
{------------------------------------------------------------------------------}

Function ROR(Value: UInt8; Shift: UInt8): UInt8;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   AL,   Value
{$ENDIF}
    MOV   CL,   Shift
    ROR   AL,   CL
end;
{$ELSE}
begin
Shift := Shift and $07;
Result := UInt8((Value shr Shift) or (Value shl (8 - Shift)));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROR(Value: UInt16; Shift: UInt8): UInt16;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   AX,   Value
{$ENDIF}
    MOV   CL,   Shift
    ROR   AX,   CL
end;
{$ELSE}
begin
Shift := Shift and $0F;
Result := UInt16((Value shr Shift) or (Value shl (16 - Shift)));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROR(Value: UInt32; Shift: UInt8): UInt32;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   EAX,  Value
{$ENDIF}
    MOV   CL,   Shift
    ROR   EAX,  CL
end;
{$ELSE}
begin
Shift := Shift and $1F;
Result := UInt32((Value shr Shift) or (Value shl (32 - Shift)));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROR(Value: UInt64; Shift: UInt8): UInt64;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   RAX,  Value
    MOV   CL,   Shift
    ROR   RAX,  CL
{$ELSE}
    MOV   ECX,  EAX
    AND   ECX,  $3F
    CMP   ECX,  32

    JAE   @Above31

  @Below32:
    MOV   EAX,  dword ptr [Value]
    MOV   EDX,  dword ptr [Value + 4]
    CMP   ECX,  0
    JE    @FuncEnd

    MOV   dword ptr [Value],  EDX
    JMP   @Rotate

  @Above31:
    MOV   EDX,  dword ptr [Value]
    MOV   EAX,  dword ptr [Value + 4]
    JE    @FuncEnd

    AND   ECX,  $1F

  @Rotate:
    SHRD  EDX,  EAX, CL
    SHR   EAX,  CL
    PUSH  EAX
    MOV   EAX,  dword ptr [Value]
    XOR   CL,   31
    INC   CL
    SHL   EAX,  CL
    POP   ECX
    OR    EAX,  ECX

  @FuncEnd:
{$ENDIF}
end;
{$ELSE}
begin
Shift := Shift and $3F;
Result := UInt64((Value shr Shift) or (Value shl (64 - Shift)));
end;
{$ENDIF}

//==============================================================================

procedure RORValue(var Value: UInt8; Shift: UInt8);
begin
Value := ROR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure RORValue(var Value: UInt16; Shift: UInt8);
begin
Value := ROR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure RORValue(var Value: UInt32; Shift: UInt8);
begin
Value := ROR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure RORValue(var Value: UInt64; Shift: UInt8);
begin
Value := ROR(Value,Shift);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Rotate left with carry (RCL)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function RCLCarry(Value: UInt8; Shift: UInt8; var CF: ByteBool): UInt8;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV     AL,   Value
    MOV     CL,   Shift
    SHR     byte ptr [CF],  1
    RCL     AL,   CL
    SETC    byte ptr [CF]
{$ELSE}
    XCHG    EDX,  ECX
    SHR     byte ptr [EDX], 1
    RCL     AL,   CL
    SETC    byte ptr [EDX]
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $07;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result shr 7) <> 0;
    Result := UInt8((Result shl 1) or (UInt8(Carry) and UInt8(1)));
    Carry := CF;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCLCarry(Value: UInt16; Shift: UInt8; var CF: ByteBool): UInt16;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   AX,   Value
    MOV   CL,   Shift
    SHR   byte ptr [CF],  1
    RCL   AX,   CL
    SETC  byte ptr [CF]
{$ELSE}
    XCHG  EDX,  ECX
    SHR   byte ptr [EDX], 1
    RCL   AX,   CL
    SETC  byte ptr [EDX]
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $0F;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result shr 15) <> 0;
    Result := UInt16((Result shl 1) or (UInt16(Carry) and UInt16(1)));
    Carry := CF;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCLCarry(Value: UInt32; Shift: UInt8; var CF: ByteBool): UInt32;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   EAX,  Value
    MOV   CL,   Shift
    SHR   byte ptr [CF],  1
    RCL   EAX,  CL
    SETC  byte ptr [CF]
{$ELSE}
    XCHG  EDX,  ECX
    SHR   byte ptr [EDX], 1
    RCL   EAX,  CL
    SETC  byte ptr [EDX]
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $1F;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result shr 31) <> 0;
    Result := UInt32((Result shl 1) or (UInt32(Carry) and 1));
    Carry := CF;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCLCarry(Value: UInt64; Shift: UInt8; var CF: ByteBool): UInt64;{$IFNDEF PurePascal}register; assembler;
{$IFDEF x64}
asm
    MOV   RAX,  Value
    MOV   CL,   Shift
    SHR   byte ptr [CF],  1
    RCL   RAX,  CL
    SETC  byte ptr [CF]
end;
{$ELSE}
var
  TempShift:  UInt32;
asm
    PUSH  EBX

    AND   EAX,  $3F
    MOV   dword ptr [TempShift],  EAX
    MOV   ECX,  EAX
    MOV   EBX,  EDX

    MOV   EAX,  dword ptr [Value]
    MOV   EDX,  dword ptr [Value + 4]
    CMP   ECX,  32

    JE    @Exactly32
    JA    @Above32

{- Shift is below 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
    TEST  ECX,  ECX
    JZ    @FuncEnd

    SHLD  EDX,  EAX, CL
    JMP   @Shift

{- Shift is above 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Above32:
    AND   ECX,  $1F

    DEC   CL
    SHLD  EDX,  EAX, CL
    INC   CL

{- Main shifting  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Shift:
    SHL   EAX,  CL
    PUSH  ECX
    PUSH  EAX
    MOV   EAX,  dword ptr [Value + 4]
    SHR   EAX,  2
    XOR   CL,   31
    SHR   EAX,  CL
    POP   ECX
    OR    EAX,  ECX
    POP   ECX
    JMP   @SetCarry

{- Shift is equal to 32, no shifting required, only swap Hi and Lo dwords - - -}
  @Exactly32:
    SHR   EDX,  1
    XCHG  EAX,  EDX

{- Write passed carry bit to the result - - - - - - - - - - - - - - - - - - - -}
  @SetCarry:
    DEC   ECX
    CMP   byte ptr [EBX], 0
    JE    @ResetBit

    BTS   EAX,  ECX
    JMP   @Swap

  @ResetBit:
    BTR   EAX,  ECX

{- Swap Hi and Lo dwords for shift > 32 - - - - - - - - - - - - - - - - - - - -}
  @Swap:
    CMP   byte ptr [TempShift],  32
    JBE   @GetCarry
    XCHG  EAX,  EDX

{- Get carry bit that will be output in CF parameter  - - - - - - - - - - - - -}
  @GetCarry:
    MOV   CL,   byte ptr [TempShift]
    AND   ECX,  $3F
    CMP   CL,   32
    JBE   @FromHigh

    AND   CL,   $1F
    DEC   CL
    XOR   CL,   31
    BT    dword ptr [Value],  ECX
    JMP   @StoreCarry

  @FromHigh:
    DEC   CL
    XOR   CL,   31
    BT    dword ptr [Value + 4], ECX

  @StoreCarry:
    SETC  CL
    MOV   byte ptr [EBX], CL

{- Restore EBX register - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @FuncEnd:
    POP   EBX
end;
{$ENDIF}
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $3F;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result shr 63) <> 0;
    Result := UInt64((Result shl 1) or (UInt64(Carry) and 1));
    Carry := CF;
  end;
end;
{$ENDIF}


//==============================================================================

Function RCL(Value: UInt8; Shift: UInt8; CF: ByteBool = False): UInt8;
begin
Result := RCLCarry(Value,Shift,{%H-}CF);
end;

//------------------------------------------------------------------------------

Function RCL(Value: UInt16; Shift: UInt8; CF: ByteBool = False): UInt16;
begin
Result := RCLCarry(Value,Shift,{%H-}CF);
end;

//------------------------------------------------------------------------------

Function RCL(Value: UInt32; Shift: UInt8; CF: ByteBool = False): UInt32;
begin
Result := RCLCarry(Value,Shift,{%H-}CF);
end;

//------------------------------------------------------------------------------

Function RCL(Value: UInt64; Shift: UInt8; CF: ByteBool = False): UInt64;
begin
Result := RCLCarry(Value,Shift,{%H-}CF);
end;

//==============================================================================

procedure RCLValueCarry(var Value: UInt8; Shift: UInt8; var CF: ByteBool);
begin
Value := RCLCarry(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCLValueCarry(var Value: UInt16; Shift: UInt8; var CF: ByteBool);
begin
Value := RCLCarry(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCLValueCarry(var Value: UInt32; Shift: UInt8; var CF: ByteBool);
begin
Value := RCLCarry(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCLValueCarry(var Value: UInt64; Shift: UInt8; var CF: ByteBool);
begin
Value := RCLCarry(Value,Shift,CF);
end;

//==============================================================================

procedure RCLValue(var Value: UInt8; Shift: UInt8; CF: ByteBool = False);
begin
Value := RCL(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCLValue(var Value: UInt16; Shift: UInt8; CF: ByteBool = False);
begin
Value := RCL(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCLValue(var Value: UInt32; Shift: UInt8; CF: ByteBool = False);
begin
Value := RCL(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCLValue(var Value: UInt64; Shift: UInt8; CF: ByteBool = False);
begin
Value := RCL(Value,Shift,CF);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                        Rotate right with carry (RCR)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function RCRCarry(Value: UInt8; Shift: UInt8; var CF: ByteBool): UInt8;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   AL,   Value
    MOV   CL,   Shift
    SHR   byte ptr [CF],  1
    RCR   AL,   CL
    SETC  byte ptr [CF]
{$ELSE}
    XCHG  EDX,  ECX
    SHR   byte ptr [EDX], 1
    RCR   AL,   CL
    SETC  byte ptr [EDX]
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $07;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    Result := UInt8((Result shr 1) or ((UInt8(Carry) and UInt8(1)) shl 7));
    Carry := CF;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCRCarry(Value: UInt16; Shift: UInt8; var CF: ByteBool): UInt16;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   AX,   Value
    MOV   CL,   Shift
    SHR   byte ptr [CF],  1
    RCR   AX,   CL
    SETC  byte ptr [CF]
{$ELSE}
    XCHG  EDX,  ECX
    SHR   byte ptr [EDX], 1
    RCR   AX,   CL
    SETC  byte ptr [EDX]
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $0F;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    Result := UInt16((Result shr 1) or ((UInt16(Carry) and UInt16(1)) shl 15));
    Carry := CF;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCRCarry(Value: UInt32; Shift: UInt8; var CF: ByteBool): UInt32;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   EAX,  Value
    MOV   CL,   Shift
    SHR   byte ptr [CF],  1
    RCR   EAX,  CL
    SETC  byte ptr [CF]
{$ELSE}
    XCHG  EDX,  ECX
    SHR   byte ptr [EDX], 1
    RCR   EAX,  CL
    SETC  byte ptr [EDX]
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $1F;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    Result := UInt32((Result shr 1) or ((UInt32(Carry) and 1) shl 31));
    Carry := CF;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCRCarry(Value: UInt64; Shift: UInt8; var CF: ByteBool): UInt64;{$IFNDEF PurePascal}register; assembler;
{$IFDEF x64}
asm
    MOV   RAX,  Value
    MOV   CL,   Shift
    SHR   byte ptr [CF],  1
    RCR   RAX,  CL
    SETC  byte ptr [CF]
end;
{$ELSE}
var
  TempShift:  UInt32;
asm
    PUSH  EBX

    AND   EAX,  $3F
    MOV   dword ptr [TempShift],  EAX
    MOV   ECX,  EAX
    MOV   EBX,  EDX

    MOV   EAX,  dword ptr [Value]
    MOV   EDX,  dword ptr [Value + 4]
    CMP   ECX,  32

    JE    @Exactly32
    JA    @Above32

{- Shift is below 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
    TEST  ECX,  ECX
    JZ    @FuncEnd

    SHRD  EAX,  EDX, CL
    JMP   @Shift

{- Shift is above 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Above32:
    AND   ECX,  $1F

    DEC   CL
    SHRD  EAX,  EDX, CL
    INC   CL

{- Main shifting  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Shift:
    SHR   EDX,  CL
    PUSH  ECX
    PUSH  EDX
    MOV   EDX,  dword ptr [Value]
    SHL   EDX,  2
    XOR   CL,   31
    SHL   EDX,  CL
    POP   ECX
    OR    EDX,  ECX
    POP   ECX
    JMP   @SetCarry

{- Shift is equal to 32, no shifting required, only swap Hi and Lo dwords - - -}
  @Exactly32:
    SHL   EAX,  1
    XCHG  EAX,  EDX

{- Write passed carry bit to the result - - - - - - - - - - - - - - - - - - - -}
  @SetCarry:
    DEC   ECX
    XOR   ECX,  31
    CMP   byte ptr [EBX], 0
    JE    @ResetBit

    BTS   EDX,  ECX
    JMP   @Swap

  @ResetBit:
    BTR   EDX,  ECX

{- Swap Hi and Lo dwords for shift > 32 - - - - - - - - - - - - - - - - - - - -}
  @Swap:
    CMP   byte ptr [TempShift],  32
    JBE   @GetCarry
    XCHG  EAX,  EDX

{- Get carry bit that will be output in CF parameter  - - - - - - - - - - - - -}
  @GetCarry:
    MOV   CL,   byte ptr [TempShift]
    AND   ECX,  $3F
    CMP   CL,   32
    JA    @FromHigh

    DEC   CL
    BT    dword ptr [Value],  ECX
    JMP   @StoreCarry

  @FromHigh:
    AND   CL,   $1F
    DEC   CL
    BT    dword ptr [Value + 4], ECX

  @StoreCarry:
    SETC  CL
    MOV   byte ptr [EBX], CL

{- Restore EBX register - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @FuncEnd:
    POP   EBX
end;
{$ENDIF}
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and $3F;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    Result := UInt64((Result shr 1) or ((UInt64(Carry) and 1) shl 63));
    Carry := CF;
  end;
end;
{$ENDIF}

//==============================================================================

Function RCR(Value: UInt8; Shift: UInt8; CF: ByteBool = False): UInt8;
begin
Result := RCRCarry(Value,Shift,{%H-}CF);
end;

//------------------------------------------------------------------------------

Function RCR(Value: UInt16; Shift: UInt8; CF: ByteBool = False): UInt16;
begin
Result := RCRCarry(Value,Shift,{%H-}CF);
end;

//------------------------------------------------------------------------------

Function RCR(Value: UInt32; Shift: UInt8; CF: ByteBool = False): UInt32;
begin
Result := RCRCarry(Value,Shift,{%H-}CF);
end;

//------------------------------------------------------------------------------

Function RCR(Value: UInt64; Shift: UInt8; CF: ByteBool = False): UInt64;
begin
Result := RCRCarry(Value,Shift,{%H-}CF);
end;

//==============================================================================

procedure RCRValueCarry(var Value: UInt8; Shift: UInt8; var CF: ByteBool);
begin
Value := RCRCarry(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCRValueCarry(var Value: UInt16; Shift: UInt8; var CF: ByteBool);
begin
Value := RCRCarry(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCRValueCarry(var Value: UInt32; Shift: UInt8; var CF: ByteBool);
begin
Value := RCRCarry(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCRValueCarry(var Value: UInt64; Shift: UInt8; var CF: ByteBool);
begin
Value := RCRCarry(Value,Shift,CF);
end;

//==============================================================================

procedure RCRValue(var Value: UInt8; Shift: UInt8; CF: ByteBool = False);
begin
Value := RCR(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCRValue(var Value: UInt16; Shift: UInt8; CF: ByteBool = False);
begin
Value := RCR(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCRValue(var Value: UInt32; Shift: UInt8; CF: ByteBool = False);
begin
Value := RCR(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCRValue(var Value: UInt64; Shift: UInt8; CF: ByteBool = False);
begin
Value := RCR(Value,Shift,CF);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Arithmetic left shift (SAL)                          }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SAL(Value: UInt8; Shift: UInt8): UInt8;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   AL,   Value
{$ENDIF}
    MOV   CL,   Shift
    SAL   AL,   CL
end;
{$ELSE}
begin
Result := UInt8(Value shl Shift);
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SAL(Value: UInt16; Shift: UInt8): UInt16;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   AX,   Value
{$ENDIF}
    MOV   CL,   Shift
    SAL   AX,   CL
end;
{$ELSE}
begin
Result := UInt16(Value shl Shift);
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SAL(Value: UInt32; Shift: UInt8): UInt32;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   EAX,  Value
{$ENDIF}
    MOV   CL,   Shift
    SAL   EAX,  CL
end;
{$ELSE}
begin
Result := UInt32(Value shl Shift);
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SAL(Value: UInt64; Shift: UInt8): UInt64;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   RAX,  Value
    MOV   CL,   Shift
    SAL   RAX,  CL
{$ELSE}
    MOV   ECX,  EAX
    AND   ECX,  $3F

    CMP   ECX,  31
    JA    @Above31

{- Shift is below 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
    MOV   EAX,  dword ptr [Value]
    MOV   EDX,  dword ptr [Value + 4]

    TEST  ECX,  ECX
    JZ    @FuncEnd

    SHLD  EDX,  EAX,  CL
    SHL   EAX,  CL
    JMP   @FuncEnd

{- Shift is above 31  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Above31:
    XOR   EAX,  EAX
    MOV   EDX,  dword ptr [Value]
    AND   ECX,  $1F
    SHL   EDX,  CL

{- End of the function  - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @FuncEnd:
{$ENDIF}
end;
{$ELSE}
begin
Result := UInt64(Value shl Shift);
end;
{$ENDIF}

//==============================================================================

procedure SALValue(var Value: UInt8; Shift: UInt8);
begin
Value := SAL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure SALValue(var Value: UInt16; Shift: UInt8);
begin
Value := SAL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure SALValue(var Value: UInt32; Shift: UInt8);
begin
Value := SAL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure SALValue(var Value: UInt64; Shift: UInt8);
begin
Value := SAL(Value,Shift);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Arithmetic right shift (SAR)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SAR(Value: UInt8; Shift: UInt8): UInt8;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   AL,   Value
{$ENDIF}
    MOV   CL,   Shift
    SAR   AL,   CL
end;
{$ELSE}
begin
Shift := Shift and $07;
If (Value shr 7) <> 0 then
  Result := UInt8((Value shr Shift) or (UInt8($FF) shl (8 - Shift)))
else
  Result := UInt8(Value shr Shift);
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SAR(Value: UInt16; Shift: UInt8): UInt16;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   AX,   Value
{$ENDIF}
    MOV   CL,   Shift
    SAR   AX,   CL
end;
{$ELSE}
begin
Shift := Shift and $0F;
If (Value shr 15) <> 0 then
  Result := UInt16((Value shr Shift) or (UInt16($FFFF) shl (16 - Shift)))
else
  Result := UInt16(Value shr Shift);
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SAR(Value: UInt32; Shift: UInt8): UInt32;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   EAX,  Value
{$ENDIF}
    MOV   CL,   Shift
    SAR   EAX,  CL
end;
{$ELSE}
begin
Shift := Shift and $1F;
If (Value shr 31) <> 0 then
  Result := UInt32((Value shr Shift) or (UInt32($FFFFFFFF) shl (32 - Shift)))
else
  Result := UInt32(Value shr Shift);
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SAR(Value: UInt64; Shift: UInt8): UInt64;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   RAX,  Value
    MOV   CL,   Shift
    SAR   RAX,  CL
{$ELSE}
    MOV   ECX,  EAX
    AND   ECX,  $3F

    CMP   ECX,  31
    JA    @Above31

{- Shift is below 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
    MOV   EAX,  dword ptr [Value]
    MOV   EDX,  dword ptr [Value + 4]

    TEST  ECX,  ECX
    JZ    @FuncEnd

    SHRD  EAX,  EDX,  CL
    SAR   EDX,  CL
    JMP   @FuncEnd

{- Shift is above 31  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Above31:
    MOV   EAX,  dword ptr [Value + 4]
    BT    EAX,  31
    JC    @BitSet

    XOR   EDX,  EDX
    JMP   @DoShift

  @BitSet:
    MOV   EDX,  $FFFFFFFF

  @DoShift:
    AND   ECX,  $1F
    SAR   EAX,  CL

{- End of the function  - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @FuncEnd:
{$ENDIF}
end;
{$ELSE}
begin
Shift := Shift and $3F;
If (Value shr 63) <> 0 then
  Result := UInt64((Value shr Shift) or (UInt64($FFFFFFFFFFFFFFFF) shl (64 - Shift)))
else
  Result := UInt64(Value shr Shift);
end;
{$ENDIF}

//==============================================================================

procedure SARValue(var Value: UInt8; Shift: UInt8);
begin
Value := SAL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure SARValue(var Value: UInt16; Shift: UInt8);
begin
Value := SAR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure SARValue(var Value: UInt32; Shift: UInt8);
begin
Value := SAR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure SARValue(var Value: UInt64; Shift: UInt8);
begin
Value := SAR(Value,Shift);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                Endianity swap                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function EndianSwap(Value: UInt16): UInt16;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   AX,   Value
{$ENDIF}
    XCHG  AL,   AH
end;
{$ELSE}
begin
Result := UInt16((Value shl 8) or (Value shr 8));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function EndianSwap(Value: UInt32): UInt32;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   EAX,  Value
{$ENDIF}
    BSWAP EAX
end;
{$ELSE}
begin
Result := UInt32(((Value and $000000FF) shl 24) or ((Value and $0000FF00) shl 8) or
                 ((Value and $00FF0000) shr 8) or ((Value and $FF000000) shr 24));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function EndianSwap(Value: UInt64): UInt64;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    MOV   RAX,  Value
    BSWAP RAX
{$ELSE}
    MOV   EAX,  dword ptr [Value + 4]
    MOV   EDX,  dword ptr [Value]
    BSWAP EAX
    BSWAP EDX
{$ENDIF}
end;
{$ELSE}
begin
Int64Rec(Result).Hi := EndianSwap(Int64Rec(Value).Lo);
Int64Rec(Result).Lo := EndianSwap(Int64Rec(Value).Hi);
end;
{$ENDIF}

//==============================================================================

procedure EndianSwapValue(var Value: UInt16);
begin
Value := EndianSwap(Value);
end;

//------------------------------------------------------------------------------

procedure EndianSwapValue(var Value: UInt32);
begin
Value := EndianSwap(Value);
end;

//------------------------------------------------------------------------------

procedure EndianSwapValue(var Value: UInt64);
begin
Value := EndianSwap(Value);
end;

//==============================================================================

procedure EndianSwap(var Buffer; Size: TMemSize);{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    XCHG  RCX,  RDX
  {$ELSE}
    MOV   RDX,  RDI
    MOV   RCX,  RSI
  {$ENDIF}

    CMP   RCX,  1
    JBE   @RoutineEnd

    LEA   RAX,  [RDX + RCX - 1]
    SHR   RCX,  1

  @LoopStart:
    MOV   R8B,  byte ptr [RDX]
    MOV   R9B,  byte ptr [RAX]
    MOV   byte ptr [RAX], R8B
    MOV   byte ptr [RDX], R9B
    INC   RDX
    DEC   RAX

    DEC   RCX
    JNZ   @LoopStart

  @RoutineEnd:
{$ELSE}
    MOV   ECX,  EDX
    CMP   ECX,  1
    JBE   @RoutineEnd

    PUSH  ESI
    PUSH  EDI

    MOV   ESI,  EAX
    LEA   EDI,  [EAX + ECX - 1]
    SHR   ECX,  1

  @LoopStart:
    MOV   AL,   byte ptr [ESI]
    MOV   DL,   byte ptr [EDI]
    MOV   byte ptr [EDI], AL
    MOV   byte ptr [ESI], DL
    INC   ESI
    DEC   EDI

    DEC   ECX
    JNZ   @LoopStart

    POP   EDI
    POP   ESI

  @RoutineEnd:
{$ENDIF}
end;
{$ELSE}
var
  i:        TMemSize;
  ByteBuff: Byte;
begin
case Size of
  Low(Size)..1: Exit;
             2: EndianSwapValue(UInt16(Buffer));
             4: EndianSwapValue(UInt32(Buffer));
             8: EndianSwapValue(UInt64(Buffer));
else
  For i := 0 to Pred(Size div 2) do
    begin
      {%H-}ByteBuff := {%H-}PByte({%H-}PtrUInt(Addr(Buffer)) + i)^;
      {%H-}PByte({%H-}PtrUInt(Addr(Buffer)) + i)^ := {%H-}PByte({%H-}PtrUInt(Addr(Buffer)) + (Size - i - 1))^;
      {%H-}PByte({%H-}PtrUInt(Addr(Buffer)) + (Size - i - 1))^ := ByteBuff;
    end;
end;
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                                 Bit test (BT)                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BT(Value: UInt8; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   DX,   $7
    BT    CX,   DX
  {$ELSE}
    AND   SI,   $7
    BT    DI,   SI
  {$ENDIF}
{$ELSE}
    AND   DX,   $7
    BT    AX,   DX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BT(Value: UInt16; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    BT    CX,   DX
  {$ELSE}
    BT    DI,   SI
  {$ENDIF}
{$ELSE}
    BT    AX,   DX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BT(Value: UInt32; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    BT    ECX,  EDX
  {$ELSE}
    BT    EDI,  ESI
  {$ENDIF}
{$ELSE}
    BT    EAX,  EDX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BT(Value: UInt64; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    BT    RCX,  RDX
  {$ELSE}
    BT    RDI,  RSI
  {$ENDIF}
{$ELSE}
    CMP   EAX,  32
    JAE   @TestHigh

    BT    dword ptr [Value],  EAX
    JMP   @SetResult

  @TestHigh:
    AND   EAX,  $1F
    BT    dword ptr [Value + 4],  EAX
{$ENDIF}
  @SetResult:
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit test and set (BTS)                            }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTS(var Value: UInt8; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND     DX,   $7
    MOVZX   RAX,  byte ptr [RCX]
    BTS     AX,   DX
    MOV     byte ptr [RCX], AL
  {$ELSE}
    AND     SI,   $7
    MOVZX   RAX,  byte ptr [RDI]
    BTS     AX,   SI
    MOV     byte ptr [RDI], AL
  {$ENDIF}
{$ELSE}
    AND     DX,   $7
    MOVZX   ECX,  byte ptr [EAX]
    BTS     CX,   DX
    MOV     byte ptr [EAX], CL
{$ENDIF}
    SETC    AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt8(Value or (UInt8(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTS(var Value: UInt16; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   DX, $FF
    BTS   word ptr [RCX], DX
  {$ELSE}
    AND   SI, $FF
    BTS   word ptr [RDI], SI
  {$ENDIF}
{$ELSE}
    AND   DX, $FF
    BTS   word ptr [EAX], DX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt16(Value or (UInt16(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTS(var Value: UInt32; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   EDX,  $FF
    BTS   dword ptr [RCX],  EDX
  {$ELSE}
    AND   ESI,  $FF
    BTS   dword ptr [RDI],  ESI
  {$ENDIF}
{$ELSE}
    AND   EDX,  $FF
    BTS   dword ptr [EAX],  EDX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt32(Value or (UInt32(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTS(var Value: UInt64; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   RDX,  $FF
    BTS   dword ptr [RCX],  RDX
  {$ELSE}
    AND   RSI,  $FF
    BTS   dword ptr [RDI],  RSI
  {$ENDIF}
{$ELSE}
    CMP   EDX,  32
    JAE   @TestHigh

    BTS   dword ptr [Value],  EDX
    JMP   @SetResult

  @TestHigh:
    AND   EDX,  $1F
    BTS   dword ptr [EAX + 4],  EDX
{$ENDIF}
  @SetResult:
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt64(Value or (UInt64(1) shl Bit));
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                           Bit test and reset (BTR)                           }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTR(var Value: UInt8; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND     DX,   $7
    MOVZX   RAX,  byte ptr [RCX]
    BTR     AX,   DX
    MOV     byte ptr [RCX], AL
  {$ELSE}
    AND     SI,   $7
    MOVZX   RAX,  byte ptr [RDI]
    BTR     AX,   SI
    MOV     byte ptr [RDI], AL
  {$ENDIF}
{$ELSE}
    AND     DX,   $7
    MOVZX   ECX,  byte ptr [EAX]
    BTR     CX,   DX
    MOV     byte ptr [EAX], CL
{$ENDIF}
    SETC    AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt8(Value and not(UInt8(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTR(var Value: UInt16; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   DX, $FF
    BTR   word ptr [RCX], DX
  {$ELSE}
    AND   SI, $FF
    BTR   word ptr [RDI], SI
  {$ENDIF}
{$ELSE}
    AND   DX, $FF
    BTR   word ptr [EAX], DX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt16(Value and not(UInt16(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTR(var Value: UInt32; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   EDX,  $FF
    BTR   dword ptr [RCX],  EDX
  {$ELSE}
    AND   ESI,  $FF
    BTR   dword ptr [RDI],  ESI
  {$ENDIF}
{$ELSE}
    AND   EDX,  $FF
    BTR   dword ptr [EAX],  EDX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt32(Value and not(UInt32(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTR(var Value: UInt64; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   RDX,  $FF
    BTR   dword ptr [RCX],  RDX
  {$ELSE}
    AND   RSI,  $FF
    BTR   dword ptr [RDI],  RSI
  {$ENDIF}
{$ELSE}
    CMP   EDX,  32
    JAE   @TestHigh

    BTR   dword ptr [Value],  EDX
    JMP   @SetResult

  @TestHigh:
    AND   EDX,  $1F
    BTR   dword ptr [EAX + 4],  EDX
{$ENDIF}
  @SetResult:
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt64(Value and not(UInt64(1) shl Bit));
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Bit test and complement (BTC)                        }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTC(var Value: UInt8; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND     DX,   $7
    MOVZX   RAX,  byte ptr [RCX]
    BTC     AX,   DX
    MOV     byte ptr [RCX], AL
  {$ELSE}
    AND     SI,   $7
    MOVZX   RAX,  byte ptr [RDI]
    BTC     AX,   SI
    MOV     byte ptr [RDI], AL
  {$ENDIF}
{$ELSE}
    AND     DX,   $7
    MOVZX   ECX,  byte ptr [EAX]
    BTC     CX,   DX
    MOV     byte ptr [EAX], CL
{$ENDIF}
    SETC    AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
If Result then Value := UInt8(Value and not(UInt8(1) shl Bit))
  else Value := UInt8(Value or (UInt8(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTC(var Value: UInt16; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   DX, $FF
    BTC   word ptr [RCX], DX
  {$ELSE}
    AND   SI, $FF
    BTC   word ptr [RDI], SI
  {$ENDIF}
{$ELSE}
    AND   DX, $FF
    BTC   word ptr [EAX], DX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
If Result then Value := UInt16(Value and not(UInt16(1) shl Bit))
  else Value := UInt16(Value or (UInt16(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTC(var Value: UInt32; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   EDX,  $FF
    BTC   dword ptr [RCX],  EDX
  {$ELSE}
    AND   ESI,  $FF
    BTC   dword ptr [RDI],  ESI
  {$ENDIF}
{$ELSE}
    AND   EDX,  $FF
    BTC   dword ptr [EAX],  EDX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
If Result then Value := UInt32(Value and not(UInt32(1) shl Bit))
  else Value := UInt32(Value or (UInt32(1) shl Bit));
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTC(var Value: UInt64; Bit: UInt8): ByteBool;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   RDX,  $FF
    BTC   dword ptr [RCX],  RDX
  {$ELSE}
    AND   RSI,  $FF
    BTC   dword ptr [RDI],  RSI
  {$ENDIF}
{$ELSE}
    CMP   EDX,  32
    JAE   @TestHigh

    BTC   dword ptr [Value],  EDX
    JMP   @SetResult

  @TestHigh:
    AND   EDX,  $1F
    BTC   dword ptr [EAX + 4],  EDX
{$ENDIF}
  @SetResult:
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr Bit) and 1) <> 0;
If Result then Value := UInt64(Value and not(UInt64(1) shl Bit))
  else Value := UInt64(Value or (UInt64(1) shl Bit));
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                      Bit test and set to a given value                       }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BitSetTo(var Value: UInt8; Bit: UInt8; NewValue: ByteBool): ByteBool;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;

//------------------------------------------------------------------------------

Function BitSetTo(var Value: UInt16; Bit: UInt8; NewValue: ByteBool): ByteBool;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;

//------------------------------------------------------------------------------

Function BitSetTo(var Value: UInt32; Bit: UInt8; NewValue: ByteBool): ByteBool;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;

//------------------------------------------------------------------------------

Function BitSetTo(var Value: UInt64; Bit: UInt8; NewValue: ByteBool): ByteBool;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit scan forward (BSF)                            }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BSF(Value: UInt8): Int32;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    XOR   RAX,  RAX
  {$IFDEF Windows}
    AND   RCX,  $FF
    BSF   AX,   CX
  {$ELSE}
    AND   RDI,  $FF
    BSF   AX,   DI
  {$ENDIF}
{$ELSE}
    AND   EAX,  $FF
    BSF   AX,   AX
{$ENDIF}
    JNZ   @RoutineEnd
    MOV   EAX,  -1
  @RoutineEnd:
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 0 to 7 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BSF(Value: UInt16): Int32;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    XOR   RAX,  RAX
  {$IFDEF Windows}
    BSF   AX,   CX
  {$ELSE}
    BSF   AX,   DI
  {$ENDIF}
{$ELSE}
    AND   EAX,  $FFFF
    BSF   AX,   AX
{$ENDIF}
    JNZ   @RoutineEnd
    MOV   EAX,  -1
  @RoutineEnd:
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 0 to 15 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BSF(Value: UInt32): Int32;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    BSF   EAX,  ECX
  {$ELSE}
    BSF   EAX,  EDI
  {$ENDIF}
{$ELSE}
    BSF   EAX,  EAX
{$ENDIF}
    JNZ   @RoutineEnd
    MOV   EAX,  -1
  @RoutineEnd:
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 0 to 31 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BSF(Value: UInt64): Int32;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    BSF   RAX,  RCX
  {$ELSE}
    BSF   RAX,  RDI
  {$ENDIF}

    JNZ   @RoutineEnd
    MOV   EAX,  -1

  @RoutineEnd:
{$ELSE}
    BSF   EAX,  dword ptr [Value]
    JNZ   @RoutineEnd

    BSF   EAX,  dword ptr [Value + 4]
    JNZ   @Add32

    MOV   EAX,  -33

  @Add32:
    ADD   EAX,  32

  @RoutineEnd:
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 0 to 63 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit scan reversed (BSR)                           }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BSR(Value: UInt8): Int32; {$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    XOR   RAX,  RAX
  {$IFDEF Windows}
    AND   RCX,  $FF
    BSR   AX,   CX
  {$ELSE}
    AND   RDI,  $FF
    BSR   AX,   DI
  {$ENDIF}
{$ELSE}
    AND   EAX,  $FF
    BSR   AX,   AX
{$ENDIF}
    JNZ   @RoutineEnd
    MOV   EAX,  -1
  @RoutineEnd:
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 7 downto 0 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BSR(Value: UInt16): Int32;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
    XOR   RAX,  RAX
  {$IFDEF Windows}
    BSR   AX,   CX
  {$ELSE}
    BSR   AX,   DI
  {$ENDIF}
{$ELSE}
    AND   EAX,  $FFFF
    BSR   AX,   AX
{$ENDIF}
    JNZ   @RoutineEnd
    MOV   EAX,  -1
  @RoutineEnd:
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 15 downto 0 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BSR(Value: UInt32): Int32;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    BSR   EAX,  ECX
  {$ELSE}
    BSR   EAX,  EDI
  {$ENDIF}
{$ELSE}
    BSR   EAX,  EAX
{$ENDIF}
    JNZ   @RoutineEnd
    MOV   EAX,  -1
  @RoutineEnd:
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 31 downto 0 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BSR(Value: UInt64): Int32;{$IFNDEF PurePascal}register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    BSR   RAX,  RCX
  {$ELSE}
    BSR   RAX,  RDI
  {$ENDIF}
{$ELSE}
    BSR   EAX,  dword ptr [Value + 4]
    JZ    @ScanLow

    ADD   EAX,  32
    JMP   @RoutineEnd

  @ScanLow:
    BSR   EAX,  dword ptr [Value]
{$ENDIF}

    JNZ   @RoutineEnd
    MOV   EAX,  -1

  @RoutineEnd:
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 63 downto 0 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                               Population count                               }
{==============================================================================}
{------------------------------------------------------------------------------}

{$IFDEF UseLookupTable}
const
  PopCountTable: array[UInt8] of UInt8 = (
    0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8);
{$ENDIF}

Function Fce_PopCount_8_Pas(Value: UInt8): Int32;
{$IFDEF UseLookupTable}
begin
Result := PopCountTable[Value];
end;
{$ELSE}
var
  i:  Integer;
begin
Result := 0;
For i := 1 to 8 do
  begin
    If (Value and 1) <> 0 then Inc(Result);
    Value := UInt8(Value shr 1);
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function Fce_PopCount_16_Pas(Value: UInt16): Int32;
{$IFDEF UseLookupTable}
begin
Result := PopCountTable[UInt8(Value)] + PopCountTable[UInt8(Value shr 8)];
end;
{$ELSE}
var
  i:  Integer;
begin
Result := 0;
For i := 1 to 16 do
  begin
    If (Value and 1) <> 0 then Inc(Result);
    Value := UInt16(Value shr 1);
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function Fce_PopCount_32_Pas(Value: UInt32): Int32;
{$IFDEF UseLookupTable}
begin
Result := PopCountTable[UInt8(Value)] + PopCountTable[UInt8(Value shr 8)] +
  PopCountTable[UInt8(Value shr 16)] + PopCountTable[UInt8(Value shr 24)];
end;
{$ELSE}
var
  i:  Integer;
begin
Result := 0;
For i := 1 to 32 do
  begin
    If (Value and 1) <> 0 then Inc(Result);
    Value := UInt32(Value shr 1);
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function Fce_PopCount_64_Pas(Value: UInt64): Int32;
{$IFDEF UseLookupTable}
begin
{$IFDEF 64bit}
Result := PopCountTable[UInt8(Value)] + PopCountTable[UInt8(Value shr 8)] +
  PopCountTable[UInt8(Value shr 16)] + PopCountTable[UInt8(Value shr 24)] +
  PopCountTable[UInt8(Value shr 32)] + PopCountTable[UInt8(Value shr 40)] +
  PopCountTable[UInt8(Value shr 48)] + PopCountTable[UInt8(Value shr 56)];
{$ELSE}
Result := PopCount(Int64Rec(Value).Lo) + PopCount(Int64Rec(Value).Hi);
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := 0;
For i := 1 to 64 do
  begin
    If (Value and 1) <> 0 then Inc(Result);
    Value := UInt64(Value shr 1);
  end;
end;
{$ENDIF}

//==============================================================================

{$IFNDEF PurePascal}

Function Fce_PopCount_8_Asm(Value: UInt8): Int32; register; assembler;
asm
{$IFDEF x64}
    MOVZX   RAX,  Value
{$ELSE}
    AND     EAX,  $FF
{$ENDIF}
{$IFDEF ASM_MachineCode}
    DB  $F3, $66, $0F, $B8, $C0   // POPCNT  AX, AX
{$ELSE}
    POPCNT  AX,   AX
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function Fce_PopCount_16_Asm(Value: UInt16): Int32; register; assembler;
asm
{$IFDEF x64}
    MOVZX   RAX,  Value
{$ELSE}
    AND     EAX,  $FFFF
{$ENDIF}
{$IFDEF ASM_MachineCode}
    DB  $F3, $66, $0F, $B8, $C0   // POPCNT  AX, AX
{$ELSE}
    POPCNT  AX,   AX
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function Fce_PopCount_32_Asm(Value: UInt32): Int32; register; assembler;
asm
{$IFDEF x64}
    MOV     EAX,  Value
{$ENDIF}
{$IFDEF ASM_MachineCode}
    DB  $F3, $0F, $B8, $C0  // POPCNT  EAX, EAX
{$ELSE}
    POPCNT  EAX,  EAX
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function Fce_PopCount_64_Asm(Value: UInt64): Int32; register; assembler;
asm
{$IFDEF x64}
    MOV     RAX,  Value
  {$IFDEF ASM_MachineCode}
    DB  $F3, $48, $0F, $B8, $C0   // POPCNT  RAX, RAX
  {$ELSE}
    POPCNT  RAX,  RAX
  {$ENDIF}
{$ELSE}
    MOV     EAX,  dword ptr [Value]
    MOV     EDX,  dword ptr [Value + 4]
  {$IFDEF ASM_MachineCode}
    DB  $F3, $0F, $B8, $C0        // POPCNT  EAX, EAX
    DB  $F3, $0F, $B8, $D2        // POPCNT  EDX, EDX
  {$ELSE}
    POPCNT  EAX,  EAX
    POPCNT  EDX,  EDX
  {$ENDIF}
    ADD     EAX,  EDX
{$ENDIF}
end;

{$ENDIF}

//==============================================================================

var
  Var_PopCount_8: Function(Value: UInt8): Int32;
  Var_PopCount_16: Function(Value: UInt16): Int32;
  Var_PopCount_32: Function(Value: UInt32): Int32;
  Var_PopCount_64: Function(Value: UInt64): Int32;

Function PopCount(Value: UInt8): Int32;
begin
Result := Var_PopCount_8(Value);
end;

//------------------------------------------------------------------------------

Function PopCount(Value: UInt16): Int32;
begin
Result := Var_PopCount_16(Value);
end;

//------------------------------------------------------------------------------

Function PopCount(Value: UInt32): Int32;
begin
Result := Var_PopCount_32(Value);
end;

//------------------------------------------------------------------------------

Function PopCount(Value: UInt64): Int32;
begin
Result := Var_PopCount_64(Value);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                              Nibble manipulation                             }
{==============================================================================}
{------------------------------------------------------------------------------}

Function GetHighNibble(Value: UInt8): TNibble;
begin
Result := (Value shr 4) and $0F;
end;

//------------------------------------------------------------------------------

Function GetLowNibble(Value: UInt8): TNibble;
begin
Result := Value and $0F;
end;

//------------------------------------------------------------------------------

Function SetHighNibble(Value: UInt8; SetTo: TNibble): UInt8;
begin
Result := (Value and $0F) or UInt8((SetTo and $0F) shl 4);
end;

//------------------------------------------------------------------------------

Function SetLowNibble(Value: UInt8; SetTo: TNibble): UInt8;
begin
Result := (Value and $F0) or UInt8(SetTo and $0F);
end;

//------------------------------------------------------------------------------

procedure SetHighNibbleValue(var Value: UInt8; SetTo: TNibble);
begin
Value := SetHighNibble(Value,SetTo);
end;

//------------------------------------------------------------------------------

procedure SetLowNibbleValue(var Value: UInt8; SetTo: TNibble);
begin
Value := SetLowNibble(Value,SetTo);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                Get flag state                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function GetFlagState(Value,FlagBitmask: UInt8; ExactMatch: Boolean = False): Boolean;
begin
If ExactMatch then
  Result := (Value and FlagBitmask) = FlagBitmask
else
  Result := (Value and FlagBitmask) <> 0;
end;

//------------------------------------------------------------------------------

Function GetFlagState(Value,FlagBitmask: UInt16; ExactMatch: Boolean = False): Boolean;
begin
If ExactMatch then
  Result := (Value and FlagBitmask) = FlagBitmask
else
  Result := (Value and FlagBitmask) <> 0;
end;

//------------------------------------------------------------------------------

Function GetFlagState(Value,FlagBitmask: UInt32; ExactMatch: Boolean = False): Boolean;
begin
If ExactMatch then
  Result := (Value and FlagBitmask) = FlagBitmask
else
  Result := (Value and FlagBitmask) <> 0;
end;

//------------------------------------------------------------------------------

Function GetFlagState(Value,FlagBitmask: UInt64; ExactMatch: Boolean = False): Boolean;
begin
If ExactMatch then
  Result := (Value and FlagBitmask) = FlagBitmask
else
  Result := (Value and FlagBitmask) <> 0;
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                   Set flag                                   }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SetFlag(Value,FlagBitmask: UInt8): UInt8;
begin
Result := Value or FlagBitmask;
end;

//------------------------------------------------------------------------------

Function SetFlag(Value,FlagBitmask: UInt16): UInt16;
begin
Result := Value or FlagBitmask;
end;

//------------------------------------------------------------------------------

Function SetFlag(Value,FlagBitmask: UInt32): UInt32;
begin
Result := Value or FlagBitmask;
end;

//------------------------------------------------------------------------------

Function SetFlag(Value,FlagBitmask: UInt64): UInt64;
begin
Result := Value or FlagBitmask;
end;

//==============================================================================

procedure SetFlagValue(var Value: UInt8; FlagBitmask: UInt8);
begin
Value := SetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

procedure SetFlagValue(var Value: UInt16; FlagBitmask: UInt16);
begin
Value := SetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

procedure SetFlagValue(var Value: UInt32; FlagBitmask: UInt32);
begin
Value := SetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

procedure SetFlagValue(var Value: UInt64; FlagBitmask: UInt64);
begin
Value := SetFlag(Value,FlagBitmask);
end;

//==============================================================================

Function SetFlags_8(Value: UInt8; Flags: array of UInt8): UInt8;
var
  TempBitmask:  UInt8;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := SetFlag(Value,TempBitmask);
end;

//------------------------------------------------------------------------------

Function SetFlags_16(Value: UInt16; Flags: array of UInt16): UInt16;
var
  TempBitmask:  UInt16;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := SetFlag(Value,TempBitmask);
end;

//------------------------------------------------------------------------------

Function SetFlags_32(Value: UInt32; Flags: array of UInt32): UInt32;
var
  TempBitmask:  UInt32;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := SetFlag(Value,TempBitmask);
end;

//------------------------------------------------------------------------------

Function SetFlags_64(Value: UInt64; Flags: array of UInt64): UInt64;
var
  TempBitmask:  UInt64;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := SetFlag(Value,TempBitmask);
end;

//==============================================================================

Function SetFlags(Value: UInt8; Flags: array of UInt8): UInt8;
begin
Result := SetFlags_8(Value,Flags);
end;

//------------------------------------------------------------------------------

Function SetFlags(Value: UInt16; Flags: array of UInt16): UInt16;
begin
Result := SetFlags_16(Value,Flags);
end;

//------------------------------------------------------------------------------

Function SetFlags(Value: UInt32; Flags: array of UInt32): UInt32;
begin
Result := SetFlags_32(Value,Flags);
end;

//------------------------------------------------------------------------------

Function SetFlags(Value: UInt64; Flags: array of UInt64): UInt64;
begin
Result := SetFlags_64(Value,Flags);
end;

//==============================================================================

procedure SetFlagsValue_8(var Value: UInt8; Flags: array of UInt8);
begin
Value := SetFlags_8(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure SetFlagsValue_16(var Value: UInt16; Flags: array of UInt16);
begin
Value := SetFlags_16(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure SetFlagsValue_32(var Value: UInt32; Flags: array of UInt32);
begin
Value := SetFlags_32(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure SetFlagsValue_64(var Value: UInt64; Flags: array of UInt64);
begin
Value := SetFlags_64(Value,Flags);
end;

//==============================================================================

procedure SetFlagsValue(var Value: UInt8; Flags: array of UInt8);
begin
SetFlagsValue_8(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure SetFlagsValue(var Value: UInt16; Flags: array of UInt16);
begin
SetFlagsValue_16(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure SetFlagsValue(var Value: UInt32; Flags: array of UInt32);
begin
SetFlagsValue_32(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure SetFlagsValue(var Value: UInt64; Flags: array of UInt64);
begin
SetFlagsValue_64(Value,Flags);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                  Reset flag                                  }
{==============================================================================}
{------------------------------------------------------------------------------}

Function ResetFlag(Value,FlagBitmask: UInt8): UInt8;
begin
Result := Value and not FlagBitmask;
end;

//------------------------------------------------------------------------------

Function ResetFlag(Value,FlagBitmask: UInt16): UInt16;
begin
Result := Value and not FlagBitmask;
end;

//------------------------------------------------------------------------------

Function ResetFlag(Value,FlagBitmask: UInt32): UInt32;
begin
Result := Value and not FlagBitmask;
end;

//------------------------------------------------------------------------------

Function ResetFlag(Value,FlagBitmask: UInt64): UInt64;
begin
Result := Value and not FlagBitmask;
end;

//==============================================================================

procedure ResetFlagValue(var Value: UInt8; FlagBitmask: UInt8);
begin
Value := ResetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

procedure ResetFlagValue(var Value: UInt16; FlagBitmask: UInt16);
begin
Value := ResetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

procedure ResetFlagValue(var Value: UInt32; FlagBitmask: UInt32);
begin
Value := ResetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

procedure ResetFlagValue(var Value: UInt64; FlagBitmask: UInt64);
begin
Value := ResetFlag(Value,FlagBitmask);
end;

//==============================================================================

Function ResetFlags_8(Value: UInt8; Flags: array of UInt8): UInt8;
var
  TempBitmask:  UInt8;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := ResetFlag(Value,TempBitmask);
end;

//------------------------------------------------------------------------------

Function ResetFlags_16(Value: UInt16; Flags: array of UInt16): UInt16;
var
  TempBitmask:  UInt16;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := ResetFlag(Value,TempBitmask);
end;

//------------------------------------------------------------------------------

Function ResetFlags_32(Value: UInt32; Flags: array of UInt32): UInt32;
var
  TempBitmask:  UInt32;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := ResetFlag(Value,TempBitmask);
end;

//------------------------------------------------------------------------------

Function ResetFlags_64(Value: UInt64; Flags: array of UInt64): UInt64;
var
  TempBitmask:  UInt64;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := ResetFlag(Value,TempBitmask);
end;

//==============================================================================

Function ResetFlags(Value: UInt8; Flags: array of UInt8): UInt8;
begin
Result := ResetFlags_8(Value,Flags);
end;

//------------------------------------------------------------------------------

Function ResetFlags(Value: UInt16; Flags: array of UInt16): UInt16;
begin
Result := ResetFlags_16(Value,Flags);
end;
//------------------------------------------------------------------------------

Function ResetFlags(Value: UInt32; Flags: array of UInt32): UInt32;
begin
Result := ResetFlags_32(Value,Flags);
end;

//------------------------------------------------------------------------------

Function ResetFlags(Value: UInt64; Flags: array of UInt64): UInt64;
begin
Result := ResetFlags_64(Value,Flags);
end;

//==============================================================================

procedure ResetFlagsValue_8(var Value: UInt8; Flags: array of UInt8);
begin
Value := ResetFlags_8(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure ResetFlagsValue_16(var Value: UInt16; Flags: array of UInt16);
begin
Value := ResetFlags_16(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure ResetFlagsValue_32(var Value: UInt32; Flags: array of UInt32);
begin
Value := ResetFlags_32(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure ResetFlagsValue_64(var Value: UInt64; Flags: array of UInt64);
begin
Value := ResetFlags_64(Value,Flags);
end;

//==============================================================================

procedure ResetFlagsValue(var Value: UInt8; Flags: array of UInt8);
begin
ResetFlagsValue_8(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure ResetFlagsValue(var Value: UInt16; Flags: array of UInt16);
begin
ResetFlagsValue_16(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure ResetFlagsValue(var Value: UInt32; Flags: array of UInt32);
begin
ResetFlagsValue_32(Value,Flags);
end;

//------------------------------------------------------------------------------

procedure ResetFlagsValue(var Value: UInt64; Flags: array of UInt64);
begin
ResetFlagsValue_64(Value,Flags);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                Set flag state                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SetFlagState(Value,FlagBitmask: UInt8; NewState: Boolean): UInt8;
begin
If NewState then
  Result := SetFlag(Value,FlagBitmask)
else
  Result := ResetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

Function SetFlagState(Value,FlagBitmask: UInt16; NewState: Boolean): UInt16;
begin
If NewState then
  Result := SetFlag(Value,FlagBitmask)
else
  Result := ResetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

Function SetFlagState(Value,FlagBitmask: UInt32; NewState: Boolean): UInt32;
begin
If NewState then
  Result := SetFlag(Value,FlagBitmask)
else
  Result := ResetFlag(Value,FlagBitmask);
end;

//------------------------------------------------------------------------------

Function SetFlagState(Value,FlagBitmask: UInt64; NewState: Boolean): UInt64;
begin
If NewState then
  Result := SetFlag(Value,FlagBitmask)
else
  Result := ResetFlag(Value,FlagBitmask);
end;

//==============================================================================

procedure SetFlagStateValue(var Value: UInt8; FlagBitmask: UInt8; NewState: Boolean);
begin
Value := SetFlagState(Value,FlagBitmask,NewState);
end;

//------------------------------------------------------------------------------

procedure SetFlagStateValue(var Value: UInt16; FlagBitmask: UInt16; NewState: Boolean);
begin
Value := SetFlagState(Value,FlagBitmask,NewState);
end;

//------------------------------------------------------------------------------

procedure SetFlagStateValue(var Value: UInt32; FlagBitmask: UInt32; NewState: Boolean);
begin
Value := SetFlagState(Value,FlagBitmask,NewState);
end;

//------------------------------------------------------------------------------

procedure SetFlagStateValue(var Value: UInt64; FlagBitmask: UInt64; NewState: Boolean);
begin
Value := SetFlagState(Value,FlagBitmask,NewState);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                   Get bits                                   }
{==============================================================================}
{------------------------------------------------------------------------------}

Function GetBits(Value: UInt8; FromBit,ToBit: Int32; ShiftDown: Boolean = True): UInt8;
begin
Result := Value and UInt8(($FF shl (FromBit and 7)) and ($FF shr (7 - (ToBit and 7))));
If ShiftDown then
  Result := Result shr (FromBit and 7);
end;

//------------------------------------------------------------------------------

Function GetBits(Value: UInt16; FromBit,ToBit: Int32; ShiftDown: Boolean = True): UInt16;
begin
Result := Value and UInt16(($FFFF shl (FromBit and 15)) and ($FFFF shr (15 - (ToBit and 15))));
If ShiftDown then
  Result := Result shr (FromBit and 15);
end;

//------------------------------------------------------------------------------

Function GetBits(Value: UInt32; FromBit,ToBit: Int32; ShiftDown: Boolean = True): UInt32;
begin
Result := Value and UInt32(($FFFFFFFF shl (FromBit and 31)) and ($FFFFFFFF shr (31 - (ToBit and 31))));
If ShiftDown then
  Result := Result shr (FromBit and 31);
end;

//------------------------------------------------------------------------------

Function GetBits(Value: UInt64; FromBit,ToBit: Int32; ShiftDown: Boolean = True): UInt64;
begin
Result := Value and UInt64((UInt64($FFFFFFFFFFFFFFFF) shl (FromBit and 63)) and (UInt64($FFFFFFFFFFFFFFFF) shr (63 - (ToBit and 63))));
If ShiftDown then
  Result := Result shr (FromBit and 63);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                   Set bits                                   }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SetBits(Value,NewBits: UInt8; FromBit,ToBit: Int32): UInt8;
var
  Mask: UInt8;
begin
Mask := UInt8(($FF shl (FromBit and 7)) and ($FF shr (7 - (ToBit and 7))));
Result := (Value and not Mask) or (NewBits and Mask);
end;

//------------------------------------------------------------------------------

Function SetBits(Value,NewBits: UInt16; FromBit,ToBit: Int32): UInt16;
var
  Mask: UInt16;
begin
Mask := UInt16(($FFFF shl (FromBit and 15)) and ($FFFF shr (15 - (ToBit and 15))));
Result := (Value and not Mask) or (NewBits and Mask);
end;

//------------------------------------------------------------------------------

Function SetBits(Value,NewBits: UInt32; FromBit,ToBit: Int32): UInt32;
var
  Mask: UInt32;
begin
Mask := UInt32(($FFFFFFFF shl (FromBit and 31)) and ($FFFFFFFF shr (31 - (ToBit and 31))));
Result := (Value and not Mask) or (NewBits and Mask);
end;

//------------------------------------------------------------------------------

Function SetBits(Value,NewBits: UInt64; FromBit,ToBit: Int32): UInt64;
var
  Mask: UInt64;
begin
Mask := UInt64((UInt64($FFFFFFFFFFFFFFFF) shl (FromBit and 63)) and (UInt64($FFFFFFFFFFFFFFFF) shr (63 - (ToBit and 63))));
Result := (Value and not Mask) or (NewBits and Mask);
end;

//==============================================================================

procedure SetBitsValue(var Value: UInt8; NewBits: UInt8; FromBit,ToBit: Int32);
begin
Value := SetBits(Value,NewBits,FromBit,ToBit);
end;

//------------------------------------------------------------------------------

procedure SetBitsValue(var Value: UInt16; NewBits: UInt16; FromBit,ToBit: Int32);
begin
Value := SetBits(Value,NewBits,FromBit,ToBit);
end;

//------------------------------------------------------------------------------

procedure SetBitsValue(var Value: UInt32; NewBits: UInt32; FromBit,ToBit: Int32);
begin
Value := SetBits(Value,NewBits,FromBit,ToBit);
end;

//------------------------------------------------------------------------------

procedure SetBitsValue(var Value: UInt64; NewBits: UInt64; FromBit,ToBit: Int32);
begin
Value := SetBits(Value,NewBits,FromBit,ToBit);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                 Reverse bits                                 }
{==============================================================================}
{------------------------------------------------------------------------------}

const
  RevBitsTable: array[UInt8] of UInt8 = (
    $00, $80, $40, $C0, $20, $A0, $60, $E0, $10, $90, $50, $D0, $30, $B0, $70, $F0,
    $08, $88, $48, $C8, $28, $A8, $68, $E8, $18, $98, $58, $D8, $38, $B8, $78, $F8,
    $04, $84, $44, $C4, $24, $A4, $64, $E4, $14, $94, $54, $D4, $34, $B4, $74, $F4,
    $0C, $8C, $4C, $CC, $2C, $AC, $6C, $EC, $1C, $9C, $5C, $DC, $3C, $BC, $7C, $FC,
    $02, $82, $42, $C2, $22, $A2, $62, $E2, $12, $92, $52, $D2, $32, $B2, $72, $F2,
    $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA, $1A, $9A, $5A, $DA, $3A, $BA, $7A, $FA,
    $06, $86, $46, $C6, $26, $A6, $66, $E6, $16, $96, $56, $D6, $36, $B6, $76, $F6,
    $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE, $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE,
    $01, $81, $41, $C1, $21, $A1, $61, $E1, $11, $91, $51, $D1, $31, $B1, $71, $F1,
    $09, $89, $49, $C9, $29, $A9, $69, $E9, $19, $99, $59, $D9, $39, $B9, $79, $F9,
    $05, $85, $45, $C5, $25, $A5, $65, $E5, $15, $95, $55, $D5, $35, $B5, $75, $F5,
    $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED, $1D, $9D, $5D, $DD, $3D, $BD, $7D, $FD,
    $03, $83, $43, $C3, $23, $A3, $63, $E3, $13, $93, $53, $D3, $33, $B3, $73, $F3,
    $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB, $1B, $9B, $5B, $DB, $3B, $BB, $7B, $FB,
    $07, $87, $47, $C7, $27, $A7, $67, $E7, $17, $97, $57, $D7, $37, $B7, $77, $F7,
    $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF, $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF);

Function ReverseBits(Value: UInt8): UInt8;
begin
Result := UInt8(RevBitsTable[UInt8(Value)]);
end;

//------------------------------------------------------------------------------

Function ReverseBits(Value: UInt16): UInt16;
begin
Result := UInt16((UInt16(RevBitsTable[UInt8(Value)]) shl 8) or
                  UInt16(RevBitsTable[UInt8(Value shr 8)]));
end;

//------------------------------------------------------------------------------

Function ReverseBits(Value: UInt32): UInt32;
begin
Result := UInt32((UInt32(RevBitsTable[UInt8(Value)]) shl 24) or
                 (UInt32(RevBitsTable[UInt8(Value shr 8)]) shl 16) or
                 (UInt32(RevBitsTable[UInt8(Value shr 16)]) shl 8) or
                  UInt32(RevBitsTable[UInt8(Value shr 24)]));
end;

//------------------------------------------------------------------------------

Function ReverseBits(Value: UInt64): UInt64;
begin
Int64Rec(Result).Hi := ReverseBits(Int64Rec(Value).Lo);
Int64Rec(Result).Lo := ReverseBits(Int64Rec(Value).Hi);
end;

//==============================================================================

procedure ReverseBitsValue(var Value: UInt8);
begin
Value := ReverseBits(Value);
end;

//------------------------------------------------------------------------------

procedure ReverseBitsValue(var Value: UInt16);
begin
Value := ReverseBits(Value);
end;

//------------------------------------------------------------------------------

procedure ReverseBitsValue(var Value: UInt32);
begin
Value := ReverseBits(Value);
end;

//------------------------------------------------------------------------------

procedure ReverseBitsValue(var Value: UInt64);
begin
Value := ReverseBits(Value);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                              Unit initialization                             }
{==============================================================================}
{------------------------------------------------------------------------------}

procedure LoadDefault;
begin
// PopCount
Var_PopCount_8 := Fce_PopCount_8_Pas;
Var_PopCount_16 := Fce_PopCount_16_Pas;
Var_PopCount_32 := Fce_PopCount_32_Pas;
Var_PopCount_64 := Fce_PopCount_64_Pas;
end;

//------------------------------------------------------------------------------

procedure Initialize;
begin
LoadDefault;
{$IF Defined(AllowASMExtensions) and not Defined(PurePascal)}
  with TSimpleCPUID.Create do
  try
    // PopCount
    If Info.SupportedExtensions.POPCNT then
      begin
        Var_PopCount_8 := Fce_PopCount_8_Asm;
        Var_PopCount_16 := Fce_PopCount_16_Asm;
        Var_PopCount_32 := Fce_PopCount_32_Asm;
        Var_PopCount_64 := Fce_PopCount_64_Asm;
      end;
  finally
    Free;
  end;
{$IFEND}
end;

initialization
  Initialize;

end.
