{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  BitOps - Binary operations

  ©František Milt 2018-02-22

  Version 1.0

===============================================================================}
unit BitOps;

{$If Defined(PurePascal) or not Defined(x64)}
  {$DEFINE no32ASM}
{$IFEND}

interface

type
{$IFDEF FPC}
  QuadWord = QWord;
{$ELSE}
  QuadWord = Int64;
{$ENDIF}

{$IFDEF x64}
  PtrUInt = UInt64;
{$ELSE}
  PtrUInt = LongWord;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                  Integer number <-> Bit string conversions                   }
{==============================================================================}
{------------------------------------------------------------------------------}

Function NumberToBits(Number: Byte): String; overload;
Function NumberToBits(Number: Word): String; overload;
Function NumberToBits(Number: LongWord): String; overload;
Function NumberToBits(Number: QuadWord): String; overload;

Function BitsToNumber(const BitString: String): QuadWord;

{------------------------------------------------------------------------------}
{==============================================================================}
{                               Rotate left (ROL)                              }
{==============================================================================}
{------------------------------------------------------------------------------}

Function ROL(Value: Byte; Shift: Integer): Byte; overload;
Function ROL(Value: Word; Shift: Integer): Word; overload;
Function ROL(Value: LongWord; Shift: Integer): LongWord; overload;
Function ROL(Value: QuadWord; Shift: Integer): QuadWord; overload;

procedure ROLValue(var Value: Byte; Shift: Integer); overload;
procedure ROLValue(var Value: Word; Shift: Integer); overload;
procedure ROLValue(var Value: LongWord; Shift: Integer); overload;
procedure ROLValue(var Value: QuadWord; Shift: Integer); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                              Rotate right (ROR)                              }
{==============================================================================}
{------------------------------------------------------------------------------}

Function ROR(Value: Byte; Shift: Integer): Byte; overload;
Function ROR(Value: Word; Shift: Integer): Word; overload;
Function ROR(Value: LongWord; Shift: Integer): LongWord; overload;
Function ROR(Value: QuadWord; Shift: Integer): QuadWord; overload;

procedure RORValue(var Value: Byte; Shift: Integer); overload;
procedure RORValue(var Value: Word; Shift: Integer); overload;
procedure RORValue(var Value: LongWord; Shift: Integer); overload;
procedure RORValue(var Value: QuadWord; Shift: Integer); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Rotate left with carry (RCL)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function RCLCarry(Value: Byte; Shift: Integer; var CF: Boolean): Byte; overload;
Function RCLCarry(Value: Word; Shift: Integer; var CF: Boolean): Word; overload;
Function RCLCarry(Value: LongWord; Shift: Integer; var CF: Boolean): LongWord; overload;
Function RCLCarry(Value: QuadWord; Shift: Integer; var CF: Boolean): QuadWord; overload;

Function RCL(Value: Byte; Shift: Integer; CF: Boolean = False): Byte; overload;
Function RCL(Value: Word; Shift: Integer; CF: Boolean = False): Word; overload;
Function RCL(Value: LongWord; Shift: Integer; CF: Boolean = False): LongWord; overload;
Function RCL(Value: QuadWord; Shift: Integer; CF: Boolean = False): QuadWord; overload;

procedure RCLValueCarry(var Value: Byte; Shift: Integer; var CF: Boolean); overload;
procedure RCLValueCarry(var Value: Word; Shift: Integer; var CF: Boolean); overload;
procedure RCLValueCarry(var Value: LongWord; Shift: Integer; var CF: Boolean); overload;
procedure RCLValueCarry(var Value: QuadWord; Shift: Integer; var CF: Boolean); overload;

procedure RCLValue(var Value: Byte; Shift: Integer; CF: Boolean = False); overload;
procedure RCLValue(var Value: Word; Shift: Integer; CF: Boolean = False); overload;
procedure RCLValue(var Value: LongWord; Shift: Integer; CF: Boolean = False); overload;
procedure RCLValue(var Value: QuadWord; Shift: Integer; CF: Boolean = False); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                        Rotate right with carry (RCR)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function RCRCarry(Value: Byte; Shift: Integer; var CF: Boolean): Byte; overload;
Function RCRCarry(Value: Word; Shift: Integer; var CF: Boolean): Word; overload;
Function RCRCarry(Value: LongWord; Shift: Integer; var CF: Boolean): LongWord; overload;
Function RCRCarry(Value: QuadWord; Shift: Integer; var CF: Boolean): QuadWord; overload;

Function RCR(Value: Byte; Shift: Integer; CF: Boolean = False): Byte; overload;
Function RCR(Value: Word; Shift: Integer; CF: Boolean = False): Word; overload;
Function RCR(Value: LongWord; Shift: Integer; CF: Boolean = False): LongWord; overload;
Function RCR(Value: QuadWord; Shift: Integer; CF: Boolean = False): QuadWord; overload;

procedure RCRValueCarry(var Value: Byte; Shift: Integer; var CF: Boolean); overload;
procedure RCRValueCarry(var Value: Word; Shift: Integer; var CF: Boolean); overload;
procedure RCRValueCarry(var Value: LongWord; Shift: Integer; var CF: Boolean); overload;
procedure RCRValueCarry(var Value: QuadWord; Shift: Integer; var CF: Boolean); overload;

procedure RCRValue(var Value: Byte; Shift: Integer; CF: Boolean = False); overload;
procedure RCRValue(var Value: Word; Shift: Integer; CF: Boolean = False); overload;
procedure RCRValue(var Value: LongWord; Shift: Integer; CF: Boolean = False); overload;
procedure RCRValue(var Value: QuadWord; Shift: Integer; CF: Boolean = False); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Arithmetic left shift (SAL)                          }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SAL(Value: Byte; Shift: Integer): Byte; overload;
Function SAL(Value: Word; Shift: Integer): Word; overload;
Function SAL(Value: LongWord; Shift: Integer): LongWord; overload;
Function SAL(Value: QuadWord; Shift: Integer): QuadWord; overload;

procedure SALValue(var Value: Byte; Shift: Integer); overload;
procedure SALValue(var Value: Word; Shift: Integer); overload;
procedure SALValue(var Value: LongWord; Shift: Integer); overload;
procedure SALValue(var Value: QuadWord; Shift: Integer); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Arithmetic right shift (SAR)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SAR(Value: Byte; Shift: Integer): Byte; overload;
Function SAR(Value: Word; Shift: Integer): Word; overload;
Function SAR(Value: LongWord; Shift: Integer): LongWord; overload;
Function SAR(Value: QuadWord; Shift: Integer): QuadWord; overload;

procedure SARValue(var Value: Byte; Shift: Integer); overload;
procedure SARValue(var Value: Word; Shift: Integer); overload;
procedure SARValue(var Value: LongWord; Shift: Integer); overload;
procedure SARValue(var Value: QuadWord; Shift: Integer); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                Endianity swap                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function EndianSwap(Value: Word): Word; overload;
Function EndianSwap(Value: LongWord): LongWord; overload;
Function EndianSwap(Value: QuadWord): QuadWord; overload;

procedure EndianSwapValue(var Value: Word); overload;
procedure EndianSwapValue(var Value: LongWord); overload;
procedure EndianSwapValue(var Value: QuadWord); overload;

procedure EndianSwap(var Buffer; Size: PtrUInt); overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                 Bit test (BT)                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BT(Value: Byte; Bit: Integer): Boolean; overload;
Function BT(Value: Word; Bit: Integer): Boolean; overload;
Function BT(Value: LongWord; Bit: Integer): Boolean; overload;
Function BT(Value: QuadWord; Bit: Integer): Boolean; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit test and set (BTS)                            }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTS(var Value: Byte; Bit: Integer): Boolean; overload;
Function BTS(var Value: Word; Bit: Integer): Boolean; overload;
Function BTS(var Value: LongWord; Bit: Integer): Boolean; overload;
Function BTS(var Value: QuadWord; Bit: Integer): Boolean; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                           Bit test and reset (BTR)                           }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTR(var Value: Byte; Bit: Integer): Boolean; overload;
Function BTR(var Value: Word; Bit: Integer): Boolean; overload;
Function BTR(var Value: LongWord; Bit: Integer): Boolean; overload;
Function BTR(var Value: QuadWord; Bit: Integer): Boolean; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Bit test and complement (BTC)                        }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTC(var Value: Byte; Bit: Integer): Boolean; overload;
Function BTC(var Value: Word; Bit: Integer): Boolean; overload;
Function BTC(var Value: LongWord; Bit: Integer): Boolean; overload;
Function BTC(var Value: QuadWord; Bit: Integer): Boolean; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                      Bit test and set to a given value                       }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BitSetTo(var Value: Byte; Bit: Integer; NewValue: Boolean): Boolean; overload;
Function BitSetTo(var Value: Word; Bit: Integer; NewValue: Boolean): Boolean; overload;
Function BitSetTo(var Value: LongWord; Bit: Integer; NewValue: Boolean): Boolean; overload;
Function BitSetTo(var Value: QuadWord; Bit: Integer; NewValue: Boolean): Boolean; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit scan forward (BSF)                            }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BSF(Value: Byte): Integer; overload;
Function BSF(Value: Word): Integer; overload;
Function BSF(Value: LongWord): Integer; overload;
Function BSF(Value: QuadWord): Integer; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit scan reversed (BSR)                           }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BSR(Value: Byte): Integer; overload;
Function BSR(Value: Word): Integer; overload;
Function BSR(Value: LongWord): Integer; overload;
Function BSR(Value: QuadWord): Integer; overload;

{------------------------------------------------------------------------------}
{==============================================================================}
{                               Population count                               }
{==============================================================================}
{------------------------------------------------------------------------------}

Function PopCount(Value: Byte): Integer; overload;
Function PopCount(Value: Word): Integer; overload;
Function PopCount(Value: LongWord): Integer; overload;
Function PopCount(Value: QuadWord): Integer; overload;


implementation

uses
  SysUtils, Math;

{$IFDEF FPC}{$ASMMODE intel}{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                  Integer number <-> Bit string conversions                   }
{==============================================================================}
{------------------------------------------------------------------------------}

Function _NumberToBits(Number: QuadWord; Bits: Byte): String;
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

Function NumberToBits(Number: Byte): String;
begin
Result := _NumberToBits(Number,8);
end;

//------------------------------------------------------------------------------

Function NumberToBits(Number: Word): String;
begin
Result := _NumberToBits(Number,16);
end;

//------------------------------------------------------------------------------

Function NumberToBits(Number: LongWord): String;
begin
Result := _NumberToBits(Number,32);
end;

//------------------------------------------------------------------------------

Function NumberToBits(Number: QuadWord): String;
begin
Result := _NumberToBits(Number,64);
end;

//==============================================================================

Function BitsToNumber(const BitString: String): QuadWord;
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

Function ROL(Value: Byte; Shift: Integer): Byte;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
  Result := (Value shl Shift) or (Value shr (8 - Shift));
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV AL, CL
{$ENDIF}
  MOV CL, DL
  ROL AL, CL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROL(Value: Word; Shift: Integer): Word;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
  Result := (Value shl Shift) or (Value shr (16 - Shift));
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV AX, CX
{$ENDIF}
  MOV CL, DL
  ROL AX, CL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROL(Value: LongWord; Shift: Integer): LongWord;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
  Result := (Value shl Shift) or (Value shr (32 - Shift));
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV EAX, ECX
{$ENDIF}
  MOV CL,  DL
  ROL EAX, CL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROL(Value: QuadWord; Shift: Integer): QuadWord;{$IFNDEF no32ASM}assembler;{$ENDIF}
{$IFDEF no32ASM}
begin
  Result := (Value shl Shift) or (Value shr (64 - Shift));
end;
{$ELSE}
asm
  MOV RAX, RCX
  MOV CL,  DL
  ROL RAX, CL
end;
{$ENDIF}

//==============================================================================

procedure ROLValue(var Value: Byte; Shift: Integer);
begin
Value := ROL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure ROLValue(var Value: Word; Shift: Integer);
begin
Value := ROL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure ROLValue(var Value: LongWord; Shift: Integer);
begin
Value := ROL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure ROLValue(var Value: QuadWord; Shift: Integer);
begin
Value := ROL(Value,Shift);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                              Rotate right (ROR)                              }
{==============================================================================}
{------------------------------------------------------------------------------}

Function ROR(Value: Byte; Shift: Integer): Byte;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
  Result := (Value shr Shift) or (Value shl (32 - Shift));
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV AL, CL
{$ENDIF}
  MOV CL, DL
  ROR AL, CL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROR(Value: Word; Shift: Integer): Word;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
  Result := (Value shr Shift) or (Value shl (16 - Shift));
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV AX, CX
{$ENDIF}
  MOV CL, DL
  ROR AX, CL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROR(Value: LongWord; Shift: Integer): LongWord;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
  Result := (Value shr Shift) or (Value shl (32 - Shift));
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV EAX, ECX
{$ENDIF}
  MOV CL,  DL
  ROR EAX, CL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ROR(Value: QuadWord; Shift: Integer): QuadWord;{$IFNDEF no32ASM}assembler;{$ENDIF}
{$IFDEF no32ASM}
begin
  Result := (Value shr Shift) or (Value shl (64 - Shift));
end;
{$ELSE}
asm
  MOV RAX, RCX
  MOV CL,  DL
  ROR RAX, CL
end;
{$ENDIF}

//==============================================================================

procedure RORValue(var Value: Byte; Shift: Integer);
begin
Value := ROR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure RORValue(var Value: Word; Shift: Integer);
begin
Value := ROR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure RORValue(var Value: LongWord; Shift: Integer);
begin
Value := ROR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure RORValue(var Value: QuadWord; Shift: Integer);
begin
Value := ROR(Value,Shift);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Rotate left with carry (RCL)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function RCLCarry(Value: Byte; Shift: Integer; var CF: Boolean): Byte;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
var
  i:      Integer;
  Carry:  Boolean;
begin
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result shr 7) <> 0;
    Result := (Result shl 1) or (Byte(Carry) and 1);
    Carry := CF;
  end;
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   RAX, RCX
  MOV   RCX, RDX
  SHR   byte ptr [R8], 1
  RCL   AL,  CL
  SETC  CL
  MOV   byte ptr [R8], CL
{$ELSE}
  XCHG  EDX, ECX
  SHR   byte ptr [EDX], 1
  RCL   AL,  CL
  SETC  CL
  MOV   byte ptr [EDX], CL
{$ENDIF}
end;
{$ENDIF}
      
//------------------------------------------------------------------------------

Function RCLCarry(Value: Word; Shift: Integer; var CF: Boolean): Word;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
var
  i:      Integer;
  Carry:  Boolean;
begin
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result shr 15) <> 0;
    Result := (Result shl 1) or (Byte(Carry) and 1);
    Carry := CF;
  end;
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   RAX, RCX
  MOV   RCX, RDX
  SHR   byte ptr [R8], 1
  RCL   AX,  CL
  SETC  CL
  MOV   byte ptr [R8], CL
{$ELSE}
  XCHG  EDX, ECX
  SHR   byte ptr [EDX], 1
  RCL   AX,  CL
  SETC  CL
  MOV   byte ptr [EDX], CL
{$ENDIF}
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCLCarry(Value: LongWord; Shift: Integer; var CF: Boolean): LongWord;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
var
  i:      Integer;
  Carry:  Boolean;
begin
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result shr 31) <> 0;
    Result := (Result shl 1) or (Byte(Carry) and 1);
    Carry := CF;
  end;
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   RAX, RCX
  MOV   RCX, RDX
  SHR   byte ptr [R8], 1
  RCL   EAX, CL
  SETC  CL
  MOV   byte ptr [R8], CL
{$ELSE}
  XCHG  EDX, ECX
  SHR   byte ptr [EDX], 1
  RCL   EAX, CL
  SETC  CL
  MOV   byte ptr [EDX], CL
{$ENDIF}
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCLCarry(Value: QuadWord; Shift: Integer; var CF: Boolean): QuadWord;{$IFNDEF no32ASM}assembler;{$ENDIF}
{$IFDEF no32ASM}
var
  i:      Integer;
  Carry:  Boolean;
begin
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result shr 63) <> 0;
    Result := (Result shl 1) or (Byte(Carry) and 1);
    Carry := CF;
  end;
end;
{$ELSE}
asm
  MOV   RAX, RCX
  MOV   RCX, RDX
  SHR   byte ptr [R8], 1
  RCL   RAX, CL
  SETC  CL
  MOV   byte ptr [R8], CL
end;
{$ENDIF}


//==============================================================================

Function RCL(Value: Byte; Shift: Integer; CF: Boolean = False): Byte;
var
  Carry:  Boolean;
begin
Carry := CF;
Result := RCLCarry(Value,Shift,Carry);
end;
 
//------------------------------------------------------------------------------

Function RCL(Value: Word; Shift: Integer; CF: Boolean = False): Word;
var
  Carry:  Boolean;
begin
Carry := CF;
Result := RCLCarry(Value,Shift,Carry);
end;

//------------------------------------------------------------------------------

Function RCL(Value: LongWord; Shift: Integer; CF: Boolean = False): LongWord;
var
  Carry:  Boolean;
begin
Carry := CF;
Result := RCLCarry(Value,Shift,Carry);
end;

//------------------------------------------------------------------------------

Function RCL(Value: QuadWord; Shift: Integer; CF: Boolean = False): QuadWord;
var
  Carry:  Boolean;
begin
Carry := CF;
Result := RCLCarry(Value,Shift,Carry);
end;

//==============================================================================

procedure RCLValueCarry(var Value: Byte; Shift: Integer; var CF: Boolean);
begin
Value := RCLCarry(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCLValueCarry(var Value: Word; Shift: Integer; var CF: Boolean);
begin
Value := RCLCarry(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCLValueCarry(var Value: LongWord; Shift: Integer; var CF: Boolean);
begin
Value := RCLCarry(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCLValueCarry(var Value: QuadWord; Shift: Integer; var CF: Boolean);
begin
Value := RCLCarry(Value,Shift,CF);
end;

//==============================================================================

procedure RCLValue(var Value: Byte; Shift: Integer; CF: Boolean = False);
begin
Value := RCL(Value,Shift,CF);
end;
 
//------------------------------------------------------------------------------

procedure RCLValue(var Value: Word; Shift: Integer; CF: Boolean = False);
begin
Value := RCL(Value,Shift,CF);
end;
 
//------------------------------------------------------------------------------

procedure RCLValue(var Value: LongWord; Shift: Integer; CF: Boolean = False);
begin
Value := RCL(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCLValue(var Value: QuadWord; Shift: Integer; CF: Boolean = False);
begin
Value := RCL(Value,Shift,CF);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                        Rotate right with carry (RCR)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function RCRCarry(Value: Byte; Shift: Integer; var CF: Boolean): Byte;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
var
  i:      Integer;
  Carry:  Boolean;
begin
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    Result := (Result shr 1) or ((Byte(Carry) and 1) shl 7);
    Carry := CF;
  end;
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   RAX, RCX
  MOV   RCX, RDX
  SHR   byte ptr [R8], 1
  RCR   AL,  CL
  SETC  CL
  MOV   byte ptr [R8], CL
{$ELSE}
  XCHG  EDX, ECX
  SHR   byte ptr [EDX], 1
  RCR   AL,  CL
  SETC  CL
  MOV   byte ptr [EDX], CL
{$ENDIF}
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCRCarry(Value: Word; Shift: Integer; var CF: Boolean): Word;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
var
  i:      Integer;
  Carry:  Boolean;
begin
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    Result := (Result shr 1) or ((Word(Carry) and 1) shl 15);
    Carry := CF;
  end;
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   RAX, RCX
  MOV   RCX, RDX
  SHR   byte ptr [R8], 1
  RCR   AX,  CL
  SETC  CL
  MOV   byte ptr [R8], CL
{$ELSE}
  XCHG  EDX, ECX
  SHR   byte ptr [EDX], 1
  RCR   AX,  CL
  SETC  CL
  MOV   byte ptr [EDX], CL
{$ENDIF}
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCRCarry(Value: LongWord; Shift: Integer; var CF: Boolean): LongWord;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
var
  i:      Integer;
  Carry:  Boolean;
begin
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    Result := (Result shr 1) or ((LongWord(Carry) and 1) shl 31);
    Carry := CF;
  end;
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   RAX, RCX
  MOV   RCX, RDX
  SHR   byte ptr [R8], 1
  RCR   EAX, CL
  SETC  CL
  MOV   byte ptr [R8], CL
{$ELSE}
  XCHG  EDX, ECX
  SHR   byte ptr [EDX], 1
  RCR   EAX, CL
  SETC  CL
  MOV   byte ptr [EDX], CL
{$ENDIF}
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function RCRCarry(Value: QuadWord; Shift: Integer; var CF: Boolean): QuadWord;{$IFNDEF no32ASM}assembler;{$ENDIF}
{$IFDEF no32ASM}
var
  i:      Integer;
  Carry:  Boolean;
begin
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    Result := (Result shr 1) or ((QuadWord(Carry) and 1) shl 63);
    Carry := CF;
  end;
end;
{$ELSE}
asm
  MOV   RAX, RCX
  MOV   RCX, RDX
  SHR   byte ptr [R8], 1
  RCR   RAX, CL
  SETC  CL
  MOV   byte ptr [R8], CL
end;
{$ENDIF}

//==============================================================================

Function RCR(Value: Byte; Shift: Integer; CF: Boolean = False): Byte;
var
  Carry:  Boolean;
begin
Carry := CF;
Result := RCRCarry(Value,Shift,Carry);
end;

//------------------------------------------------------------------------------

Function RCR(Value: Word; Shift: Integer; CF: Boolean = False): Word;
var
  Carry:  Boolean;
begin
Carry := CF;
Result := RCRCarry(Value,Shift,Carry);
end;

//------------------------------------------------------------------------------

Function RCR(Value: LongWord; Shift: Integer; CF: Boolean = False): LongWord;
var
  Carry:  Boolean;
begin
Carry := CF;
Result := RCRCarry(Value,Shift,Carry);
end;

//------------------------------------------------------------------------------

Function RCR(Value: QuadWord; Shift: Integer; CF: Boolean = False): QuadWord;
var
  Carry:  Boolean;
begin
Carry := CF;
Result := RCRCarry(Value,Shift,Carry);
end;

//==============================================================================

procedure RCRValueCarry(var Value: Byte; Shift: Integer; var CF: Boolean);
begin
Value := RCRCarry(Value,Shift,CF);
end;
 
//------------------------------------------------------------------------------

procedure RCRValueCarry(var Value: Word; Shift: Integer; var CF: Boolean);
begin
Value := RCRCarry(Value,Shift,CF);
end;
  
//------------------------------------------------------------------------------

procedure RCRValueCarry(var Value: LongWord; Shift: Integer; var CF: Boolean);
begin
Value := RCRCarry(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCRValueCarry(var Value: QuadWord; Shift: Integer; var CF: Boolean);
begin
Value := RCRCarry(Value,Shift,CF);
end;

//==============================================================================

procedure RCRValue(var Value: Byte; Shift: Integer; CF: Boolean = False);
begin
Value := RCR(Value,Shift,CF);
end;
 
//------------------------------------------------------------------------------

procedure RCRValue(var Value: Word; Shift: Integer; CF: Boolean = False);
begin
Value := RCR(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCRValue(var Value: LongWord; Shift: Integer; CF: Boolean = False);
begin
Value := RCR(Value,Shift,CF);
end;

//------------------------------------------------------------------------------

procedure RCRValue(var Value: QuadWord; Shift: Integer; CF: Boolean = False);
begin
Value := RCR(Value,Shift,CF);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Arithmetic left shift (SAL)                          }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SAL(Value: Byte; Shift: Integer): Byte;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := Value shl Shift;
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV RAX, RCX
{$ENDIF}
  MOV CL,  DL
  SAL AL,  CL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SAL(Value: Word; Shift: Integer): Word;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := Value shl Shift;
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV RAX, RCX
{$ENDIF}
  MOV CL,  DL
  SAL AX,  CL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SAL(Value: LongWord; Shift: Integer): LongWord;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := Value shl Shift;
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV RAX, RCX
{$ENDIF}
  MOV CL,  DL
  SAL EAX, CL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SAL(Value: QuadWord; Shift: Integer): QuadWord;{$IFNDEF no32ASM}assembler;{$ENDIF}
{$IFDEF no32ASM}
begin
Result := Value shl Shift;
end;
{$ELSE}
asm
  MOV RAX, RCX
  MOV CL,  DL
  SAL RAX, CL
end;
{$ENDIF}

//==============================================================================

procedure SALValue(var Value: Byte; Shift: Integer);
begin
Value := SAL(Value,Shift);
end;
 
//------------------------------------------------------------------------------

procedure SALValue(var Value: Word; Shift: Integer);
begin
Value := SAL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure SALValue(var Value: LongWord; Shift: Integer);
begin
Value := SAL(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure SALValue(var Value: QuadWord; Shift: Integer);
begin
Value := SAL(Value,Shift);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Arithmetic right shift (SAR)                         }
{==============================================================================}
{------------------------------------------------------------------------------}

Function SAR(Value: Byte; Shift: Integer): Byte;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
If (Value shr 7) <> 0 then
  Result := (Value shr Shift) or ($FF shl (8 - Shift))
else
  Result := Value shr Shift;
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV RAX, RCX
{$ENDIF}
  MOV CL,  DL
  SAR AL,  CL
end;
{$ENDIF}
 
//------------------------------------------------------------------------------

Function SAR(Value: Word; Shift: Integer): Word;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
If (Value shr 15) <> 0 then
  Result := (Value shr Shift) or ($FFFF shl (16 - Shift))
else
  Result := Value shr Shift;
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV RAX, RCX
{$ENDIF}
  MOV CL,  DL
  SAR AX,  CL
end;
{$ENDIF}
  
//------------------------------------------------------------------------------

Function SAR(Value: LongWord; Shift: Integer): LongWord;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
If (Value shr 31) <> 0 then
  Result := (Value shr Shift) or ($FFFFFFFF shl (32 - Shift))
else
  Result := Value shr Shift;
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV RAX, RCX
{$ENDIF}
  MOV CL,  DL
  SAR EAX, CL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SAR(Value: QuadWord; Shift: Integer): QuadWord;{$IFNDEF no32ASM}assembler;{$ENDIF}
{$IFDEF no32ASM}
begin
If (Value shr 63) <> 0 then
  Result := (Value shr Shift) or ($FFFFFFFFFFFFFFFF shl (64 - Shift))
else
  Result := Value shr Shift;
end;
{$ELSE}
asm
  MOV RAX, RCX
  MOV CL,  DL
  SAR RAX, CL
end;
{$ENDIF}

//==============================================================================

procedure SARValue(var Value: Byte; Shift: Integer);
begin
Value := SAL(Value,Shift);
end;
 
//------------------------------------------------------------------------------

procedure SARValue(var Value: Word; Shift: Integer);
begin
Value := SAR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure SARValue(var Value: LongWord; Shift: Integer);
begin
Value := SAR(Value,Shift);
end;

//------------------------------------------------------------------------------

procedure SARValue(var Value: QuadWord; Shift: Integer);
begin
Value := SAR(Value,Shift);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                                Endianity swap                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function EndianSwap(Value: Word): Word;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := (Value shl 8) or (Value shr 8);
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   RAX, RCX
{$ENDIF}
  XCHG  AL,  AH
end;
{$ENDIF}
  
//------------------------------------------------------------------------------

Function EndianSwap(Value: LongWord): LongWord;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := (Value and $000000FF shl 24) or (Value and $0000FF00 shl 8) or
          (Value and $00FF0000 shr 8) or (Value and $FF000000 shr 24);
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   RAX, RCX
{$ENDIF}
  BSWAP EAX
end;
{$ENDIF}
      
//------------------------------------------------------------------------------

Function EndianSwap(Value: QuadWord): QuadWord;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Int64Rec(Result).Hi := EndianSwap(Int64Rec(Value).Lo);
Int64Rec(Result).Lo := EndianSwap(Int64Rec(Value).Hi);
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   RAX, RCX
  BSWAP RAX
{$ELSE}
  MOV EAX, dword ptr [Value + 4]
  MOV EDX, dword ptr [Value]
  BSWAP EAX
  BSWAP EDX
{$ENDIF}
end;
{$ENDIF}

//==============================================================================

procedure EndianSwapValue(var Value: Word);
begin
Value := EndianSwap(Value);
end;
             
//------------------------------------------------------------------------------

procedure EndianSwapValue(var Value: LongWord);
begin
Value := EndianSwap(Value);
end;

//------------------------------------------------------------------------------

procedure EndianSwapValue(var Value: QuadWord);
begin
Value := EndianSwap(Value);
end;

//==============================================================================

procedure EndianSwap(var Buffer; Size: PtrUInt);{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
var
  i:        LongWord;
  ByteBuff: Byte;
begin
case Size of
  Low(Size)..1: Exit;
             2: EndianSwapValue(Word(Buffer));
             4: EndianSwapValue(LongWord(Buffer));
             8: EndianSwapValue(QuadWord(Buffer));
else
  For i := 0 to Pred(Size div 2) do
    begin
      {%H-}ByteBuff := PByte(PtrUInt(@Buffer) + i)^;
      {%H-}PByte(PtrUInt(@Buffer) + i)^ := PByte(PtrUInt(@Buffer) + (Size - i) - 1)^;
      {%H-}PByte(PtrUInt(@Buffer) + (Size - i) - 1)^ := ByteBuff;
    end;
end;
end;
{$ELSE}
asm
{$IFDEF x64}
  XCHG  RCX, RDX
  CMP   RCX, 1
  JLE   @RoutineEnd

  LEA   RAX, [RDX + RCX - 1]
  SHR   RCX, 1

{$IFDEF FPC}
  db $66 db $66 db $90  // Explicit alignment of loop start
{$ENDIF}

@LoopStart:
  MOV   R8B, byte ptr [RDX]
  MOV   R9B, byte ptr [RAX]
  MOV   byte ptr [RAX], R8B
  MOV   byte ptr [RDX], R9B
  INC   RDX
  DEC   RAX
  LOOP  @LoopStart

@RoutineEnd:
{$ELSE}
  MOV   ECX, EDX
  CMP   ECX, 1
  JLE   @RoutineEnd

  PUSH  ESI
  PUSH  EDI
  MOV   ESI, EAX
  LEA   EDI, [EAX + ECX - 1]
  SHR   ECX, 1

{$IFNDEF FPC}
  db $66 db $66 db $90  // Explicit alignment of loop start
{$ENDIF}

@LoopStart:
  MOV   AL,  byte ptr [ESI]
  MOV   DL,  byte ptr [EDI]
  MOV   byte ptr [EDI], AL
  MOV   byte ptr [ESI], DL
  INC   ESI
  DEC   EDI
  LOOP  @LoopStart

  POP   EDI
  POP   ESI

@RoutineEnd:
{$ENDIF}
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                                 Bit test (BT)                                }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BT(Value: Byte; Bit: Integer): Boolean;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := ((Value shr Bit) and 1) <> 0;
end;
{$ELSE}
asm
{$IFDEF x64}
  BT    CX, DX
{$ELSE}
  BT    AX, DX
{$ENDIF}
  SETC  AL
end;
{$ENDIF}
           
//------------------------------------------------------------------------------

Function BT(Value: Word; Bit: Integer): Boolean;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := ((Value shr Bit) and 1) <> 0;
end;
{$ELSE}
asm
{$IFDEF x64}
  BT    CX, DX
{$ELSE}
  BT    AX, DX
{$ENDIF}
  SETC  AL
end;
{$ENDIF}
           
//------------------------------------------------------------------------------

Function BT(Value: LongWord; Bit: Integer): Boolean;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := ((Value shr Bit) and 1) <> 0;
end;
{$ELSE}
asm
{$IFDEF x64}
  BT    ECX, EDX
{$ELSE}
  BT    EAX, EDX
{$ENDIF}
  SETC  AL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BT(Value: QuadWord; Bit: Integer): Boolean;{$IFNDEF no32ASM}assembler;{$ENDIF}
{$IFDEF no32ASM}
begin
Result := ((Value shr Bit) and 1) <> 0;
end;
{$ELSE}
asm
  BT    RCX, RDX
  SETC  AL
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit test and set (BTS)                            }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTS(var Value: Byte; Bit: Integer): Boolean;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := Value or (Byte(1) shl Bit);
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   AL, byte ptr [RCX]
  BTS   AX, DX
  MOV   byte ptr [RCX], AL
{$ELSE}
  MOV   CL,  byte ptr [EAX]
  BTS   CX,  DX
  MOV   byte ptr [EAX], CL
{$ENDIF}
  SETC  AL
end;
{$ENDIF}
           
//------------------------------------------------------------------------------

Function BTS(var Value: Word; Bit: Integer): Boolean;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := Value or (Word(1) shl Bit);
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   AX, word ptr [RCX]
  BTS   AX, DX
  MOV   word ptr [RCX], AX
{$ELSE}
  MOV   CX,  word ptr [EAX]
  BTS   CX,  DX
  MOV   word ptr [EAX], CX
{$ENDIF}
  SETC  AL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTS(var Value: LongWord; Bit: Integer): Boolean;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := Value or (LongWord(1) shl Bit);
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   EAX, dword ptr [RCX]
  BTS   EAX, EDX
  MOV   dword ptr [RCX], EAX
{$ELSE}
  MOV   ECX, dword ptr [EAX]
  BTS   ECX, EDX
  MOV   dword ptr [EAX], ECX
{$ENDIF}
  SETC  AL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTS(var Value: QuadWord; Bit: Integer): Boolean;{$IFNDEF no32ASM}assembler;{$ENDIF}
{$IFDEF no32ASM}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := Value or (QuadWord(1) shl Bit);
end;
{$ELSE}
asm
  MOV   RAX, qword ptr [RCX]
  BTS   RAX, RDX
  MOV   qword ptr [RCX], RAX
  SETC  AL
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                           Bit test and reset (BTR)                           }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTR(var Value: Byte; Bit: Integer): Boolean;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := Value and not(1 shl Bit);
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   AL, byte ptr [RCX]
  BTR   AX, DX
  MOV   byte ptr [RCX], AL
{$ELSE}
  MOV   CL, byte ptr [EAX]
  BTR   CX, DX
  MOV   byte ptr [EAX], CL
{$ENDIF}
  SETC  AL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTR(var Value: Word; Bit: Integer): Boolean;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := Value and not(1 shl Bit);
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   AX, word ptr [RCX]
  BTR   AX, DX
  MOV   word ptr [RCX], AX
{$ELSE}
  MOV   CX, word ptr [EAX]
  BTR   CX, DX
  MOV   word ptr [EAX], CX
{$ENDIF}
  SETC  AL
end;
{$ENDIF}
           
//------------------------------------------------------------------------------

Function BTR(var Value: LongWord; Bit: Integer): Boolean;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := Value and not(1 shl Bit);
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   EAX, dword ptr [RCX]
  BTR   EAX, EDX
  MOV   dword ptr [RCX], EAX
{$ELSE}
  MOV   ECX, dword ptr [EAX]
  BTR   ECX, EDX
  MOV   dword ptr [EAX], ECX
{$ENDIF}
  SETC  AL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTR(var Value: QuadWord; Bit: Integer): Boolean;{$IFNDEF no32ASM}assembler;{$ENDIF}
{$IFDEF no32ASM}
begin
Result := ((Value shr Bit) and 1) <> 0;
Value := Value and not(QuadWord(1) shl Bit);
end;
{$ELSE}
asm
  MOV   RAX, qword ptr [RCX]
  BTR   RAX, RDX
  MOV   qword ptr [RCX], RAX
  SETC  AL
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                         Bit test and complement (BTC)                        }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BTC(var Value: Byte; Bit: Integer): Boolean;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := ((Value shr Bit) and 1) <> 0;
If Result then Value := Value and not(1 shl Bit)
  else Value := Value or (Byte(1) shl Bit);
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   AL, byte ptr [RCX]
  BTC   AX, DX
  MOV   byte ptr [RCX], AL
{$ELSE}
  MOV   CL, byte ptr [EAX]
  BTC   CX, DX
  MOV   byte ptr [EAX], CL
{$ENDIF}
  SETC  AL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTC(var Value: Word; Bit: Integer): Boolean;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := ((Value shr Bit) and 1) <> 0;
If Result then Value := Value and not(1 shl Bit)
  else Value := Value or (Word(1) shl Bit);
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   AX, word ptr [RCX]
  BTC   AX, DX
  MOV   word ptr [RCX], AX
{$ELSE}
  MOV   CX, word ptr [EAX]
  BTC   CX, DX
  MOV   word ptr [EAX], CX
{$ENDIF}
  SETC  AL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTC(var Value: LongWord; Bit: Integer): Boolean;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
Result := ((Value shr Bit) and 1) <> 0;
If Result then Value := Value and not(1 shl Bit)
  else Value := Value or (LongWord(1) shl Bit);
end;
{$ELSE}
asm
{$IFDEF x64}
  MOV   EAX, dword ptr [RCX]
  BTC   EAX, EDX
  MOV   dword ptr [RCX], EAX
{$ELSE}
  MOV   ECX, dword ptr [EAX]
  BTC   ECX, EDX
  MOV   dword ptr [EAX], ECX
{$ENDIF}
  SETC  AL
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BTC(var Value: QuadWord; Bit: Integer): Boolean;{$IFNDEF no32ASM}assembler;{$ENDIF}
{$IFDEF no32ASM}
begin
Result := ((Value shr Bit) and 1) <> 0;
If Result then Value := Value and not(QuadWord(1) shl Bit)
  else Value := Value or (QuadWord(1) shl Bit);
end;
{$ELSE}
asm
  MOV   RAX, qword ptr [RCX]
  BTC   RAX, RDX
  MOV   qword ptr [RCX], RAX
  SETC  AL
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                      Bit test and set to a given value                       }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BitSetTo(var Value: Byte; Bit: Integer; NewValue: Boolean): Boolean;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;
             
//------------------------------------------------------------------------------

Function BitSetTo(var Value: Word; Bit: Integer; NewValue: Boolean): Boolean;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;
            
//------------------------------------------------------------------------------

Function BitSetTo(var Value: LongWord; Bit: Integer; NewValue: Boolean): Boolean;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;

//------------------------------------------------------------------------------

Function BitSetTo(var Value: QuadWord; Bit: Integer; NewValue: Boolean): Boolean;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit scan forward (BSF)                            }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BSF(Value: Byte): Integer;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
For Result := 0 to 7 do
  If (Value shr Result) and 1 <> 0 then Exit;
Result := -1;
end;
{$ELSE}
asm
{$IFDEF x64}
  AND RCX, $FF
  AND RAX, $FFFF
  BSF AX,  CX
{$ELSE}
  AND EAX, $FF
  BSF AX,  AX
{$ENDIF}
  JNZ @RoutineEnd
  MOV EAX,  -1
@RoutineEnd:
end;
{$ENDIF}
              
//------------------------------------------------------------------------------

Function BSF(Value: Word): Integer;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
For Result := 0 to 15 do
  If (Value shr Result) and 1 <> 0 then Exit;
Result := -1;
end;
{$ELSE}
asm
{$IFDEF x64}
  AND RCX, $FFFF
  AND RAX, $FFFF
  BSF AX,  CX
{$ELSE}
  AND EAX, $FFFF
  BSF AX,  AX
{$ENDIF}
  JNZ @RoutineEnd
  MOV EAX,  -1
@RoutineEnd:
end;
{$ENDIF}
             
//------------------------------------------------------------------------------

Function BSF(Value: LongWord): Integer;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
For Result := 0 to 31 do
  If (Value shr Result) and 1 <> 0 then Exit;
Result := -1;
end;
{$ELSE}
asm
{$IFDEF x64}
  BSF EAX, ECX
{$ELSE}
  BSF EAX, EAX
{$ENDIF}
  JNZ @RoutineEnd
  MOV EAX, -1
@RoutineEnd:
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BSF(Value: QuadWord): Integer;{$IFNDEF no32ASM}assembler;{$ENDIF}
{$IFDEF no32ASM}
begin
For Result := 0 to 63 do
  If (Value shr Result) and 1 <> 0 then Exit;
Result := -1;
end;
{$ELSE}
asm
  BSF RAX, RCX
  JNZ @RoutineEnd
  MOV RAX, -1
@RoutineEnd:
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                            Bit scan reversed (BSR)                           }
{==============================================================================}
{------------------------------------------------------------------------------}

Function BSR(Value: Byte): Integer; register;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
For Result := 7 downto 0 do
  If (Value shr Result) and 1 <> 0 then Exit;
Result := -1;
end;
{$ELSE}
asm
{$IFDEF x64}
  AND RCX, $FF
  AND RAX, $FFFF
  BSR AX,  CX
{$ELSE}
  AND EAX, $FF
  BSR AX,  AX
{$ENDIF}
  JNZ @RoutineEnd
  MOV EAX,  -1
@RoutineEnd:
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BSR(Value: Word): Integer;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
For Result := 15 downto 0 do
  If (Value shr Result) and 1 <> 0 then Exit;
Result := -1;
end;
{$ELSE}
asm
{$IFDEF x64}
  AND RCX, $FFFF
  AND RAX, $FFFF
  BSR AX,  CX
{$ELSE}
  AND EAX, $FFFF
  BSR AX,  AX
{$ENDIF}
  JNZ @RoutineEnd
  MOV EAX,  -1
@RoutineEnd:
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BSR(Value: LongWord): Integer;{$IFNDEF PurePascal}assembler;{$ENDIF}
{$IFDEF PurePascal}
begin
For Result := 31 downto 0 do
  If (Value shr Result) and 1 <> 0 then Exit;
Result := -1;
end;
{$ELSE}
asm
{$IFDEF x64}
  BSR EAX, ECX
{$ELSE}
  BSR EAX, EAX
{$ENDIF}
  JNZ @RoutineEnd
  MOV EAX, -1
@RoutineEnd:
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function BSR(Value: QuadWord): Integer;{$IFNDEF no32ASM}assembler;{$ENDIF}
{$IFDEF no32ASM}
begin
For Result := 63 downto 0 do
  If (Value shr Result) and 1 <> 0 then Exit;
Result := -1;
end;
{$ELSE}
asm
  BSR RAX, RCX
  JNZ @RoutineEnd
  MOV RAX, -1
@RoutineEnd:
end;
{$ENDIF}

{------------------------------------------------------------------------------}
{==============================================================================}
{                               Population count                               }
{==============================================================================}
{------------------------------------------------------------------------------}

Function PopCount(Value: Byte): Integer;
var
  i:  Integer;
begin
Result := 0;
For i := 1 to 8 do
  begin
    If (Value and 1) <> 0 then Inc(Result);
    Value := Value shr 1;
  end;
end;

//------------------------------------------------------------------------------

Function PopCount(Value: Word): Integer;
var
  i:  Integer;
begin
Result := 0;
For i := 1 to 16 do
  begin
    If (Value and 1) <> 0 then Inc(Result);
    Value := Value shr 1;
  end;
end;

//------------------------------------------------------------------------------

Function PopCount(Value: LongWord): Integer;
var
  i:  Integer;
begin
Result := 0;
For i := 1 to 32 do
  begin
    If (Value and 1) <> 0 then Inc(Result);
    Value := Value shr 1;
  end;
end;

//------------------------------------------------------------------------------

Function PopCount(Value: QuadWord): Integer;
var
  i:  Integer;
begin
Result := 0;
For i := 1 to 64 do
  begin
    If (Value and 1) <> 0 then Inc(Result);
    Value := Value shr 1;
  end;
end;

end.
