VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form Form1 
   Appearance      =   0  'Flat
   AutoRedraw      =   -1  'True
   BackColor       =   &H00000000&
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Form1"
   ClientHeight    =   4485
   ClientLeft      =   150
   ClientTop       =   420
   ClientWidth     =   5970
   DrawStyle       =   5  'Transparent
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   299
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   398
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox picControls 
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   1800
      Left            =   15
      Picture         =   "Planets.frx":0000
      ScaleHeight     =   1800
      ScaleWidth      =   840
      TabIndex        =   1
      Top             =   15
      Width           =   840
   End
   Begin VB.PictureBox PIC 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   1170
      Index           =   0
      Left            =   1995
      ScaleHeight     =   78
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   87
      TabIndex        =   0
      Top             =   300
      Width           =   1305
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   3450
      Top             =   2685
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Shape Shape1 
      BorderColor     =   &H00FFFFFF&
      Height          =   150
      Left            =   2625
      Shape           =   2  'Oval
      Top             =   2460
      Width           =   345
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' Planets.frm by Robert Rayment 14/12/01

' Demonstrates wrapping a bitmap around a sphere
' then moving, rotating & magnification if image.

' Key controls only:-

' Arrow keys -                 move image

' Shift left ot right arrows-  rotation speed

' Ctrl up or down-             move closer to or away from image

' L-                           load a bitmap

' Space-                       halt, speed=0

' A-                           toggle between VB & Machine Code ASM

' Esc-                         Exit.


Option Base 1
DefLng A-W
DefSng X-Z


Private Sub Form_Load()

KeyPreview = True

'Top = 0
'Left = 0
Width = 11900
Height = 8000

With Shape1
   .Left = 5950 / 15 - 30
   .Top = 4000 / 15
End With

With PIC(0)
   .Width = 256
   .Height = 256
End With
PICW = 256
PICH = 256

PIC(0).Visible = False

'===================================================================
'Get app path
PathSpec$ = App.Path
If Right$(PathSpec$, 1) <> "\" Then PathSpec$ = PathSpec$ & "\"

' This for save last Folder, on exit to INI file
AppName$ = "Planets"
GetInifile
'===================================================================

ASM = True
Speed = -2
' Form image size & position
FTop = 150: FLeft = 250: FWidth = 256: FHeight = 256

Caption = "Top =" & Str$(FTop) & " Left =" & Str$(FLeft) _
& " Width =" & Str$(FWidth) & " Height =" & Str$(FHeight) _
& " Speed =" & Str$(Speed)

ReDim WrapRotateMC(1)

Loadmcode PathSpec$ & "WrapRotate.bin", WrapRotateMC()

End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)

Select Case KeyCode

Case vbKeyLeft
   
   If Shift = 1 Then 'SHIFT
      Speed = Speed + 1
   ElseIf Shift = 2 Then 'CTRL
      FLeft = FLeft + 4
   ElseIf Shift = 0 Then
      FLeft = FLeft + 4
   End If
   
Case vbKeyRight
   
   If Shift = 1 Then 'SHIFT
      Speed = Speed - 1
   ElseIf Shift = 2 Then 'CTRL
      FLeft = FLeft - 4
   ElseIf Shift = 0 Then
      FLeft = FLeft - 4
   End If
   
Case vbKeyUp
   
   If Shift = 1 Then 'SHIFT
   ElseIf Shift = 2 Then  'CTRL
      FHeight = FHeight + 4
      FWidth = FWidth + 4
      FLeft = FLeft - 2
      FTop = FTop - 2
   ElseIf Shift = 0 Then
      FTop = FTop + 4
   End If
   
Case vbKeyDown
   
   If Shift = 1 Then 'SHIFT
   ElseIf Shift = 2 Then 'CTRL
      FHeight = FHeight - 4
      FWidth = FWidth - 4
      FLeft = FLeft + 2
      FTop = FTop + 2
   ElseIf Shift = 0 Then
      FTop = FTop - 4
   End If

Case vbKeySpace    ' HALT
   
   Speed = 0

Case vbKeyL

   '======================================================================
   Title$ = "Load a Picture file"
   Choice$ = "Pictures (*.bmp *.gif *.jpg *.wmf *.ico *.cur)|*.bmp;*.gif;*.jpg;*.wmf;*.ico;*.cur|All files|*.*"
   InitDir$ = LoadDir$
   LoadSave = True
   LSDialog Title$, Choice$, InitDir$, LoadSave
   ' Output:  Public LoadFileSpec$, SaveFileSpec$
   '======================================================================

   If LoadFileSpec$ <> "" Then
   
      ASM = True
      Speed = -2
      ' Form image size & position
      FTop = 150: FLeft = 250: FWidth = 256: FHeight = 256

      Caption = "Top =" & Str$(FTop) & " Left =" & Str$(FLeft) _
      & " Width =" & Str$(FWidth) & " Height =" & Str$(FHeight) _
      & " Speed =" & Str$(Speed)


      SqueezePIC

   End If

Case vbKeyA

   ASM = Not ASM
   'If ASM Then LabVBASM.Caption = "A" Else LabVBASM.Caption = "V"

Case 27  ' Esc

   Done = True

   Erase PalBGR

   '======================================================================
   ' Save last folders to ini file
   IniSpec$ = PathSpec$ & AppName$ & ".ini"
   Open IniSpec$ For Output As #1

   Print #1, LoadDir$
   Print #1, SaveDir$
   Close #1
   '======================================================================

   End

End Select

' Limit motion

If FTop > 800 Then FTop = 800
If FTop < -600 Then FTop = -600

If FLeft > 800 Then FLeft = 800
If FLeft < -400 Then FLeft = -400

If FHeight > 600 Then
   FHeight = 600
   FWidth = 600
End If
If FHeight < 4 Then
   FHeight = 4
   FWidth = 4
End If

If Speed < -8 Then Speed = -8
If Speed > 8 Then Speed = 8

Caption = "Top =" & Str$(FTop) & " Left =" & Str$(FLeft) _
& " Width =" & Str$(FWidth) & " Height =" & Str$(FHeight) _
& " Speed =" & Str$(Speed)

End Sub

Private Sub Form_Resize()
'Top = 0
'Left = 0
'Width = 11999
'Height = 8999
End Sub

Private Sub SqueezePIC()

On Error GoTo PICError

'Public LoadFileSpec$
         
Load PIC(1)
With PIC(1)
   .AutoRedraw = True
   .AutoSize = True
   .BorderStyle = 0
   .ScaleMode = vbPixels
   .Visible = False
End With
PIC(1).Picture = LoadPicture(LoadFileSpec$)
PIC(1).Refresh
LPicWidth& = PIC(1).Width
LPicHeight& = PIC(1).Height
         
'Place whole BMP on PIC(0) from PIC(1)
rs& = StretchBlt(PIC(0).HDC, 0, 0, PICW, PICH, _
PIC(1).HDC, 0, 0, LPicWidth&, LPicHeight&, vbSrcCopy)
         
' Clear PIC(1)
PIC(1).Picture = LoadPicture
' Unload temporary picbox to save memory
Unload PIC(1)

PIC(0).Refresh

GETDIBS PIC(0).Image

Wrap_Rotate

Exit Sub
'========
PICError:
MsgBox "PIC ERROR"
End
End Sub

Private Sub GeneratePalBGRs()

' Mem storage

'PalBGR for 32 bitcount

' BLUE     GREEN     RED       ALPHA
'(1,1,1,1)(2,1,1,1),(3,1,1,1),(4,1,1,1),,,(1,X,1,1),(2,X,1,1),(3,X,1,1),(4,X,1,1)

'(1,1,Y,1)(2,1,Y,1),(3,1,Y,1),(4,1,1,1),,,(1,X,Y,1),(2,X,Y,1),(3,X,Y,1),(4,X,Y,1)
'-----------------------------------------------------------------------------------
'(1,1,1,2)(2,1,1,2),(3,1,1,2),(4,1,1,2),,,(1,X,1,2),(2,X,1,2),(3,X,1,2),(4,X,1,2)

'(1,1,Y,2)(2,1,Y,1),(3,1,Y,1),(4,1,1,1),,,(1,X,Y,2),(2,X,Y,2),(3,X,Y,2),(4,X,Y,2)
'-----------------------------------------------------------------------------------

' Save 2 copies of palette
'PalSize = 4 * PICW * PICH     ' Bytes
'CopyMemory PalBGR(1, 1, 1, 2), PalBGR(1, 1, 1, 1), PalSize
'CopyMemory PalBGR(1, 1, 1, 3), PalBGR(1, 1, 1, 1), PalSize

End Sub

Private Sub GETDIBS(ByVal PICIM As Long)

' PICIM is PIC(0).Image - handle to picbox memory
' from which pixels will be extracted and
' stored in PalBGRA()

On Error GoTo DIBError

'Get info on picture loaded into PIC
GetObjectAPI PICIM, Len(bmp), bmp

NewDC = CreateCompatibleDC(0&)
OldH = SelectObject(NewDC, PICIM)

' Set up bm struc for GetDIBits & StretchDIBits
With bm.bmiH
   .biSize = 40
   .biwidth = bmp.bmWidth
   .biheight = bmp.bmHeight
   .biPlanes = 1
   .biBitCount = 32          ' Sets up BGRA pixels
   .biCompression = 0
   BytesPerScanLine = ((((.biwidth * .biBitCount) + 31) \ 32) * 4)
   PadBytesPerScanLine = _
       BytesPerScanLine - (((.biwidth * .biBitCount) + 7) \ 8)
   .biSizeImage = BytesPerScanLine * Abs(.biheight)
End With

' Not sure about necessity of  31) \ 32)  &  7) \ 8)
   
' Set PalBGR to receive color bytes BGRA
'ReDim PalBGR(4, PICW, PICH, 3) As Byte
ReDim PalBGR(4, PICW, PICH, 2) As Byte

' Load color bytes to 1st half of PalBGR
ret = GetDIBits(NewDC, PICIM, 0, PICH, PalBGR(1, 1, 1, 1), bm, 1)

' Clear mem
SelectObject NewDC, OldH
DeleteDC NewDC

PalSize = 4 * PICW * PICH     ' Bytes

Exit Sub
'==========
DIBError:
  MsgBox "Error"
  On Error GoTo 0
End Sub


Private Sub Form_Unload(Cancel As Integer)

Done = True

Erase PalBGR

'======================================================================
' Save last folders to ini file
IniSpec$ = PathSpec$ & AppName$ & ".ini"
Open IniSpec$ For Output As #1

Print #1, LoadDir$
Print #1, SaveDir$
Close #1
'======================================================================

End

End Sub





Private Sub Loadmcode(InFile$, MCCode() As Byte)
'Load machine code into InCode() byte array
On Error GoTo InFileErr
If Dir$(InFile$) = "" Then
   MsgBox (InFile$ & " missing")
   DoEvents
   Unload Form1
   End
End If
Open InFile$ For Binary As #1
MCSize& = LOF(1)
If MCSize& = 0 Then
InFileErr:
   MsgBox (InFile$ & " missing")
   DoEvents
   Unload Form1
   End
End If
ReDim MCCode(MCSize&)
Get #1, , MCCode
Close #1
On Error GoTo 0
End Sub


