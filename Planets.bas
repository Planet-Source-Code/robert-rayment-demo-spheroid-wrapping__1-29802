Attribute VB_Name = "Module2"
' Module2: Planets.bas

Option Base 1
DefLng A-W
DefSng X-Z

Public Sub Wrap_Rotate()

ReDim ixdent(PICH)
ReDim zrdsq(PICH)
ReDim PalLineCopy(4, PICW)    ' Zeros - BLACK

' Spheroid radius
zR = 0.5 * PICW

' Pre-calculate slice indentation & radius squared
For iy = 1 To PICH

   ixdent(iy) = zR - Sqr(iy * (2 * zR - iy)) + 2
   zrdsq(iy) = (0.5 * (PICW - 2 * ixdent(iy))) ^ 2

Next iy

' Source & destination x-centre coords
ixsc = PICW / 2
ixdc = PICW / 2

' Proportionality factor
zLsDpi = (PICW / 2) / pi#

Done = False

' For ASM
   MCODE.PICW = PICW
   MCODE.PICH = PICH
   MCODE.zLsDpi = zLsDpi
   MCODE.Ptrixdent = VarPtr(ixdent(1))
   MCODE.Ptrzrdsq = VarPtr(zrdsq(1))
   MCODE.PtrPalBGR = VarPtr(PalBGR(1, 1, 1, 1))

   ptrStruc = VarPtr(MCODE.PICW)
   ptMC = VarPtr(WrapRotateMC(1))

Do

   If Speed <> 0 Then
      If Not ASM Then
         Rotate
      Else
         ' ASM Rotate
         MCODE.ixsc = ixsc
         MCODE.ixdc = ixdc
         res = CallWindowProc(ptMC, ptrStruc, 0&, 0&, 0&)
      End If
   End If
   
   ShowPalBGR 2

   ' Shift source centre coord to rotate

   ' Rotate Left/Right
   ixsc = ixsc + Speed
   If ixsc > PICW Then ixsc = 1
   If ixsc <= 0 Then ixsc = PICW


   If GetInputState() <> 0 Then DoEvents

   'DoEvents

Loop Until Done

ReDim PalLineCopy(1, 1)

End Sub

Public Sub Rotate()  ' Called from Wrap_Rotate

For iy = PICH To 1 Step -1
      
   'ReDim PalLineCopy(4, PICW)    ' Zeros - BLACK

   For ixd = ixdent(iy) To PICW - ixdent(iy)
         
      zddx = ixdc - ixd
         
      zy = Sqr(zrdsq(iy) - zddx * zddx)
      zx = zddx
      
      'ztheta = zATan2(zy, zx)
      If zx <> 0 Then
         ztheta = Atn(zy / zx)
         If zx < 0 Then
            ztheta = ztheta + Sgn(zy) * pi#
         End If
      Else  ' zx=0
         If Abs(zy) > Abs(zx) Then   'Must be an overflow
            ztheta = Sgn(zy) * pi# / 2
         Else
            ztheta = 0   'Must be an underflow
         End If
      End If
      
      ixs = ixsc + zLsDpi * ztheta
      
      If ixs < 1 Then ixs = PICW + ixs
      If ixs > PICW Then ixs = ixs - PICW
      
      'PalLineCopy(1, ixd) = PalBGR(1, ixs, iy, 1)
      'PalLineCopy(2, ixd) = PalBGR(2, ixs, iy, 1)
      'PalLineCopy(3, ixd) = PalBGR(3, ixs, iy, 1)
      'PalLineCopy(4, ixd) = PalBGR(4, ixs, iy, 1)
      
      PalBGR(1, ixd, iy, 2) = PalBGR(1, ixs, iy, 1)
      PalBGR(2, ixd, iy, 2) = PalBGR(2, ixs, iy, 1)
      PalBGR(3, ixd, iy, 2) = PalBGR(3, ixs, iy, 1)
      
   
   Next ixd

   'CopyMemory PalBGR(1, 1, iy, 2), PalLineCopy(1, 1), 4 * PICW

Next iy

End Sub

Public Sub ShowPalBGR(N)

' Blit PalBGR(N) to Form

' N= 1,2

'Form1.Picture = LoadPicture()
Form1.Cls
'Form1.Visible = True
'FTop = 150: FLeft = 250: FWidth = 256: FHeight = 256

PalBGRPtr = VarPtr(PalBGR(1, 1, 1, N))

bm.bmiH.biwidth = PICW
bm.bmiH.biheight = PICH

If StretchDIBits(Form1.HDC, _
   FLeft, FTop, _
   FWidth, FHeight, _
   1, 1, _
   PICW, PICH, _
   ByVal PalBGRPtr, bm, _
   1, vbSrcCopy) = 0 Then
         
       Erase PalBGR
       MsgBox ("Blit Error")
       End
   
End If

Form1.Refresh

End Sub

Public Function zATan2(ByVal zy, ByVal zx)
' Find angle Atan from -pi#/2 to +pi#/2
' Public pi#
If zx <> 0 Then
   zATan2 = Atn(zy / zx)
   If (zx < 0) Then
      zATan2 = zATan2 + pi# * Sgn(zy)
      'If (zy < 0) Then zATan2 = zATan2 - pi# Else zATan2 = zATan2 + pi#
   End If
Else  ' zx=0
   If Abs(zy) > Abs(zx) Then   'Must be an overflow
      zATan2 = pi# / 2 * Sgn(zy)
      'If zy > 0 Then zATan2 = pi# / 2 Else zATan2 = -pi# / 2
   Else
      zATan2 = 0   'Must be an underflow
   End If
End If
End Function


